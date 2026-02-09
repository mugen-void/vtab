;;; vtab.el --- Vertical tab bar -*- lexical-binding: t; -*-

;; Author: mugen <mugen.void42@gmail.com>
;; URL: https://github.com/mugen-void/vtab
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, frames
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 2025 mugen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extends Emacs `tab-bar-mode' to display a vertical tab bar in a side window.
;;
;; Usage:
;;   (require 'vtab)
;;   (vtab-mode 1)
;;
;; Keybindings (M-s prefix by default):
;;   M-s M-c  New tab
;;   M-s M-k  Close tab
;;   M-s M-n  Next tab
;;   M-s M-p  Previous tab
;;   M-s M-s  Go to tab by number
;;   M-s [key]  Direct tab selection (right-hand layout):
;;     7890 -> tab 1-4,  uiop -> tab 5-8
;;     jkl; -> tab 9-12, m,./ -> tab 13-16
;;   Customize via (define-key vtab-mode-map ...)

;;; Code:

(require 'tab-bar)
(require 'seq)

;;;; Customization

(defgroup vtab nil
  "Vertical tab bar."
  :group 'tab-bar
  :prefix "vtab-")

(defcustom vtab-new-tab-position 'rightmost
  "Position where new tabs are inserted."
  :type '(choice (const :tag "End" rightmost)
                 (const :tag "Beginning" leftmost)
                 (const :tag "Right of current" right)
                 (const :tag "Left of current" left))
  :group 'vtab)

(defcustom vtab-window-width 25
  "Width of the side window."
  :type 'integer
  :group 'vtab)

(defcustom vtab-side 'left
  "Side where the tab bar is displayed."
  :type '(choice (const :tag "Left" left)
                 (const :tag "Right" right))
  :group 'vtab)

(defcustom vtab-new-tab-choice "*scratch*"
  "Buffer to display in new tabs."
  :type 'string
  :group 'vtab)

(defcustom vtab-style-window-divider t
  "Non-nil means vtab sets window-divider to 1px thin line when enabled."
  :type 'boolean
  :group 'vtab)

(defcustom vtab-style-fringe t
  "Non-nil means vtab makes fringe background transparent when enabled."
  :type 'boolean
  :group 'vtab)

(defvar vtab-mode) ; Forward declaration for byte-compiler; defined by `define-minor-mode'.

;;;; Keymaps

(defvar vtab-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-s M-c") #'tab-new)
    (define-key map (kbd "M-s M-k") #'tab-close)
    (define-key map (kbd "M-s M-n") #'tab-next)
    (define-key map (kbd "M-s M-p") #'tab-previous)
    (define-key map (kbd "M-s M-s") #'vtab-goto-tab)
    ;; Direct tab selection (right-hand layout)
    (dotimes (i 16)
      (let* ((n (1+ i))
             (keys ["7" "8" "9" "0" "u" "i" "o" "p"
                    "j" "k" "l" ";" "m" "," "." "/"])
             (key (aref keys i)))
        (define-key map (kbd (format "M-s %s" key))
                    (lambda () (interactive) (tab-bar-select-tab n)))))
    map)
  "Keymap for `vtab-mode'.")

;;;; Variables

(defun vtab--get-buffer (&optional frame)
  "Get or create the vtab buffer for FRAME.
Each frame gets its own dedicated buffer stored as a frame parameter."
  (let* ((f (or frame (selected-frame)))
         (buf (frame-parameter f 'vtab--buffer)))
    (if (buffer-live-p buf)
        buf
      (let ((new-buf (generate-new-buffer " *vtab*")))
        (set-frame-parameter f 'vtab--buffer new-buf)
        new-buf))))

(defvar vtab--saved-settings nil
  "Alist of settings saved before enabling `vtab-mode'.")

(defvar vtab--resizing nil
  "Non-nil while vtab is resizing window to prevent infinite loop.")

(defvar vtab--buffer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'vtab--click)
    (define-key map (kbd "RET") #'vtab--select)
    map)
  "Keymap used in the vertical tab bar buffer.")

;;;; Faces

(defface vtab-active-face
  '((t :background "#3a3a8a" :foreground "#aaaaaa" :weight bold))
  "Face for the active tab.")

;;;; Internal Functions

(defun vtab--current-tab-index ()
  "Return the index of the current tab."
  (seq-position (tab-bar-tabs)
                'current-tab
                (lambda (tab _) (eq (car tab) 'current-tab))))

(defun vtab--get-tabs ()
  "Return list of tab names."
  (mapcar (lambda (tab)
            (alist-get 'name tab))
          (tab-bar-tabs)))

(defun vtab--refresh ()
  "Refresh the vertical tab bar buffer."
  (let* ((tabs (vtab--get-tabs))
         (current (vtab--current-tab-index))
         (new-state (cons current tabs)))
    (when (and current
               (not (equal new-state (frame-parameter nil 'vtab--tab-state))))
      (set-frame-parameter nil 'vtab--tab-state new-state)
      (let ((buf (vtab--get-buffer)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (dotimes (i (length tabs))
              (let* ((name (nth i tabs))
                     (is-current (= i current))
                     (marker (if is-current ">" " "))
                     (line (format "%s %d: %s\n" marker (1+ i) name)))
                (insert (propertize line
                                    'vtab-index (1+ i)
                                    'mouse-face 'highlight
                                    'keymap vtab--buffer-keymap
                                    'face (when is-current 'vtab-active-face)))))
            (setq buffer-read-only t)))))))

(defun vtab--click (event)
  "Select tab by mouse click EVENT."
  (interactive "e")
  (let* ((pos (posn-point (event-end event)))
         (idx (get-text-property pos 'vtab-index)))
    (when idx
      (tab-bar-select-tab idx)
      (vtab--refresh))))

(defun vtab--select ()
  "Select tab at point."
  (interactive)
  (let ((idx (get-text-property (point) 'vtab-index)))
    (when idx
      (tab-bar-select-tab idx)
      (vtab--refresh))))

(defun vtab--ensure-visible ()
  "Ensure the vertical tab bar is visible when `vtab-mode' is enabled."
  (when vtab-mode
    (let ((win (get-buffer-window (vtab--get-buffer))))
      (unless win
        (setq win (display-buffer-in-side-window
                   (vtab--get-buffer)
                   `((side . ,vtab-side)
                     (window-width . ,vtab-window-width)))))
      ;; Exclude from other-window (C-x o)
      (set-window-parameter win 'no-other-window t)
      ;; Protect from delete-other-windows
      (set-window-parameter win 'no-delete-other-windows t)
      ;; Remove fringes for clean border
      (set-window-fringes win 0 0))
    (vtab--refresh)))

;;;; Commands

(defun vtab-goto-tab ()
  "Switch to a tab by number."
  (interactive)
  (let ((num (read-number "Tab number: ")))
    (tab-bar-select-tab num)))

;;;; Hook Functions

(defun vtab--on-tab-select (&rest _)
  "Hook function called after tab selection."
  (vtab--ensure-visible))

(defun vtab--on-tab-open (&rest _)
  "Hook function called after tab creation."
  (vtab--ensure-visible))

(defun vtab--on-buffer-change (&rest _)
  "Hook function called after buffer change."
  (when vtab-mode
    (vtab--refresh)))

(defun vtab--on-org-agenda-finalize ()
  "Hook function called after `org-agenda' display."
  (vtab--ensure-visible))

(defun vtab--on-window-size-change (&optional frame)
  "Hook function called after window size change.
Adjust the side window width to match `vtab-window-width'."
  (when (and vtab-mode (not vtab--resizing))
    (let ((f (or frame (selected-frame))))
      (when-let* ((win (get-buffer-window (vtab--get-buffer f) f)))
        (let ((current-width (window-width win)))
          (unless (= current-width vtab-window-width)
            (let ((vtab--resizing t))
              (window-resize win (- vtab-window-width current-width) t))))))))

(defun vtab--setup-new-frame (frame)
  "Setup vtab on new FRAME.
Hide top tab bar and show side window if `vtab-mode' is enabled."
  (set-frame-parameter frame 'tab-bar-lines 0)
  (when vtab-mode
    (with-selected-frame frame
      (vtab--ensure-visible))))

(defun vtab--on-frame-delete (frame)
  "Clean up vtab resources for FRAME."
  (when-let* ((buf (frame-parameter frame 'vtab--buffer)))
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (set-frame-parameter frame 'vtab--buffer nil)
  (set-frame-parameter frame 'vtab--tab-state nil))

;;;; Enable / Disable

(defun vtab--enable ()
  "Internal function to enable `vtab-mode'."
  ;; Save original settings
  (setq vtab--saved-settings
        (list (cons 'tab-bar-show tab-bar-show)
              (cons 'tab-bar-new-tab-to tab-bar-new-tab-to)
              (cons 'tab-bar-new-tab-choice tab-bar-new-tab-choice)
              (cons 'tab-bar-lines (frame-parameter nil 'tab-bar-lines))))
  (when vtab-style-window-divider
    (push (cons 'window-divider-mode (bound-and-true-p window-divider-mode))
          vtab--saved-settings)
    (push (cons 'window-divider-default-right-width window-divider-default-right-width)
          vtab--saved-settings)
    (push (cons 'window-divider-default-places window-divider-default-places)
          vtab--saved-settings))
  (when vtab-style-fringe
    (push (cons 'fringe-background (face-background 'fringe nil t))
          vtab--saved-settings))
  ;; Enable tab-bar-mode but hide top tab bar
  (tab-bar-mode 1)
  (setq tab-bar-show nil)
  ;; Force hide top tab bar on all frames
  (modify-all-frames-parameters '((tab-bar-lines . 0)))
  ;; Hook to setup vtab on new frames (daemon/emacsclient support)
  (add-hook 'after-make-frame-functions #'vtab--setup-new-frame)
  ;; Hook to clean up vtab on frame deletion
  (add-hook 'delete-frame-functions #'vtab--on-frame-delete)
  ;; Apply defcustom values
  (setq tab-bar-new-tab-to vtab-new-tab-position)
  (setq tab-bar-new-tab-choice vtab-new-tab-choice)
  ;; Add hooks
  (add-hook 'tab-bar-tab-post-select-functions #'vtab--on-tab-select)
  (add-hook 'tab-bar-tab-post-open-functions #'vtab--on-tab-open)
  (add-hook 'window-buffer-change-functions #'vtab--on-buffer-change)
  (add-hook 'org-agenda-finalize-hook #'vtab--on-org-agenda-finalize)
  (add-hook 'window-size-change-functions #'vtab--on-window-size-change)
  ;; Add to window-persistent-parameters
  (add-to-list 'window-persistent-parameters '(no-delete-other-windows . t))
  ;; Thin window divider
  (when vtab-style-window-divider
    (setq window-divider-default-right-width 1)
    (setq window-divider-default-places 'right-only)
    (window-divider-mode 1))
  ;; Make fringe background transparent for clean border
  (when vtab-style-fringe
    (set-face-background 'fringe nil))
  ;; Show side window
  (vtab--ensure-visible))

(defun vtab--disable ()
  "Internal function to disable `vtab-mode'."
  ;; Clean up all frames: windows, buffers, and frame parameters
  (dolist (frame (frame-list))
    (when-let* ((buf (frame-parameter frame 'vtab--buffer)))
      (when-let* ((win (get-buffer-window buf frame)))
        (delete-window win))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (set-frame-parameter frame 'vtab--buffer nil)
    (set-frame-parameter frame 'vtab--tab-state nil))
  ;; Remove frame hooks
  (remove-hook 'after-make-frame-functions #'vtab--setup-new-frame)
  (remove-hook 'delete-frame-functions #'vtab--on-frame-delete)
  ;; Remove hooks
  (remove-hook 'tab-bar-tab-post-select-functions #'vtab--on-tab-select)
  (remove-hook 'tab-bar-tab-post-open-functions #'vtab--on-tab-open)
  (remove-hook 'window-buffer-change-functions #'vtab--on-buffer-change)
  (remove-hook 'org-agenda-finalize-hook #'vtab--on-org-agenda-finalize)
  (remove-hook 'window-size-change-functions #'vtab--on-window-size-change)
  ;; Remove from window-persistent-parameters
  (setq window-persistent-parameters
        (delete '(no-delete-other-windows . t) window-persistent-parameters))
  ;; Restore original settings
  (when vtab--saved-settings
    (setq tab-bar-show (alist-get 'tab-bar-show vtab--saved-settings))
    (setq tab-bar-new-tab-to (alist-get 'tab-bar-new-tab-to vtab--saved-settings))
    (setq tab-bar-new-tab-choice (alist-get 'tab-bar-new-tab-choice vtab--saved-settings))
    (modify-all-frames-parameters
     `((tab-bar-lines . ,(alist-get 'tab-bar-lines vtab--saved-settings))))
    ;; Restore window divider settings (only if saved)
    (when (assq 'window-divider-mode vtab--saved-settings)
      (setq window-divider-default-right-width
            (alist-get 'window-divider-default-right-width vtab--saved-settings))
      (setq window-divider-default-places
            (alist-get 'window-divider-default-places vtab--saved-settings))
      (window-divider-mode (if (alist-get 'window-divider-mode vtab--saved-settings) 1 -1)))
    ;; Restore fringe background (only if saved)
    (when (assq 'fringe-background vtab--saved-settings)
      (set-face-background 'fringe (alist-get 'fringe-background vtab--saved-settings)))))

;;;; Minor Mode

;;;###autoload
(define-minor-mode vtab-mode
  "Toggle vertical tab bar display.
When enabled, displays a vertical tab bar in a side window."
  :global t
  :group 'vtab
  :lighter " VTab"
  :keymap vtab-mode-map
  (if vtab-mode
      (vtab--enable)
    (vtab--disable)))

(provide 'vtab)

;;; vtab.el ends here
