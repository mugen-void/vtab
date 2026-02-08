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

(defvar vtab-mode)

(defun vtab--set-key (sym val)
  "Custom setter for key variables.
Update the variable and rebind keys if `vtab-mode' is active."
  (when (bound-and-true-p vtab-mode)
    (vtab--restore-keybindings))
  (set-default sym val)
  (when (bound-and-true-p vtab-mode)
    (vtab--setup-keybindings)))

(defcustom vtab-key-new-tab "M-s M-c"
  "Key sequence for creating a new tab."
  :type 'string
  :set #'vtab--set-key
  :group 'vtab)

(defcustom vtab-key-close-tab "M-s M-k"
  "Key sequence for closing the current tab."
  :type 'string
  :set #'vtab--set-key
  :group 'vtab)

(defcustom vtab-key-next-tab "M-s M-n"
  "Key sequence for switching to the next tab."
  :type 'string
  :set #'vtab--set-key
  :group 'vtab)

(defcustom vtab-key-prev-tab "M-s M-p"
  "Key sequence for switching to the previous tab."
  :type 'string
  :set #'vtab--set-key
  :group 'vtab)

(defcustom vtab-key-goto-tab "M-s M-s"
  "Key sequence for going to a tab by number."
  :type 'string
  :set #'vtab--set-key
  :group 'vtab)

(defcustom vtab-goto-keys
  '(("M-s 7" . 1) ("M-s 8" . 2) ("M-s 9" . 3) ("M-s 0" . 4)
    ("M-s u" . 5) ("M-s i" . 6) ("M-s o" . 7) ("M-s p" . 8)
    ("M-s j" . 9) ("M-s k" . 10) ("M-s l" . 11) ("M-s ;" . 12)
    ("M-s m" . 13) ("M-s ," . 14) ("M-s ." . 15) ("M-s /" . 16))
  "Alist mapping full key sequences to tab numbers.
Each element is (KEY-SEQUENCE . TAB-NUMBER).
KEY-SEQUENCE is a string like \"M-s 7\" or \"C-x t 1\"."
  :type '(alist :key-type string :value-type integer)
  :set #'vtab--set-key
  :group 'vtab)

;;;; Keymaps

;;;; Variables

(defvar vtab-mode nil
  "Non-nil if `vtab-mode' is enabled.
Set by `define-minor-mode'.")

(defvar vtab--buffer-name "*vtab*"
  "Name of the vertical tab bar buffer.")

(defvar vtab--saved-settings nil
  "Alist of settings saved before enabling `vtab-mode'.")

(defvar vtab--saved-keybindings nil
  "Alist of original keybindings saved before enabling `vtab-mode'.
Each element is (KEY-SEQUENCE . ORIGINAL-BINDING).")

(defvar vtab--last-tab-state nil
  "Cache of (CURRENT-INDEX . TAB-NAMES) for dirty checking.")

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

(defun vtab--setup-keybindings ()
  "Setup keybindings from defcustom variables.
Save original bindings to `vtab--saved-keybindings' for later restoration."
  (setq vtab--saved-keybindings nil)
  ;; Command keybindings
  (let ((bindings `((,vtab-key-new-tab . tab-new)
                    (,vtab-key-close-tab . tab-close)
                    (,vtab-key-next-tab . tab-next)
                    (,vtab-key-prev-tab . tab-previous)
                    (,vtab-key-goto-tab . vtab-goto-tab))))
    (dolist (binding bindings)
      (let* ((key-str (car binding))
             (command (cdr binding))
             (key (kbd key-str))
             (orig (lookup-key global-map key))
             (original (unless (numberp orig) orig)))
        (push (cons key-str original) vtab--saved-keybindings)
        (global-set-key key command))))
  ;; Direct tab selection keybindings
  (dolist (entry vtab-goto-keys)
    (let* ((key-str (car entry))
           (tab-num (cdr entry))
           (key (kbd key-str))
           (orig (lookup-key global-map key))
           (original (unless (numberp orig) orig)))
      (push (cons key-str original) vtab--saved-keybindings)
      (let ((n tab-num))
        (global-set-key key
                        (lambda () (interactive) (tab-bar-select-tab n)))))))

(defun vtab--restore-keybindings ()
  "Restore original keybindings from `vtab--saved-keybindings'."
  (dolist (entry vtab--saved-keybindings)
    (let* ((key-str (car entry))
           (original (cdr entry))
           (key (kbd key-str)))
      (if original
          (global-set-key key original)
        (global-unset-key key))))
  (setq vtab--saved-keybindings nil))

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
    (unless (equal new-state vtab--last-tab-state)
      (setq vtab--last-tab-state new-state)
      (let ((buf (get-buffer-create vtab--buffer-name)))
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
    (let ((win (get-buffer-window vtab--buffer-name)))
      (unless win
        (setq win (display-buffer-in-side-window
                   (get-buffer-create vtab--buffer-name)
                   `((side . ,vtab-side)
                     (window-width . ,vtab-window-width)))))
      ;; Exclude from other-window (C-x o)
      (set-window-parameter win 'no-other-window t)
      ;; Protect from delete-other-windows
      (set-window-parameter win 'no-delete-other-windows t))
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

(defun vtab--on-window-size-change (&optional _frame)
  "Hook function called after window size change.
Adjust the side window width to match `vtab-window-width'."
  (when (and vtab-mode (not vtab--resizing))
    (when-let ((win (get-buffer-window vtab--buffer-name)))
      (let ((current-width (window-width win)))
        (unless (= current-width vtab-window-width)
          (let ((vtab--resizing t))
            (window-resize win (- vtab-window-width current-width) t)))))))

(defun vtab--setup-new-frame (frame)
  "Setup vtab on new FRAME.
Hide top tab bar and show side window if `vtab-mode' is enabled."
  (set-frame-parameter frame 'tab-bar-lines 0)
  (when vtab-mode
    (with-selected-frame frame
      (vtab--ensure-visible))))

;;;; Enable / Disable

(defun vtab--enable ()
  "Internal function to enable `vtab-mode'."
  ;; Save original settings
  (setq vtab--saved-settings
        (list (cons 'tab-bar-show tab-bar-show)
              (cons 'tab-bar-new-tab-to tab-bar-new-tab-to)
              (cons 'tab-bar-new-tab-choice tab-bar-new-tab-choice)
              (cons 'tab-bar-lines (frame-parameter nil 'tab-bar-lines))))
  ;; Setup keybindings
  (vtab--setup-keybindings)
  ;; Enable tab-bar-mode but hide top tab bar
  (tab-bar-mode 1)
  (setq tab-bar-show nil)
  ;; Force hide top tab bar on all frames
  (modify-all-frames-parameters '((tab-bar-lines . 0)))
  ;; Hook to setup vtab on new frames (daemon/emacsclient support)
  (add-hook 'after-make-frame-functions #'vtab--setup-new-frame)
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
  ;; Show side window
  (vtab--ensure-visible))

(defun vtab--disable ()
  "Internal function to disable `vtab-mode'."
  ;; Clear dirty-check cache
  (setq vtab--last-tab-state nil)
  ;; Delete side window
  (when-let ((win (get-buffer-window vtab--buffer-name)))
    (delete-window win))
  ;; Remove frame hook
  (remove-hook 'after-make-frame-functions #'vtab--setup-new-frame)
  ;; Remove hooks
  (remove-hook 'tab-bar-tab-post-select-functions #'vtab--on-tab-select)
  (remove-hook 'tab-bar-tab-post-open-functions #'vtab--on-tab-open)
  (remove-hook 'window-buffer-change-functions #'vtab--on-buffer-change)
  (remove-hook 'org-agenda-finalize-hook #'vtab--on-org-agenda-finalize)
  (remove-hook 'window-size-change-functions #'vtab--on-window-size-change)
  ;; Remove from window-persistent-parameters
  (setq window-persistent-parameters
        (delete '(no-delete-other-windows . t) window-persistent-parameters))
  ;; Restore keybindings
  (vtab--restore-keybindings)
  ;; Restore original settings
  (when vtab--saved-settings
    (setq tab-bar-show (alist-get 'tab-bar-show vtab--saved-settings))
    (setq tab-bar-new-tab-to (alist-get 'tab-bar-new-tab-to vtab--saved-settings))
    (setq tab-bar-new-tab-choice (alist-get 'tab-bar-new-tab-choice vtab--saved-settings))
    (modify-all-frames-parameters
     `((tab-bar-lines . ,(alist-get 'tab-bar-lines vtab--saved-settings))))))

;;;; Minor Mode

;;;###autoload
(define-minor-mode vtab-mode
  "Toggle vertical tab bar display.
   When enabled, displays a vertical tab bar in a side window."
  :global t
  :group 'vtab
  :lighter " VTab"
  (if vtab-mode
      (vtab--enable)
    (vtab--disable)))

(provide 'vtab)

;;; vtab.el ends here
