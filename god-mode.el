;;; god-mode.el --- God-like command entering minor mode

;; Copyright (C) 2013 Chris Done
;; Copyright (C) 2013 Magnar Sveen
;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/chrisdone/god-mode
;; Version: 2.7.2

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This library defines the following mapping:
;;
;; 1. All commands are assumed to be C-<something> unless otherwise
;;    indicated. Examples:
;;    * a    -> C-a
;;    * s    -> C-s
;;    * akny -> C-a C-k C-n C-y
;;   * `x s`  → `C-x s`
;;
;;   Note the use of space to produce `C-x s`.
;;
;; 2. `g' is a special key to indicate M-<something>. This means that
;;    there is no way to write `C-g' in this mode, you must therefore
;;    type `C-g' directly. Examples:
;;    * gf -> M-f
;;    * gx -> M-f
;;
;;    This key can be configured.
;;
;; 6. There is a convention of uppercase special keys to indicate
;;    two modifier keys in action. Those are:
;;    * Gx -> C-M-x
;;
;; 7. There is a literal interpretation key as `l' that can
;;    be used on chained commands, e.g.
;;
;;    * xs -> C-x C-s
;;    * xlb -> C-x b
;;    * xlh -> C-x h
;;    * xp -> C-x C-p
;;    * xx -> C-x C-x
;;
;;    This key can be configured.
;;
;; 8. There is a key (default `i') to disable God mode, similar to
;;    Vim's i.

;;; Code:

(defcustom god-literal-key
  " "
  "The key used for literal interpretation."
  :group 'god
  :type 'string)

(defcustom god-exempt-major-modes
  '(dired-mode
    help-mode
    grep-mode
    magit-log-edit-mode
    magit-status-mode
    vc-annotate-mode
    package-menu-mode
    Buffer-menu-mode)
  "List of major modes that should not start in god-local-mode."
  :group 'god
  :type '(function))

(defvar god-global-mode nil
  "Activate God mode on all buffers?")

;;;###autoload
(defun god-mode ()
  "Toggle global God mode."
  (interactive)
  (setq god-global-mode (not god-global-mode))
  (if god-global-mode
      (god-local-mode 1)
    (god-local-mode -1)))

(defvar god-local-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map [remap self-insert-command] 'god-mode-self-insert)
    (define-key map (kbd "z") 'repeat)
    (define-key map (kbd "i") 'god-local-mode)
    map))

(defvar god-mode-universal-argument-map
  (let ((map (copy-keymap universal-argument-map)))
    (define-key map (kbd "u") 'universal-argument-more)
    map)
  "Keymap used while processing \\[universal-argument] with god-mode on.")

(defadvice save&set-overriding-map (before god-mode-add-to-universal-argument-map (map) activate compile)
  "This is used to set special keybindings after C-u is pressed. When god-mode is active, intercept the call to add in our own keybindings."
  (if (and god-local-mode (equal universal-argument-map map))
      (setq map god-mode-universal-argument-map)))

;;;###autoload
(define-minor-mode god-local-mode
  "Minor mode for running commands."
  nil " God" god-local-mode-map
  (if god-local-mode
      (run-hooks 'god-mode-enabled-hook)
    (run-hooks 'god-mode-disabled-hook)))

(defun god-mode-self-insert ()
  "Handle self-insert keys."
 (interactive)
  (let ((key (aref (this-command-keys-vector) (- (length (this-command-keys-vector)) 1))))
    (god-mode-interpret-key key)))

(defun god-mode-interpret-key (&optional key key-string-so-far)
  "Interpret the given key. This function sometimes recurses."
  (setq key (or key (read-event key-string-so-far)))
  (setq key (god-mode-sanitized-key-string key))
  (setq key-string-so-far (or key-string-so-far ""))

  (setq key-string-so-far (key-string-after-consuming-key key key-string-so-far))
  (let* ((command (read-kbd-macro key-string-so-far))
         (binding (key-binding command)))
    (god-mode-execute-binding key-string-so-far binding))
)

(defun god-mode-sanitized-key-string (key)
  "TODO"
  (setq key (char-to-string key))
  (cond
   ((string= key " ") "SPC")
   ((eq key 'backspace) "DEL")
   (t key)
   )
  )

(defun key-string-after-consuming-key (key key-string-so-far)
  "TODO"
  (let ((key-consumed t) next-modifier next-key)
    (message key-string-so-far)
    (if (and (not (string= key-string-so-far "")) (string= key "SPC")) (setq key " "))
    (setq next-modifier
          (cond
           ((string= key god-literal-key) "")
           ((string= key "g") "M-")
           ((string= key "G") "C-M-")
           (t
            (setq key-consumed nil)
            "C-"
            )))
    (setq next-key (if key-consumed (god-mode-sanitized-key-string (read-event key-string-so-far)) key))
    (concat key-string-so-far " " next-modifier next-key)))

(defun god-mode-execute-binding (key-string binding)
  "Execute extended keymaps such as C-c, or if it is a command,
call it."
  (cond ((commandp binding)
         (setq this-original-command binding)
         (setq this-command binding)
         (setq real-this-command binding)    ;; `real-this-command'  is used by emacs to populate `last-repeatable-command', which is used by `repeat'.
         (call-interactively binding))
        ((keymapp binding)
         (god-mode-interpret-key nil key-string))
        (:else
         (error "God: Unknown key binding for `%s`" key-string))))

(add-hook 'after-change-major-mode-hook 'god-mode-maybe-activate)

(defun god-mode-maybe-activate ()
  "Activate God mode locally on individual buffers when appropriate."
  (when (and god-global-mode
             (not (minibufferp))
             (not (memq major-mode god-exempt-major-modes)))
    (god-local-mode 1)))

(provide 'god-mode)

;;; god-mode.el ends here
