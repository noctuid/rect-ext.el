;;; rectangle-ext.el --- Extensions to rect.el. -*- lexical-binding: t -*-

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/rectangle-ext.el
;; Created: November 15, 2016
;; Keywords: rectangle
;; Package-Requires: ((cl-lib "0.5") (emacs "24.4"))
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides commands that simulate narrowing for a rectangular
;; selection.

;; For more information see the README in the online repository.

;;; Code:
;; (require 'rect)
(require 'cl-lib)

(defgroup rectangle-ext nil
  "Provides extensions to rect.el such as a narrowing command for rectangles."
  :group 'convenience
  :prefix 'rectangle-ext-)

(defvar-local rectangle-ext--narrowed-p nil)
(defvar-local rectangle-ext--original-buffer nil)
(defvar-local rectangle-ext--beg nil)
(defvar-local rectangle-ext--end nil)
(defvar rectangle-ext--replace-lines nil)

(defun rectangle-ext-narrow (beg end)
  "Simulate `narrow-to-region' for a rectangular selection.
BEG and END are the bounds of the rectangle and will default to the region
beginning and end."
  (interactive (list (region-beginning) (region-end)))
  (unless rectangle-ext--narrowed-p
    (let ((buffer (current-buffer))
          (rect-lines (extract-rectangle beg end))
          (new-buffer (generate-new-buffer (concat (buffer-name) "-narrowed"))))
      (setq rectangle-ext--narrowed-p t)
      (switch-to-buffer new-buffer)
      (dolist (line rect-lines)
        (insert line)
        (insert "\n"))
      (setq rectangle-ext--original-buffer buffer)
      (setq rectangle-ext--beg beg)
      (setq rectangle-ext--end end))))

(defun rectangle-ext--replace-line (startcol endcol)
  "Replace the region from STARTCOL to ENDCOL.
The next item in `rectangle-ext--replace-string' will be used as the replacement
text."
  (move-to-column startcol t)
  (delete-rectangle-line startcol endcol nil)
  (insert (pop rectangle-ext--replace-lines)))

(defun rectangle-ext-widen ()
  "Widen the currently narrowed rectangle."
  (interactive)
  (when rectangle-ext--original-buffer
    (let ((text (buffer-string))
          (buffer rectangle-ext--original-buffer)
          (beg rectangle-ext--beg)
          (end rectangle-ext--end))
      (kill-this-buffer)
      (switch-to-buffer buffer)
      (setq rectangle-ext--narrowed-p nil)
      (setq rectangle-ext--replace-lines (split-string text "\n+"))
      (apply-on-rectangle #'rectangle-ext--replace-line beg end))))

(with-eval-after-load 'evil
  (defmacro rectangle-ext-with-restriction (beg end &rest body)
    "For the rectangle delimited by BEG and END, execute BODY."
    (declare (indent 2))
    (let ((rect-lines (cl-gensym)))
      `(let ((,rect-lines (extract-rectangle ,beg ,end)))
         (with-temp-buffer
           (dolist (line ,rect-lines)
             (insert line)
             (insert "\n"))
           ,@body
           (setq rectangle-ext--replace-lines
                 (split-string (buffer-string) "\n+")))
         (apply-on-rectangle #'rectangle-ext--replace-line ,beg ,end))))

  (evil-define-command rectangle-ext-evil-rectangle (beg end command-string)
    "Alter a rectangular selection with an evil ex command.
For the rectangle delimited by BEG and END, execute the evil ex COMMAND-STRING."
    (interactive "<r><a>")
    (rectangle-ext-with-restriction beg end
      (let ((evil-ex-current-buffer (current-buffer)))
        (evil-ex-execute (concat "%" command-string)))))

  (evil-ex-define-cmd "B" #'rectangle-ext-evil-rectangle))

(provide 'rectangle-ext)
;;; rectangle-ext.el ends here
