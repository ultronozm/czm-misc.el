;;; czm-misc.el --- miscellaneous elisp functions     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-misc.el
;; Package-Requires: ((emacs "29.1") (avy))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
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

;; This package contains general-purpose elisp functions that were
;; cluttering my init file.

;;; Code:

;;;###autoload
(defun czm-misc-resize-frame-to-bottom-third ()
  "Reshape current frame to occupy bottom third of the screen."
  (interactive)
  (if (frame-parameter (selected-frame) 'fullscreen)
      (toggle-frame-fullscreen))
  (redisplay)
  (let* ((_window-system-frame-alist
          (cdr (assq initial-window-system
                     window-system-default-frame-alist)))
         (width (display-pixel-width))
         (height (display-pixel-height))
         (frame-padding-chars (frame-parameter nil 'internal-border-width))
         (frame-padding (* (frame-char-height) frame-padding-chars))
         (frame-top (- height (/ height 3)))
         (frame-height (- (/ height 3) (* 2 frame-padding)))
         (frame-width (- width frame-padding))
         (frame-left 0))
    (set-frame-position (selected-frame) frame-left frame-top)
    (set-frame-size (selected-frame) frame-width frame-height t)))

;;;###autoload
(defun czm-misc-insert-date ()
    "Insert the current date and time."
    (interactive)
    (insert (format-time-string "%Y%m%dT%H%M%S")))

(defvar czm-misc-dired-popup-buffer nil)

;;;###autoload
(defun czm-misc-dired-popup ()
  "Toggle a pop-up buffer with `dired'."
  (interactive)
  (if (and czm-misc-dired-popup-buffer (get-buffer-window czm-misc-dired-popup-buffer))
      (delete-window (get-buffer-window czm-misc-dired-popup-buffer))
    (let ((buffer (dired-noselect default-directory)))
      (setq czm-misc-dired-popup-buffer buffer)
      (display-buffer-in-side-window buffer '((side . right) (slot . -1))))))

;;;###autoload
(defun czm-misc-reload-file ()
  "Reload the current file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-buffer (current-buffer))
    (find-file filename)))

;;;###autoload
(defun czm-misc-yank-escaped-string ()
  "Yank the current kill as a string literal."
  (interactive)
  (insert (prin1-to-string (substring-no-properties (current-kill 0)))))

;;;###autoload
(defun czm-misc-count-lines-matching-regex-in-dir (regex)
  "Count the number of lines matching REGEX in the current directory."
  (interactive (list (read-regexp "Regex: ")))
  (let* ((total-lines 0)
         (output-buffer (get-buffer-create "*File Line Counts*"))
         (current-directory default-directory)
         (directory-string (format "Directory: %s\n" current-directory))
         (regex-string (format "Regex: %s\n\n" regex)))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert directory-string)
      (insert regex-string)
      (dolist (file (directory-files-recursively current-directory regex))
        (when (and (not (file-directory-p file))
                   (string-match-p regex file))
          (let ((line-count 0))
            (with-temp-buffer
              (insert-file-contents file)
              (setq line-count (count-lines (point-min)
                                            (point-max))))
            (setq total-lines (+ total-lines line-count))
            (insert (format "%s : %d\n" (file-relative-name file current-directory)
                            line-count))))))
    (with-current-buffer output-buffer
      (insert (format "\nTotal lines in files matching %s: %d" regex total-lines)))
    (display-buffer output-buffer)))

(require 'avy)

(defun czm-misc-avy-copy ()
  "Copy and yank sexp selected with avy."
  (interactive)
  (let ((avy-action #'avy-action-copy))
    (when (avy-jump "(")
      (yank))))

(defun czm-misc-avy-jump ()
  "Goto or copy sexp selected with avy."
  (interactive)
  (let ((avy-action #'avy-action-goto))
    (avy-jump "(")))

(defun czm-misc-avy-goto-or-copy-line (&optional arg)
  (interactive "P")
  (if arg
      (avy-copy-line 1)
    (avy-goto-line)))

(defun czm-misc-delete-horizontal-space-in-region (start end)
  "Delete repeated whitespace between START and END."
  (interactive "r")
  (let ((end-marker (copy-marker end)))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\s-+" end-marker t)
        (replace-match " ")))))

(defun czm-misc-delete-horizontal-space-on-line ()
  "Delete repeated whitespace on current line."
  (interactive)
  (czm-misc-delete-horizontal-space-in-region
   (line-beginning-position)
   (line-end-position)))

(defun czm-misc-split-line-below ()
  "Split line below point."
  (interactive)
  (forward-line)
  (split-line))

(defun czm-misc-get-mark-and-pop ()
  "Get mark and pop it."
  (let ((pos (mark t)))
    (pop-mark)
    pos))

;; similar to transpose-regions
(defun czm-misc-transpose-abc-to-cba ()
  "Rearrange regions delimited by point and last three mark values.
Similar to `transpose-regions', but insensitive to order of point
and marks."
  (interactive)
  (let ((points
         (sort
          (list
           (point)
           (czm-misc-get-mark-and-pop)
           (czm-misc-get-mark-and-pop)
           (czm-misc-get-mark-and-pop))
          #'<)))
    (cl-destructuring-bind (pos-a pos-b pos-c end)
        points
      (let ((region-a (buffer-substring pos-a pos-b))
            (region-b (buffer-substring pos-b pos-c))
            (region-c (buffer-substring pos-c end)))
        (save-excursion
          (goto-char pos-a)
          (delete-region pos-a end)
          (insert region-c region-b region-a))))))

;;;###autoload
(defun czm-misc-show-advice (function-symbol)
  "Display all advice associated with FUNCTION-SYMBOL in *advice display* buffer."
  (interactive "aFunction: ")
  (let ((advice-list '())
        (display-buffer (get-buffer-create "*advice display*")))
    (advice-mapc
     (lambda (advice _props)
       (push advice advice-list))
     function-symbol)
    (with-current-buffer display-buffer
      (erase-buffer)
      (pp-display-expression advice-list "*advice display*"))
    advice-list))

(defun czm-misc-show-overlays-at-pt ()
  "Display all overlays at point in *ovs* buffer."
  (interactive)
  (pp-display-expression (mapcar (lambda (ov) (overlay-properties ov)) (overlays-at (point))) "*ovs*"))

(defun czm-misc-delete-indentation-t ()
  "Join current line to the following line and fix up whitespace."
  (interactive)
  (delete-indentation t))

(defun czm-misc-delete-indentation-nil ()
  "Join this line to the previous line and fix up whitespace."
  (interactive)
  (delete-indentation nil))

(defun czm-misc-split-window-below-variant ()
  "Split frame with current buffer on top and next buffer below."
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (other-window 1)
  (next-buffer)
  (other-window 1))

(defun czm-misc-split-window-right-variant ()
  "Split frame with current buffer at left and next buffer at right." (delete-other-windows)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (next-buffer)
  (other-window 1))

(defun czm-misc-clone-indirect-buffer-same-window ()
  "Clone the current buffer in-place."
  (interactive)
  (let (buf)
    (save-window-excursion
      (setq buf
            (call-interactively #'clone-indirect-buffer)))
    (switch-to-buffer buf)))

(defun czm-misc-double-split-window-below-and-delete ()
  "Split window below of quarter size, set to next buffer."
  (interactive)
  (split-window-below)
  (other-window 1)
  (split-window-below)
  (delete-window)
  (other-window 1)
  (next-buffer))

(defun czm-misc-double-split-window-right-and-delete ()
  "Split window right of quarter size, set to next buffer."
  (interactive)
  (split-window-right)
  (other-window 1)
  (split-window-right)
  (delete-window)
  (other-window 1)
  (next-buffer))

(defun czm-misc-kill-or-delete-region (start end &optional arg)
  "Kill or delete region based on the presence of an optional argument.
By default, kill region between START and END, copying result to
the kill ring.  If called with prefix ARG, then delete region without
copying."
  (interactive "r\nP")
  (if arg
      (delete-region start end)
    (kill-region start end)))

(defmacro czm-misc-computation-time (&rest body)
  "Compute the time it takes to evaluate BODY."
  `(let ((start-time (current-time)))
     ,@body
     (float-time (time-subtract (current-time) start-time))))

(provide 'czm-misc)
;;; czm-misc.el ends here
