;;; symbol-navigation.el ---  Symbol at point navigation -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Extracted symbol navigation functions from highlight-symbol.el
;; This code provides functions to jump to the next and previous
;; occurrences of the symbol at point, without any highlighting.

;;; Code:

(require 'thingatpt)

(defconst highlight-symbol-border-pattern
  (if (>= emacs-major-version 22) '("\\_<" . "\\_>") '("\\<" . "\\>")))

(defcustom highlight-symbol-ignore-list '()
  "List of regexp rules that specifies what symbols should not be navigated."
  :type '(repeat string)
  :group 'highlight-symbol)

(defun highlight-symbol-get-symbol ()
  "Return a regular expression identifying the symbol at point.
Returns nil if no symbol is at point or if the symbol is in
`highlight-symbol-ignore-list'."
  (let ((symbol (thing-at-point 'symbol)))
    (when (and symbol
               (not (member 0 (mapcar
                               (lambda (e) (string-match e symbol))
                               highlight-symbol-ignore-list))))
      (concat (car highlight-symbol-border-pattern)
              (regexp-quote symbol)
              (cdr highlight-symbol-border-pattern)))))

(defun highlight-symbol-jump (dir)
  "Jump to the next or previous occurence of the symbol at point.
DIR must be 1 for next, -1 for previous."
  (interactive)
  (let ((symbol (highlight-symbol-get-symbol)))
    (if symbol
        (let* ((case-fold-search nil)
               (bounds (bounds-of-thing-at-point 'symbol))
               (offset (- (point) (if (< 0 dir) (cdr bounds) (car bounds)))))
          (unless (eq last-command 'highlight-symbol-jump)
            (push-mark))
          (goto-char (- (point) offset))
          (let ((target (re-search-forward symbol nil t dir)))
            (unless target
              (goto-char (if (< 0 dir) (point-min) (point-max)))
              (message "Continued from %s of buffer" (if (< 0 dir) "beginning" "end"))
              (setq target (re-search-forward symbol nil nil dir)))
            (goto-char (+ target offset)))
          (setq this-command 'highlight-symbol-jump))
      (message "No symbol at point"))))

;;;###autoload
(defun next-symbol ()
  "Jump to the next location of the symbol at point within the buffer."
  (interactive)
  (highlight-symbol-jump 1))

;;;###autoload
(defun prev-symbol ()
  "Jump to the previous location of the symbol at point within the buffer."
  (interactive)
  (highlight-symbol-jump -1))

;;;###autoload
(defun prev-symbol-in-defun ()
 "Jump to the next location of the symbol at point within the defun."
 (interactive)
 (save-restriction
   (narrow-to-defun)
   (highlight-symbol-jump 1)))

;;;###autoload
(defun next-symbol-in-defun ()
 "Jump to the previous location of the symbol at point within the defun."
 (interactive)
 (save-restriction
   (narrow-to-defun)
   (highlight-symbol-jump -1)))

(provide 'symbol-navigation)
;;; symbol-navigation.el ends here
