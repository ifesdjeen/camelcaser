;;
;;
;;

(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun camelize-method (s)
  "Convert dashed-notation string S to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "-")) ""))

(defun camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (camelize-method txt)))
    (if cml (progn (delete-region beg end) (insert cml)))))


;; (add-hook 'post-command-hook 'camelcase-last-word)

(defun camelcase-last-word ()
  (interactive)
  (when (and (eq last-command 'self-insert-command)
             (looking-back "\\w\\W")
             (not (eq (char-before) ?-))
             (not (eq (char-after) ?-)))
    (save-excursion
      (backward-word) (camelscore-word-at-point) (forward-word) )))

(define-minor-mode toggle-camelcase-word
  "Toggle making words you type camelcase."
  nil nil nil
  (if toggle-camelcase-word
      (add-hook 'post-command-hook 'camelcase-last-word)
    (remove-hook 'post-command-hook 'camelcase-last-word)))
