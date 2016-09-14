;----------------------;
;;; Custom Functions ;;;
;----------------------;

; unfill a paragraph, i.e., make it so the text does not wrap in the
; paragraph where the cursor is
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

; unfill a region, i.e., make is so the text in that region does not
; wrap
(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

(defun make-plugin-path (plugin)
  (expand-file-name
   (concat plugin-path plugin)))

(defun include-plugin (plugin)
  (add-to-list 'load-path (make-plugin-path plugin)))

(defun make-elget-path (plugin)
  (expand-file-name
   (concat elget-path plugin)))

(defun include-elget-plugin (plugin)
  (add-to-list 'load-path (make-elget-path plugin)))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

;; Go to the matching parenthesis when you press
;; % if on parenthesis otherwise insert %
(defun goto-matching-paren-or-insert (arg)
(interactive "p")
(cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
      ((looking-at "[])}]") (forward-char) (backward-sexp 1))
      (t (self-insert-command (or arg 1)))))

; --- definition of the Siara c-style
   
     (defconst siara-c-style
       '((c-comment-only-line-offset    . 0)
         (c-hanging-braces-alist        . ((substatement-open after)
                                           (defun-open before after)
                                           (defun-close before after)
                                           (block-open after)
                                           (block-close . c-snug-do-while)
                                           (brace-list-open)
                                           (brace-list-close)
                                           (extern-lang-open after)))
         (c-hanging-colons-alist        . ((member-init-intro before)
                                           (inher-intro)
                                           (case-label after)
                                           (label after)
                                           (access-label after)))
         (c-cleanup-list                . (brace-else-brace
                                           brace-elseif-brace
                                           scope-operator
                                           empty-defun-braces
                                           defun-close-semi))
         (c-offsets-alist               . ((arglist-close . c-lineup-arglist)
                                           (substatement-open    . 0)
                                           (case-label           . 2)
                                           (statement-case-intro . 2)
                                           (knr-argdecl-intro    . -)
                                           (member-init-intro    . ++)))
         (c-echo-syntactic-information-p . t)
         )
       "Siara C Programming Style")

; --- customizations for c-mode, c++-mode, and objc-mode

     (defun siara-c-mode-common-hook ()
       (c-add-style "SIARA" siara-c-style t)
       ;;(c-toggle-auto-state          1) ; auto newlines
       (setq c-tab-always-indent     t)
       (setq c-basic-offset          4)
       ;;(setq tab-width               4)
       (setq indent-tabs-mode      nil) ; don't insert tab chars
       ;; keybindings for C, C++, and Objective-C.  We can put these in
       ;; c-mode-map because c++-mode-map and objc-mode-map inherit it
       (define-key c-mode-map "\C-m" 'newline-and-indent)
     )

(provide 'custom-functions)







