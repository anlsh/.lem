
(in-package :lem-user)
(lem-vi-mode:vi-mode)

(let ((plugin-path "~/.lem/plugins"))
  (mapcar (lambda (x) (push x asdf:*central-registry*))
          (uiop/filesystem:subdirectories plugin-path)))

(setf lem-core::*default-prompt-gravity* :bottom-display)
(setf lem/prompt-window::*prompt-completion-window-gravity*
      :horizontally-above-window)
(setf lem/prompt-window::*fill-width* t)

(add-hook *prompt-after-activate-hook*
          (lambda ()
            (call-command 'lem/prompt-window::prompt-completion nil)))

(add-hook *prompt-deactivate-hook*
          (lambda ()
            (lem/completion-mode:completion-end)))

(ql:quickload :cl-git)
(ql:quickload "lem/legit")

(defun kill-current-buffer () (lem:kill-buffer (lem:current-buffer)))

(define-keys lem-vi-mode:*normal-keymap*
  ("Space f f" 'find-file)
  ("Space f s" 'save-current-buffer)
  ("Space Space" 'execute-command)
  ("Space h d k" 'describe-key)
  ("Space h d d" 'describe)
  ("Space p f" 'project-find-file)
  ("Space g g" 'legit-status)
  ("Space c d" 'find-definitions)
  ("Space w d" 'delete-active-window)
  ("Space b i" 'lem/list-buffers:list-buffers)
;;  ("Space b d" 'lem-user:kill-current-buffer)
  )
(asdf:load-system :lem-pareto)
(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer)
                      'lem-lisp-mode:lisp-mode)
              (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t)
              (change-buffer-mode buffer 'lem-pareto-mode:pareto-mode t))))