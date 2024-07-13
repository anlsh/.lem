
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

(define-command kill-current-buffer () ()  (lem-core/commands/window:kill-buffer (lem:current-buffer)))

(define-keys lem-vi-mode:*normal-keymap*
  ("Space f f" 'find-file)
  ("Space f s" 'save-current-buffer)
  ("Space Space" 'execute-command)
  ("Space h d k" 'describe-key)
  ("Space h d b" 'describe-bindings)
  ("Space h d d" 'describe)
  ("Space p f" 'project-find-file)
  ("Space g g" 'legit-status)
  ("Space c d" 'find-definitions)
  ("Space w d" 'delete-active-window)
  ("Space b i" 'lem/list-buffers:list-buffers)
  ("Space b d" 'kill-current-buffer))

(let ((compile-keymap (make-keymap :name "lisp-compile-keymap"))
      (macro-keymap (make-keymap :name "lisp-macro-keymap"))
      (local-keymap (make-keymap :name "lisp-local-leader-keymap")))
  ;; Compile keymap
  (define-key compile-keymap "c" 'lem-lisp-mode:lisp-compile-defun)
  (define-key compile-keymap "l" 'lem-lisp-mode:lisp-compile-and-load-file)
  ;; Macro keymap
  (define-key macro-keymap "e" 'lem-lisp-mode/macroexpand:lisp-macrostep-expand)
  (define-key macro-keymap "a" 'lem-lisp-mode/macroexpand:lisp-macroexpand-all)
  ;; Set up the local leader keymap
  (define-key local-keymap "c" compile-keymap)
  (define-key local-keymap "m" macro-keymap)
  (define-key local-keymap "r r" 'lem-lisp-mode:lisp-switch-to-repl-buffer)
  ;; Bind to , as local leader in evil mode!
  (defmethod lem-vi-mode/core:mode-specific-keymaps ((mode lem-lisp-mode:lisp-mode))
    (let ((localleader (make-keymap :name "lisp-localleader-keymap")))
      (define-key localleader "," local-keymap)
      (list localleader))))

(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer)
                      'lem-lisp-mode:lisp-mode)
              (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t))))
