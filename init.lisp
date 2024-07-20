
(in-package :lem-user)
(lem-vi-mode:vi-mode)
;; (ql:quickload :lem/legit)

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

(defmacro define-keys* (&body args)
  (alexandria:with-gensyms (keymap-name)
    `(let ((,keymap-name (make-keymap)))
       ,@(loop for c in args
              collect (destructuring-bind (bind keymap-spec) c
                        (if (and (listp keymap-spec) (eq (first keymap-spec) 'quote))
                            `(define-key ,keymap-name ,bind ,keymap-spec)
                            `(define-key ,keymap-name ,bind (define-keys* ,@keymap-spec)))))
       ,keymap-name)))

(defparameter *lsp-keymap*
  (define-keys* ("d d" 'lem/language-mode:find-definitions)))

(let ((doom-normal-keymap 
        (define-keys*
          ;; Files
          ("f" (("f" 'find-file)
                ("s" 'save-current-buffer)))
          ;; Buffers
          ("b" (("i" 'lem/list-buffers:list-buffers)
                ("d" 'kill-current-buffer)
                ("n" 'lem:next-buffer)
                ("p" 'lem:previous-buffer)))
          ("Space" 'execute-command)
          ("g g" 'lem/legit:legit-status)
          ;; Projects
          ("p" (("f" 'lem-core/commands/project:project-find-file)
                ("c" 'lem-core/commands/project:project-save)
                ("d" 'lem-core/commands/project:project-unsave)
                ("p" 'lem-core/commands/project:project-switch)))
          ;; Windows
          ("w" (("d" 'lem:delete-active-window)
                ("v" 'lem:split-active-window-vertically)
                ("s" 'lem:split-active-window-horizontally)))
          ;; Quitting
          ("q" (("q" 'lem:exit-lem)))
          ;; LSP Mode
          ("c l" '*lsp-keymap*)
          ;; Help/describe
          ("h d" (("k" 'describe-key)
                  ("b" 'describe-bindings)
                  ("d" 'describe))))))
  (define-keys lem-vi-mode:*normal-keymap*
    ("Space" doom-normal-keymap)))

(defparameter *lisp-leader-keymap*
  (define-keys*
    ;; Compilation
    ("c" (("c" 'lem-lisp-mode:lisp-compile-defun)
          ("r" 'lem-lisp-mode:lisp-compile-region)
          ("l" 'lem-lisp-mode:lisp-compile-and-load-file)))
    ;; Macros
    ("m" (("e" 'lem-lisp-mode/macroexpand:lisp-macrostep-expand)
          ("a" 'lem-lisp-mode/macroexpand:lisp-macroexpand-all)))
    ;; Repl
    ("r" (("r" 'lem-lisp-mode:lisp-switch-to-repl-buffer)
          ("s" 'lem-lisp-mode/internal:lisp-listen-in-current-package)))))

(defmethod lem-vi-mode/core:mode-specific-keymaps ((mode lem-lisp-mode:lisp-mode))
  (let ((localleader (make-keymap :name "lisp-localleader-keymap")))
    (define-key localleader "," *lisp-leader-keymap*)
    (list localleader)))

(add-hook *find-file-hook*
          (lambda (buffer)
            (when (eq (buffer-major-mode buffer)
                      'lem-lisp-mode:lisp-mode)
              (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t))))

(defun executables ()
    (loop with path = (uiop:getenv "PATH")
                  for p in (uiop:split-string path :separator ":")
                  for dir = (probe-file p)
                  when (uiop:directory-exists-p dir)
                    append (uiop:directory-files dir)))

(defun find-executable (name)
    (find name (executables)
                  :test #'equalp
                          :key #'pathname-name))

(let ((c-lsp "clangd"))
  (if (not (find-executable c-lsp))
      (message "[Config Warning] Can't set up LSP configuration for C! No clang")
      (lem-lsp-mode:define-language-spec (c-spec lem-c-mode:c-mode)
        :language-id "c"
        :root-uri-patterns '("compile-commands.json")
        :connection-mode :stdio
        :command '("clangd"))))

;; (let ((google-init-file "~/.lem/google.lisp"))
;;   (when (uiop:file-exists-p google-init-file)
;;     (load google-init-file)))
