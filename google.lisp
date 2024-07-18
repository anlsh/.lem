(defvar *amoorthy-google-config* t)
;; Google-specific stuff
(lem-lsp-mode:define-language-spec (google3-spec lem-c-mode:c-mode)
  :language-id "google3C"
  ;; I have no idea what a correct file may identify a root of a Lua project
  :root-uri-patterns '("google3")
  :command '("/google/bin/releases/cider/ciderlsp/ciderlsp")
  :connection-mode :stdio)
