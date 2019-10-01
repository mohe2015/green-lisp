;; enable line numbers
(global-display-line-numbers-mode)

;; show parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; MELPA
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)
;(package-refresh-contents)

(package-install 'sly)
(package-install 'sly-asdf)
(package-install 'sly-named-readtables)
(package-install 'sly-repl-ansi-color)
(package-install 'sly-macrostep)
(package-install 'sly-quicklisp)

(package-install 'dracula-theme)

;(add-to-list 'load-path "~/sly")
;(require 'sly-autoloads)
(setq inferior-lisp-program "sbcl")

(require 'dracula-theme)

(sly)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(package-selected-packages (quote (company dracula-theme slime)))
 '(safe-local-variable-values (quote ((Base . 10) (Syntax . ANSI-Common-Lisp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
