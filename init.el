;; Prueba siguiendo el tutorial de DT
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
;; C-x C-s is save file
;; Carga de configuracion de Emacs

;; (let ((config-dir (expand-file-name "config" user-emacs-directory)))
;;   (add-to-list 'load-path config-dir)
;;   (load(expand-file-name "packages" config-dir))
;;   (load(expand-file-name "general" config-dir))
;;   (load(expand-file-name "appearance" config-dir))
;; )

;; (let ((package-dir (expand-file-name "config/package" user-emacs-directory)))
;;   (add-to-list 'load-path package-dir)
;;   (load "treemacs")                ;;Configuracion del treemacs
;; )

;; (let ((functions-dir (expand-file-name "config/functions" user-emacs-directory)))
;;   (add-to-list 'load-path functions-dir)
;;   (load(expand-file-name "general-functions" functions-dir))
;; )



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" default))
 '(package-selected-packages
   '(all-the-icons-completion all-the-icons-nerd-fonts rust-mode dashboard treemacs-tab-bar treemacs-projectile treemacs-perspective treemacs-persp treemacs-nerd-icons treemacs-magit treemacs-icons-dired treemacs-evil treemacs-all-the-icons no-littering nerd-icons-ibuffer nerd-icons-completion lsp-ui doom-themes doom-modeline dap-mode clang-format auto-package-update all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )