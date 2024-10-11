(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org". "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
(package-refresh-contents))

;;Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
(package-install 'use-package))
;; Configuracion para el auto update de los paquetes
(require 'use-package)
(setq use-package-always-ensure t)
(use-package auto-package-update
:custom
(auto-package-update-interval 7)
(auto-package-update-prompt-before-update t)
(auto-package-update-hide-results t)
:config
(auto-package-update-maybe)
(auto-package-update-at-time "09:00"))

(use-package nerd-icons
  :ensure t)

(use-package all-the-icons
:ensure t
:if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda() (all-the-icons-dired-mode t))))

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode +1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "errantProgrammer")
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (registers . 5)))
  (setq dashboard-item-shortcuts '((recents   . "r")
			       (bookmarks . "m")
			       (projects  . "p")
			       (agenda    . "a")
			       (registers . "e")))
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-item-names '(("Recent Files:"               . "Recently opened files:")
			   ("Agenda for today:"               . "Today's agenda:")
			   ("Agenda for the coming week:"     . "Agenda:")))
  (setq dashboard-display-icon-p t))

;; Set up the visible bell
(setq visible-bell t)

;; Archivo de configuracion general
;; Mueve todos mis archivos de trabajo a un solo lugar, para mantener limpio mi entorno de trabajo.
(use-package no-littering)
(setq auto-save-file-name-transforms
      `((".*",(no-littering-expand-var-file-name "auto-save/") t)))

;; Configuracion de la apariencia de emacs
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(load-theme );; para mi caso estoy usando doom emacs

(windmove-default-keybindings);; habilita con shift para moverme entre ventanas4f


;; cargar un thema
;;(require 'nerd-icons)
;;(use-package nerd-icons)

(use-package doom-themes
:ensure t
:config
(load-theme 'doom-tokyo-night))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35
	doom-modeline-bar-width 5
	doom-modeline-persp-name t
	doom-modeline-persp-icon t)
  (setq doom-mode-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-lsp-icon t)
  (setq doom-modeline-workspace-name t)
  )

;; Transpariencia
(set-frame-parameter (selected-frame) 'alpha '(95 . 95)) ;; Ajusta los valores para la transparencia
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(set-face-attribute 'default nil
		  :font "Hurmit Nerd Font"
		  :height 110
		  :weight 'medium)
 (set-face-attribute 'variable-pitch nil
		:font "Hurmit Nerd Font"
		:height 120
		:weight 'medium)
 (set-face-attribute 'fixed-pitch nil
		:font "Hurmit Nerd Font"
		:height 110
		:weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)
