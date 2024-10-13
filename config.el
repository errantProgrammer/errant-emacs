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

(let ((scripts-dir (expand-file-name "scripts" user-emacs-directory)))
  (add-to-list 'load-path scripts-dir)
  (load(expand-file-name "load-init" scripts-dir))
  ;; (load(expand-file-name "latex" scripts-dir));; funciones para la configuracion de latex
)

(defun emacs-run-launcher ()
  "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame 
    (make-frame '((name . "emacs-run-launcher")
                  (minibuffer . only)
                  (fullscreen . t) ; fullscreen
                  (undecorated . t) ; remove title bar
                  ;;(auto-raise . t) ; focus on this frame
                  ;;(tool-bar-lines . 0)
                  ;;(menu-bar-lines . 0)
                  (internal-border-width . 10)
                  (width . 80)
                  (height . 11)))
                  (unwind-protect
                    (app-launcher-run-app)
                    (delete-frame))))

(use-package all-the-icons
:ensure t
:if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda() (all-the-icons-dired-mode t))))

(use-package nerd-icons
  :ensure t)

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-number t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))
(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-log-title "errantProgrammer")
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-display-icon-p t)
  (setq dashboard-items '((recents . 5)
			  (bookmarks . 5)
			  (projects . 5)
			  (registers . 5)))
  (setq dashboard-item-shortcuts '((recents   . "r")
				   (bookmarks . "m")
				   (projects  . "p")
				   (agenda    . "a")
				   (registers . "e")))
  (setq dashboard-center-content nil)
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook)
  ;; (setq dashboard-projects-display 'columns);; muestra los proyectos en columnas
  )

(use-package diminish)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
	  evil-want-keybinding nil
	  evil-vsplit-window-right t
	  evil-split-window-below t
	  evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
    (evil-mode))

(use-package evil-collection
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here 
  ;; for documentation purposes in case you need it.  
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))
(use-package evil-tutor)

;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))
;; Setting RETURN key in org-mode to follow links
  (setq org-return-follows-link  t)

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))

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
(setq-default line-spacing 0.12)

(use-package toc-org
:commands toc-org-enable
:init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook' 'org-ident-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(electric-indent-mode 1)
(setq org-edit-src-content-indentation 0)

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(require 'org-tempo)

(use-package general
  :config
  (general-evil-setup)
  ;; set up 'SPC' as the global leader key
  (general-create-definer errant/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "C-SPC") ;; access leader in insert mode

  (errant/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file")
    "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "TAB TAB" '(comment-line :wk "Comment lines")
    ";" '(comment-region :wk "Comment or uncomment region")
    "u" '(universal-argument :wk "Universal argument"))
  ;; find direct or file
  (errant/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired"))
  ;; shell
  (errant/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "m" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t v" '(vterm-toggle :wk "Toggle vterm"))
  ;;  Projectile
  (errant/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))
  ;; Buffer
  (errant/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(switch-to-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file")) 
  ;; Lenguajes de edicion de texto
  ;; Org-mode
  (errant/leader-keys
    "o m" '(org-mode :wk "Toggle org mode")
    "o t" '(toc-org-insert-toc :wk "Insert TOC")
    "o l" '(org-goto :wk "Search Heading Jump"))
  )

(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))

(use-package counsel
  :after ivy
  :diminish
  :config 
    (counsel-mode)
    (setq ivy-initial-inputs-alist nil)) ;; removes starting ^ regex in M-x

(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package neotree
  :config
  (setq neo-smart-open t
	neo-show-hidden-files t
	neo-window-width 55
	neo-window-fixed-size nil
	inhibit-compacting-font-caches t
	dashboard-projects-backend 'projectile
	projectile-switch-project-action 'neotree-projectile-action) 
  ;; truncate long file names in neotree
  (add-hook 'neo-after-create-hook
	    #'(lambda (_)
		(with-current-buffer (get-buffer neo-buffer-name)
		  (setq truncate-lines t)
		  (setq word-wrap nil)
		  (make-local-variable 'auto-hscroll-mode)
		  (setq auto-hscroll-mode nil))))
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Instalacion de projectile en caso no exista
;; (unless (package-installed-p 'projectile)
;;   (package-install 'projectile))
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)	   ; para mac-users
	      ("C-c p" . projectile-command-map)) ; para windows o linux users
  :config
  (when (file-exists-p "~/.emacs.d/projects.el")
    (load-file "~/.emacs.d/projects.el"))
 )

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

(windmove-default-keybindings);; habilita con shift para moverme entre ventanas4f
;; el hecho de movernos entre ventans es con shift + flecha
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(setq backup-directory-alist `(("." . "~/.emacs.d/save")))

(setq custom-safe-themes t)
(use-package doom-themes
:ensure t
:config
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-tokyo-night)

)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35
	doom-modeline-bar-width 5
	doom-modeline-persp-name t
	doom-modeline-persp-icon t))
  ;; (setq doom-mode-icon t)
  ;; (setq doom-modeline-buffer-state-icon t)
  ;; (setq doom-modeline-lsp-icon t)
  ;; (setq doom-modeline-workspace-name t)
  ;; (setq doom-modeline-project-detection 'auto)
  ;; )

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)
(setq find-file-visit-truename t)

;;  algunas variables
(setq inhibit-startup-message t
      recentf-max-saved-items 50);; t is true
;; numero de lineas
(global-display-line-numbers-mode +1)
(setq display-line-numbers-type 'relative)

;; Mostrar número de columna
(column-number-mode 1)

;; Márgenes laterales
(fringe-mode '(8 . 8))

;; Transpariencia
(set-frame-parameter (selected-frame) 'alpha '(95 . 95)) ;; Ajusta los valores para la transparencia
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))
