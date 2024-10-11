;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (setq default-directory (file-name-directory buffer-file-name))))

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
)

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
			  (projects . 10)
			  (registers . 5)))
  (setq dashboard-item-shortcuts '((recents   . "r")
				   (bookmarks . "m")
				   (projects  . "p")
				   (agenda    . "a")
				   (registers . "e")))
  (setq dashboard-center-content nil)
  ;; configuracion para que no se abra treemas al abrir el dashboard
  (add-hook 'dashboard-mode-hook
	    (lambda ()
	      (when (treemacs-get-local-window)
		(delete-window (treemacs-get-local-window)))))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook)
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

(require 'org-tempo)

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))
;; :config
;; (setq persp-state-default-file "~/.config/emacs/sessions"))
(add-hook 'ibuffer-hook
	  (lambda ()
	    (persp-ibuffer-set-filter-groups)
	    (unless (eq ibuffer-sorting-mode 'alphabetic)
	      (ibuffer-do-sort-by-alphabetic))))
(add-hook 'kill-emacs-hook #'persp-state-save)

(use-package projectile
  :config
  (projectile-mode 1))

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

;; configuramos la tipografia
  ;;(set-frame-font "Hurmit Nerd Font 14" nil t)
;;(add-hook 'find-file-hook
  ;;        (lambda ()
    ;;        (setq default-directory (file-name-directory buffer-file-name))))

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

(use-package lua-mode)
(use-package rust-mode)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay        0.5
	  treemacs-directory-name-transformer      #'identity
	  treemacs-display-in-side-window          t
	  treemacs-eldoc-display                   'simple
	  treemacs-file-event-delay                2000
	  treemacs-file-extension-regex            treemacs-last-period-regex-value
	  treemacs-file-follow-delay               0.2
	  treemacs-file-name-transformer           #'identity
	  treemacs-follow-after-init               nil
	  treemacs-expand-after-init               nil
	  treemacs-find-workspace-method           'find-for-file-or-pick-first
	  treemacs-git-command-pipe                ""
	  treemacs-git-mode                        'none
	  treemacs-goto-tag-strategy               'refetch-index
	  treemacs-header-scroll-indicators        '(nil . "^^^^^^")
	  treemacs-hide-dot-git-directory          nil
	  treemacs-indentation                     2
	  treemacs-indentation-string              " "
	  treemacs-is-never-other-window           nil
	  treemacs-max-git-entries                 5000
	  treemacs-missing-project-action          'ask
	  treemacs-move-files-by-mouse-dragging    t
	  treemacs-move-forward-on-expand          nil
	  treemacs-no-png-images                   nil
	  treemacs-no-delete-other-windows         t
	  treemacs-project-follow-cleanup          nil
	  treemacs-no-persist                      nil
	  treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                        'left
	  treemacs-read-string-input               'from-child-frame
	  treemacs-recenter-distance               0.1
	  treemacs-recenter-after-file-follow      nil
	  treemacs-recenter-after-tag-follow       nil
	  treemacs-recenter-after-project-jump     'always
	  treemacs-recenter-after-project-expand   'on-distance
	  treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
	  treemacs-project-follow-into-home        nil
	  treemacs-show-cursor                     nil
	  treemacs-show-hidden-files               t
	  treemacs-silent-filewatch                nil
	  treemacs-silent-refresh                  nil
	  treemacs-sorting                         'alphabetic-asc
	  treemacs-select-when-already-in-treemacs 'move-back
	  treemacs-space-between-root-nodes        t
	  treemacs-tag-follow-cleanup              t
	  treemacs-tag-follow-delay                1.5
	  treemacs-text-scale                      nil
	  treemacs-user-mode-line-format           nil
	  treemacs-user-header-line-format         nil
	  treemacs-wide-toggle-width               70
	  treemacs-width                           35
	  treemacs-width-increment                 1
	  treemacs-width-is-initially-locked       t
	  treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 22)
    (with-eval-after-load 'treemacs
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    )
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'simple))
      (`(t . _)
       (treemacs-git-mode 'none)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t d"   . treemacs-select-directory)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(treemacs-start-on-boot)
