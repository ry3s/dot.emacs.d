;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;; from straight.el README
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)

;; use-packageをstraight.elにフォールバックする
(setq straight-use-package-by-default t)

;; windows size
(setq default-frame-alist
      '((width . 100)
        (height . 45)
        ))

;;補完
(use-package company
  :init
  (setq company-selection-wrap-around t)
  :bind
  (:map company-active-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-h" . nil))
  :config
  ;; ソート順
  (setq company-transformers '(company-sort-by-backend-importance))
  ;; case sensitive に補完
  (setq company-dabbrev-downcase nil)
  (setq completion-ignore-case t)
  (setq company-idle-delay 0)
  ;;
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)
  ;; すべてのバッファで有効にする
  (global-company-mode))

;; (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定


;; flycheck

(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-clang-language-standard "c++14")
  (flycheck-gcc-language-standard "c++14")
  (flycheck-python-flack8-executalbe "python3")
  (flyckeck-python-pycompile-executable "python3")
  (flyckeck-python-pylint-executable "python3"))

(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

(use-package ivy
  :bind
  ("C-x s" . swiper)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-height 30) ;; mini-buffer のサイズ
  )

;;------------------------------------------------------------------------------
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;---------------------------------------------------------------------------
(add-to-list 'default-frame-alist '(font . "Monaco-13" ))

;;(setq resize-mini-windows nil)
(setq mouse-drag-copy-region t)
;;スタートアップメッセージを表示しない
(setq inhibit-startup-message t)
;;バックアップファイルを作らない
(setq make-backup-files nil)
;;終了時にオートセーブファイルを削除
(setq delete-auto-save-files t)
;; メニューバーを非表示
(menu-bar-mode 1)
;; ツールバーを非表示
(if window-system (tool-bar-mode -1))
(if window-system (scroll-bar-mode 0))
;;ビープ音と画面フラッシュを消す
(setq ring-bell-function 'ignore)
;;テーマ
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-thmes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  )

;;カーソルの点滅をやめる
(blink-cursor-mode 0)
;;カーソル行のハイライト
(global-hl-line-mode t)
;;タブの挙動（左端ではインデント，それ以外はタブの挿入）
(setq tab-always-indent t)
;;タブをスペースに
(setq-default tab-width 4
              indent-tabs-mode nil)
;;列数を表示する
(column-number-mode t)
;;行数を表示する
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode))
;;(global-linum-mode t)
;;(setq linum-format "%4d|")
;;スクロールは１行ごと
(setq scroll-conservatively 1)
(setq scroll-preserve-screen-position 'always)
;;macのoptionをメタキィにする
(setq mac-option-modifier 'meta)
;; カッコの自動対応
(electric-pair-mode 1)
;;shellの設定を引き継ぐ
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))
;;最後に改行を入れる
(setq require-final-newline t)
;;行末の空白を表示
;;(setq-default show-trailing-whitespace t)
;;自動で空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;------------------------------------------------------------------------------
;;元に戻す
(global-set-key "\C-u" 'undo)
;;buffer listを現在のウィンドウに表示
(global-set-key "\C-x\C-b" 'buffer-menu)

;; cc mode settings
(use-package cc-mode
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq c-default-style "k&r")
              (setq indent-tabs-mode nil)
              (setq c-basic-offset 4)
              (c-toggle-electric-state 1)
              )))

;; Proof General
(load "~/.emacs.d/lisp/PG/generic/proof-site")

;; haskell intero
(use-package intero
  :config (add-hook 'haskell-mode-hook 'intero-mode)
  :custom (haskell-stylish-on-save t))

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-x i i" . yas-insert-snippet)
              ("C-x i n" . yas-new-snippet)
              ("C-x i v" . yas-visit-snippet-file)
              ("C-x i l" . yas-describe-tables)
              ("C-x i g" . yas-reload-all))
  :config
  (yas-global-mode 1)
  ;;(setq yas-prompt-functions '(yas-ido-prompt))
  )

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode))
(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; (use-package 'all-the-icons)
(use-package neotree
  :config
  (setq neo-show-hidden-files t)
  (global-set-key "\C-q" 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; rust-mode
(use-package rust-mode
  :config
  (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  )

;; which key
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))
(put 'downcase-region 'disabled nil)

;; elm
(use-package elm-mode)

;; purescript
(use-package purescript-mode)

;; python
(add-hook 'python-mode-hook
          (lambda () (setq python-indent-offset 4)))
;; end of file
