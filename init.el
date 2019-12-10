;;; package --- Summary
;;; Commentary:
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
        (height . 45)))

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

;; flycheck
(use-package flycheck
  :init (global-flycheck-mode)
  :custom
  (flycheck-clang-language-standard "c++17")
  (flycheck-gcc-language-standard "c++17")
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
  ;; mini-buffer のサイズ
  (setq ivy-height 30))

(use-package lsp-mode
  :init
  (setq lsp-prefer-flymake :none))
(use-package lsp-ui
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))
(use-package company-lsp)
;;------------------------------------------------------------------------------
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;---------------------------------------------------------------------------
(add-to-list 'default-frame-alist '(font . "Monaco-13" ))

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
  (doom-themes-org-config))
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
;;スクロールは１行ごと
(setq scroll-conservatively 1)
(setq scroll-preserve-screen-position 'always)
;;macのoptionをメタキィにする
(setq mac-option-modifier 'meta)
;; カッコの自動対応
(electric-pair-mode 1)
;;shellの設定を引き継ぐ
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))
;;最後に改行を入れる
(setq require-final-newline t)
;; 自動で空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;------------------------------------------------------------------------------
;;元に戻す
(global-set-key "\C-u" 'undo)
;;buffer listを現在のウィンドウに表示
(global-set-key "\C-x\C-b" 'buffer-menu)

;; expand-region.el
(use-package expand-region
  :init
  (global-set-key (kbd "C-@") 'er/expand-region)
  (global-set-key (kbd "C-M-@") 'er/contract-region))
;; irony (for c++)
(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-common-hook 'irony-mode)
  ;; C++言語用にコンパイルオプションを設定する.
  (add-hook 'c++-mode-hook
            '(lambda ()
               (setq irony-additional-clang-options '("-std=c++14" "-Wall" "-Wextra"))))
  (add-hook 'c++mode-hook
            (lambda ()
              (setq flyckeck-clang-include-path
                    (list ("/opt/local/include")))))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-cmpile-options))
(use-package company-irony
  :defer t
  :config
  ;; companyの補完のバックエンドにironyを使用する.
  (add-to-list 'company-backends '(company-irony-c-headers company-irony)))
;; cc mode settings
(use-package cc-mode
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq c-default-style "k&r")
              (setq indent-tabs-mode nil)
              (setq c-basic-offset 4))))
;; Proof General
(load "~/.emacs.d/lisp/PG/generic/proof-site")

;; haskell intero
(use-package intero
  :config (add-hook 'haskell-mode-hook 'intero-mode)
  :custom (haskell-stylish-on-save t))

;; yasnippet
(use-package yasnippet
  :init (yas-global-mode 1)
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-x i i" . yas-insert-snippet)
              ("C-x i n" . yas-new-snippet)
              ("C-x i v" . yas-visit-snippet-file)
              ("C-x i l" . yas-describe-tables)
              ("C-x i g" . yas-reload-all)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

;; OCaml
(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode))
(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode))

;; pure script
(use-package psc-ide
  :init
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (turn-on-purescript-indentation)))
  (setq psc-ide-use-npm-bin t))
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; rust-mode
(use-package rustic
  :init
  (setq rustic-flycheck-setup-mode-line-p nil))
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
(use-package company-jedi)
(add-hook 'python-mode-hook
          (lambda () (setq python-indent-offset 4))
          (add-to-list 'company-backends 'company-jedi))
(add-to-list 'auto-mode-alist '("\\\.py\\\'" . python-mode))

;; magit
(use-package magit
  :config (global-set-key (kbd "C-x g") 'magit-status))
;; nasm
(use-package nasm-mode)
;; end of file
(provide 'init)
;;; init.el
