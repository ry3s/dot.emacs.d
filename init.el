;; -*- lexical-binding: t -*-
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

;; use-packageをstraight.elにフォールバックする (:straight t)
(setq straight-use-package-by-default t)

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
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)
  (defun edit-category-table-for-company-dabbrev (&optional table)
    (define-category ?s "word constituents for company-dabbrev" table)
    (let ((i 0))
      (while (< i 128)
        (if (equal ?w (char-syntax i))
            (modify-category-entry i ?s table)
          (modify-category-entry i ?s table t))
        (setq i (1+ i)))))
  (edit-category-table-for-company-dabbrev)
  (setq company-dabbrev-char-regexp "\\cs")
  ;; すべてのバッファで有効にする
  (global-company-mode))

;; flycheck
(use-package flycheck
  :init (global-flycheck-mode))

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

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Font
;; (set-frame-font "Monaco 13")
(set-face-attribute 'default nil :family "Monaco" :height 130)
(set-fontset-font "fontset-default"
                  'japanese-jisx0208
                  '("Hiragino Kaku Gothic ProN"))
(set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("Hiragino Kaku Gothic ProN"))

(set-frame-size (selected-frame) 120 57)
;; (add-to-list 'default-frame-alist '(font . "Monaco-13" ))
(setq mouse-drag-copy-region t)
;; スタートアップメッセージを表示しない
(setq inhibit-startup-message t)
;; バックアップファイルを作らない
(setq make-backup-files nil)
;; 終了時にオートセーブファイルを削除
(setq delete-auto-save-files t)
;; メニューバーを非表示
(menu-bar-mode nil)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
;; ツールバーを非表示
(if window-system (tool-bar-mode -1))
(if window-system (scroll-bar-mode 0))
(when (eq window-system 'ns)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))
;; ビープ音と画面フラッシュを消す
(setq ring-bell-function 'ignore)
;; テーマ
(use-package base16-theme
  :config
  (load-theme 'base16-monokai t)
  (set-face-foreground 'font-lock-comment-face "#969079")
  (set-face-foreground 'font-lock-comment-delimiter-face "#969079"))

;; カーソルの点滅をやめる
(blink-cursor-mode 0)
;; カーソル行のハイライト
(global-hl-line-mode t)
;; タブの挙動（左端ではインデント，それ以外はタブの挿入）
(setq tab-always-indent t)
;; タブをスペースに
(setq-default tab-width 4
              indent-tabs-mode nil)
;; 列数を表示する
(column-number-mode t)
;; 行数を表示する
(global-display-line-numbers-mode)
;; スクロールは１行ごと
(setq scroll-conservatively 1)
(setq scroll-preserve-screen-position 'always)
;;macのoptionをメタキィにする
(setq mac-option-modifier 'meta)
;; カッコの自動対応
(electric-pair-mode 1)
;; BSしたときに対応する閉じカッコを消さない
(setq electric-pair-delete-adjacent-pairs nil)
;; 対応するカッコ
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
;; shellの設定を引き継ぐ
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))
;; (add-to-list 'load-path "~/.emacs.d/lisp/satysfi")
;; 最後に改行を入れる
(setq require-final-newline t)
;; 自動で空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; C-x C-c で容易にEmacsを終了させないように質問する
(setq confirm-kill-emacs 'y-or-n-p)
;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)
;; GC
(setq gc-cons-threshold 50000000)
;; buffer list を現在のウィンドウに表示
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; back space の設定
(global-set-key (kbd "C-h") 'delete-backward-char)
;; dired kb表示
(setq dired-listing-switches "-alh")
;; for redo, undo
(use-package redo+
  :config
  (global-set-key (kbd "C-M-/") 'redo))

;; projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))

;; irony (for C++)
(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-common-hook 'irony-mode)
  ;; C++言語用にコンパイルオプションを設定する.
  (add-hook 'c++-mode-hook
            (lambda ()
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

;; Rust
(use-package lsp-mode)
(use-package company-lsp)
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil))
(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))
  ;; rust-modeで開かれる時があるのでrustic-modeを末尾に追加し直す
  ;;(cl-delete-if (lambda (element) (equal (cdr element) 'rust-mode)) auto-mode-alist)
  ;;(cl-delete-if (lambda (element) (equal (cdr element) 'rustic-mode)) auto-mode-alist)
  )

;; Haskell
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

(use-package tuareg
  :mode
  ("\\.ml\\'" . tuareg-mode)
  ("\\.mll\\'" . tuareg-mode)
  ("\\.mly\\'" . tuareg-mode))
(use-package merlin
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-fontify-code-blocks-natively t)
  :init (setq markdown-command "multimarkdown"))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

;; python
(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))


(use-package magit
  :config (global-set-key (kbd "C-x g") 'magit-status))

(use-package nasm-mode
  :config
  (add-hook 'asm-mode-hook 'nasm-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))


(use-package json-mode
  :config
  (add-hook 'js-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 4))))

(use-package go-mode)


(defun swap-screen ()
  "Swap two screen, leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(global-set-key [f2] 'swap-screen)

(provide 'init)
;;; init.el ends here
