;;
;;; Main Setting:
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
     '(
       (width . 100)
       (height . 45)
       ))
;;透明化
;; (if window-system (progn
;;     (set-frame-parameter nil 'alpha 92) ;透明度
;;     ))


;;パッケージ有効化
;; (package-initialize)
;; (setq package-archives
;;       '(("gnu" . "http://elpa.gnu.org/packages/")
;;         ("melpa" . "http://melpa.org/packages/")
;;         ("org" . "http://orgmode.org/elpa/")))
;;(require 'use-package)
;;------------------------------------------------------------------------------
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(custom-safe-themes
;;    (quote
;;     ("54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" default)))
;;  '(doom-themes-enable-italic t)
;;  '(doom-thmes-enable-bold t t)
;;  '(flycheck-clang-language-standard "c++14")
;;  '(flycheck-gcc-language-standard "c++14")
;;  '(haskell-stylish-on-save t)
;;  '(js-indent-level 2)
;;  '(package-selected-packages
;;    (quote
;;     (zenburn-theme dracula-theme yasnippet prop-menu proof-general doom-themes use-package magit projectile intero yaml-mode haskell-mode company-ghc company-ghci yatex json-mode counsel ivy flycheck-elm flycheck elm-mode exec-path-from-shell cargo rust-mode multi-term sml-mode madhat2r-theme company)))
;;  '(sml-indent-level 2))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(doom-modeline-bar ((t (:background "#6272a4")))))
;;------------------------------------------------------------------------------
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
    (global-company-mode))

;; (require 'company)
;; (global-company-mode) ; 全バッファで有効にする
;; (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
;; (setq company-minimum-prefix-length 3) ; デフォルトは4
;; (setq company-idle-delay 0) ; デフォルトは0.5
;; (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
;; (setq completion-ignore-case t)
;; (setq company-dabbrev-downcase nil)
;; (global-set-key (kbd "C-M-i") 'company-complete)
;; (define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; (define-key company-search-map (kbd "C-n") 'company-select-next)
;; (define-key company-search-map (kbd "C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
;; (define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
;; (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
;; (define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fで候補を設定
;; ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
;; (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

;; ;;flycheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;; ;;ivy configurations
;; (require 'ivy)
;; (ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
;; (setq enable-recursive-minibuffers t)
;; (setq ivy-height 30) ;; minibufferのサイズを拡大！（重要）
;; (setq ivy-extra-directories nil)
;; (setq ivy-re-builders-alist
;;       '((t . ivy--regex-plus)))

;; ;;counsel configurations
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; find-fileもcounsel任せ！
;; (defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))

;;------------------------------------------------------------------------------
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;;---------------------------------------------------------------------------
(add-to-list 'default-frame-alist '(font . "MonacoB-13" ))

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
(setq-default tab-width 2 indent-tabs-mode nil)
;;列数を表示する
(column-number-mode t)
;;行数を表示する
(global-linum-mode t)
(setq linum-format "%4d|")
;;スクロールは１行ごと
(setq scroll-conservatively 1)
(setq scroll-preserve-screen-position 'always)
;;macのoptionをメタキィにする
(setq mac-option-modifier 'meta)
;; カッコの自動対応
(electric-pair-mode 1)
;;shellの設定を引き継ぐ
;;(exec-path-from-shell-initialize)
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
(use-package cc-mode)

;; Kernighan & Ritche Style
(setq c-default-style "k&r")
;;;; BSキーを賢くする。
;;;; インデント幅は4、タブはスペースに変換
;; (add-hook 'c-mode-common-hook
;; 		      '(lambda ()
;; 			       (progn
;; 			         (c-toggle-hungry-state 1)
;; 			         (setq c-basic-offset 4 indent-tabs-mode nil))))

;;;; .hppをC++の拡張子とする。
;; (setq auto-mode-alist
;; 	    (append
;; 	     '(("\\.hpp$" . c++-mode)
;; 		     ) auto-mode-alist))
;; shellの設定を引き継ぐ
;; (exec-path-from-shell-initialize)
;;------------------------------------------------------------------------------
;; ocaml merlin
 (push "<SHARE_DIR>/emacs/site-lisp" load-path) ; directory containing merlin.el
 ;;(setq merlin-command "<BIN_DIR>/ocamlmerlin")  ; needed only if ocamlmerlin not already in your PATH
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

;;OCamlのパッケージtuaregを有効化
(load "/Users/saffron/.opam/system/share/emacs/site-lisp/tuareg-site-file")
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml code." t)
(autoload 'tuareg-run-ocaml "tuareg" "Run an inferior OCaml process." t)
(autoload 'ocamldebug "ocamldebug" "Run the OCaml debugger." t)

;;------------------------------------------------------------------------------
;;coq proof general
;;(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
;; Proof General
(load "~/.emacs.d/lisp/PG/generic/proof-site")
;;--------------------------------------------------------------------

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; haskell intero
;; (package-install 'intero)
;; (add-hook 'haskell-mode-hook 'intero-mode)

;; ;; yaml-mode
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
;; (define-key yaml-mode-map "\C-m" 'newline-and-indent)

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
