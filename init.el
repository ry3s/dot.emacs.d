;;; package --- My init file
;;; Commentary:
;;; Code:
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf leaf-convert :ensure t)
    :config
    (leaf-keywords-init)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el")))
  :hook (kill-emacs-hook . (lambda ()
							 (if (file-exists-p custom-file)
								 (delete-file custom-file)))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :custom
  (tool-bar-mode . nil)
  (scroll-bar-mode . nil)
  (blink-cursor-mode . nil) ; カーソルの点滅をやめる
  (global-hl-line-mode . t) ; カーソル行のハイライト
  (column-number-mode . t) ; 列数を表示する
  (global-display-line-numbers-mode . t) ; 行数を表示する
  (inhibit-startup-message . t) ; スタートアップメッセージを表示しない
  (ring-bell-function . 'ignore) ; ビープ音と画面フラッシュを消す
  (show-paren-mode . t) ; 対応するカッコ
  (electric-pair-mode . t) ; カッコの自動対応
  (electric-pair-delete-adjacent-pairs . nil) ; BSしたときに対応する閉じカッコを消さない
  (scroll-preserve-screen-position . 'always)
  (indent-tabs-mode . nil)
  (tab-always-indent . t)
  (tab-width . 4)
  (require-final-newline . t)
  (fill-column . 100)
  (mac-option-modifier . 'meta)
  (make-backup-files . nil) ; バックアップファイルを作らない
  (delete-auto-save-files . t) ; 終了時にオートセーブファイルを削除
  (confirm-kill-emacs . 'y-or-n-p) ; C-x C-c で容易にEmacsを終了させないように質問する
  (bidi-display-reordering . nil) ; 右から左に読む言語に対応させないことで描画高速化
  (dired-listing-switches . "-alh") ; dired kb表示
  (recentf-save-file . "~/.emacs.d/recentf")
  (recentf-max-saved-items . 200)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (add-hook 'before-save-hook 'delete-trailing-whitespace) ; 自動で空白を削除
  (global-set-key (kbd "C-x C-b") 'buffer-menu)
  (global-set-key (kbd "C-h") 'delete-backward-char)
  (global-display-fill-column-indicator-mode)
  (setq default-frame-alist '((width . 110) (height . 53)))
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (set-face-attribute 'default nil :family "Monaco" :height 130))

(leaf exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(leaf doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  :config
  (leaf doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)))

(leaf undo-tree
  :ensure t
  :global-minor-mode global-undo-tree-mode
  :config
  (global-set-key (kbd "C-M-/") 'undo-tree-redo))

(leaf flycheck
  :ensure t
  :global-minor-mode global-flycheck-mode)

(leaf company
  :ensure t
  :global-minor-mode global-company-mode
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-h" . nil)))
  :custom
  (company-idle-delay . 0)
  (company-minimum-prefix-length . 3)
  (company-dabbrev-downcase . nil) ; case sensitive に補完
  (completion-ignore-case . t)
  (company-transformers . '(company-sort-by-backend-importance))
  :config
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
  (leaf company-box
    :ensure t
    :hook (company-mode . company-box-mode)))

(leaf vertico
  :ensure t
  :global-minor-mode t
  :config
  (leaf orderless
    :ensure t
    :custom (completion-styles . '(orderless)))
  (leaf marginalia
    :ensure t
    :global-minor-mode t)
  (leaf consult
    :ensure t))

(leaf dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(leaf which-key
  :ensure t
  :hook (after-init-hook))

(leaf neotree
  :ensure t
  :bind (([f8] . neotree-toggle)))

(leaf magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(leaf projectile
  :ensure t
  :global-minor-mode t
  :bind ((projectile-mode-map
          ("C-c p" . projectile-command-map))))

(leaf lsp-mode
  :custom  '((lsp-completion-provider . :capf))
  :config
  (leaf lsp-ui
    :ensure t
    :custom ((lsp-ui-doc-enable . nil))))

(leaf cc-mode
  :mode-hook
  (c-mode-hook . ((c-set-style "k&r")
                  (setq c-basic-offset 4)))
  (c++-mode-hook . ((setq c-default-style "k&r")
                    (setq c-basic-offset 4))))

(leaf irony
  :ensure t
  :custom (irony-additional-clang-options
           . '("-std=c++17" "-Wall" "-Wextra"
               "-I/usr/local/include"
               "-I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/c++/v1"
               "-I/Library/Developer/CommandLineTools/usr/lib/clang/12.0.5/include"
               "-I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
               "-I/Library/Developer/CommandLineTools/usr/include"))
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (leaf flycheck-irony
    :ensure t
    :config
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  (leaf company-irony
    :ensure t
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))))

(leaf rustic :ensure t)

(leaf dante
  :ensure t
  :after haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook 'dante-mode))

(leaf proof-general :ensure t)

(leaf tuareg
  :ensure t
  :mode ("\\.ml\\'")
  :config
  (leaf ocamlformat
    :ensure t
    :custom '(ocamlformat-enable . 'enable-outside-detected-project)
    :hook (before-save-hook . ocamlformat-before-save))
  (leaf merlin
    :ensure t
    :hook (tuareg-mode-hook . merlin-mode)))

(leaf markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         "\\.md\\'" "\\.markdown\\'")
  :custom '((markdown-fontify-code-blocks-natively . nil)))

(leaf elpy
  :ensure t
  :init (elpy-enable))

(leaf yaml-mode
  :ensure t
  :mode ("\\.ya?ml$"))

(leaf dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'"))

(leaf json-mode
  :ensure t
  :config
  (add-hook 'js-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

(leaf go-mode
  :ensure t
  :hook (go-mode-hook . lsp)
  :custom
  (indent-tabs-mode . t)
  (tab-width . 4)
  :config
  (add-hook 'go-mode-hook 'gofmt-before-save))

(leaf elm-mode
  :ensure t
  :hook ((elm-mode-hook . lsp)
         (elm-mode-hook . elm-format-on-save-mode)))

(leaf tex-mode
  :custom (tex-fontify-script . nil)
  :config
  (leaf reftex
    :hook (latex-mode-hook . reftex-mode))
  (leaf flyspell
    :ensure t
    :hook (latex-mode-hook . flyspell-mode))
  (leaf ispell
    :custom (ispell-program-name . "/usr/local/bin/aspell")
    :config
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))))

(provide 'init)
;;; init ends here
