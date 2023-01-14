;; -*- lexical-binding: t -*-

(require 'package)
(require 'cl-lib)

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(setq custom-file (locate-user-emacs-file "custom.el"))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(show-paren-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(ido-mode 1)

(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)

(add-to-list 'custom-theme-load-path (expand-file-name "themes/monokai-emacs" user-emacs-directory))

(load-theme 'monokai t)

(global-display-line-numbers-mode)

(defun require-package (package)
  (or (package-installed-p package)
      (and (or (assoc package package-archive-contents)
               (package-refresh-contents))
           (package-install package))))

(require-package 'lsp-mode)
(require-package 'ccls)
(require-package 'clang-format)
(require-package 'clang-format+)

(require 'ccls)
(setq lsp-lens-enable nil)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'clang-format+-mode)

(require-package 'company)

(require-package 'centered-cursor-mode)
(global-centered-cursor-mode)

(require-package 'git-gutter)
(global-git-gutter-mode +1)

(setq evil-toggle-key "<f12>")
(require-package 'evil)
(require-package 'key-chord)
(require 'evil)
(evil-mode 1)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.2)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

(require-package 'slime)
(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
(require-package 'slime-company)
(slime-setup '(slime-fancy slime-company))

(require-package 'lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (company-mode 1)))
(add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))

(global-set-key (kbd "C-c d") 'kill-whole-line)

(require-package 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

(add-hook 'after-save-hook #'evil-normal-state)

(fset 'yes-or-no-p 'y-or-n-p)
