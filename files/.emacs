(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" default))
 '(package-selected-packages '(magit oauth2 company monokai-theme geiser-guile paredit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; 启用monokai主题
(load-theme 'monokai t)
;; 显示行号
(global-display-line-numbers-mode t)
;; 显示列号
(column-number-mode t)
;; 启用ede大型程序模式
(global-ede-mode t)
;; 修改自动备份文件的储存目录
(setq backup-directory-alist '(("." . "~/.saves")))
;; 显示行尾空格
(setq-default show-trailing-whitespace t)
;; 显示文件末尾多余的行
(setq-default indicate-empty-lines t)
;; 映射提示
(setq abbrev-suggest t)
;; 自动补全括号
(electric-pair-mode t)
;; 自动刷新缓冲区
(global-auto-revert-mode t)
;; 关闭欢迎界面
(setq inhibit-startup-message t)
;; 关闭Tool bar
(tool-bar-mode -1)
;; 启用company自动补全
(add-hook 'after-init-hook 'global-company-mode)
