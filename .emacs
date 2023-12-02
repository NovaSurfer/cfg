;; ................ Open in the center of the screen
(defun my/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
                            frame)
                      (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

(add-hook 'after-init-hook #'my/frame-recenter)
(add-hook 'after-make-frame-functions #'my/frame-recenter)
;; ............... Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(weeuu))
 '(custom-safe-themes
   '("c83a4eb86ca80750c7bd4f2715649e2566c2457b91ca18c3037fd4345239c075" default))
 '(package-selected-packages
   '(company yasnippet all-the-icons nov magit cmake-mode dashboard lsp-ui dap-mode which-key lsp-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "CaskaydiaCove Nerd Font" :foundry "SAJA" :slant normal :weight semi-bold :height 113 :width normal))))
 '(lsp-headerline-breadcrumb ((t (:foreground "yellow" :weight bold))))
 '(lsp-headerline-error ((t (:foreground "red" :weight bold))))
 '(lsp-headerline-peek ((t (:foreground "green" :slant italic))))
 '(lsp-headerline-usage ((t (:foreground "blue" :weight bold))))
 '(lsp-headerline-warning ((t (:foreground "orange" :weight bold)))))

;; enable line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq pixel-scroll-precision-mode t)
(setq-default indent-tabs-mode nil)        ;; Disable indent with tabs
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq indent-line-function 'insert-tab)

;; align code more to the center
;;(add-hook 'prog-mode-hook (lambda ()
;;  (setq left-margin-width 10)))

;; ....................... Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-center-content t)
(setq dashboard-projects-backend 'project-el)
(setq dashboard-items '((recents  . 5)
                        (projects . 5)))
(setq dashboard-startup-banner "~/.emacs.d/logo.png")
;; ........................ MELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.
;; See `package-archive-priorities` and `package-pinned-packages`.
;; Most users will not need or want to do this.
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; ......................... LSP STUFF
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-completion-provider :capf)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp)
         (c-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom (lsp-headerline-breadcrumb-enable t))

  ;; Arguments given to clangd server. See https://emacs-lsp.github.io/lsp-mode/lsp-mode.html#lsp-clangd
(setq lsp-clients-clangd-args '(
				"--clang-tidy"
				))


;; ........................ enabling Ido
(require 'ido)
(ido-mode t)
(require 'lsp-ido)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;;.................... DAP settings
(dap-mode 1)

;; vscode-cpptools
(require 'dap-cpptools)

;; The modes below are optional
(dap-ui-mode 1)

;; enables mouse hover support
(dap-tooltip-mode 1)

;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)

;; displays floating panel with debug buttons
;; requies emacs 26+
(dap-ui-controls-mode 1)

;; .................... epub reader
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


;; .................... modeline
(when (display-graphic-p)
  (require 'all-the-icons))

(add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)

(setq-default mode-line-format
  '("%e"
    mode-line-front-space
    mode-line-client
    (:eval
     (if buffer-read-only
         (concat
          (propertize (all-the-icons-faicon "lock" :v-adjust 0.0 :height 1.2) 'face '(:foreground "#F3B4F4"))
          " | ")
       (concat
        (propertize (all-the-icons-faicon "unlock" :v-adjust 0.0 :height 1.2) 'face '(:foreground "#F3B4F4"))
        " | ")))
    (:eval
     (if (buffer-modified-p)
         (concat
          (propertize (all-the-icons-faicon "pencil" :v-adjust 0.0 :height 1.2) 'face '(:foreground "#E19A9A"))
          "  ፨  ")
       (concat
        (propertize (all-the-icons-faicon "check" :v-adjust 0.0 :height 1.2) 'face '(:foreground "#8ADA9A"))
        "  ፨  ")))
    mode-line-buffer-identification
    (:eval (if (featurep 'lsp-mode) "፨ " (lsp--mode-line)))
    "  "
    mode-line-misc-info
    mode-line-end-spaces
    ))

(set-face-attribute 'mode-line nil
                    :background "#293323"
                    :foreground "white"
                    :box nil
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#162110"
                    :foreground "white"
                    :box nil
                    :overline nil
                    :underline nil)

;; ............... Code completion
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends '((company-capf company-clang)))


