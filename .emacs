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
   '("c83a4eb86ca80750c7bd4f2715649e2566c2457b91ca18c3037fd4345239c075" "92886288b151f8f6aed14cc7ac5f053d4407071722d0a8ecaec3a560a95c06a5" "e6d6f9d74f8a39b277e3e094672227ea54f612def2a5ed3943cb2c65e6cef107" "324fec65c268a843216b57cbe39b3b02f1ec6391ce04fd9aa8b713b63351812b" "51d59a8e512f9f03b309742a616dd9d5f8f388fb8654a086701f51493ad4ceb1" "6abd1596b7690dd5753adce37364b83d3c8db05bd71141daa7d078bfa35a1f20" "157e361b27558b3c00cc746ae2ac3dd510bb6775fb7a147f9593bdcb6d24cc78" "adc9e24b773ff09590af6621bcbf8fc7e6c06dbf1a488df7a7536ea1f9d6701c" "19ae711bc3052d3a6eb5b54611bf14418374f4022fe961ad9c954192e3cbfb9d" "509fe47c3ba7b9cd2cc6c3ccae233ec0b2406cb68a819c21eb65ec4b4adb38a6" "96ba34352fe5bff046ea0de3cb1c9882a0f33bb8c66547742234b5fa64233210" "8e2d3693cd3050837f3353e0ded5d7a65d2d763bb18cd18bb180d49af56c2f1e" "c2d45ec7506dca1135664a39401ced2e2b55aa662c5dc3c756c10d15ab121fd5" "ade2576a3822602a46ef0363b090f5054327d162cc4d4025d6887fec9ea1c579" "6d60d2037fded563bc375802641e15fbd215bc1f05ef7afb205896436f03e681" "e9f50e542e36fde5f572a795610e4b836a70513c0f9b56b9298a6d57bcb2a8a1" "404f7faf818aa7f3ce87d6e47ff5b42a97768e6ad52c2ade674cf8df67d2d6da" "d3132cb5e0976ad25ae7088a9a39f62bdcbaca076f55eba0556b60fed7ed772a" "e3e840bbd66fcb82aadd23859e8da9084ed7544c23ab1eb34029ce404a41af7e" "cb3e6ff7d6207158b0c4b10d492af812e928f33389d329af6bf7db3d9a12528a" "c2f2a6df93b2a50fdec36bc732862dbf64f580a88e704fd24ac3d0c635d1cc96" "3a91dcaaf08db07f1cbe0c6f7a17159dfbc35dcc2d2fff9de6bded4d2017d8fd" "63df6cf879bf3d918720b59248a6fdca519c5004973dc9b374e3f6903edf6a54" "f029c8f1507a81ca48a554c184afffa6352359eed4339e55c39ea2d156662d8b" "b4d65ac53999e1c92d8574c811b21ffc2bb3a39cef22b2f518182d0e7af4f40a" "eae1ebb04358e8b666dde07a60b80c31562777e20e4f74c17d83feea91cfec88" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" "9031991af43f3a15e2b1fab62b5a0a985c46f24e596385f05bbf87770347d8ed" "d982bf16e6dba3cfe57ed04214322c6159679dc6889c42246a1f7f5628ce1736" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "3fec737266204a5422e5acc776ea55e1a2fcd3a8104fd8c70ee0a300e56ece3c" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" default))
 '(package-selected-packages
   '(nov magit cmake-mode dashboard lsp-ui dap-mode which-key lsp-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "CaskaydiaCove Nerd Font" :foundry "SAJA" :slant normal :weight semi-bold :height 113 :width normal)))))

;; enable line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
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
  ;; turn off snippet
  (setq lsp-enable-snippet nil)
  (setq lsp-completion-provider :none)
;;  (setq lsp-completion-enable-additional-text-edit nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp)
         (c-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

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
