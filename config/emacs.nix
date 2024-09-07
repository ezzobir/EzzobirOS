{ pkgs, ... }:
{

  services.emacs.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      vterm
      treesit-grammars.with-all-grammars
      evil
      which-key
      general
      ivy
      counsel
      company
    ];
    extraConfig = ''
;; UI Costomization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; disable welcome screen
(setq inhibit-startup-message t)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bar
(scroll-bar-mode -1)

;; Disable the tooltips
(tooltip-mode -1)

;; Disable the blinking cursor
(blink-cursor-mode -1)

;; hl-line
(global-hl-line-mode 1)

;; delete comment lines from scratch buffer
(setq initial-scratch-message nil)

;; tab width
(setq-default tab-width 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Set default font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height 140
                    :weight 'normal
                    :width 'normal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Set up package.el to work with MELPA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Evil Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable Evil mode
(require 'evil)
(evil-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; To Make Your Folder Config Clean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-directory-alist
      `(("." . "/home/ezzobir/.config/emacs/backups")))

(setq custom-file "/home/ezzobir/.config/emacs/emacs-custom.el")
(load custom-file t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Relative Line Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable line numbers globally
(global-display-line-numbers-mode)

;; Set line numbers to be relative
(setq display-line-numbers-type 'relative)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ivy 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable Ivy mode
(require 'ivy)
(ivy-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up Counsel for file finding
(require 'counsel)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Keybinding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable which-key
(require 'which-key)
(which-key-mode)

;; Optional: Configure which-key appearance
(setq which-key-idle-delay 1) ;; Delay in seconds before which-key pops up

;; general package setup
(require 'general)

;; Define prefix key for leader in Evil mode normal and visual states only
(general-create-definer my/leader-keys
  :states '(normal visual)  ; Only in normal and visual states
  :prefix "SPC")

;; Define file management keybindings
(my/leader-keys
  "f"  '(:ignore t :which-key "file")
  "ff" '(counsel-find-file :which-key "find file")
  "fs" '(save-buffer :which-key "save file")
  "fe" '(:ignore t :which-key "emacs")
  "fei" '(open-my-init-file :which-key "open my init.el")
  "fj" '(dired-jump :which-key "dired")
  "fr" '(recentf-open-files :which-key "recent files"))

;; Define file management keybindings
(my/leader-keys
  "d"  '(:ignore t :which-key "dired")
  "dd"  '(dired :which-key "dired")
  "do" '(dired-other-window :which-key "dired other window")
  "dh" '(open-dired-home :which-key "dired home"))

;; Define buffer management keybindings
(my/leader-keys
  "b"  '(:ignore t :which-key "buffer")
  "bd" '(kill-this-buffer :which-key "delete buffer")
  "bb" '(switch-to-buffer :which-key "list buffers")
  "bn" '(next-buffer :which-key "next buffer")
  "bp" '(previous-buffer :which-key "previous buffer")
  "bM" '(delete-other-buffers :which-key "delete other buffers"))

;; Define window management keybindings
(my/leader-keys
  "w"  '(:ignore t :which-key "window")
  "wv" '(split-window-right :which-key "split right")
  "wb" '(split-window-below :which-key "split below")
  "wl" '(windmove-right :which-key "move right")
  "wh" '(windmove-left :which-key "move left")
  "wj" '(windmove-down :which-key "move down")
  "wk" '(windmove-up :which-key "move up")
  "wd" '(delete-window :which-key "delete window")
  "wM" '(delete-other-windows :which-key "delete other windows"))

;; Define keybinding to switch to last buffer
(my/leader-keys
  "SPC" '(evil-switch-to-windows-last-buffer :which-key "last buffer"))

;; Ensure windmove commands are available
(windmove-default-keybindings)

;; Function to open emacs.nix file
(defun open-my-init-file ()
  "Open the user's custom init file."
  (interactive)
  (find-file "/home/ezzobir/ezzobiros/config/emacs.nix"))

;; Function to open home directory in dired
(defun open-dired-home ()
  "Open Dired in the home directory."
  (interactive)
  (dired "~"))

;; Keybinding for dired mode
(general-define-key
    :states '(normal)
    :keymaps 'dired-mode-map
    "$" 'evil-end-of-line
    "^" 'evil-first-non-blank
    "v" 'evil-visual-char)
(general-define-key
    :states '(normal visual)
    :keymaps 'dired-mode-map
    :prefix "SPC"
    "SPC" '(evil-switch-to-windows-last-buffer :which-key "last buffer")
    "bb"  '(switch-to-buffer :which-key "list buffers")
    "bn"  '(next-buffer :which-key "next buffer")
    "bp"  '(previous-buffer :which-key "previous buffer")
    "bd"  '(kill-this-buffer :which-key "kill buffer"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install and enable company mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; typst
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    '';
  };
}
