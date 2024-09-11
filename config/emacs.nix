{ pkgs, ... }:
{

  services.emacs.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      treesit-grammars.with-all-grammars
      evil
      evil-collection
      evil-tutor
      general
      all-the-icons
      all-the-icons-dired
      ivy
      ivy-rich
      all-the-icons-ivy-rich
      swiper
      counsel
      toc-org
      org-bullets
      rainbow-mode
      eshell-syntax-highlighting
      vterm
      vterm-toggle
      sudo-edit
      which-key
      company
      doom-modeline
      nix-mode
    ];
    extraConfig = ''
      ;; evil, evil-collection, evil-tutor
      (use-package evil
        :init
        (setq evil-want-integration t)
        (setq evil-want-keybinding nil)
        (setq evil-want-C-u-scroll t)
        (setq evil-want-C-i-jump nil)
        :config
        (evil-mode 1)
        (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
        (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

        ;; Use visual line motions even outside of visual-line-mode buffers
        (evil-global-set-key 'motion "j" 'evil-next-visual-line)
        (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

        (evil-set-initial-state 'messages-buffer-mode 'normal)
        (evil-set-initial-state 'dashboard-mode 'normal))

      (use-package evil-collection
        :after evil
        :config
        (setq evil-collection-mode-list '(dashboard dired ibuffer))
        (evil-collection-init))

      (use-package evil-tutor)

      ;; general

      (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

      (use-package general
        :config
        (general-evil-setup)

        ;; set up 'SPC' as the global leader key
        (general-create-definer my/leader-keys
          :states '(normal insert visual emacs)
          :keymaps 'override
          :prefix "SPC" ;; set leader
          :global-prefix "M-SPC") ;; access leader in insert mode

        (my/leader-keys
            ";" '(comment-line :wk "Comment lines"))
        (my/leader-keys
          "b" '(:ignore t :wk "buffer")
          "bb" '(counsel-switch-buffer :wk "Switch buffer")
          "bi" '(ibuffer :wk "list all buffers")
          "bd" '(kill-this-buffer :wk "Kill this buffer")
          "bn" '(next-buffer :wk "Next buffer")
          "bp" '(previous-buffer :wk "Previous buffer")
          "br" '(revert-buffer :wk "Reload buffer"))

        (my/leader-keys
            "e" '(:ignore t :wk "Eshell/Evaluate")    
            "eb" '(eval-buffer :wk "Evaluate elisp in buffer")
            "ed" '(eval-defun :wk "Evaluate defun containing or after point")
            "ee" '(eval-expression :wk "Evaluate and elisp expression")
            "eh" '(counsel-esh-history :which-key "Eshell history")
            "el" '(eval-last-sexp :wk "Evaluate elisp expression before point")
            "er" '(eval-region :wk "Evaluate elisp in region")
            "es" '(eshell :which-key "Eshell"))

        (my/leader-keys
            "f"  '(:ignore t :wk "file")
            "ff" '(counsel-find-file :wk "Find file")
            "fs" '(save-buffer :wk "save File")
            "fr" '(counsel-recentf :wk "Find recent files")
            "fe" '(:ignore t :wk "Emacs")
            "fec" '(open-my-init-file :wk "Open my init.el")
            "fer" '(reload-my-init-file :wk "Reload my init.el")
            "fj" '(dired-jump :wk "Dired"))

        (my/leader-keys
            "w" '(:ignore t :wk "Windows")
            ;; Window splits
            "wd" '(evil-window-delete :wk "Close window")
            "wo" '(delete-other-windows :which-key "delete other windows")
            "wn" '(evil-window-new :wk "New window")
            "ws" '(evil-window-split :wk "Horizontal split window")
            "wv" '(evil-window-vsplit :wk "Vertical split window")
            ;; Window motions
            "wh" '(evil-window-left :wk "Window left")
            "wj" '(evil-window-down :wk "Window down")
            "wk" '(evil-window-up :wk "Window up")
            "wl" '(evil-window-right :wk "Window right")
            "ww" '(evil-window-next :wk "Goto next window")
            ;; Move Windows
            "wH" '(buf-move-left :wk "Buffer move left")
            "wJ" '(buf-move-down :wk "Buffer move down")
            "wK" '(buf-move-up :wk "Buffer move up")
            "wL" '(buf-move-right :wk "Buffer move right"))

        (my/leader-keys
            "h" '(:ignore t :wk "Help")
            "hf" '(describe-function :wk "Describe function")
            "hv" '(describe-variable :wk "Describe variable")
            "hk" '(describe-key :wk "Describe key"))

        (my/leader-keys
            "t" '(:ignore t :wk "Toggle")
            "tn" '(display-line-numbers-mode :wk "Toggle line numbers")
            "tl" '(visual-line-mode :wk "Toggle truncated lines")
            "t v" '(vterm-toggle :wk "Toggle vterm"))

        (my/leader-keys
            "SPC" '(evil-switch-to-windows-last-buffer :which-key "last buffer"))
        )

        ;; emacs-counsel-launcher
        (defun emacs-counsel-launcher ()
          "Create and select a frame called emacs-counsel-launcher which consists only of a minibuffer and has specific dimensions. Runs counsel-linux-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
          (interactive)
          (with-selected-frame 
            (make-frame '((name . "emacs-run-launcher")
                          (minibuffer . only)
                          (fullscreen . 0) ; no fullscreen
                          (undecorated . t) ; remove title bar
                          ;;(auto-raise . t) ; focus on this frame
                          ;;(tool-bar-lines . 0)
                          ;;(menu-bar-lines . 0)
                          (internal-border-width . 10)
                          (width . 80)
                          (height . 11)))
                          (unwind-protect
                            (counsel-linux-app)
                            (delete-frame))))

    ;; all-the-icons
    (use-package all-the-icons
      :if (display-graphic-p))

    (use-package all-the-icons-dired
      :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

    ;; Font

    (set-face-attribute 'default nil
      :family "JetBrainsMono Nerd Font Mono"
      :height 160
      :weight 'medium)

    (set-face-attribute 'variable-pitch nil
      :family "Ubuntu"
      :height 160
      :weight 'medium)

    (set-face-attribute 'fixed-pitch nil
      :family "JetBrainsMono Nerd Font Mono"
      :height 160
      :weight 'medium)

    ;; Makes commented text and keywords italics.
    ;; This is working in emacsclient but not emacs.
    ;; Your font must have an italic face available.
    (set-face-attribute 'font-lock-comment-face nil
      :slant 'italic)

    (set-face-attribute 'font-lock-keyword-face nil
      :slant 'italic)

    ;; This sets the default font on all graphical frames created after restarting Emacs.
    ;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
    ;; are not right unless I also add this method of setting the default font.
    (add-to-list 'default-frame-alist '(family . "JetBrainsMono Nerd Font Mono"))

    ;; Uncomment the following line if line spacing needs adjusting.
    (setq-default line-spacing 0.12)

    ;; Zooming In/Out
    (global-set-key (kbd "C-=") 'text-scale-increase)
    (global-set-key (kbd "C--") 'text-scale-decrease)

    (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
    (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

    (global-set-key (kbd "C-0") 'text-scale-adjust)

    ;; disable-menubar-toolbars-and-scrollbars
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)

    ;; display-line-numbers-and-truncated-lines
    (global-display-line-numbers-mode 1)
    (setq display-line-numbers-type 'relative)
    (global-hl-line-mode t)
    (global-visual-line-mode t)

    ;; Other ui stuff
    ;; Disable the tooltips
    (tooltip-mode -1)
    ;; Disable the blinking cursor
    (blink-cursor-mode -1)
    ;; disable welcome screen
    (setq inhibit-startup-message t)
    ;; delete comment lines from scratch buffer
    (setq initial-scratch-message nil)
    ;; tab width
    (setq-default tab-width 4)

    ;; Ivy
    (use-package ivy
      :diminish
      :bind (("C-s" . swiper)                  ; Bind swiper to C-s for search
             ("M-x" . counsel-M-x)             ; Use counsel for M-x
             ("C-x C-f" . counsel-find-file)   ; Use counsel to find files
             ("C-x b" . ivy-switch-buffer)     ; Switch buffers using ivy
             ("C-x C-r" . counsel-recentf)     ; Use counsel for recentf files
             :map ivy-minibuffer-map
             ("TAB" . ivy-alt-done)
             ("C-l" . ivy-alt-done)
             ("C-j" . ivy-next-line)
             ("C-k" . ivy-previous-line)
             :map ivy-switch-buffer-map
             ("C-k" . ivy-previous-line)
             ("C-l" . ivy-done)
             ("C-d" . ivy-switch-buffer-kill)
             :map ivy-reverse-i-search-map
             ("C-k" . ivy-previous-line)
             ("C-d" . ivy-reverse-i-search-kill))
      :config
          (ivy-mode 1)                             ; Enable ivy mode
          (setq ivy-use-virtual-buffers t          ; Show recently killed buffers and more
                  ivy-count-format "(%d/%d) "      ; Display counters in the minibuffer
                  ivy-wrap t))                     ; Enable wrapping in ivy completion

    (use-package ivy-rich
      :after ivy
      :init
      (ivy-rich-mode 1))

    (use-package all-the-icons-ivy-rich
      :init (all-the-icons-ivy-rich-mode 1))

    (use-package counsel
      :custom
      (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
      :config
      (counsel-mode 1))

    ;; org mode
    (use-package org
      :hook (org-mode . org-indent-mode))
    ;; Enabling table of contents
    (use-package toc-org
      :hook (org-mode . toc-org-enable))
    ;; Enabling org bullets
    (use-package org-bullets
      :after org
      :hook (org-mode . org-bullets-mode))
    ;; Disable electric indent
    (electric-indent-mode -1)
    (setq org-edit-src-content-indentation 0)

    ;; Source code block tag expansion
    (require 'org-tempo)

    ;; Rainbow mode
    (use-package rainbow-mode
      :hook 
      ((org-mode prog-mode) . rainbow-mode))

    ;; Shells and terminals
    ;;eshell
    (use-package eshell-syntax-highlighting
      :after esh-mode
      :config
      (eshell-syntax-highlighting-global-mode +1))

    (setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
          eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
          eshell-history-size 5000
          eshell-buffer-maximum-lines 5000
          eshell-hist-ignoredups t
          eshell-scroll-to-bottom-on-input t
          eshell-destroy-buffer-when-process-dies t
          eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

    ;; Vterm
    (require 'vterm)
    (setq shell-file-name "/etc/profiles/per-user/ezzobir/bin/fish"
          vterm-max-scrollback 5000)

    (use-package vterm-toggle
      :after vterm
      :config
      (setq vterm-toggle-fullscreen-p nil)
      (setq vterm-toggle-scope 'project)
      (add-to-list 'display-buffer-alist
                   '((lambda (buffer-or-name _)
                         (let ((buffer (get-buffer buffer-or-name)))
                           (with-current-buffer buffer
                             (or (equal major-mode 'vterm-mode)
                                 (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                      (display-buffer-reuse-window display-buffer-at-bottom)
                      ;;(display-buffer-reuse-window display-buffer-in-direction)
                      ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                      ;;(direction . bottom)
                      ;;(dedicated . t) ;dedicated is supported in emacs27
                      (reusable-frames . visible)
                      (window-height . 0.3))))

    ;; Sudo edit
    (use-package sudo-edit
      :config
        (my/leader-keys
          "fu" '(sudo-edit-find-file :wk "Sudo find file")
          "fU" '(sudo-edit :wk "Sudo edit file")))

    ;; Which key
    (use-package which-key
    :init
      (which-key-mode 1)
    :config
    (setq which-key-side-window-location 'bottom
      which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-slot -10
      which-key-side-window-max-height 0.25
      which-key-idle-delay 0.8
      which-key-max-description-length 25
      which-key-allow-imprecise-window-fit t
      which-key-separator " → " ))
    '';
  };
}
