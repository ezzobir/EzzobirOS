{ pkgs, ... }:
{

  services.emacs.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: with epkgs; [
      vterm
      vterm-toggle
    ];
    extraConfig = ''
      ;; vterm
      (use-package vterm
      :config
      (setq shell-file-name "/etc/profiles/per-user/ezzobir/bin/sh"
            vterm-max-scrollback 5000))

      ;; vterm-toggle
      (use-package vterm-toggle
        :after vterm
        :config
        ;; When running programs in Vterm and in 'normal' mode, make sure that ESC
        ;; kills the program as it would in most standard terminal programs.
        (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)
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
                        (window-height . 0.4))))
    '';
  };
}
