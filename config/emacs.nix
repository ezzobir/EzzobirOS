{ inputs, pkgs, ... }:
{

  nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];
  # services.emacs.enable = true;

  programs.emacs = {
    enable = true;
    package =
      (pkgs.emacsWithPackagesFromUsePackage {
        defaultInitFile = true;
        package = pkgs.emacs29-pgtk;
        extraPackages = epkgs: with epkgs; [
          vterm
          treesit-grammars.with-all-grammars
          evil
          evil-collection
          which-key
          general
          ivy
          ivy-rich
          swiper
          counsel
          company
          all-the-icons
          doom-modeline
          nix-mode
        ];
        config = ./emacs/emacs.el;
      });
    # extraConfig = ''
    # '';
  };
}
