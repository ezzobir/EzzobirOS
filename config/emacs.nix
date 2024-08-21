{ pkgs, ... }:
{

  services.emacs.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: [
      epkgs.vterm
      epkgs.treesit-grammars.with-all-grammars
    ];
  };


}
