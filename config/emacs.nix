{ pkgs, inputs, ... }:
{
  programs.emacs = {
    enable = true;
  };
  services= {
    emacs = {
      enable = true;
      startWithGraphical = true;
      client.enable = true;
    };
  };
}
