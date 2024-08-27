
{ config, pkgs, host, username, ... }:

let
  myAliases = {
    # -----------------------------------------------------
    #terminal
    # -----------------------------------------------------
    c="clear";
    e="exit";

    # -----------------------------------------------------
    #nix
    # -----------------------------------------------------

    # fu = "nh os switch --hostname ${host} --update /home/${username}/ezzobiros";
    fu = "cd /home/ezzobir/ezzobiros;sudo nix flake update";
    # fr = "nh os switch --hostname ${host} /home/${username}/ezzobiros";
    fr = "sudo nixos-rebuild switch --flake /home/ezzobir/ezzobiros/#default";
    ncg = "nix-collect-garbage --delete-old && sudo nix-collect-garbage -d && sudo /run/current-system/bin/switch-to-configuration boot";
    eu = "sh <(curl -L https://gitlab.com/ezzobir/ezzobiros/-/raw/main/install-ezzobiros.sh)";

    # -----------------------------------------------------
    # vim
    # -----------------------------------------------------
    vi="vim";

    # -----------------------------------------------------
    # neovim
    # -----------------------------------------------------
    v="nvim";
    sv = "sudo nvim";

    # -----------------------------------------------------
    # emacs
    # -----------------------------------------------------
    em="emacs -nw";

    # -----------------------------------------------------
    # tty-clock
    # -----------------------------------------------------
    clock="tty-clock -c";

    # -----------------------------------------------------
    #see content
    # -----------------------------------------------------
    # ll="ls -Fl";
    # la="ls -FAl";
    ls = "eza --icons";
    ll = "eza -lh --icons --grid --group-directories-first";
    la = "eza -lah --icons --grid --group-directories-first";
    ".." = "cd ..";
    cat = "bat";

    # -----------------------------------------------------
    # EDIT CONFIG FILES
    # -----------------------------------------------------
    confa="nvim ~/.config/alacritty/alacritty.toml";
    confb="nvim ~/.bashrc";
    confi3="nvim ~/.config/i3/config";
    confsway="nvim ~/.config/sway/config";
    confp="nvim ~/.config/picom/picom.conf";

    # -----------------------------------------------------
    # Use Sioyek For PDF
    # -----------------------------------------------------
    pdf="sioyek --new-window";

    # -----------------------------------------------------
    # youtube-dlp
    # -----------------------------------------------------
    da="yt-dlp -x --audio-format mp3";
    dap="yt-dlp -x --audio-format mp3 -o '%(title)s.%(ext)s'";
    dac="yt-dlp --extract-audio --audio-format mp3 --ignore-errors";
    dv="yt-dlp -f mp4";
    dvp="yt-dlp -o '%(playlist_index)s_%(title)s.%(ext)s' -f mp4 --yes-playlist";

    # -----------------------------------------------------
    # ytfzf
    # -----------------------------------------------------
    yt="ytfzf -t";

    # -----------------------------------------------------
    # Change wallpaper using feh
    # -----------------------------------------------------
    cw="DISPLAY=:0 feh --no-fehbg --bg-fill --randomize ~/Pictures/wallpapers/*.{jpg,png}";
  };
in
{
  
  #bash
  programs = {
    bash = {
      enable = true;
      enableCompletion = true;
      profileExtra = ''
        #if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
        #  exec Hyprland
        #fi
      '';
      initExtra = ''
        # fastfetch
        # Begin Vi Mode
        # -----------------------------------------------------
        set -o vi
        # -----------------------------------------------------
        # End Vi Mode
        if [ -f $HOME/.bashrc-personal ]; then
          source $HOME/.bashrc-personal
        fi
      '';
      shellAliases = myAliases; 
    };
  
    #fish
    fish = {
      enable = true;
      # enableCompletion = true;
      loginShellInit = ''
        # if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        #     exec Hyprland
        # end
      '';
      interactiveShellInit = ''
        # fastfetch
        set -U fish_greeting ""
        # Begin Vi Mode
        # -----------------------------------------------------
        fish_vi_key_bindings
        # -----------------------------------------------------
        # End Vi Mode
      '';
      shellAliases = myAliases; 
      # shellInitLast = ''
      # '';
    };
  };

}
