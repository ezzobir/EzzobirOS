
{ pkgs, ... }:
pkgs.writeShellScriptBin "toggle-waybar" ''
  if pgrep "waybar" > /dev/null; then
      pkill "waybar"
      sleep 1
  else
      waybar &
  fi
''


