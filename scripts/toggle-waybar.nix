
{ pkgs, ... }:
pkgs.writeShellScriptBin "toggle-waybar" ''
  if pgrep -x ".waybar-wrapped" > /dev/null; then
      pkill -x waybar
      sleep 1
  else
      waybar &
  fi
''


