# { pkgs }:
# 
# pkgs.writeShellScriptBin "rofi-launcher" ''
#   if pgrep -x "rofi" > /dev/null; then
#     # Rofi is running, kill it
#     pkill -x rofi
#     exit 0
#   fi
#   rofi -show drun
# ''


{ pkgs, ... }:
pkgs.writeShellScriptBin "toggle-waybar" ''
  if ps aux | grep "[w]aybar" > /dev/null; then
      pkill -x "waybar"
  else
      waybar &
  fi
''

    # if pgrep -x "waybar" > /dev/null; then
    #     pkill -x "waybar"
    #     sleep 1
    # else
    #     waybar &
    # fi

