# 
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

  WAYBAR_PID=$(pgrep -x waybar)

  if [ -z "$WAYBAR_PID" ]; then
      waybar &
  else
      pkill $WAYBAR_PID
  fi
''

