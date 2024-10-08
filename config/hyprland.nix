{
  lib,
  username,
  host,
  config,
  pkgs,
  ...
}:

let
  inherit (import ../hosts/${host}/variables.nix)
    browser
    terminal
    extraMonitorSettings
    keyboardLayout
    ;
in
with lib;
{
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    systemd.enable = true;
    plugins = with pkgs; [ hyprlandPlugins.hy3 ];
    extraConfig =
      let
        modifier = "SUPER";
      in
      concatStrings [
        ''
          env = NIXOS_OZONE_WL, 1
          env = NIXPKGS_ALLOW_UNFREE, 1
          env = XDG_CURRENT_DESKTOP, Hyprland
          env = XDG_SESSION_TYPE, wayland
          env = XDG_SESSION_DESKTOP, Hyprland
          env = GDK_BACKEND, wayland, x11
          env = CLUTTER_BACKEND, wayland
          env = QT_QPA_PLATFORM=wayland;xcb
          env = QT_WAYLAND_DISABLE_WINDOWDECORATION, 1
          env = QT_AUTO_SCREEN_SCALE_FACTOR, 1
          env = SDL_VIDEODRIVER, x11
          env = MOZ_ENABLE_WAYLAND, 1
          exec-once = dbus-update-activation-environment --systemd --all
          exec-once = systemctl --user import-environment QT_QPA_PLATFORMTHEME WAYLAND_DISPLAY XDG_CURRENT_DESKTOP
          exec-once = killall -q swww;sleep .5 && swww-daemon --format xrgb
          # exec-once = killall -q waybar;sleep .5 && waybar
          exec-once = killall -q swaync;sleep .5 && swaync
          exec-once = nm-applet --indicator
          exec-once = lxqt-policykit-agent
          exec-once = sleep 1.5 && swww img /home/${username}/Pictures/Wallpapers/ezzobir-wallpaper.jpg
          monitor=,preferred,auto,1
          ${extraMonitorSettings}
          general {
            gaps_in = 3 #3
            gaps_out = 0 #5
            border_size = 3
            layout = hy3
            resize_on_border = true
            col.active_border = rgb(${config.stylix.base16Scheme.base08}) rgb(${config.stylix.base16Scheme.base0C}) 45deg
            col.inactive_border = rgb(${config.stylix.base16Scheme.base01})
          }
          input {
            kb_layout = ${keyboardLayout}
            kb_options = grp:win_space_toggle
            follow_mouse = 1
            touchpad {
              natural_scroll = true
              disable_while_typing = true
              scroll_factor = 0.8
            }
            sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
            accel_profile = flat
          }
          windowrule = noborder,^(wofi)$
          windowrule = center,^(wofi)$
          windowrule = center,^(steam)$
          windowrule = float, nm-connection-editor|blueman-manager
          windowrule = float, swayimg|vlc|Viewnior|pavucontrol
          windowrule = float, nwg-look|qt5ct|mpv
          windowrule = float, zoom
          windowrulev2 = stayfocused, title:^()$,class:^(steam)$
          windowrulev2 = minsize 1 1, title:^()$,class:^(steam)$
          windowrulev2 = opacity 0.9 0.7, class:^(Brave)$
          windowrulev2 = opacity 0.9 0.7, class:^(thunar)$
          gestures {
            workspace_swipe = true
            workspace_swipe_fingers = 3
          }
          misc {
            initial_workspace_tracking = 0
            mouse_move_enables_dpms = true
            key_press_enables_dpms = false
          }
          animations {
            enabled = yes
            bezier = wind, 0.05, 0.9, 0.1, 1.05
            bezier = winIn, 0.1, 1.1, 0.1, 1.1
            bezier = winOut, 0.3, -0.3, 0, 1
            bezier = liner, 1, 1, 1, 1
            animation = windows, 1, 6, wind, slide
            animation = windowsIn, 1, 6, winIn, slide
            animation = windowsOut, 1, 5, winOut, slide
            animation = windowsMove, 1, 5, wind, slide
            animation = border, 1, 1, liner
            animation = fade, 1, 10, default
            animation = workspaces, 1, 5, wind
          }
          decoration {
            rounding = 0 # 10
            drop_shadow = true
            shadow_range = 4
            shadow_render_power = 3
            col.shadow = rgba(1a1a1aee)
            blur {
                enabled = true
                size = 5
                passes = 3
                new_optimizations = on
                ignore_opacity = off
            }
          }
          plugin {
            hyprtrails {
            }
            hy3 {
              no_gaps_when_only = 1 
              tab_first_window = false
              tabs {
                  height = 4
                  padding = 6
                  render_text = false
              }

              autotile {
                  enable = true
                  trigger_width = 800
                  trigger_height = 500
              }
            }
          }
          # dwindle {
          #   pseudotile = true
          #   preserve_split = true
          # }
          bind = ${modifier},Return,exec,${terminal}
          bind = ${modifier},D,exec,rofi-launcher
          # bind = ${modifier},Y,exec, waybar &
          # bind = ${modifier}CONTROL,Y,exec, pkill waybar
          bind = ${modifier},Y,exec, pkill waybar || waybar &
          bind = ${modifier}SHIFT,N,exec,swaync-client -rs
          bind = ${modifier},W,exec,${browser}
          bind = ${modifier}SHIFT,W,exec,web-search
          bind = ${modifier}ALT,W,exec,wallsetter
          bind = ${modifier}CONTROL,E,exec,emopicker9000
          # bind = ${modifier},P,exec,screenshootin
          bind = ${modifier},P,exec,flameshot gui
          bind = ${modifier},C,exec,hyprpicker -a
          bind = ${modifier},T,exec,thunar
          bind = ${modifier},M,exec,spotify
          bind = ${modifier},Q,hy3:killactive,
          # bind = ${modifier},P,pseudo,

          bind = ${modifier},F,fullscreen, 0
          bind = ${modifier}SHIFT,F,fullscreen, 1
          bind = ${modifier}SHIFT,Tab,togglefloating

          bind = ${modifier},Tab,hy3:changegroup,toggletab
          bind = ${modifier},semicolon,hy3:makegroup,h
          bind = ${modifier},comma,hy3:makegroup,v
          bind = ${modifier},period,hy3:makegroup,tab
          bind = ${modifier},A,hy3:changefocus,raise
          bind = ${modifier}SHIFT,A,hy3:changefocus,lower
          bind = ${modifier},E,hy3:expand,expand
          bind = ${modifier}SHIFT,E,hy3:expand,base
          bind = ${modifier},I,hy3:changegroup,opposite

          bind = ${modifier}, h, hy3:movefocus, l
          bind = ${modifier}, j, hy3:movefocus, d
          bind = ${modifier}, k, hy3:movefocus, u
          bind = ${modifier}, l, hy3:movefocus, r
          bind = ${modifier}, left, hy3:movefocus, l
          bind = ${modifier}, down, hy3:movefocus, d
          bind = ${modifier}, up, hy3:movefocus, u
          bind = ${modifier}, right, hy3:movefocus, r
          
          # begin resize mode
          # will switch to a submap called resize
          bind = ${modifier}, R, submap, resize
          # will start a submap called "resize"
          submap = resize
          # sets repeatable binds for resizing the active window
          binde = , l, resizeactive, 10 0
          binde = , h, resizeactive, -10 0
          binde = , k, resizeactive, 0 -10
          binde = , j, resizeactive, 0 10
          # use reset to go back to the global submap
          bind = , escape, submap, reset 
          # will reset the submap, which will return to the global submap
          submap = reset
          # keybinds further down will be global again...
          # end resize mode

          # bind = ${modifier}CONTROL, h, hy3:movefocus, l, visible, nowarp
          # bind = ${modifier}CONTROL, j, hy3:movefocus, d, visible, nowarp
          # bind = ${modifier}CONTROL, k, hy3:movefocus, u, visible, nowarp
          # bind = ${modifier}CONTROL, l, hy3:movefocus, r, visible, nowarp
          # bind = ${modifier}CONTROL, left, hy3:movefocus, l, visible, nowarp
          # bind = ${modifier}CONTROL, down, hy3:movefocus, d, visible, nowarp
          # bind = ${modifier}CONTROL, up, hy3:movefocus, u, visible, nowarp
          # bind = ${modifier}CONTROL, right, hy3:movefocus, r, visible, nowarp
          
          bind = ${modifier}SHIFT, h, hy3:movewindow, l, once
          bind = ${modifier}SHIFT, j, hy3:movewindow, d, once
          bind = ${modifier}SHIFT, k, hy3:movewindow, u, once
          bind = ${modifier}SHIFT, l, hy3:movewindow, r, once
          bind = ${modifier}SHIFT, left, hy3:movewindow, l, once
          bind = ${modifier}SHIFT, down, hy3:movewindow, d, once
          bind = ${modifier}SHIFT, up, hy3:movewindow, u, once
          bind = ${modifier}SHIFT, right, hy3:movewindow, r, once
          
          # bind = ${modifier}CONTROL SHIFT, h, hy3:movewindow, l, once, visible
          # bind = ${modifier}CONTROL SHIFT, j, hy3:movewindow, d, once, visible
          # bind = ${modifier}CONTROL SHIFT, k, hy3:movewindow, u, once, visible
          # bind = ${modifier}CONTROL SHIFT, l, hy3:movewindow, r, once, visible
          # bind = ${modifier}CONTROL SHIFT, left, hy3:movewindow, l, once, visible
          # bind = ${modifier}CONTROL SHIFT, down, hy3:movewindow, d, once, visible
          # bind = ${modifier}CONTROL SHIFT, up, hy3:movewindow, u, once, visible
          # bind = ${modifier}CONTROL SHIFT, right, hy3:movewindow, r, once, visible

          bind = ${modifier}SHIFT,C,exit,

          bind = ${modifier},1,workspace,1
          bind = ${modifier},2,workspace,2
          bind = ${modifier},3,workspace,3
          bind = ${modifier},4,workspace,4
          bind = ${modifier},5,workspace,5
          bind = ${modifier},6,workspace,6
          bind = ${modifier},7,workspace,7
          bind = ${modifier},8,workspace,8
          bind = ${modifier},9,workspace,9
          bind = ${modifier},0,workspace,10

          bind = ${modifier}SHIFT,V,movetoworkspace,special
          bind = ${modifier},V,togglespecialworkspace
          bind = ${modifier}SHIFT,1,movetoworkspace,1
          bind = ${modifier}SHIFT,2,movetoworkspace,2
          bind = ${modifier}SHIFT,3,movetoworkspace,3
          bind = ${modifier}SHIFT,4,movetoworkspace,4
          bind = ${modifier}SHIFT,5,movetoworkspace,5
          bind = ${modifier}SHIFT,6,movetoworkspace,6
          bind = ${modifier}SHIFT,7,movetoworkspace,7
          bind = ${modifier}SHIFT,8,movetoworkspace,8
          bind = ${modifier}SHIFT,9,movetoworkspace,9
          bind = ${modifier}SHIFT,0,movetoworkspace,10
          bind = ${modifier}CONTROL,right,workspace,e+1
          bind = ${modifier}CONTROL,left,workspace,e-1
          bind = ${modifier},mouse_down,workspace, e+1
          bind = ${modifier},mouse_up,workspace, e-1
          bindm = ${modifier},mouse:272,movewindow
          bindm = ${modifier},mouse:273,resizewindow
          # bind = ALT,Tab,cyclenext
          # bind = ALT,Tab,bringactivetotop
          bind = ,XF86AudioRaiseVolume,exec,wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+
          bind = ,XF86AudioLowerVolume,exec,wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
          binde = ,XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
          bind = ,XF86AudioPlay, exec, playerctl play-pause
          bind = ,XF86AudioPause, exec, playerctl play-pause
          bind = ,XF86AudioNext, exec, playerctl next
          bind = ,XF86AudioPrev, exec, playerctl previous
          bind = ,XF86MonBrightnessDown,exec,brightnessctl set 5%-
          bind = ,XF86MonBrightnessUp,exec,brightnessctl set +5%
        ''
      ];
  };
}
