{ pkgs }:

pkgs.writeShellApplication {
  name = "screenshot";
  runtimeInputs = with pkgs; [
    fuzzel
    grim
    hyprland
    jq
    libnotify
    slurp
    sway
    xdg-user-dirs
  ];

  text = ''
    notify_view() {
      notify-send -u low --replace-id 699 "Copied to clipboard." "$dir/$file"
      if [[ -e "$dir/$file" ]]; then
    	   notify-send -u low --replace-id 699 "Screenshot Saved."
      else
        notify-send -u low --replace-id 699 "Screenshot Deleted."
      fi
    }

    countdown() {
        for sec in $(seq "$1" -1 1); do
    	      notify-send -t 1000 --replace 699 "Taking shot in : $sec"
    	      sleep 1
        done
    }

    get_geometry() {
      if [[ ''${XDG_CURRENT_DESKTOP} = "Hyprland" ]]; then
    	   hyprctl -j monitors |
           jq -r "\"\(.[$1].x),\(.[$1].y) \(.[$1].width)x\(.[$1].height)"\"
      elif [[ ''${XDG_CURRENT_DESKTOP} = "sway" ]]; then
    	   swaymsg -t get_outputs |
    	     jq -r '.[] | select(.focused) | .current_mode | .rect | "\(.x),\(.y) \(.width)x\(.height)"'
      fi
    }

    get_resolution() {
      if [[ ''${XDG_CURRENT_DESKTOP} = "Hyprland" ]]; then
    	   hyprctl -j monitors |
    	     jq -r "\"\(.[$1].width)x\(.[$1].height)\""
      elif [[ ''${XDG_CURRENT_DESKTOP} = "sway" ]]; then
    	   swaymsg -t get_outputs |
    	     jq -r '.[] | select(.focused) | .current_mode | .rect | "\(.width)x\(.height)"'
      fi
    }

    get_position() {
      if [[ ''${XDG_CURRENT_DESKTOP} = "sway" ]]; then
    	   swaymsg -t get_tree |
    	     jq -r '.. | select(.pid? and .visible?) | .rect | "\(.x),\(.y) \(.width)x\(.height)"'
      elif [[ ''${XDG_CURRENT_DESKTOP} = "Hyprland" ]]; then
    	   hyprctl -j activewindow |
    	     jq -r '"\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"'
      fi
    }

    take_shot() {
      grim -g - - | tee "$file" | wl-copy --type image/png
    }

    take_shot_screen() {
      cd "$dir" && get_geometry "$1" | take_shot
    }

    take_shot_now() {
      sleep 0.5 && take_shot_screen "$1"
      notify_view
    }

    take_shot_delay() {
      countdown "$2"
      sleep 1 && take_shot_screen "$1"
      notify_view
    }

    take_shot_win() {
      sleep 0.5 && cd "$dir" && get_position | take_shot
      notify_view
    }

    take_shot_area() {
      cd "$dir" && slurp | take_shot
      notify_view
    }

    time=$(date +%Y-%m-%d-%I-%M-%S)
    dir="$(xdg-user-dir PICTURES)/Screenshots"

    monitorid="0"
    geometry="$(get_resolution "$monitorid")"
    file="Screenshot_''${time}_''${geometry}.png"

    screen1=" Capture Desktop 1"
    screen2=" Capture Desktop 2"
    area=" Capture Area"
    window=" Capture Window"
    infive1=" Take in 3s (Desktop 1)"
    infive2=" Take in 3s (Desktop 2)"
    inten1=" Take in 10s (Desktop 1)"
    inten2=" Take in 10s (Desktop 2)"

    options="$screen1\n$screen2\n$area\n$window\n$infive1\n$infive2\n$inten1\n$inten2"

    chosen="$(echo -e "$options" | fuzzel --dmenu)"

    if [[ ! -d "$dir" ]]; then
      mkdir -p "$dir"
    fi

    case "$chosen" in
      "$screen1" | "$screen2")
        if [[ "$chosen" = "$screen1" ]]; then
    	     monitorid="0"
    	  elif [[ "$chosen" = "$screen2" ]]; then
    	     monitorid="1"
    	  fi
    	  geometry="$(get_resolution "$monitorid")"
    	  file="Screenshot_''${time}_''${geometry}.png"
    	  take_shot_now "$monitorid"
    	  ;;
      "$area")
    	  take_shot_area
    	  ;;
      "$window")
    	  take_shot_win
    	  ;;
      "$infive1" | "$infive2")
    	  if [[ "$chosen" = "$infive1" ]]; then
    	      monitorid="0"
    	  elif [[ "$chosen" = "$infive2" ]]; then
    	      monitorid=1
    	  fi
    	  geometry="$(get_resolution "$monitorid")"
    	  file="Screenshot_''${time}_''${geometry}.png"
    	  take_shot_delay "$monitorid" 5
    	  ;;
      "$inten1" | "$inten2")
    	  if [[ "$chosen" = "$inten1" ]]; then
    	      monitorid="0"
    	  elif [[ "$chosen" = "$inten2" ]]; then
    	      monitorid=1
    	  fi
    	  geometry="$(get_resolution "$monitorid")"
    	  file="Screenshot_''${time}_''${geometry}.png"
    	  take_shot_delay "$monitorid" 10
    	  ;;
    esac
  '';
}
