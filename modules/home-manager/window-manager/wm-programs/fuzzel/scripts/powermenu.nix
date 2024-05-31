{ pkgs }:

pkgs.writeShellApplication {
  name = "powermenu";
  runtimeInputs = with pkgs; [
    coreutils
    fuzzel
    gawk
    playerctl
    procps
    swaylock
  ];

  text = ''
    uptime="$(${pkgs.procps}/bin/uptime -p | sed -e 's/up //g')"

    shutdown="󰐥 Shutdown"
    reboot="󰜉 Restart"
    lock="󰌾 Lock"
    suspend="󰒲 Sleep"
    logout="󰩈 Logout"

    rdialog() {
        fuzzel -l1 -p "Are you sure?: " --dmenu
    }

    options="$lock\n$suspend\n$logout\n$reboot\n$shutdown"
    lines="$(echo -e "$options" | awk 'END{print NR}')"
    chosen="$(echo -e "$options" | fuzzel --password="" -p "UPTIME: $uptime" --dmenu -l "$lines" -w 40)"

    case $chosen in
        "$shutdown")
            ans=$(rdialog &)
            if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
                systemctl poweroff
            elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
                exit
            fi
            ;;
        "$reboot")
            ans=$(rdialog &)
            if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
                systemctl reboot
            elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
                exit
            fi
            ;;
        "$lock")
            swaylock
            ;;
        "$suspend")
            ans=$(rdialog &)
            if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
                playerctl pause
                systemctl suspend
            elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
                exit
            fi
            ;;
        "$logout")
            ans=$(rdialog &)
            if [[ $ans == "yes" ]] || [[ $ans == "YES" ]] || [[ $ans == "y" ]]; then
                killall -u "$USER"
            elif [[ $ans == "no" ]] || [[ $ans == "NO" ]] || [[ $ans == "n" ]]; then
                exit
            fi
            ;;
    esac
  '';
}
