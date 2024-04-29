{ pkgs }:

pkgs.writeShellApplication {
  name = "wob";
  runtimeInputs = with pkgs; [ bc wob brightnessctl wireplumber findutils ];

  text = ''
    SYS=
    VAL=
    WOBSOCK="$XDG_RUNTIME_DIR/wob.sock"
    SINK="@DEFAULT_AUDIO_SINK@"

    set_volume() {
        wpctl set-volume -l 1.5 "$SINK" "$VAL"
        local cur_vol
        cur_vol="$(wpctl get-volume "$SINK" |
            cut -c 9-12 |
            xargs -I '{}' echo 'scale=4;{}*100' |
            bc |
            cut -d "." -f 1)"
        echo "$cur_vol"
        echo "$cur_vol" >"$WOBSOCK"
    }

    set_brightness() {
        local cur_bright
        cur_bright="$(brightnessctl set "$VAL" | sed -En 's/.*\(([0-9]+)%\).*/\1/p')"
        echo "$cur_bright" >"$WOBSOCK"
        exit 0
    }

    while getopts "vbs:i:d:" opt; do
        case "''${opt}" in
            v)
                [ -n "$SYS" ] && exit 1 || SYS="volume"
                ;;
            b)
                [ -n "$SYS" ] && exit 1 || SYS="brightness"
                ;;
            s)
                WOBSOCK="''${OPTARG}"
                ;;
            i)
                [ -n "$VAL" ] && exit 1 || VAL="''${OPTARG}%+"
                ;;
            d)
                [ -n "$VAL" ] && exit 1 || VAL="''${OPTARG}%-"
                ;;
            *)
                exit 1
                ;;
        esac
    done

    if [[ "$SYS" == "volume" ]]; then
        set_volume
    elif [[ "$SYS" == "brightness" ]]; then
        set_brightness
    fi
  '';
}
