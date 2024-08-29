{ pkgs }:

pkgs.writeShellApplication {
  name = "check-mute";
  runtimeInputs = with pkgs; [
    wireplumber
    libnotify
  ];

  text = ''
    usage() {
        cat <<EOF
    Script for muting the speaker or microphone.
    m: Toggle microphone mute.
    s: Toggle speaker mute.
    h: Help.
    EOF
    }

    toggle_device() {
        device="$1"
        default_device="$2"

        if wpctl get-volume "$default_device" ǀ grep -q '[MUTED]'; then
            notify-send "Muted $device"
        else
            notify-send "Unmuted $device"
        fi
    }

    while getopts "hms" arg; do
        case "$arg" in
            m)
                toggle_device 'Microphone' '@DEFAULT_SOURCE@'
                ;;
            s)
                toggle_device 'Speaker' '@DEFAULT_SINK@'
                ;;
            h)
                usage
                exit 0
                ;;
            *)
                usage
                exit 1
                ;;
        esac
    done
  '';
}
