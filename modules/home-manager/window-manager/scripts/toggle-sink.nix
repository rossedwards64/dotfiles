{ pkgs }:

pkgs.writeShellApplication {
  name = "toggle-sink";
  runtimeInputs = with pkgs; [
    wireplumber
    libnotify
  ];

  text = ''
    headset='Headset Analog'
    monitor='HDMI'
    device_id=

    get_device_id() {
        local sink="$1"
        local status_line

        status_line="$(wpctl status | grep "$sink" | grep 'vol')"
        sed -En 's/^[[:space:]]*│[[:space:]]*\*?//g;s/\..*$//p' <<<"$status_line" | tr -d '[:space:]'
    }

    is_default_device() {
        local sink="$1"
        local status_line
        local device

        status_line="$(wpctl status | grep "$sink" | grep 'vol')"
        device="$(sed -En 's/│//g;s/\..*$//p' <<<"$status_line" | tr -d '[:space:]')"

        if [[ "$device" =~ ^\*.*$ ]]; then
            return 0
        else
            return 1
        fi
    }

    if is_default_device "$headset"; then
        device_id="$(get_device_id "$monitor")"
        notify-send "Changing default sink to monitor."
    elif is_default_device "$monitor"; then
        device_id="$(get_device_id "$headset")"
        notify-send "Changing default sink to headphones."
    else
        notify-send "No valid sinks to switch to."
    fi

    wpctl set-default "$device_id"
  '';
}
