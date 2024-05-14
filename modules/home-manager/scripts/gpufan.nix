{ pkgs }:

pkgs.writeShellApplication {
  name = "gpufan";
  runtimeInputs = with pkgs; [ bc coreutils gawk ];

  text = ''
    temp_input=("/sys/class/drm/card"[[:digit:]]"/device/hwmon/"[[:print:]]*"/temp1_input")
    pwm1=("/sys/class/drm/card"[[:digit:]]"/device/hwmon/"[[:print:]]*"/pwm1")
    pwm1_enable=("/sys/class/drm/card"[[:digit:]]"/device/hwmon/"[[:print:]]*"/pwm1_enable")

    percentage='0'
    pwm_value='0'

    usage()
    {
        cat <<EOF
    Script to change GPU fan speed.
    Enter the percentage of the desired PWM value.
    p: Fan speed percentage between 0 and 100. This will be converted to a PWM value.
    r: Get current speed.
    d: Daemonize script to respond to GPU temperature and change speed accordingly.
    h: Help.
    EOF
    }

    get_pwm_from_percent()
    {
        echo "scale=2;$percentage * 2.55" | bc | awk '{ printf "%d", $1 }'
    }

    get_percent_from_pwm()
    {
        echo "scale=2;($pwm_value / 255) * 100" | bc | awk '{ printf "%d", $1 }'
    }

    get_speed()
    {
        echo "Current GPU fan speed is: $(get_percent_from_pwm "$pwm_value")%"
        exit 0
    }

    enable_fan()
    {
        if [[ "$(head "''${pwm1_enable[0]}")" == "1" ]]; then
                change_speed "$percentage"
        else
            echo -n "The GPU fan is currently in automatic mode. Would you like to change it to manual mode? y/n "
            read -r choice

            if [[ $choice == "y" ]]; then
                echo "Setting fan to manual mode..."
                echo 1 | sudo tee -a "''${pwm1_enable[0]}"
            else
                echo "Keeping fan in automatic mode."
                exit 0
            fi
        fi
    }

    change_speed()
    {
        if [[ "$percentage" -lt 0 || "$percentage" -gt 100 ]]; then
            echo "Value given must be between 0 and 100."
        else
            pwm_value="$(get_pwm_from_percent "$percentage")"
            echo "Converted $percentage to $pwm_value."
            echo "Now setting fan speed to $percentage% speed..."
            echo "$pwm_value" | sudo tee -a "''${pwm1[0]}"
        fi
    }

    daemonize()
    {
        set -x
        tcurve="$(readarray -td',' <<<"$(head -n1 ~/.config/gpufan/config)")"
        fcurve="$(readarray -td',' <<<"$(tail -n2 ~/.config/gpufan/config | tr -d '\n')")"

        while :; do
            sleep 5
            temp=$(bc < "''${temp_input[0]}")

            for t in "''${!tcurve[@]}"; do
                cat<<-EOF
                temp: ''${tcurve[$t]}
                fanspeed: ''${fcurve[$t]}
    EOF
                if [[ ''${temp::-3} -eq "''${tcurve[$t]}" ]]; then
                    change_speed "''${fcurve[$t]}"
                fi
            done
        done
    }

    while getopts 'p:drh' arg; do
        case "$arg" in
            p)
                percentage="$OPTARG"
                if [[ "$percentage" =~ ^[0-9]+$ ]]; then
                    enable_fan
                    change_speed "$percentage"
                else
                    echo "Invalid percentage value."
                    usage
                    exit 1
                fi
                ;;
            d)
                # nohup daemonize & &>/dev/null disown
                daemonize
                ;;
            r)
                pwm_value="$(head "''${pwm1[0]}")"
                get_speed
                ;;
            h)
                usage
                exit 0
                ;;
            *)
                echo "Invalid argument."
                usage
                exit 1
                ;;
        esac
    done
  '';
}
