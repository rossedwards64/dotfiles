{ pkgs }:

pkgs.writeShellApplication {
  name = "weather";
  runtimeInputs = with pkgs; [
    coreutils
    curl
  ];

  text = ''
    text="Weather: $(curl -s "https://wttr.in/?format=1")"
    tooltip="$(curl -s "https://wttr.in/?0QT" |
        sed 's/\\/\\\\/g' |
        sed ':a;N;$!ba;s/\n/\\n/g' |
        sed 's/"/\\"/g')"

    if ! grep -q "Unknown location" <<< "$text"; then
        echo "{\"text\": \"$text\", \"tooltip\": \"<tt>$tooltip</tt>\", \"class\": \"weather\"}"
    fi
  '';
}
