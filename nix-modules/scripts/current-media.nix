{ inputs, ... }:
{
  flake.scripts.currentMedia =
    let
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
    in
    pkgs.writeShellApplication {
      name = "current-media";
      runtimeInputs = with pkgs; [
        coreutils
        gnugrep
        playerctl
      ];

      text = ''
        icon=""
        player="$(playerctl -l | grep -E "(spotify|ncspot|Spot|spotify_player)" | head -n1)"
        class="$(playerctl metadata --player="''${player}" --format '{{lc(status)}}')"
        info="$(playerctl metadata --player="''${player}" --format '{{artist}} - {{title}}')"
        pos="$(playerctl metadata --player="''${player}" --format '{{duration(position)}}|{{duration(mpris:length)}}')"

        if [[ "''${#info}" -gt 40 ]]; then
          info="$(echo "$info" | cut -c1-40)..."
        fi

        text="$icon $info $pos"

        if [[ "$class" == "paused" ]]; then
          text+=" 󰏤"
        elif [[ "$class" == "stopped" ]]; then
          text+=" "
        fi

        echo -e "{\"text\":\"$text\", \"class\":\"$class\"}"
      '';
    };
}
