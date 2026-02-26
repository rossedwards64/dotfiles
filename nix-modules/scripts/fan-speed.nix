{ inputs, ... }:
{
  flake.scripts.fanSpeed =
    let
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
    in
    pkgs.writeShellApplication {
      name = "fan-speed";
      runtimeInputs = with pkgs; [
        coreutils
        gnugrep
      ];

      text = ''
        grep 'speed' /proc/acpi/ibm/fan | head -n1 | cut -f3
      '';
    };
}
