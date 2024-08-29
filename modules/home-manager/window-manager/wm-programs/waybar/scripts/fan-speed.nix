{ pkgs }:

pkgs.writeShellApplication {
  name = "fan-speed";
  runtimeInputs = with pkgs; [
    coreutils
    gnugrep
  ];

  text = ''
    grep 'speed' /proc/acpi/ibm/fan | head -n1 | cut -f3
  '';
}
