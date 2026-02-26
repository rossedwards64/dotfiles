{
  inputs,
  lib,
  config,
  ...
}:
{
  flake.scripts.powerMenu =
    let
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
    in
    pkgs.writers.writeGuileBin "powermenu"
      {
        libraries = with pkgs; [
          fuzzel
          procps
          coreutils
          systemd
          playerctl
          systemd
          coreutils
        ];
      }
      (
        builtins.replaceStrings [ "#!/usr/bin/env guile\n!#" ] [ "" ] (
          lib.readFile "/home/${config.flake.meta.user.username}/.dotfiles/.config/fuzzel/scripts/powermenu"
        )
      );
}
