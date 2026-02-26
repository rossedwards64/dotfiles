{
  inputs,
  lib,
  config,
  ...
}:
{
  flake.scripts.toggleMute =
    let
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
    in
    pkgs.writers.writeGuileBin "toggle-mute"
      {
        libraries = with pkgs; [
          libnotify
          wireplumber
        ];
      }
      (
        builtins.replaceStrings [ "#!/usr/bin/env guile\n!#" ] [ "" ] (
          lib.readFile "/home/${config.flake.meta.user.username}/.local/share/bin/toggle-mute"
        )
      );
}
