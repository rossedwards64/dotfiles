{ inputs, config, ... }:
let
  inherit (config.flake.meta.user) username;
in
{
  configurations = {
    nixos.ross-desktop.module = {
      imports = [ inputs.slippi-nix.nixosModules.default ];
    };

    home."${username}@ross-desktop".module = {
      imports = [ inputs.slippi-nix.homeManagerModules.default ];

      slippi-launcher = {
        isoPath = "/SSD/Games/GameCube and Wii/Super Smash Bros. Melee (USA) (En,Ja) (v1.02).iso";
        launchMeleeOnPlay = true;
        enableJukebox = true;
        rootSlpPath = "/home/${username}/Documents/Slippi/Matches";
        spectateSlpPath = "/home/${username}/Documents/Slippi/Spectate";
        useMonthlySubfolders = true;
      };
    };
  };
}
