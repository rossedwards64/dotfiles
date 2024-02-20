{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  nix = {
    package = pkgs.nixFlakes;
    settings = {
      auto-optimise-store = true;
      experimental-features = [ "nix-command" "flakes" ];
      allowed-users = [ "ross" ];
    };
  };

  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "23.11";
}
