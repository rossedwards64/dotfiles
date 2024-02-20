{ ... }:

{
  imports = [
    ./boot.nix
    ./desktop.nix
    ./environment.nix
    ./networking.nix
    ./programming.nix
    ./security.nix
    ./services.nix
    ./system.nix
    ./user.nix
  ];

  config.modules = {
    boot.enable = true;
    desktop.enable = true;
    environment.enable = true;
    networking.enable = true;
    programming.enable = true;
    security.enable = true;
    services.enable = true;
    system.enable = true;
    user.enable = true;
  };
}