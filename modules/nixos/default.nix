{ ... }:

{
  imports = [
    /etc/nixos/cachix.nix
    ./boot
    ./desktop
    ./environment
    ./games
    ./gnome
    ./kde
    ./networking
    ./nix
    ./qemu
    ./syncthing
    ./system
    ./thinkpad
    ./user
    ./fonts
    ./window-manager
  ];
}
