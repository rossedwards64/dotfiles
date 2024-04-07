{ config, ... }:

{
  imports = [
    ./boot
    ./desktop
    ./environment
    ./games
    ./gnome
    ./kde
    ./networking
    ./programming
    ./qemu
    ./services
    ./syncthing
    ./system
    ./thinkpad
    ./user
  ];
}
