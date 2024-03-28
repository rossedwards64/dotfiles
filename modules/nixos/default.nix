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
    ./services
    ./system
    ./thinkpad
    ./user
  ];
}
