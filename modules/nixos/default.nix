{ config, ... }:

{
  imports = [
    ./boot
    ./desktop
    ./environment
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
