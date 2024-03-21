{ config, ... }:

{
  imports = [
    ./boot
    ./desktop
    ./environment
    ./networking
    ./programming
    ./services
    ./system
    ./thinkpad
    ./user
  ];
}
