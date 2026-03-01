{
  configurations.nixos.ross-thinkpad-x230.module = {
    networking.hostName = "ross-thinkpad-x230";
    system.stateVersion = "23.11";

    services = {
      blueman.enable = true;
      xserver.xkb.layout = "gb";
      tlp.settings.NATACPI_ENABLE = 1;
    };

    stylix.fonts.sizes = {
      applications = 12;
      terminal = 12;
      desktop = 10;
      popups = 14;
    };
  };
}
