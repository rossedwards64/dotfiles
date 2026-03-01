{
  flake.modules.homeManager.base.wayland.windowManager.sway.config.input = {
    "1:1:AT_Translated_Set_2_keyboard" = {
      xkb_layout = "gb";
      xkb_options = "ctrl:nocaps";
    };

    "12625:16386:ROYUAN_EPOMAKER_TH66_Keyboard" = {
      xkb_layout = "us";
      xkb_options = "ctrl:nocaps,altwin";
    };

    "2:7:SynPS/2_Synaptics_TouchPad" = {
      dwt = "enabled";
      tap = "enabled";
      natural_scroll = "enabled";
      middle_emulation = "enabled";
    };

    "1356:2508:Sony_Interactive_Entertainment_Wireless_Controller_Touchpad" = {
      events = "disabled";
    };
  };
}
