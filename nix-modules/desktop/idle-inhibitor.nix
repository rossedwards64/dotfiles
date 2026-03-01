{ inputs, ... }:
{
  flake.modules.homeManager.base = {
    imports = [ inputs.wayland-pipewire-idle-inhibit.homeModules.default ];

    services.wayland-pipewire-idle-inhibit = {
      enable = true;
      systemdTarget = "sway-session.target";
      settings.idle_inhibitor = "wayland";
    };
  };
}
