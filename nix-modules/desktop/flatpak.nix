{ inputs, ... }:
{
  flake.modules.homeManager.base = {
    imports = [ inputs.nix-flatpak.homeManagerModules.nix-flatpak ];

    services.flatpak = {
      enable = true;
      uninstallUnmanaged = true;
      packages = [
        "io.freetubeapp.FreeTube"
        "org.kartkrew.RingRacers"
      ];

      update.auto = {
        enable = true;
        onCalendar = "weekly";
      };
    };
  };
}
