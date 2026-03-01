{ inputs, ... }:
{
  flake.modules = {
    nixos.base = {
      imports = with inputs.nix-gaming.nixosModules; [
        pipewireLowLatency
        wine
      ];

      services.pipewire.lowLatency.enable = true;

      programs = {
        gamemode.enable = true;
        wine = {
          enable = true;
          ntsync = true;
        };
      };
    };
  };
}
