{ inputs, ... }:
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
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
          package = inputs.nix-gaming.packages.${pkgs.stdenv.hostPlatform.system}.wine-ge;
        };
      };
    };
}
