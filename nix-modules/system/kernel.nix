{ inputs, ... }:
{
  flake.modules.nixos.base =
    { pkgs, ... }:
    {
      boot.kernelPackages =
        inputs.nix-cachyos-kernel.legacyPackages.${pkgs.stdenv.hostPlatform.system}.linuxPackages-cachyos-latest;
    };
}
