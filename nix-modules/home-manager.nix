{
  lib,
  config,
  inputs,
  ...
}:
{
  options.configurations.home = lib.mkOption {
    type = lib.types.lazyAttrsOf (
      lib.types.submodule {
        options.module = lib.mkOption {
          type = lib.types.deferredModule;
        };
      }
    );
  };

  config.flake.homeConfigurations = lib.flip lib.mapAttrs config.configurations.home (
    name:
    { module }:
    inputs.home-manager.lib.homeManagerConfiguration {
      pkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
      modules = [
        module
        config.flake.modules.homeManager.base
      ];
    }
  );
}
