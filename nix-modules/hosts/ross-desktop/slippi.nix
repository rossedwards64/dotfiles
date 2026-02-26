{ inputs, config, ... }:
{
  configurations.home."${config.flake.meta.user.username}@ross-desktop".module = {
    imports = [ inputs.slippi-nix.homeManagerModules.default ];
  };
}
