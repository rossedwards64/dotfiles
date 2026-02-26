{ config, lib, ... }:
{
  flake = {
    meta.user = {
      email = "redwards64@hotmail.com";
      name = "Ross Edwards";
      username = "ross";
    };

    modules = {
      nixos.base =
        { pkgs, ... }:
        {
          users.users.${config.flake.meta.user.username} = {
            isNormalUser = true;
            description = "Ross Edwards";
            extraGroups = [
              "networkmanager"
              "wheel"
              "video"
              "libvirtd"
              "plugdev"
            ];
            shell = pkgs.zsh;
            useDefaultShell = true;
          };
        };
    };
  };
}
