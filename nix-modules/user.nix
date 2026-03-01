{ config, ... }:
{
  flake = {
    meta.user = {
      username = "ross";
      name = "Ross Edwards";
      email = "redwards64@hotmail.com";
    };

    modules = {
      nixos.base =
        { pkgs, ... }:
        {
          users.users.${config.flake.meta.user.username} = {
            description = config.flake.meta.user.name;
            isNormalUser = true;
            shell = pkgs.zsh;
            useDefaultShell = true;
            extraGroups = [
              "networkmanager"
              "wheel"
              "video"
              "libvirtd"
              "plugdev"
            ];
          };
        };
    };
  };
}
