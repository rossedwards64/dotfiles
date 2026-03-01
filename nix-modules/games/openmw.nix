{ inputs, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.openmw
      ]
      ++ (with inputs.openmw-nix.packages.${pkgs.stdenv.hostPlatform.system}; [
        curldl
        delta-plugin
        groundcoverify
        momw-configurator
        openmw-validator
        plox
        s3lightfixes
        umo
      ]);
    };
}
