{ lib, ... }:
{
  options.flake.pkgs = lib.mkOption { type = lib.types.anything; };
}
