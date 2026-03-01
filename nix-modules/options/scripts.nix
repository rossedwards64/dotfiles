{ lib, ... }:
{
  options.flake.scripts = lib.mkOption { type = lib.types.anything; };
}
