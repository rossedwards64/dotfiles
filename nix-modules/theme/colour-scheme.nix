{ inputs, ... }:
let
  stylix = {
    base16Scheme = "${inputs.tinted-schemes}/base16/catppuccin-mocha.yaml";
    polarity = "dark";
  };
in
{
  flake.modules = {
    nixos.base = { inherit stylix; };
    homeManager.base = { inherit stylix; };
  };
}
