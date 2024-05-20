{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ gcc gnumake clang-tools_17 gdb seer ];
}
