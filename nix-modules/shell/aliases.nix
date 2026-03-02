{ lib, ... }:
{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      programs.zsh.shellAliases = {
        vim = "${lib.getExe pkgs.neovim}";
        mv = "${lib.getExe' pkgs.coreutils "mv"} -iv";
        cp = "${lib.getExe' pkgs.coreutils "cp"} -iv";
        rm = "${lib.getExe' pkgs.coreutils "rm"} -iv";
        ls = "${lib.getExe pkgs.eza} --icons --color=always";
        la = "${lib.getExe pkgs.eza} --icons --color=always -ah";
        l = "${lib.getExe pkgs.eza} --icons --color=always -lah";
        cd = "z";
        cat = "${lib.getExe pkgs.bat}";
        grep = "${lib.getExe pkgs.ripgrep}";
        find = "${lib.getExe pkgs.fd}";
        du = "${lib.getExe pkgs.dust} -Hr";
        clear = "clear && ${lib.getExe' pkgs.coreutils "stty"} sane";
      };
    };
}
