{
  flake.modules.homeManager.base =
    { pkgs, ... }:
    {
      programs.zsh.shellAliases = {
        vim = "${pkgs.neovim}/bin/nvim";
        mv = "${pkgs.coreutils}/bin/mv -iv";
        cp = "${pkgs.coreutils}/bin/cp -iv";
        rm = "${pkgs.coreutils}/bin/rm -iv";
        ls = "${pkgs.eza}/bin/eza --icons --color=always";
        la = "${pkgs.eza}/bin/eza --icons --color=always -ah";
        l = "${pkgs.eza}/bin/eza --icons --color=always -lah";
        cd = "z";
        cat = "${pkgs.bat}/bin/bat";
        grep = "${pkgs.ripgrep}/bin/rg";
        find = "${pkgs.fd}/bin/fd";
        du = "${pkgs.dust}/bin/dust -Hr";
        clear = "clear && ${pkgs.coreutils}/bin/stty sane";
      };
    };
}
