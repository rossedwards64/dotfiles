{ ... }@moduleArgs:
{
  flake.modules.homeManager.base =
    { pkgs, config, ... }:
    {
      programs.zsh.sessionVariables =
        let
          xdg = config.xdg;
          emacsPackage = moduleArgs.config.flake.pkgs.emacs;
        in
        {
          _JAVA_AWT_WM_NONREPARENTING = "1";
          ALTERNATE_EDITOR = "${pkgs.neovim}/bin/nvim";
          ARCHFLAGS = "-arch x86_64";
          BROWSER = "${pkgs.librewolf}/bin/librewolf";
          CARGO_HOME = "${xdg.dataHome}/cargo";
          DIRENV_ALLOW_NIX = "1";
          EDITOR = "${emacsPackage}/bin/emacs";
          EMACSDIR = "${xdg.configHome}/emacs";
          GDBHISTFILE = "${xdg.dataHome}/gdb/history";
          GOPATH = "${xdg.dataHome}/go";
          GRADLE_USER_HOME = "${xdg.dataHome}/gradle";
          KDEHOME = "${xdg.configHome}/kde";
          LEDGER_FILE = "$HOME/Documents/finance/$(${pkgs.coreutils}/bin/date -I | cut -d'-' -f1).journal";
          LEIN_HOME = "${xdg.dataHome}/lein";
          NH_FLAKE = "$HOME/.dotfiles";
          PASSWORD_STORE_DIR = "${xdg.dataHome}/pass";
          PLATFORMIO_CORE_DIR = "${xdg.configHome}/platformio";
          ROSWELL_HOME = "${xdg.configHome}/roswell";
          RUSTUP_HOME = "${xdg.dataHome}/rustup";
          SDL_VIDEODRIVER = "wayland,x11";
          STACK_XDG = 1;
          TEXMFCONFIG = "${xdg.configHome}/texlive/texmf-config";
          TEXMFHOME = "${xdg.dataHome}/texlive/texmf";
          TEXMFVAR = "${xdg.stateHome}/texlive/texmf-var";
          VISUAL = "${emacsPackage}/bin/emacsclient -c -a emacs";
          WAKATIME_HOME = "${xdg.configHome}/wakatime";
          WINEPREFIX = "${xdg.dataHome}/wineprefixes/default";
          XCURSOR_SIZE = 24;
          ZSH_COMPDUMP = "\${ZSH}/cache/.zcompdump-\${HOST}";
        };
    };
}
