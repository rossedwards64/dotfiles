{
  flake.modules.homeManager.base.programs.zsh.oh-my-zsh = {
    enable = true;
    extraConfig = "zstyle ':omz:update' mode auto";
    plugins = [
      "alias-finder"
      "aliases"
      "colored-man-pages"
      "command-not-found"
      "cp"
      "direnv"
      "extract"
      "fzf"
      "git"
      "gitfast"
      "gpg-agent"
      "history"
      "isodate"
      "jsontools"
      "lein"
      "otp"
      "pass"
      "ros"
      "rsync"
      "tmux"
      "torrent"
    ];
  };
}
