{
  flake.modules.homeManager.base =
    { config, ... }:
    {
      programs.zsh.history =
        let
          xdg = config.xdg;
        in
        {
          append = true;
          expireDuplicatesFirst = true;
          extended = true;
          ignoreAllDups = true;
          ignoreDups = true;
          ignoreSpace = true;
          path = "${xdg.dataHome}/zsh/history";
          save = 10000;
          share = true;
          size = 10000;
        };
    };
}
