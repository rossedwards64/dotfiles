{
  flake.modules.homeManager.base.programs.tealdeer = {
    enable = true;

    settings = {
      display = {
        compact = true;
        nuse_pager = true;
      };

      updates = {
        auto_update = true;
      };
    };
  };
}
