{
  flake.modules.homeManager.base =
    {
      home.sessionVariables = {
        MOZ_USE_XINPUT = 1;
        MOZ_ENABLE_WAYLAND = 1;
      };

      programs.librewolf = {
        enable = true;

        profiles.plain = {
          id = 1;
          name = "plain";
        };
      };
    };
}
