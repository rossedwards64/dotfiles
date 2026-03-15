{ config, ... }:
with config.flake.meta.windowManager;
let
  games =
    with regexps;
    [
      steam.game
      game
      gameConqueror
    ]
    |> map (name: {
      matches = [
        { title = name; }
        { app-id = name; }
      ];

      open-on-workspace = "5";
      open-focused = true;
      open-maximized = true;
    });

  chatApps =
    with regexps;
    [
      vesktop
      caprine
    ]
    |> map (name: {
      matches = [
        { title = name; }
        { app-id = name; }
      ];

      open-on-workspace = "2";
    });

  gameLaunchers =
    with regexps;
    [
      steam.client
      epicGames
      itchio.client
      faugus-launcher
      minecraft
    ]
    |> map (name: {
      matches = [
        { title = name; }
        { app-id = name; }
      ];

      open-on-workspace = "4";
    });

  media =
    with regexps;
    [
      spotify
      freetube
      mpv
      vlc
    ]
    |> map (name: {
      matches = [
        { title = name; }
        { app-id = name; }
      ];
      open-on-workspace = "3";
    });

  programming =
    with regexps;
    [
      emacs
      terminal
    ]
    |> map (name: {
      matches = [
        { title = name; }
        { app-id = name; }
      ];

      open-on-workspace = "1";
    });
in
{
  flake.modules.homeManager.base.programs.niri.settings = {
    workspaces = with config.flake.meta.monitors; {
      "1".open-on-output = hdmi.name;
      "2".open-on-output = dp2.name;
      "3".open-on-output = hdmi.name;
      "4".open-on-output = dp2.name;
      "5".open-on-output = dp1.name;
    };

    window-rules = builtins.concatLists [
      chatApps
      gameLaunchers
      games
      media
      programming
    ];
  };
}
