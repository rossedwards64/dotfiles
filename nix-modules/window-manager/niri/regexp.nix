{ config, lib, ... }:
let
  regexp = {
    any = ".*";
    caprine = "^Caprine$";
    vesktop = "^vesktop$";
    emacs = "^emacs(client)?$";
    epicGames = "^heroic$";
    librewolf = "^librewolf$";
    freetube = "^FreeTube$";
    gameConqueror = "^GameConqueror.py$";
    game = "^(${
      lib.strings.concatStrings (
        lib.intersperse "|" [
          "Godot_Engine"
          "VampireSurvivors.exe"
          "blue-revolver"
          "dwarfort"
          "factorio"
          "gamescope"
          "nwmain-linux"
          "soulstorm"
          "spring"
        ]
      )
    }).*$";
    intellij = "^jetbrains-idea$";
    itchio = {
      client = "^itch$";
      game = "^.*\.x86_64$";
    };
    lutris = "^net.lutris.Lutris$";
    minecraft = "^com-atlauncher-App$";
    mpv = "^mpv$";
    qBitTorrent = "^org.qbittorrent.qBittorrent$";
    spotify = "^(dev.alextren.)?(?i)spot(ify)?$";
    steam = {
      client = "^steam$";
      game = "^steam_app_[0-9]*$";
    };
    terminal = "^Alacritty$";
    vlc = "^vlc$";
    volume = "^myxer$";
    zathura = "^org.pwmt.zathura$";
  };

  games =
    with regexp;
    [
      steam.game
      game
      gameConqueror
    ]
    |> builtins.map (name: {
      matches = [
        { title = name; }
        { app-id = name; }
      ];

      open-on-workspace = "5";
      open-focused = true;
      open-maximized = true;
    });

  chatApps =
    with regexp;
    [
      vesktop
      caprine
    ]
    |> builtins.map (name: {
      matches = [
        { title = name; }
        { app-id = name; }
      ];

      open-on-workspace = "2";
    });

  gameLaunchers =
    with regexp;
    [
      steam.client
      epicGames
      itchio.client
      lutris
      minecraft
    ]
    |> builtins.map (name: {
      matches = [
        { title = name; }
        { app-id = name; }
      ];

      open-on-workspace = "4";
    });

  media =
    with regexp;
    [
      spotify
      freetube
      mpv
      vlc
    ]
    |> builtins.map (name: {
      matches = [
        { title = name; }
        { app-id = name; }
      ];
      open-on-workspace = "3";
    });

  programming =
    with regexp;
    [
      emacs
      terminal
    ]
    |> builtins.map (name: {
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
