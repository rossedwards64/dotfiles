{ lib, ... }:
{
  flake.modules.homeManager.base.wayland.windowManager.sway.config =
    let
      regexp = {
        any = ".*";
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

      focusOnGameCommand = ''
        {
          allow_tearing yes
          inhibit_idle fullscreen
          fullscreen enable
          focus
        }
      '';

      focusOnGame =
        [
          regexp.steam.game
          regexp.game
        ]
        |> lib.concatMap (class: [
          {
            criteria.class = class;
            command = focusOnGameCommand;
          }
          {
            criteria.app_id = class;
            command = focusOnGameCommand;
          }
        ]);

      floatingEnable =
        [
          "pop-up"
          "task_dialog"
          "dialog"
        ]
        |> lib.concatMap (window_role: [
          {
            criteria = {
              inherit window_role;
            };
            command = "floating enable";
          }
        ]);

      sendToScratchPad =
        [
          regexp.volume
          regexp.zathura
          regexp.qBitTorrent
        ]
        |> map (app_id: {
          criteria = {
            inherit app_id;
          };
          command = ''
            {
              floating enable
              move to scratchpad
              move position center
              resize set 80ppt 80ppt
            }
          '';
        });
    in
    {
      assigns = {
        "workspace 1" = [ { app_id = regexp.emacs; } ];
        "workspace 2" = [ { app_id = regexp.terminal; } ];
        "workspace 3" = [ { app_id = regexp.librewolf; } ];
        "workspace 4" = [ { app_id = regexp.vesktop; } ];
        "workspace 5" = [
          { app_id = regexp.spotify; }
          { class = regexp.spotify; }
          { class = regexp.freetube; }
          { app_id = regexp.mpv; }
          { class = regexp.vlc; }
        ];
        "workspace 6" = [
          { class = regexp.steam.client; }
          { class = regexp.epicGames; }
          { class = regexp.itchio.client; }
          { app_id = regexp.lutris; }
          { class = regexp.minecraft; }
          { app_id = regexp.gameConqueror; }
        ];
        "workspace 7" = [
          { class = regexp.itchio.game; }
          { class = regexp.steam.game; }
          { class = regexp.game; }
          { app_id = regexp.game; }
        ];
        "workspace 8" = [ ];
        "workspace 9" = [ { class = regexp.intellij; } ];
        "workspace 10" = [ ];
      };

      window.commands = builtins.concatLists [
        floatingEnable
        focusOnGame
        sendToScratchPad
        [
          {
            criteria = {
              title = "^(?!Steam).*$";
              class = regexp.steam.client;
            };

            command = ''
              {
                floating enable
                resize set 80ppt 80ppt;
              }
            '';
          }
          {
            criteria = {
              all = true;
            };

            command = ''
              {
                inhibit_idle fullscreen
                title_format "<b>%title</b> (%app_id%instance,%shell)"
              }
            '';
          }
        ]
      ];
    };
}
