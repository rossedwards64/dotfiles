{ lib, config, ... }:
{
  flake.modules.homeManager.base.wayland.windowManager.sway.config =
    with config.flake.meta.windowManager;
    let
      focusOnGameCommand = ''
        {
          allow_tearing yes
          inhibit_idle fullscreen
          fullscreen enable
          focus
        }
      '';

      focusOnGame =
        with regexps;
        [
          steam.game
          game
        ]
        |> lib.lists.concatMap (class: [
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
        |> lib.lists.concatMap (window_role: [
          {
            criteria = {
              inherit window_role;
            };
            command = "floating enable";
          }
        ]);

      sendToScratchPad =
        with regexps;
        [
          volume
          zathura
          qBitTorrent
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
      assigns = with regexps; {
        "workspace 1" = [ { app_id = emacs; } ];
        "workspace 2" = [ { app_id = terminal; } ];
        "workspace 3" = [ { app_id = librewolf; } ];
        "workspace 4" = [ { app_id = vesktop; } ];
        "workspace 5" = [
          { app_id = spotify; }
          { class = spotify; }
          { class = freetube; }
          { app_id = mpv; }
          { class = vlc; }
        ];
        "workspace 6" = [
          { class = steam.client; }
          { class = epicGames; }
          { class = itchio.client; }
          { app_id = faugus-launcher; }
          { class = minecraft; }
          { app_id = gameConqueror; }
        ];
        "workspace 7" = [
          { class = itchio.game; }
          { class = steam.game; }
          { class = game; }
          { app_id = game; }
        ];
        "workspace 8" = [ ];
        "workspace 9" = [ { class = intellij; } ];
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
              class = regexps.steam.client;
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
