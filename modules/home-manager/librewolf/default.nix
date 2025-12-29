{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.librewolf;
  defaultEngine = rec {
    name = "DuckDuckGo";
    url = "https://www.duckduckgo.com/";
    icon = "${url}/favicon.ico";
    params = [
      {
        name = "q";
        value = "{searchTerms}";
      }
    ];
    alias = "@${lib.toLower name}";
  };
  updateInterval = 24 * 60 * 60 * 1000;
in
{
  options.modules.librewolf = {
    enable = mkEnableOption "librewolf";
  };

  config = mkIf cfg.enable {
    home.sessionVariables = {
      MOZ_USE_XINPUT = 1;
      MOZ_ENABLE_WAYLAND = 1;
    };

    nixpkgs.config.packageOverrides = pkgs: {
      nur = import (fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
        inherit pkgs;
      };
    };

    programs.librewolf = {
      enable = true;

      profiles = {
        ross = {
          id = 0;
          name = "Ross";
          isDefault = true;
          settings = {
            "sidebar.verticalTabs" = true;
            "sidebar.main.tools" = "syncedtabs,history,bookmarks";
            "sidebar.expandOnHover" = false;
            "browser.startup.homepage" = defaultEngine.url;
            "browser.search.defaultenginename" = defaultEngine.name;
            "browser.search.order.1" = defaultEngine.name;
            "extensions.getAddons.showPane" = false;
            "browser.bookmarks.restore_default_bookmarks" = false;
            "browser.contentblocking.category" = "strict";
            "browser.newtabpage.pinned" = defaultEngine.url;
            "browser.bookmarks.showMobileBookmarks" = true;
          };

          extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
            decentraleyes
            disable-javascript
            h264ify
            istilldontcareaboutcookies
            redirector
            simple-translate
            skip-redirect
            ublock-origin
          ];

          search = {
            force = true;
            default = defaultEngine.name;
            engines = {
              ${defaultEngine.name} = {
                inherit updateInterval;
                inherit (defaultEngine) icon;
                urls = [
                  {
                    template = "${defaultEngine.url}";
                    params = defaultEngine.params;
                  }
                ];
                definedAliases = [ defaultEngine.alias ];
              };

              MyNixOS = {
                inherit updateInterval;
                urls = [
                  {
                    template = "https://mynixos.com/search";
                    params = defaultEngine.params;
                  }
                ];
                definedAliases = [ "@nixpkgs" ];
                icon = "https://mynixos.com/favicon.ico";
              };

              "NixOS Wiki" = {
                inherit updateInterval;
                urls = [ { template = "https://wiki.nixos.org/wiki/{searchTerms}"; } ];
                definedAliases = [ "@nixwiki" ];
                icon = "https://wiki.nixos.org/nixos.png";
              };

              Marginalia = {
                inherit updateInterval;
                urls = [
                  {
                    template = "https://search.marginalia.nu/search";
                    params = [
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
                definedAliases = [ "@marginalia" ];
                icon = "https://search.marginalia.nu/favicon.ico";
              };

              "Arch Linux" = {
                inherit updateInterval;
                urls = [
                  {
                    template = "https://wiki.archlinux.org/title/{searchTerms}";
                  }
                ];
                definedAliases = [ "@archlinux" ];
                icon = "https://wiki.archlinux.org/favicon.ico";
              };

              Gentoo = {
                inherit updateInterval;
                urls = [
                  {
                    template = "https://wiki.gentoo.org/wiki/{searchTerms}";
                  }
                ];
                definedAliases = [ "@gentoo" ];
                icon = "https://wiki.gentoo.org/favicon.ico";
              };

              "Dwarf Fortress" = {
                inherit updateInterval;
                urls = [
                  {
                    template = "https://dwarffortresswiki.org/index.php/{searchTerms}";
                  }
                ];
                definedAliases = [ "@dwarffortress" ];
                icon = "https://dwarffortresswiki.org/favicon.ico";
              };

              ProtonDB = {
                inherit updateInterval;
                urls = [
                  {
                    template = "https://protondb.com/search";
                    params = defaultEngine.params;
                  }
                ];
                definedAliases = [ "@protondb" ];
                icon = "https://protondb.com/favicon.ico";
              };
            };
          };
        };

        plain = {
          id = 1;
          name = "plain";
        };
      };
    };
  };
}
