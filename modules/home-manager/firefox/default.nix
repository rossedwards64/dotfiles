{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.firefox;
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
  options.modules.firefox = {
    enable = mkEnableOption "firefox";
  };

  config = mkIf cfg.enable {
    home.sessionVariables = {
      MOZ_USE_XINPUT = 1;
      MOZ_ENABLE_WAYLAND = 1;
    };

    nixpkgs.config.packageOverrides = pkgs: {
      nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
        inherit pkgs;
      };
    };

    programs.firefox = {
      enable = true;
      package = pkgs.firefox.overrideAttrs (attrs: {
        meta.priority = pkgs.firefox.meta.priority + 1;
      });

      policies = {
        DisableTelemetry = true;
        DisableFirefoxStudies = true;
        DisablePocket = true;
        DisplayBookmarksToolbar = "always";
        DisplayMenuBar = "default-off";

        EnableTrackingProtection = {
          Value = true;
          Locked = true;
          Cryptomining = true;
          Fingerprinting = true;
        };

        FirefoxHome = {
          Pocket = false;
          Snippets = false;
        };

        SearchBar = "separate";

        UserMessaging = {
          ExtensionRecommendations = false;
          SkipOnboarding = true;
        };
      };

      profiles = {
        ross = {
          id = 0;
          name = "Ross";
          isDefault = true;
          settings = {
            "browser.startup.homepage" = defaultEngine.url;
            "browser.search.defaultenginename" = defaultEngine.name;
            "browser.search.order.1" = defaultEngine.name;
            "privacy.trackingprotection.enabled" = true;
            "privacy.trackingprotection.socialtracking.enabled" = true;
            "extensions.getAddons.showPane" = false;
            "browser.bookmarks.restore_default_bookmarks" = false;
            "browser.contentblocking.category" = "strict";
            "browser.newtabpage.pinned" = defaultEngine.url;
            "browser.bookmarks.showMobileBookmarks" = true;
            "extensions.activeThemeID" = "{f5525f34-4102-4f6e-8478-3cf23cfeff7a}";
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

              google.metaData.hidden = true;
              amazondotcom-us.metaData.hidden = true;
              bbc.metaData.hidden = true;
              bing.metaData.hidden = true;
              facebook.metaData.hidden = true;
              youtube.metaData.hidden = true;
              ebay.metaData.hidden = true;
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
