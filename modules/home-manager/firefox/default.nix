{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.firefox;
  braveSearch = "Brave";
  braveSearchURL = "https://search.brave.com";
  updateInterval = 24 * 60 * 60 * 1000;
  defaultEngineParams = [{
    name = "q";
    value = "{searchTerms}";
  }];
in {
  options.modules.firefox = { enable = mkEnableOption "firefox"; };

  config = mkIf cfg.enable {
    home.sessionVariables = {
      MOZ_USE_XINPUT = 1;
      MOZ_ENABLE_WAYLAND = 1;
    };

    programs.firefox = {
      enable = true;
      package = pkgs.firefox.overrideAttrs
        (attrs: { meta.priority = pkgs.firefox.meta.priority + 1; });

      policies = {
        DisableTelemetry = true;
        DisableFirefoxStudies = true;
        DisablePocket = true;
        DisplayBookmarksToolbar = "never";
        DisplayMenuBar = "default-off";

        EnableTrackingProtection = {
          Value = true;
          Locked = true;
          Cryptomining = true;
          Fingerprinting = true;
        };

        SearchBar = "separate";
      };

      profiles = {
        ross = {
          id = 0;
          name = "Ross";
          isDefault = true;
          settings = {
            "browser.startup.homepage" = braveSearchURL;
            "browser.search.defaultenginename" = braveSearch;
            "browser.search.order.1" = braveSearch;
            "privacy.trackingprotection.enabled" = true;
            "privacy.trackingprotection.socialtracking.enabled" = true;
            "extensions.getAddons.showPane" = false;
            "browser.bookmarks.restore_default_bookmarks" = false;
            "browser.contentblocking.category" = "strict";
            "browser.newtabpage.pinned" = braveSearchURL;
            "browser.bookmarks.showMobileBookmarks" = true;
            "extensions.activeThemeID" =
              "{f5525f34-4102-4f6e-8478-3cf23cfeff7a}";
          };

          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
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
            default = braveSearch;

            engines = {
              "${braveSearch}" = {
                inherit updateInterval;
                urls = [{
                  template = "https://search.brave.com/search";
                  params = defaultEngineParams;
                }];
                definedAliases = [ "@brave" ];
                iconUpdateURL =
                  "https://cdn.search.brave.com/serp/v2/_app/immutable/assets/favicon.acxxetWH.ico";
              };

              "MyNixOS" = {
                inherit updateInterval;
                urls = [{
                  template = "https://mynixos.com/search";
                  params = defaultEngineParams;
                }];
                definedAliases = [ "@nixpkgs" ];
                iconUpdateURL = "https://mynixos.com/favicon.ico";
              };

              "NixOS Wiki" = {
                inherit updateInterval;
                urls =
                  [{ template = "https://wiki.nixos.org/wiki/{searchTerms}"; }];
                definedAliases = [ "@nixwiki" ];
                iconUpdateURL = "https://wiki.nixos.org/nixos.png";
              };

              "Amazon.com".metaData.hidden = true;
              "BBC".metaData.hidden = true;
              "Bing".metaData.hidden = true;
              "Facebook".metaData.hidden = true;
              "Google".metaData.hidden = true;
              "YouTube".metaData.hidden = true;
              "eBay".metaData.hidden = true;
            };
          };
        };
      };
    };
  };
}
