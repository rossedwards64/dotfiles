{ inputs, config, ... }:
{
  flake.modules.homeManager.base = {
    stylix.targets.librewolf.profileNames = [ config.flake.meta.user.username ];
    programs.librewolf.profiles.${config.flake.meta.user.username} =
      let
        name = "DuckDuckGoLite";
        url = "https://lite.duckduckgo.com";
      in
      {
        id = 0;
        name = "Ross";
        isDefault = true;

        extensions.packages = with inputs.nur.legacyPackages."x86_64-linux".repos.rycee.firefox-addons; [
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
          default = name;
          privateDefault = name;
        };

        settings = {
          "sidebar.verticalTabs" = true;
          "sidebar.main.tools" = "syncedtabs,history,bookmarks";
          "sidebar.expandOnHover" = false;
          "browser.startup.homepage" = url;
          "browser.search.defaultenginename" = name;
          "browser.search.order.1" = name;
          "extensions.getAddons.showPane" = false;
          "browser.bookmarks.restore_default_bookmarks" = false;
          "browser.contentblocking.category" = "strict";
          "browser.newtabpage.pinned" = url;
          "browser.bookmarks.showMobileBookmarks" = true;
        };
      };
  };
}
