{
  flake.modules.homeManager.base =
    { pkgs, config, ... }:
    {
      home.packages = with pkgs; [ gcr ];

      programs.gpg = {
        enable = true;
        homedir = "${config.xdg.dataHome}/gnupg";
      };

      services = {
        gnome-keyring = {
          enable = true;
          components = [
            "pkcs11"
            "secrets"
            "ssh"
          ];
        };

        gpg-agent = {
          enable = true;
          enableZshIntegration = true;
          pinentry.package = pkgs.pinentry-qt;
        };
      };
    };
}
