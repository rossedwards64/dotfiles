{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.modules.email;
  fullName = "Ross Edwards";
  gpgConf = {
    key = "rossedwards";
    signByDefault = true;
  };
in {
  options.modules.email = { enable = mkEnableOption "email"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ mu mu.mu4e isync msmtp ];

    programs = {
      mu.enable = true;
      msmtp.enable = true;
      mbsync.enable = true;

    };

    accounts.email = {
      maildirBasePath = ".local/share/mail";
      accounts = {
        outlook = {
          realName = fullName;
          address = "redwards64@hotmail.com";
          passwordCommand = "pass email/hotmail/personal";
          flavor = "outlook.office365.com";
          msmtp.enable = true;
          primary = true;
          mu.enable = true;

          imap = {
            host = "outlook.office365.com";
            tls.enable = true;
          };

          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            subFolders = "Verbatim";
            patterns = [
              "*"
              "![Outlook]*"
              "[Outlook]/Sent Mail"
              "[Outlook]/Starred"
              "[Outlook]/All Mail"
            ];
          };
          gpg = gpgConf;
        };

        gmail = {
          realName = fullName;
          address = "redwards6469@gmail.com";
          passwordCommand = "pass email/google/app-password";
          flavor = "gmail.com";
          msmtp.enable = true;
          primary = false;
          mu.enable = true;

          imap = {
            host = "imap.gmail.com";
            tls.enable = true;
          };

          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            subFolders = "Verbatim";
            patterns = [
              "*"
              "![Gmail]*"
              "[Gmail]/Sent Mail"
              "[Gmail]/Starred"
              "[Gmail]/All Mail"
            ];
          };

          gpg = gpgConf;
        };
      };
    };
  };
}
