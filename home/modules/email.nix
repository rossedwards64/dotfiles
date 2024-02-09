{ lib, config, pkgs, ... }:
with lib;
let cfg = config.modules.email;
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
          realName = "Ross Edwards";
          address = "redwards64@hotmail.com";
          passwordCommand = "echo 'KT3Myj5XRaNgp?B@'";
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

          gpg = {
            key = "";
            signByDefault = true;
          };
        };

        gmail = {
          realName = "Ross Edwards";
          address = "redwards6469@gmail.com";
          passwordCommand = "echo 'qgylfyeksqiucisd'";
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

          gpg = {
            key = "";
            signByDefault = true;
          };
        };
      };
    };
  };
}
