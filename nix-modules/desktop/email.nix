{ lib, ... }@moduleArgs:
{
  flake.modules.homeManager.base =
    {
      pkgs,
      config,
      ...
    }:
    let
      inherit (config) xdg;
      realName = moduleArgs.config.flake.meta.user.name;
      gpg = {
        key = "rossedwards";
        signByDefault = true;
      };
    in
    {
      programs = {
        mu.enable = true;
        msmtp.enable = true;
        mbsync.enable = true;
      };

      accounts.email = {
        maildirBasePath = "${xdg.dataHome}/mail";
        accounts = {
          outlook = {
            inherit realName gpg;
            address = moduleArgs.config.flake.meta.user.email;
            passwordCommand = "${lib.getExe pkgs.pass-wayland} email/hotmail/personal";
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
              extraConfig.account.AuthMechs = "LOGIN";
            };
          };

          gmail = {
            inherit realName gpg;
            address = "redwards6469@gmail.com";
            passwordCommand = "${lib.getExe pkgs.pass-wayland} email/google/app-password";
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
          };
        };
      };
    };
}
