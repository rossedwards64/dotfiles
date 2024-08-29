{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.topgrade;
in
{
  options.modules.topgrade = {
    enable = mkEnableOption "topgrade";
  };

  config = mkIf cfg.enable {
    programs.topgrade = {
      enable = true;

      settings = {
        misc = {
          assume_yes = true;
          bashit_branch = "stable";
          cleanup = true;
          disable = [ ];
          display_time = true;
          ignore_failures = [ ];
          no_retry = true;
          no_self_update = true;
          notify_each_step = false;
          only = [ ];
          pre_sudo = true;
          run_in_tmux = false;
          set_title = true;
          skip_notify = false;
          ssh_arguments = "-o ConnectTimeout=2";
          sudo_command = "sudo";
          tmux_arguments = "-S /var/tmux.sock";
        };

        git = {
          max_concurrency = 5;
          repos = [ "~/Documents/programming/repos/*" ];
          arguments = "--rebase --autostash";
          pull_predefined = true;
        };

        linux = {
          arch_package_manager = "yay";
          emerge_sync_flags = "-q";
          emerge_update_flags = "-uDNa --with-bdeps=y @world";
          enable_tlmgr = true;
          home_manager_arguments = [
            "--flake"
            "file"
          ];
          nix_arguments = "--flake";
          nix_env_arguments = "--prebuilt-only";
          show_arch_news = true;
          yay_arguments = "--nodevel --answerclean None --removemake --topdown --cleanafter";
        };

        python = {
          enable_pip_review = false;
          enable_pip_review_local = false;
          enable_pipupgrade = false;
          pipupgrade_arguments = "-y -y --pip-path pip";
        };

        composer.self_update = true;
        npm.use_sudo = true;
        firmware.upgrade = true;
        flatpak.use_sudo = true;
        yarn.use_sudo = true;
        vim.force_plug_update = true;
      };
    };
  };
}
