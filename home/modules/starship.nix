{ config, pkgs, lib, ... }:
with lib;
let
  generic_fmt = "[[$symbol($version)]($style)]";
  cfg = config.modules.system;
in {
  options.modules.starship = { enable = mkEnableOption "starship"; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ starship ];

    programs.starship = {
      enable = true;
      enableZshIntegration = true;

      settings = {
        format = lib.concatStrings [ "$directory" "$all" " $character" ];

        add_newline = false;
        continuation_prompt = ">> ";
        line_break.disabled = true;
        jobs.number_threshold = 1;
        os.disabled = true;
        shell.disabled = true;

        character = {
          success_symbol = "[➜](bold green) ";
          error_symbol = "[➜](bold red) ";
        };

        battery = {
          full_symbol = "🔋 ";
          charging_symbol = "🔌 ";
          discharging_symbol = "⚡ ";

          display = [{
            threshold = 30;
            style = "bold red";
          }];
        };

        cmd_duration = {
          format = "\\[[⏱ $duration]($style)\\]";
          min_time = 5;
        };
        dotnet.format = "[[$symbol($version)(🎯 $tfm)]($style)]";
        gcloud.format = "[[$symbol$account(@$domain)(($region))]($style)]";

        git_status = {
          conflicted = "!=";
          format = "([\\[$all_status$ahead_behind\\]]($style))";
        };

        git_branch = {
          symbol = " ";
          format = "\\[[$symbol$branch]($style)\\]";
        };

        git_commit.only_detached = true;

        git_state = { };

        git_metrics = {
          format =
            "\\[([+$added]($added_style))/([-$deleted]($deleted_style))\\]";
          disabled = false;
        };

        kubernetes.format = "[[$symbol$context( ($namespace))]($style)]";
        ocaml.format =
          "[[$symbol($version)(($switch_indicator$switch_name))]($style)]";
        openstack.format = "[[$symbol$cloud(($project))]($style)]";
        pulumi.format = "[[$symbol$stack]($style)]";
        raku.format = "'[[$symbol($version-$vm_version)]($style)]'";
        sudo.format = "[[as $symbol]]";
        terraform.format = "[[$symbol$workspace]($style)]";
        time.format = "[[$time]($style)]";

        username = {
          format = "\\[[$user]($style)\\]";
          show_always = true;
        };

        aws = {
          symbol = "  ";
          format =
            "\\[[$symbol($profile)(\\($region\\))(\\[$duration\\])]($style)\\]";
        };

        buf.symbol = " ";

        c = {
          symbol = " ";
          detect_extensions = [ "c" "h" "cpp" "hpp" ];
          format = "\\[[$symbol($version(-$name))]($style)\\]";
        };

        conda = {
          symbol = " ";
          format = "\\[[$symbol$environment]($style)\\]";
        };

        dart = {
          symbol = " ";
          format = generic_fmt;
        };

        directory = {
          truncation_length = 8;
          truncation_symbol = ".../";
        };

        docker_context = {
          symbol = " ";
          format = "\\[[$symbol$context]($style)\\]";
        };

        elixir = {
          symbol = " ";
          format = "\\[[$symbol($version \\(OTP $otp_version\\))]($style)\\]";
        };

        elm = {
          symbol = " ";
          format = generic_fmt;
        };

        golang = {
          symbol = " ";
          format = generic_fmt;
        };

        haskell = {
          symbol = " ";
          format = generic_fmt;
        };

        hg_branch = {
          symbol = " ";
          format = "\\[[$symbol$branch]($style)\\]";
        };

        java = {
          symbol = " ";
          format = generic_fmt;
        };

        julia = {
          symbol = " ";
          format = generic_fmt;
        };

        memory_usage = {
          symbol = "󰍛 ";
          format = "\\[$symbol[$ram( | $swap)]($style)\\]";
        };

        nim = {
          symbol = "";
          format = "[[$symbol($version)]($style)]";
        };

        nix_shell = {
          symbol = " ";
          format = "\\[[$symbol$state( \\($name\\))]($style)\\]";
        };

        nodejs = {
          symbol = " ";
          format = generic_fmt;
        };

        package = {
          symbol = " ";
          format = "\\[[$symbol$version]($style)\\]";
        };

        python = {
          symbol = " ";
          format = "[[$symbol$pyenv_prefix($version)(($virtualenv))]($style)]";
        };

        spack = {
          symbol = "🅢 ";
          format = "\\[[$symbol$environment]($style)\\]";
        };

        rust = {
          symbol = " ";
          format = generic_fmt;
        };

        cmake.format = generic_fmt;
        cobol.format = generic_fmt;
        crystal.format = generic_fmt;
        daml.format = generic_fmt;
        deno.format = generic_fmt;
        erlang.format = generic_fmt;
        helm.format = generic_fmt;
        kotlin.format = generic_fmt;
        lua.format = generic_fmt;
        perl.format = generic_fmt;
        php.format = generic_fmt;
        purescript.format = generic_fmt;
        red.format = generic_fmt;
        ruby.format = generic_fmt;
        scala.format = generic_fmt;
        swift.format = generic_fmt;
        vagrant.format = generic_fmt;
        vlang.format = generic_fmt;
        zig.format = generic_fmt;
      };
    };
  };
}
