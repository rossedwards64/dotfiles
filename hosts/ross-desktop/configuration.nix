{
  config,
  lib,
  pkgs,
  specialArgs,
  ...
}:
let
  folderConfig = {
    enable = true;
    devices = [
      "ross-thinkpad-x230"
      "ross-thinkpad-x200"
      "ross-phone"
    ];
  };

  fc = rec {
    devPathPrefix = "/sys/devices";
    cpu = devPathPrefix + "/platform/it87.2656/hwmon";
    gpu = devPathPrefix + "/pci0000:00/0000:00:03.1/0000:0a:00.0/0000:0b:00.0/0000:0c:00.0/hwmon";
    hwmonPathPrefix = "/hwmon[[:print:]]*";
    pwm = x: hwmonPathPrefix + "/pwm${toString x}";
    tempInput = x: hwmonPathPrefix + "/temp${toString x}_input";
    fanInput = x: hwmonPathPrefix + "/fan${toString x}_input";

    makeAssignStatement =
      pair:
      lib.concatStrings [
        (builtins.head pair)
        "="
        (builtins.head (builtins.tail pair))
      ];

    assignToInput =
      input:
      toString (
        map makeAssignStatement (
          [
            [
              (gpu + (pwm 1))
              (gpu + (input 1))
            ]
          ]
          ++ map (x: [
            (cpu + (pwm x))
            (cpu + (input x))
          ]) (lib.range 1 3)
        )
      );

    assignToNumber =
      num:
      let
        numStr = toString num;
      in
      toString (
        map makeAssignStatement (
          [
            [
              (gpu + (pwm 1))
              numStr
            ]
          ]
          ++ map (x: [
            (cpu + (pwm x))
            numStr
          ]) (lib.range 1 3)
        )
      );
  };
in
{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    extraModulePackages = [ config.boot.kernelPackages.gcadapter-oc-kmod ];
    kernelModules = [
      "gcadapter_oc"
      "it87"
    ];
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };

      grub = {
        efiSupport = true;
        device = "nodev";
      };
    };
  };

  fileSystems."/HDD" = {
    label = "HDD";
    device = "/dev/disk/by-uuid/992809b3-ca72-4c22-ae5b-41fd2f3967b2";
    fsType = "ext4";
  };

  fileSystems."/SSD" = {
    label = "SSD";
    device = "/dev/disk/by-uuid/d1055482-e9bc-4abc-90a9-53018bd30937";
    fsType = "ext4";
  };

  fileSystems."/SSD2" = {
    label = "SSD2";
    device = "/dev/disk/by-uuid/280db991-0e21-444e-a497-073c021ddec7";
    fsType = "ext4";
  };

  environment.systemPackages = with pkgs; [
    atlauncher
    cemu
    dolphin-emu
    pcsx2
    rpcs3
    ryujinx
    xemu
  ];

  services = {
    xserver = {
      xkb.layout = "us";
      videoDrivers = [ "modesetting" ];
    };

    syncthing = {
      settings = {
        folders = {
          "Org Files" = folderConfig;
          "Books" = folderConfig;
          "Papers" = folderConfig;
          "Manuals" = folderConfig;
          "Pictures" = folderConfig;
        };
      };
    };

    udev.packages = [ pkgs.dolphinEmu ];
  };

  hardware = {
    graphics = {
      extraPackages = with pkgs; [
        vulkan-loader
        vulkan-validation-layers
        vulkan-extension-layer
      ];
    };

    fancontrol = {
      enable = true;
      config = with fc; ''
        INTERVAL=10
        FCTEMPS=${assignToInput tempInput}
        FCANS=${assignToInput fanInput}
        MINTEMP=${assignToNumber 40}
        MAXTEMP=${assignToNumber 65}
        MINSTART=${assignToNumber 80}
        MINSTOP=${assignToNumber 80}
        MINPWM=${assignToNumber 125}
        MAXPWM=${assignToNumber 255}
      '';
    };

    cpu.amd.updateMicrocode = true;
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = "performance";
  };

  networking.hostName = "ross-desktop";

  system.stateVersion = "23.11";
}
