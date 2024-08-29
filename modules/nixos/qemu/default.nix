{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.modules.qemu;
in
{
  options.modules.qemu = {
    enable = mkEnableOption "qemu";
  };

  config = mkIf cfg.enable {
    environment = {
      systemPackages = with pkgs; [
        qemu
        libvirt
        virt-manager
        virt-viewer
        win-virtio
        win-spice
        spice
        spice-protocol
      ];
    };

    virtualisation = {
      spiceUSBRedirection.enable = true;

      libvirtd = {
        enable = true;
        qemu = {
          package = pkgs.qemu_kvm;
          runAsRoot = true;
          swtpm.enable = true;
          ovmf = {
            enable = true;
            packages = with pkgs; [
              (OVMF.override {
                secureBoot = true;
                tpmSupport = true;
              }).fd
            ];
          };
        };
      };
    };

    services = {
      spice-webdavd.enable = true;
      spice-vdagentd.enable = true;
    };
  };
}
