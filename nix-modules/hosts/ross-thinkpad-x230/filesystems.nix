{
  configurations.nixos.ross-thinkpad-x230.module = {
    swapDevices = [
      { device = "/dev/disk/by-uuid/3e6686d2-fe49-4cc2-b567-77c4e4e5c20c"; }
    ];

    fileSystems = {
      "/boot" = {
        device = "/dev/disk/by-uuid/AA05-9D30";
        fsType = "vfat";
      };

      "/" = {
        device = "/dev/disk/by-uuid/d84b921a-01ff-4844-a3e0-28e7083232c3";
        fsType = "ext4";
      };
    };
  };
}
