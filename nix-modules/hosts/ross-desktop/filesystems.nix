{
  configurations.nixos.ross-desktop.module = {
    swapDevices = [
      { device = "/dev/disk/by-uuid/ab72b956-a558-4d28-ab69-e3a97b163028"; }
    ];

    fileSystems = {
      "/boot" = {
        device = "/dev/disk/by-uuid/8C89-63FA";
        fsType = "vfat";
        options = [
          "fmask=0077"
          "dmask=0077"
        ];
      };

      "/" = {
        device = "/dev/disk/by-uuid/3e1d77b3-5767-4057-9e06-4173d5571e32";
        fsType = "ext4";
      };

      "/HDD" = {
        device = "/dev/disk/by-uuid/992809b3-ca72-4c22-ae5b-41fd2f3967b2";
        fsType = "ext4";
      };

      "/SSD" = {
        device = "/dev/disk/by-uuid/9c430e65-08a3-4aa1-b059-6ea713ff04e8";
        fsType = "ext4";
      };

      "/SSD2" = {
        device = "/dev/disk/by-uuid/280db991-0e21-444e-a497-073c021ddec7";
        fsType = "ext4";
      };
    };
  };
}
