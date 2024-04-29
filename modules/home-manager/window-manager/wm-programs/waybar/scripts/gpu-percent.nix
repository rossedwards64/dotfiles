{ pkgs }:

pkgs.writeShellApplication {
  name = "gpu-percent";
  runtimeInputs = with pkgs; [ coreutils ];

  text = ''
    gpu_dir=/sys/class/drm/card1/device
    gpu_percent=$(cat $gpu_dir/gpu_busy_percent)

    echo "$gpu_percent%"
  '';
}
