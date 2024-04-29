{ pkgs }:

pkgs.writeShellApplication {
  name = "gpu-mem";
  runtimeInputs = with pkgs; [ coreutils gnused ];

  text = ''
    gpu_dir=/sys/class/drm/card1/device
    gpu_mem_total=$(cat $gpu_dir/mem_info_vram_total)
    gpu_mem_used=$(cat $gpu_dir/mem_info_vram_used)

    format_num() {
        echo "$(numfmt --to=iec-i --format "%-8.2f" "$1")B" | tr -d ' ' | sed 's/G/ G/g' | sed 's/M/ M/g'
    }

    echo "$(format_num "$gpu_mem_used") / $(format_num "$gpu_mem_total")"
  '';
}
