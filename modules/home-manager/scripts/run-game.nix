{ pkgs }:

pkgs.writeShellApplication {
  name = "run-game";
  runtimeInputs = with pkgs; [
    mangohud
    gamemode
    gamescope
    steam-run
  ];

  text = ''
    width=1920
    height=1080
    fullscreen='-f'
    exec_mangohud='mangohud'
    exec_gamemode='gamemoderun'

    use_fullscreen=true
    use_mangohud=false
    use_gamemode=false
    debug=false

    usage() {
    	cat <<EOF
    Wrapper script for running games under gamescope.
    H: Set game height.
    W: Set game width.
    f: Play in fullscreen.
    n: Disable fullscreen.
    m: Use MangoHUD.
    g: Use GameMode.
    h: Help.
    EOF
    }

    while getopts "H:W:fnmgh" arg; do
    	case "$arg" in
    	H)
    		height="$OPTARG"
    		;;
    	W)
    		width="$OPTARG"
    		;;
    	f)
    		use_fullscreen=true
    		;;
    	n)
    		use_fullscreen=false
    		;;
    	m)
    		use_mangohud=true
    		;;
    	g)
    		use_gamemode=true
    		;;
    	h)
    		usage
    		exit 0
    		;;
    	*)
    		echo "Invalid argument."
    		usage
    		exit 1
    		;;
    	esac
    done

    shift

    game="$1"

    if command -v "$(dirname "$game")" >/dev/null; then
    	game="$(command -v "$game")"
    else
    	game="$(readlink -f "$game")"
    	cd "$(dirname "$game")" || exit 1
    fi

    if [[ "$use_fullscreen" = false ]]; then
    	unset fullscreen
    fi

    if [[ "$use_mangohud" = false ]]; then
    	unset exec_mangohud
    fi

    if [[ "$use_gamemode" = false ]]; then
       unset exec_gamemode
    fi

    size_args=( "-w $width" "-h $height" "-W $width" "-H $height" )
    exec_args=( "$exec_mangohud" "$exec_gamemode" )

    pwd

    if [[ "$debug" = true ]]; then
        steam-run gamescope "''${size_args[@]}" "$fullscreen" -- "''${exec_args[@]}" "$game"
    else
        nohup steam-run gamescope "''${size_args[@]}" -- "''${exec_args[@]}" "$game" &>/dev/null & disown
    fi
  '';
}
