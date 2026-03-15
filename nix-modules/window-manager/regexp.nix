{ lib, ... }:
{
  flake.meta.windowManager.regexps = {
    any = ".*";
    caprine = "^Caprine$";
    emacs = "^emacs(client)?$";
    epicGames = "^heroic$";
    librewolf = "^librewolf$";
    freetube = "^FreeTube$";
    gameConqueror = "^GameConqueror.py$";
    game = "^(${
      lib.strings.concatStrings
      <| lib.intersperse "|" [
        "Dr Robotnik's Ring Racers v.*"
        "Godot_Engine"
        "SRB2 v.*"
        "SRB2Kart v.*"
        "VampireSurvivors.exe"
        "blue-revolver"
        "dwarfort"
        "factorio"
        "gamescope"
        "nwmain-linux"
        "soulstorm"
        "spring"
      ]
    }).*$";
    intellij = "^jetbrains-idea$";
    itchio = {
      client = "^itch$";
      game = "^.*\.x86_64$";
    };
    faugus-launcher = "^faugus-launcher$";
    minecraft = "^org.prismlauncher.PrismLauncher$";
    mpv = "^mpv$";
    qBitTorrent = "^org.qbittorrent.qBittorrent$";
    spotify = "^(dev.alextren.)?(?i)spot(ify)?$";
    steam = {
      client = "^steam$";
      game = "^steam_app_[0-9]*$";
    };
    terminal = "^Alacritty$";
    vesktop = "^vesktop$";
    vlc = "^vlc$";
    volume = "^myxer$";
    zathura = "^org.pwmt.zathura$";
  };
}
