{ pkgs, lib }:

let
  name = "screenshot";
in
pkgs.writers.makeBinWriter
  (
    let
      sbcl = (pkgs.sbcl.withPackages (ps: with ps; [ com_dot_inuoe_dot_jzon ]));
      compileArgs = lib.escapeShellArgs [
        "--no-userinit"
        "--no-sysinit"
        "--non-interactive"
        "--disable-debugger"
        "--eval"
        ''(load (sb-ext:posix-getenv "ASDF"))''
        "--eval"
        ''(asdf:load-system "com.inuoe.jzon")''
        "--eval"
        ''(asdf:load-system "sb-bsd-sockets")''
        "--load"
        "tmp.lisp"
        "--eval"
        ''(sb-ext:save-lisp-and-die "${name}" :executable t :toplevel #'${name}:main)''
        "--eval"
        "(quit)"
      ];
    in
    {
      compileScript = ''
        cp $contentPath tmp.lisp
        ${sbcl}/bin/sbcl ${compileArgs}
        mv ${name} $out
      '';

      strip = false;
      makeWrapperArgs = [
        "--set"
        "PATH"
        (lib.makeBinPath (
          with pkgs;
          [
            fuzzel
            grim
            libnotify
            slurp
            wl-clipboard
            xdg-user-dirs
            coreutils
          ]
        ))
      ];
    }
  )
  "/bin/screenshot"
  (
    lib.strings.replaceStrings [ "#!/usr/bin/env sbcl --script" ] [ "" ] (
      lib.readFile ../../../../../../.config/fuzzel/scripts/screenshot
    )
  )
