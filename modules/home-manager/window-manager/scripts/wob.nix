{ pkgs, lib }:
let
  name = "wob";
in
pkgs.writers.makeBinWriter
  (
    let
      sbcl = (
        pkgs.sbcl.withPackages (
          ps: with ps; [
            serapeum
            str
            unix-opts
          ]
        )
      );
      compileArgs = lib.escapeShellArgs [
        "--no-userinit"
        "--no-sysinit"
        "--non-interactive"
        "--disable-debugger"
        "--eval"
        ''(load (sb-ext:posix-getenv "ASDF"))''
        "--eval"
        ''(asdf:load-system "serapeum")''
        "--eval"
        ''(asdf:load-system "str")''
        "--eval"
        ''(asdf:load-system "unix-opts")''
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
            brightnessctl
            coreutils
            wireplumber
            wob
            xdg-user-dirs
          ]
        ))
      ];
    }
  )
  "/bin/wob"
  (
    lib.strings.replaceStrings [ "#!/usr/bin/env sbcl --script" ] [ "" ] (
      lib.readFile ../../../../.local/share/bin/wob
    )
  )
