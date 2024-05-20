{ pkgs ? import <nixpkgs> { } }:

let sbcl' = pkgs.sbcl.withPackages (ps: with ps; [ ]);
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ sbcl' lispPackages.quicklisp asdf roswell ];
  buildInputs = with pkgs; [ openssl ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.openssl ]}:${
      pkgs.lib.makeLibraryPath [ pkgs.sqlite ]
    }"
  '';
}
