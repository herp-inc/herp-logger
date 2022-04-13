{}:
let
  pkgs = import ./nix/pkgs.nix {};
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.git
      pkgs.coreutils
      pkgs.clang
      pkgs.haskellPackages.cabal-install
      pkgs.haskell.compiler.ghc921
      pkgs.zlib
    ];
    shellHook = ''
      export LANG=C.UTF-8
    '';
  }
