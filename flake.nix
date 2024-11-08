{
  description = "haskell package";
  inputs = { nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable"; };
  outputs = { self, nixpkgs }:
  let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
  in
  {
    packages.x86_64-linux.default = pkgs.haskellPackages.developPackage {
      root = ./.;
    };

    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = [
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.ghc
        pkgs.xorg.xorgserver
        pkgs.xorg.libX11
        pkgs.xorg.libXext
        pkgs.xorg.libXinerama
        pkgs.xorg.libXrandr
        pkgs.xorg.libXrender
        pkgs.xorg.libXScrnSaver
        pkgs.xterm
        (pkgs.writeShellApplication {
          name = "test-app";
          text = ''
            cabal build && (
              trap 'kill 0' SIGINT;
              Xephyr :1 -ac -screen 800x600 &
              DISPLAY=:1 cabal run &
              wait
            )
          '';
        })
      ];
    };
  };
}
