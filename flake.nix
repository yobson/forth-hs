{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in {

    packages.default = pkgs.haskellPackages.callCabal2nix "forth-hs" ./. {} ;

    devShells.default = pkgs.haskellPackages.shellFor {
        packages = p: [self.packages.${system}.default ];
        nativeBuildInputs = with pkgs; [ cabal-install haskell-language-server ];
    };

  });
}
