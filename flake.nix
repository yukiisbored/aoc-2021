{
  description = "Yuki's Advent of Code 2021 solutions";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };

          # Haskell with (most of) GHC Boot libraries
          haskell = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [
            base
            transformers
            stm
            deepseq
            mtl
            exceptions
            array
            binary
            bytestring
            containers
            text
            time
            template-haskell
            directory
            filepath
            unix
            process
            parsec
            pretty
          ]);
        in {
          devShell = pkgs.mkShell {
            packages = with pkgs.haskellPackages; [
              haskell
              haskell-language-server
            ];
          };
        });
}
