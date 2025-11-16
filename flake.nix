{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs }: let
    applySystems = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
    eachSystem = f: applySystems (system: f nixpkgs.legacyPackages.${system});
  in {
    devShells = eachSystem (pkgs: let
      haskell = pkgs.haskell.packages.ghc984;
    in {
      default = pkgs.mkShell {
        packages = with pkgs; [
          chez
        ] ++ (with haskell; [
          ghc
          cabal-install
          hlint
          ormolu
        ]);
      };
    });
  };
}
