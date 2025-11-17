{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    git-hooks,
  }: let
    applySystems = nixpkgs.lib.genAttrs ["x86_64-linux"];
    eachSystem = f: applySystems (system: f nixpkgs.legacyPackages.${system});

    inherit (nixpkgs) lib;
  in {
    checks = eachSystem (
      pkgs: {
        pre-commit-check = git-hooks.lib.${pkgs.system}.run {
          src = ./.;
          hooks =
            {
              commit-name = {
                enable = true;
                name = "commit name";
                stages = ["commit-msg"];
                entry = ''
                  ${pkgs.python310.interpreter} ${./scripts/apply-commit-convention.py}
                '';
              };
            }
            // (lib.genAttrs [
              "alejandra"
              "cabal-fmt"
              "ormolu"
              "hlint"
            ] (x: {enable = true;}));
        };
      }
    );

    formatter = eachSystem (pkgs: pkgs.alejandra);

    devShells = eachSystem (pkgs: let
      haskell = pkgs.haskell.packages.ghc984;
    in {
      default = pkgs.mkShell {
        inherit (self.checks.${pkgs.system}.pre-commit-check) shellHook;

        packages = with pkgs;
          [
            chez
            curl
          ]
          ++ (with haskell; [
            ormolu
            cabal-install
            hlint

            (ghcWithPackages (p: [
              hspec
              hspec-expectations
            ]))
          ]);
      };
    });
  };
}
