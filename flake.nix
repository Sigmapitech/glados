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
    inherit (nixpkgs) lib;

    genSystems = lib.genAttrs ["x86_64-linux"];

    eachSystem = f:
      genSystems
      (system: f nixpkgs.legacyPackages.${system});

    eachSystem' = f:
      genSystems
      (system: f self.shared.${system});
  in {
    checks = eachSystem' (
      {
        pkgs,
        haskell,
      }: {
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
              ] (x: {
                enable = true;
                package = lib.getBin (haskell.${x} or pkgs.${x});
              }));
        };
      }
    );

    formatter = eachSystem (pkgs: pkgs.alejandra);

    devShells = eachSystem' ({
      pkgs,
      haskell,
    }: {
      default = pkgs.mkShell {
        inherit (self.checks.${pkgs.system}.pre-commit-check) shellHook;

        packages = with pkgs;
          [
            chez
            curl
            jq
          ]
          ++ self.checks.${pkgs.system}.pre-commit-check.enabledPackages
          ++ (with haskell; [
            cabal-install

            (ghcWithPackages (p: [
              hspec
              hspec-expectations
              parsec
              megaparsec
            ]))
          ]);
      };
    });

    shared = eachSystem (pkgs: {
      inherit pkgs;

      haskell = pkgs.haskell.packages.ghc984;
    });
  };
}
