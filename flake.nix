# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "quotes-api";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        # DON'T FORGET TO PUT YOUR PACKAGE NAME HERE, REMOVING `throw`
        packageName = "quotes-api";
      in {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self
          rec {
            # Dependency overrides go here
          };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        nixosModules = {
          quotes-api = { config, lib, pkgs, ... }:

            with lib;
            let cfg = config.services.quotes-api;
            in {
              options.services.quotes-api = {
                enable = mkEnableOption "Enables quotes api service";

                port = mkOption rec {
                  type = types.int;
                  default = 8123;
                  example = default;
                  description = "bind port";
                };

                dbpath = mkOption rec {
                  type = types.string;
                  default = "/tmp/sqlite.db";
                  example = default;
                  description = "Path to sqlite database";
                };
              };

              config = mkIf cfg.enable {
                systemd.services."quotes-api" = {
                  wantedBy = [ "multi-user.target" ];

                  serviceConfig =
                    let pkg = self.packages.${pkgs.system}.default;
                    in {
                      Restart = "on-failure";
                      ExecStart = "${pkg}/bin/quotes-api --port ${
                          builtins.toString cfg.port
                        } --dbpath ${cfg.dbpath}";
                    };
                };

              };
            };
        };

        nixosModule = self.nixosModules.${system}.quotes-api;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            hlint
            ghc
            cabal-install
          ];
          inputsFrom =
            map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
