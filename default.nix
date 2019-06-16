{ compiler  ? "ghc865"
, rev       ? "8c14a6f641b7f3baa57e55e784a0d8626325446b"
, sha256    ? "0jbkkjqpjjr63agh7p8pbnlyq2kknl9a1wbrxhaf8033j9f9fni7"
, profile   ? false
}:

let
  pkgs = import (builtins.fetchTarball {
     url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
     inherit sha256; }) {inherit config;};

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = super: self: {
              cabal-helper = pkgs.haskell.lib.doJailbreak 
                (super.callPackage ./nix/cabal-helper.nix {});
              selda-sqlite = pkgs.haskell.lib.doJailbreak 
                (super.callPackage ./nix/selda-sqlite.nix {});
              # ghc-lib = pkgs.haskell.lib.doJailbreak
                # (super.callPackage ./nix/ghc-lib.nix {});
              # ghc-lib-parser = pkgs.haskell.lib.doJailbreak
              #  (super.callPackage ./nix/ghc-lib-parser.nix {});
                ex-hack = super.callPackage ./nix/ex-hack.nix {
                  stack = pkgs.stack;
                  profile = profile;
                  pygments = pkgs.python36Packages.pygments;
                };
            };
          };
        };
      };
    };
  };

  buildTools = with pkgs; 
    [ zlib gmp sqlite python36Packages.pygments
      haskell.packages.${compiler}.cabal-install 
    ];

  in

  { 
    ex-hack = pkgs.haskell.lib.addBuildTools
                pkgs.haskell.packages.${compiler}.ex-hack 
                buildTools;
  }
