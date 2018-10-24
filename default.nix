{ compiler  ? "ghc843"
, rev       ? "f9002b83fd1998a6cc6fb8d66b8c9752b42c7fcd"
, sha256    ? "19cb7rf2yv933k5p6mc60i2wqwy7i1ralrb49gvma65f1kipk0rv"
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
                ex-hack = super.callPackage ./nix/ex-hack.nix {
                  stack = pkgs.stack;
                  pygments = pkgs.python36Packages.pygments;
                };
            };
          };
        };
      };
    };
  };

  buildTools = with pkgs; 
    [ zlib gmp sqlite
      haskell.packages.${compiler}.cabal-install 
    ];

  in

  { 
    ex-hack = pkgs.haskell.lib.addBuildTools
                pkgs.haskell.packages.${compiler}.ex-hack 
                buildTools;
  }
