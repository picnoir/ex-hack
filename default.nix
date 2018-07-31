{ compiler ? "ghc843"
, rev     ? "cd3283f9218b70fdf39640ba1be6fa16e137c209"
, sha256  ? "1kwffir31hnp1q7cs6h6vgbi4n3byhrv343k99kirp6sf87k99dm"
, pkgs ?
    if builtins.compareVersions builtins.nixVersion "2.0" < 0
    then abort "Exhs requires at least nix 2.0"
    else import (builtins.fetchTarball {
           url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
           inherit sha256; }) {
           config.allowUnfree = true;
           config.allowBroken = false;
         }
 , returnShellEnv ? pkgs.lib.inNixShell
 , mkDerivation ? null

 , doStrict ? false
}:
let
  haskellPackages = pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  root = ./.;
  source-overrides = {
  };
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    testHaskellDepends = attrs.testHaskellDepends ++
    [ pkgs.nix

        # Use the same version of hpack no matter what the compiler version
        # is, so that we know exactly what the contents of the generated
        # .cabal file will be. Otherwise, Travis may error out claiming that
        # the cabal file needs to be updated because the result is different
        # that the version we committed to Git.
        pkgs.haskell.packages.ghc843.hpack

        (let cabalInstallVersion = {
            ghc843 = "2.2.0.0"; 
            ghc822 = "2.0.0.1";
        }; in
        haskellPackages.callHackage "cabal-install"
        cabalInstallVersion.${compiler} {})
      ];
    configureFlags = 
      pkgs.stdenv.lib.optional doStrict "--ghc-options=-Werror";
   });

   inherit returnShellEnv;
}
