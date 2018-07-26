{ compiler ? "ghc843"
, rev     ? "49bdae006e66e70ad3245a463edc01b5749250d3"
, sha256  ? "1ijsifmap47nfzg0spny94lmj66y3x3x8i6vs471bnjamka3dx8p"
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
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    testHaskellDepends = attrs.testHaskellDepends ++
    [ pkgs.nix
      pkgs.cabal-install

        # Use the same version of hpack no matter what the compiler version
        # is, so that we know exactly what the contents of the generated
        # .cabal file will be. Otherwise, Travis may error out claiming that
        # the cabal file needs to be updated because the result is different
        # that the version we committed to Git.
        pkgs.haskell.packages.ghc843.hpack ];
    configureFlags = 
      pkgs.stdenv.lib.optional doStrict "--ghc-options=-Werror";
   });

   inherit returnShellEnv;
}
