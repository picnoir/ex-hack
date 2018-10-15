{ mkDerivation, base, direct-sqlite, directory, exceptions, selda
, stdenv, text
}:
mkDerivation {
  pname = "selda-sqlite";
  version = "0.1.6.0";
  sha256 = "c67ba89114a82ece42b7e478bcf480ae0241cefb41e2e9b340a268f9f08be390";
  revision = "2";
  editedCabalFile = "198pg9i0lfx3fwf7b7cw0x5kial6vbf0dqwh18jnh7na3pyn1jr6";
  libraryHaskellDepends = [
    base direct-sqlite directory exceptions selda text
  ];
  homepage = "https://github.com/valderman/selda";
  description = "SQLite backend for the Selda database EDSL";
  license = stdenv.lib.licenses.mit;
}
