{ mkDerivation, attoparsec, base, bytestring, network-simple, pipes
, pipes-attoparsec, pipes-bytestring, pipes-network, pipes-parse
, pipes-safe, stdenv, transformers
}:
mkDerivation {
  pname = "pipes-telnet";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring network-simple pipes pipes-attoparsec
    pipes-bytestring pipes-network pipes-parse pipes-safe transformers
  ];
  executableHaskellDepends = [ attoparsec base bytestring ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/alasconnect/pipes-telnet#readme";
  license = stdenv.lib.licenses.bsd3;
}
