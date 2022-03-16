{ mkDerivation, aeson, async, base, base64-bytestring, binary
, bytestring, containers, deepseq, exceptions, fast-logger
, generic-lens, generic-lens-core, gitrev, hashable, herp-util
, hpack, http-conduit, lens, lrucaching, mono-traversable, mtl
, optparse-generic, proto3-suite, raven-haskell, rio, safe
, safe-exceptions, stdenv, stm, text, time, transformers
, unix-compat, unix-time, unordered-containers, uuid, vector
}:
mkDerivation {
  pname = "herp-logger";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base base64-bytestring binary bytestring containers
    deepseq exceptions fast-logger generic-lens generic-lens-core
    gitrev hashable herp-util http-conduit lens lrucaching
    mono-traversable mtl optparse-generic proto3-suite raven-haskell
    rio safe safe-exceptions stm text time transformers
    unix-compat unix-time unordered-containers uuid vector
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
