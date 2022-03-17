{ mkDerivation, aeson, async, base, base64-bytestring, binary
, bytestring, case-insensitive, containers, deepseq, exceptions
, fast-logger, gitrev, hashable, herp-util, hpack, http-conduit
, lib, lrucaching, mono-traversable, mtl, optparse-generic
, proto3-suite, proto3-wire, raven-haskell, rio, safe
, safe-exceptions, stm, text, time, transformers, unix-compat
, unix-time, unordered-containers, uuid, vector, wai
}:
mkDerivation {
  pname = "herp-logger";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base base64-bytestring binary bytestring
    case-insensitive containers deepseq exceptions fast-logger gitrev
    hashable herp-util http-conduit lrucaching mono-traversable mtl
    optparse-generic proto3-suite proto3-wire raven-haskell rio safe
    safe-exceptions stm text time transformers unix-compat unix-time
    unordered-containers uuid vector wai
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
