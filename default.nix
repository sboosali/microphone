{ mkDerivation, base, binary, bytestring, containers, deepseq
, doctest, either, errors, hashable, hspec, managed, mtl, pipes
, pipes-group, portaudio, QuickCheck, semigroups, stdenv, stm
, transformers
}:
mkDerivation {
  pname = "microphone";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers deepseq either errors hashable
    managed mtl pipes pipes-group portaudio semigroups stm transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest hspec QuickCheck ];
  homepage = "http://github.com/sboosali/microphone#readme";
  description = "a.k.a. @pipes-portaudio@: stream from a microphone";
  license = stdenv.lib.licenses.bsd3;
}
