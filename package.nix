{ mkDerivation, base, binary, bytestring, containers, criterion
, deepseq, doctest, either, errors, hashable, hspec, managed, mtl
, optparse-generic, pipes, pipes-group, portaudio, QuickCheck
, semigroups, stdenv, stm, stm-chans, transformers
}:
mkDerivation {
  pname = "microphone";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring containers deepseq either errors hashable
    managed mtl optparse-generic pipes pipes-group portaudio semigroups
    stm stm-chans transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest hspec QuickCheck ];
  benchmarkHaskellDepends = [ base criterion deepseq ];
  homepage = "http://github.com/sboosali/microphone#readme";
  description = "a.k.a. @pipes-portaudio@: stream from a microphone";
  license = stdenv.lib.licenses.bsd3;
}
