cabal build && dist/build/example-microphone/example-microphone
# cabal build && cabal exec -- example-microphone audio.wav
aplay --file-type wav --format=S16_LE --channels=1 --rate=16000 audio.wav
