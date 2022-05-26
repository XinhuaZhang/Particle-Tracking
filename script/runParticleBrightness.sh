InputFolder=/vast/home/xinhuazhang/Downloads/Stack1M_Compressed/
PixelTreshold=13000
Threads=16
time stack test :ParticleBrightness-test --test-arguments "-F ${InputFolder} -p ${PixelTreshold} +RTS -N${Threads} -RTS"

