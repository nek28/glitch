The library implements random bit glitching (through randomGlitch) and sorting the bytes of a subsection of a jpeg file (randomSortBytes).

The image is specified as a command line argument, the result is written in a file which has the same name, but prefixed with "glitched_".

The image is represented as a strict ByteString of Char8's. 

__Usage:__
    cabal new-exec glitch apic.jpg