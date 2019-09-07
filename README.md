# ViewHex

## Wat?

This is a simple hex viewer made in Haskell that uses ncurses.

## Compiling

This program is written in Haskell. If you don't already have Haskell, I'd recommend getting Haskell Platform.

You also need to install the ncurses library and install the Haskell ncurses package, which provides bindings to the ncurses library
In other words, go install GNU ncurses and then run ``cabal install ncurses`` to get the Haskell bindings

You now also need to install lens, as I use it to mess with my data types. ``cabal install lens`` to install it

Then run
```
ghc ViewHex.hs
```

## Usage

The usage is as follows
```
./ViewHex [in file]
```
You would replace the "./Main" with "Main.exe" if you are using Windows prompts

## Features

I tried to follow vim-like conventions, although it differs a little

j,k to move around (that's right! And you are going to like it!)

g to jump to a particular offset. Append +,- to your number for a relative offset. Of note is that if you don't append "0x", it'll default to base 10. I might change this at some point

/,? to search forwards and backwards for hex strings

n,N to repeat last search, with N going in the reverse direction

m plus letter to set a mark. \` plus letter to go to a mark


