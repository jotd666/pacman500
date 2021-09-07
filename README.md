This is a (successful) attempt by jotd to create a 1:1 port of the famous arcade game on Amiga 500 using 100% 68k assembly.

The display is 4:3 so scores, lives, bonuses are on the side rather than on top/botton. The gameplay layout is 1:1 vs
the original, though.

REQUIRES:

- Any kickstart, 1MB memory (could be reduced to 512k with lower quality sounds)

FEATURES:

- original visual & sounds
- 50 frames per second (PAL) even on a 1MB 68000 A500
- all levels & bonuses & intermission sequences
- original intro
- joystick controlled (port 1)

BUGS:

- loselife, capture inky/pinky/clyde: sometimes ghost is trapped never exits? monitor "ghost_who_counts_dots" var
- pac leaving a few pixels behind
- pacman traverses ghosts sometimes: detect collisions after each move not after ghost+pac move

MINOR ISSUES:

- some loops not correct (bad loop timing now that we went 50hz)
- reset "frightened" palette to blue (for intro/intermission)
- demo mode (record input not replaying properly!!): timer pb 50/60 ???
- after game over: intro text shifted (draw timer vs update timer bug!!!)
- guru on exit? (well, happened once)

  (reported, not reproduced):

- 'Game Over' text staying up even when playing
- had to downgrade to OCS display in the Boot Menu as AGA gave corrupted graphics

TO DO:

- startup: pacman appears at mid-music
- sequence to enable cheat keys
- highscore save
- cheat keys to kill ghosts

BUILDING FROM SOURCES:

Prerequesites:

- python
- sox
- vasm 68k

Build process:

- To create the ".bin" files and some palette .s asm files,
  just run the "convert_sprites.py" python script, then use the "convert_sounds.py"
  python script (audio).
- python and sox must be installed to be able to perform the wav2raw conversions

Binary assets must be created first, then makefile must be called to create the "pacman" program


