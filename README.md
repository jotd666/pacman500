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

- loselife, capture inky/pinky/clyde: sometimes ghost is trapped never exits?
- pac leaving a few pixels behind
- pacman traverses ghosts sometimes: detect collisions after each move not after ghost+pac move
- guru on exit? (well, that's new!!)

MINOR ISSUES:

- some loops not correct (bad loop timing now that we went 50hz)
- reset "frightened" palette to blue (for intro/intermission)
- pt player sample play issues: yes but covered by sound loop most of the time
- demo mode (record input not replaying properly!!)
- after game over: intro text shifted (draw timer vs update timer bug!!!)
- pac leaving a few pixels behind

  (reported, not reproduced):

- 'Game Over' text staying up even when playing
- had to downgrade to OCS display in the Boot Menu as AGA gave corrupted graphics

TO DO:

- sequence to enable cheat keys
- highscore save
- cheat keys to kill ghosts
- intermission sequences:
  * 3"20 (after level 2): ghost chases pacman, big pacman chases back => DONE
  * 7"50 (after level 5): ghost chases pacman, tears his drape on a nail
  * 13"00 (after level 9): ghost chases pacman with repaired drape, but returns almost naked
  * 18"00 (after level 13): same as before


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


