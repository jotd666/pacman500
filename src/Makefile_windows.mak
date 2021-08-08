#added by python script

PROGNAME = pacman
HDBASE = K:\jff\AmigaHD
WHDBASE = $(HDBASE)\PROJETS\HDInstall\DONE\WHDLoad
WHDLOADER = ../$(PROGNAME).slave
SOURCE = $(PROGNAME)HD.s
MAIN = ..\pacman
all: $(MAIN)

$(WHDLOADER) : $(SOURCE)
	wdate.py> datetime
	vasmm68k_mot -DDATETIME -I$(HDBASE)/amiga39_JFF_OS/include -I$(WHDBASE)\Include -I$(WHDBASE) -devpac -nosym -Fhunkexe -o $(WHDLOADER) $(SOURCE)

#ptplayer.o : ptplayer.s
#	vasmm68k_mot -phxass -nosym -Fhunk -kick1hunks -maxerrors=0 -I$(HDBASE)/amiga39_JFF_OS/include -o ptplayer.o ptplayer.s

$(MAIN) : pacman.s ptplayer.s 
	vasmm68k_mot -phxass -nosym -Fhunkexe -kick1hunks -maxerrors=0 -I$(HDBASE)/amiga39_JFF_OS/include -o $(MAIN) pacman.s
