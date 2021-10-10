import subprocess,os,struct

sox = r"k:\progs\sox-14-4-2\sox.exe"

wav_files = ["pacman_killed.wav","credit.wav","eat_1.wav","eat_2.wav",
"extra_life.wav","ghost_eaten.wav","bonus_eaten.wav","loop_1.wav","loop_2.wav",
"loop_3.wav","loop_4.wav","loop_5.wav","loop_fright.wav",
"loop_eyes.wav"]
outdir = "../sounds"

sampling_rate = 22050
nb_duplicates = 2
extra_margin = 0

for wav_file in wav_files:
    raw_file = os.path.join(outdir,os.path.splitext(os.path.basename(wav_file))[0]+".raw")
    def get_sox_cmd(sr,output):
        return [sox,"--volume","1.0",wav_file,"--channels","1","--bits","8","-r",str(sr),"--encoding","signed-integer",output]
    cmd = get_sox_cmd(sampling_rate,raw_file)

    subprocess.check_call(cmd)
    with open(raw_file,"rb") as f:
        contents = f.read()
    if "loop" in raw_file:
        # double the size, the loops are too short to be reworked without it being noticed
        if nb_duplicates > 1:
            contents = b"".join(contents for _ in range(nb_duplicates))
        # file size must be (roughly) a multiple of sampling rate/50 (here 441)
        size = len(contents)
        divider = (sampling_rate//50)
        nb_chunks,modulus = divmod(size,divider)
        aligned_size=nb_chunks*divider + extra_margin

        ratio = size/aligned_size
        if ratio-int(ratio) < 1e-3:
            # loop is almost perfect, okay, don't hack it any further
            print("{}: almost perfect loop".format(raw_file))
        else:
            # resample just enough so the size is a multiple of rate/50
            # (re-do the conversion process from the start)
            new_sampling_rate = (aligned_size * sampling_rate + 0.5) // size
            cmd = get_sox_cmd(new_sampling_rate,raw_file)
            subprocess.check_call(cmd)
            with open(raw_file,"rb") as f:
                contents = f.read()
                # double size again
            if nb_duplicates > 1:
                contents = b"".join(contents for _ in range(nb_duplicates))
            size = len(contents)
            nb_chunks,new_modulus = divmod(size,divider)
            aligned_size=nb_chunks*divider
            ratio = size/aligned_size

            print(raw_file,aligned_size,size,ratio)
        with open(raw_file,"wb") as f:
           f.write(contents)
    else:
        # pre-pad with 0W, used by ptplayer for idling
        if contents[0] != b'\x00' and contents[1] != b'\x00':
            # add zeroes
            with open(raw_file,"wb") as f:
               f.write(struct.pack(">H",0))
               f.write(contents)

