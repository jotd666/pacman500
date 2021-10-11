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

    # pre-pad with 0W, used by ptplayer for idling
    if contents[0] != b'\x00' and contents[1] != b'\x00':
        # add zeroes
        with open(raw_file,"wb") as f:
           f.write(struct.pack(">H",0))
           f.write(contents)

