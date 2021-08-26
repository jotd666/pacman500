import subprocess,os

sox = r"k:\progs\sox-14-4-2\sox.exe"

wav_files = ["pacman_killed.wav","credit.wav","eat_1.wav","eat_2.wav",
"extra_life.wav","ghost_eaten.wav","bonus_eaten.wav","loop_1.wav","loop_fright.wav","loop_eyes.wav"]
outdir = "../sounds"

sampling_rate = 22500

for wav_file in wav_files:
    raw_file = os.path.join(outdir,os.path.splitext(os.path.basename(wav_file))[0]+".raw")
    if not os.path.exists(raw_file):
        cmd = [sox,"--volume","1.0",wav_file,"--channels","1","--bits","8","-r",str(sampling_rate),"--encoding","signed-integer",raw_file]
        subprocess.check_call(cmd)
##    with open(raw_file,"rb") as f:
##        contents = f.read()
##    with open(raw_file,"wb") as f:
##         f.write(struct.pack(">H",sampling_rate))
##         f.write(contents)
