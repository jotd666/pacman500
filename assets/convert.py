import os,bitplanelib,json
from PIL import Image


with open("tiles.json") as f:
    tiles = json.load(f)

default_width = tiles["width"]
default_height = tiles["height"]
default_horizontal = tiles["horizontal"]

x_offset = tiles["x_offset"]
y_offset = tiles["y_offset"]

sprite_page = tiles["source"]

sprites = Image.open(sprite_page)
maze = Image.open("maze.png")
maze_palette = bitplanelib.palette_extract(maze,0xF0)
bitplanelib.palette_dump(maze_palette,"maze.s")

# we first did that to get the palette but we need to control
# the order of the palette

#game_palette = bitplanelib.palette_extract(img,0xF0)
#bitplanelib.palette_dump(game_palette,"palette.s")

# ORDER MATTERS!!!!
game_palette_txt = """
     dc.w	$0000,$022f,$0ff0,$0F0,$FBB     ; black, maze blue, pac yellow, green (whatever), dot pink
     dc.w   $00ff,$04ba,$04bf,$0d95,$0ddf
	 dc.w	$0edf,$0f00,$0fb5,$0fbb,$0fbf,$0000
     ; sprite palette 16-32
     ; red ghost
     dc.w	$0000,$0f00,$022f,$0edf
     ; pink ghost
     dc.w	$0000,$0fbf,$022f,$0edf
     ; cyan ghost
     dc.w	$0000,$00ff,$022f,$0edf
     ; orange ghost
     dc.w	$0000,$0fb5,$022f,$0edf
"""

maze_palette_txt = "  dc.w  $0000,$021f"        # almost same blue as in game palette, same order

game_palette = bitplanelib.palette_dcw2palette(game_palette_txt)
maze_palette = bitplanelib.palette_dcw2palette(maze_palette_txt)

bitplanelib.palette_image2raw(maze,r"../src/maze.bin",maze_palette,palette_precision_mask=0xF0)
bitplanelib.palette_image2raw(sprites,r"../src/sprites.bin",game_palette,palette_precision_mask=0xF0)
bitplanelib.palette_dump(game_palette,r"../src/palette_clist.s",as_copperlist=True)

outdir = "dumps"

for object in tiles["objects"]:
    name = object["name"]
    start_x = object["start_x"]+x_offset
    start_y = object["start_y"]+y_offset
    horizontal = object.get("horizontal",default_horizontal)
    width = object.get("width",default_width)
    height = object.get("height",default_height)

    for i in range(object["frames"]):
        if horizontal:
            x = i*width+start_x
            y = start_y
        else:
            x = start_x
            y = i*height+start_y

        area = (x, y, x + width, y + height)
        cropped_img = sprites.crop(area)
        cropped_name = os.path.join(outdir,"{}_{}.png".format(name,i))
        cropped_img.save(cropped_name)

        # save
        x_size = cropped_img.size[0]
        sprite_number = object.get("sprite_number")
        if sprite_number is not None:
            if x_size != 16:
                raise Exception("{} (frame #{}) width (as sprite) should 16, found {}".format(name,i,x_size))
            sprite_palette_offset = 16+(sprite_number//2)*4
            bitplanelib.palette_image2sprite(cropped_img,"../src/{}.bin".format(name),
                game_palette[sprite_palette_offset:sprite_palette_offset+4],palette_precision_mask=0xF0)
        else:
            # blitter object
            if x_size % 16:
                raise Exception("{} (frame #{}) with should be a multiple of 16, found {}".format(name,i,x_size))
            # pacman is special: 1 plane
            p = bitplanelib.palette_extract(cropped_img,palette_precision_mask=0xF0)
            # add 16 pixels
            img = Image.new("RGB",(x_size+16,cropped_img.size[1]))
            img.paste(cropped_img)
            if len(p)==2:
                # 1 plane
                bitplanelib.palette_image2raw(img,"../src/{}.bin".format(name),p,palette_precision_mask=0xF0)
