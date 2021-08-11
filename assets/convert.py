import os,bitplanelib,json
from PIL import Image

sprites_dir = "sprites"
maze_palette_txt = "  dc.w  $0000,$021f"        # almost same blue as in game palette, same order
maze_palette = bitplanelib.palette_dcw2palette(maze_palette_txt)
maze = Image.open("maze.png")
maze_palette = bitplanelib.palette_extract(maze,0xF0)
bitplanelib.palette_image2raw(maze,r"../{}/maze.bin".format(sprites_dir),maze_palette,palette_precision_mask=0xF0)

# palette order matters
# some key colors are located at a 2**n position
# so they can be drawn with just one plane (pacman, white fonts)
game_palette_txt = """
     dc.w	$0000,$022f,$0ff0,$0fbf     ; black, maze blue, pac yellow, pen gate pink
     dc.w   $FBB,$00ff,$04ba,$0d95,$0FFF,$0ddf    ; dot pink, whatever, whatever, whatever, white
	 dc.w	$0edf,$0f00,$0fb5,$0fbb,$0F0,$04bf
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


game_palette = bitplanelib.palette_dcw2palette(game_palette_txt)
bitplanelib.palette_dump(game_palette,r"../src/palette_clist.s",as_copperlist=True)

outdir = "dumps"

def process_tiles():
    json_file = "tiles.json"
    with open(json_file) as f:
        tiles = json.load(f)

    default_width = tiles["width"]
    default_height = tiles["height"]
    default_horizontal = tiles["horizontal"]

    x_offset = tiles["x_offset"]
    y_offset = tiles["y_offset"]

    sprite_page = tiles["source"]

    sprites = Image.open(sprite_page)



    name_dict = {"bonus_{}".format(i):n for i,n in enumerate(["cherry","strawberry","orange","apple","melon","galaxian","bell","key"])}
    # we first did that to get the palette but we need to control
    # the order of the palette

    #game_palette = bitplanelib.palette_extract(img,0xF0)
    #bitplanelib.palette_dump(game_palette,"palette.s")



    for object in tiles["objects"]:
        if object.get("ignore"):
            continue
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
                bitplanelib.palette_image2sprite(cropped_img,"../{}/{}_{}.bin".format(sprites_dir,name,i),
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
                # if 1 plane, pacman frames, save only 1 plane, else save all 4 planes
                used_palette = p if len(p)==2 else game_palette

                namei = "{}_{}".format(name,i)
                bitplanelib.palette_image2raw(img,"../{}/{}.bin".format(sprites_dir,name_dict.get(namei,namei)),used_palette,palette_precision_mask=0xF0)

def process_fonts():
    json_file = "fonts.json"
    with open(json_file) as f:
        tiles = json.load(f)

    default_width = tiles["width"]
    default_height = tiles["height"]
    default_horizontal = tiles["horizontal"]

    x_offset = tiles["x_offset"]
    y_offset = tiles["y_offset"]

    sprite_page = tiles["source"]

    sprites = Image.open(sprite_page)



    name_dict = {"letter_row_0_{}".format(i):chr(ord('A')+i) for i in range(0,16)}
    name_dict.update({"letter_row_1_{}".format(i):chr(ord('P')+i) for i in range(0,11)})
    name_dict["letter_row_1_11"] = "exclamation"
    name_dict.update({"digit_row_0_{}".format(i):chr(ord('0')+i) for i in range(0,10)})
    # we first did that to get the palette but we need to control
    # the order of the palette



    for object in tiles["objects"]:
        if object.get("ignore"):
            continue
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

            # blitter object
            if x_size % 8:
                raise Exception("{} (frame #{}) with should be a multiple of 8, found {}".format(name,i,x_size))
            # pacman is special: 1 plane
            p = bitplanelib.palette_extract(cropped_img,palette_precision_mask=0xF0)
            # add 16 pixels
            img = Image.new("RGB",(x_size,cropped_img.size[1]))
            img.paste(cropped_img)
            # if 1 plane, pacman frames, save only 1 plane, else save all 4 planes
            used_palette = p if len(p)==2 else game_palette

            namei = "{}_{}".format(name,i)
            bitplanelib.palette_image2raw(img,"../{}/{}.bin".format(sprites_dir,name_dict.get(namei,namei)),used_palette,palette_precision_mask=0xF0)

process_tiles()

process_fonts()
