	include	"exec/types.i"
	include	"exec/memory.i"
	include	"exec/libraries.i"
	include	"exec/execbase.i"

	include "dos/dos.i"
	include "dos/var.i"
	include "dos/dostags.i"
	include "dos/dosextens.i"
	include "intuition/intuition.i"
	include	"hardware/cia.i"
	include	"hardware/custom.i"
	include	"hardware/intbits.i"
	include	"graphics/gfxbase.i"
	include	"graphics/videocontrol.i"
	include	"graphics/view.i"
	include	"devices/console.i"
	include	"devices/conunit.i"
	include	"libraries/lowlevel.i"
	INCLUDE	"workbench/workbench.i"
	INCLUDE	"workbench/startup.i"
	
	include "lvo/exec.i"
	include "lvo/dos.i"
	include "lvo/lowlevel.i"
	include "lvo/graphics.i"
	
    
    include "whdmacros.i"

    incdir "../sprites"
    incdir "../sounds"

	
;CIA-A registre port A (bouton souris)

CIAAPRA = $BFE001

    STRUCTURE   SpritePalette,0
    UWORD   color0
    UWORD   color1
    UWORD   color2
    UWORD   color3
    LABEL   SpritePalette_SIZEOF
    
	STRUCTURE	Character,0
    ULONG   character_id
	UWORD	xpos
	UWORD	ypos
    UWORD   h_speed
    UWORD   v_speed
	UWORD	direction   ; sprite orientation
    UWORD   frame
    UWORD   speed_table_index
	LABEL	Character_SIZEOF

	STRUCTURE	Player,0
	STRUCT      BaseCharacter1,Character_SIZEOF
    UWORD   prepost_turn
    UBYTE   still_timer
    UBYTE   pad
    LABEL   Player_SIZEOF
    
	STRUCTURE	Ghost,0
	STRUCT      BaseCharacter2,Character_SIZEOF
	STRUCT      palette,SpritePalette_SIZEOF
    APTR     behaviour
    APTR     frame_table
    APTR     frightened_ghost_white_frame_table
    APTR     frightened_ghost_blue_frame_table
    APTR     eye_frame_table
    APTR     copperlist_address
    APTR     color_register
    UWORD    home_corner_xtile
    UWORD    home_corner_ytile
    UWORD    target_xtile
    UWORD    target_ytile
    UWORD    mode_timer     ; number of 1/50th to stay in the current mode
    UWORD    mode           ; current mode
    UWORD    previous_mode_timer     ; number of 1/50th to stay in the current mode
    UWORD    previous_mode           ; current mode
    UWORD    mode_counter   ; 0: first scatter, 1: first attack, etc.
    UWORD    flash_timer
    UWORD    flash_toggle_timer
    UWORD    pen_timer
    UWORD    pen_nb_dots
    UBYTE    reverse_flag   ; direction change flag
    UBYTE    flashing_as_white

	LABEL	Ghost_SIZEOF
    
    ;Exec Library Base Offsets


;graphics base

StartList = 38

;autres labels

Execbase  = 4

MODE_SCATTER = 10
MODE_CHASE = 20
MODE_FRIGHT = 30
MODE_EYES = 40

; TODO: set 60
NB_TICKS_PER_SEC = 50

; wall tile types
W = 4   ; wall
P = 3   ; pen space (pac block)
T = 2   ; tunnel
B = 1   ; ghost up block
O = 0   ; empty

TOTAL_NUMBER_OF_DOTS = 244

NB_BYTES_PER_LINE = 40
NB_BYTES_PER_MAZE_LINE = 28
MAZE_PLANE_SIZE = NB_BYTES_PER_LINE*NB_LINES
SCREEN_PLANE_SIZE = 40*NB_LINES
NB_PLANES   = 4

TUNNEL_MASK_X = NB_BYTES_PER_MAZE_LINE*8
TUNNEL_MASK_Y = OTHERS_YSTART_POS-27


NB_TILES_PER_LINE = 2+28+2    ; 2 fake tiles in the start & end
NB_TILE_LINES = 31+3    ; 3 fake tiles before the maze to simulate ghosts targets
NB_LINES = NB_TILE_LINES*8

MAZE_BLINK_TIME = NB_TICKS_PER_SEC/2

NB_FLASH_FRAMES = 14

; matches the pac kill animation
PLAYER_KILL_TIMER = NB_TICKS_PER_SEC+NB_TICKS_PER_SEC/2+(NB_TICKS_PER_SEC/8)*9+NB_TICKS_PER_SEC/4+NB_TICKS_PER_SEC
GHOST_KILL_TIMER = (NB_TICKS_PER_SEC*5)/6

    
X_START = 16
Y_START = 24
; tunnel max
X_MAX = (NB_TILES_PER_LINE-1)*8

RED_YSTART_POS = 92+Y_START
RED_XSTART_POS = 112+X_START
OTHERS_YSTART_POS = RED_YSTART_POS+24

BONUS_X_POS = RED_XSTART_POS-24
BONUS_Y_POS = RED_YSTART_POS+16
BONUS_OFFSET = $28F  ;(NB_TILES_PER_LINE*20)+14
BONUS_TIMER_VALUE = NB_TICKS_PER_SEC*10
BONUS_SCORE_TIMER_VALUE = NB_TICKS_PER_SEC*2
BLINK_RATE = 2*(NB_TICKS_PER_SEC/5) ; for powerdots
PREPOST_TURN_LOCK = 4

DOT_PLANE_OFFSET = SCREEN_PLANE_SIZE*2-(X_START/8)

; direction enumerates, follows order of ghosts in the sprite sheet
RIGHT = 0
LEFT = 1<<2
UP = 2<<2
DOWN = 3<<2

; possible direction bits, clockwise
DIRB_RIGHT = 0
DIRB_DOWN = 1
DIRB_LEFT = 2
DIRB_UP = 3
; direction masks
DIRF_RIGHT = 1<<DIRB_RIGHT
DIRF_DOWN = 1<<DIRB_DOWN
DIRF_LEFT = 1<<DIRB_LEFT
DIRF_UP = 1<<DIRB_UP

; states, 4 by 4, starting by 0

STATE_PLAYING = 0
STATE_GAME_OVER = 1*4
STATE_LEVEL_COMPLETED = 2*4
STATE_NEXT_LEVEL = 3*4
STATE_LIFE_LOST = 4*4

; jump table macro, used in draw and update
DEF_STATE_CASE_TABLE:MACRO
    move.w  current_state(pc),d0
    lea     .case_table(pc),a0
    move.l     (a0,d0.w),a0
    jmp (a0)
    
.case_table
    dc.l    .playing
    dc.l    .game_over
    dc.l    .level_completed
    dc.l    .next_level
    dc.l    .life_lost
    ENDM
    
; write current PC value to some address
LOGPC:MACRO
     bsr    .next_\1
.next_\1
      addq.l    #6,(a7) ; skip this & next instruction
      move.l    (a7)+,$\1
      ENDM

MUL_TABLE:MACRO
mul\1_table
	rept	256
	dc.w	REPTN*\1
	endr
    ENDM
    
ADD_XY_TO_A1:MACRO
    lea mul40_table(pc),\1
    add.w   d1,d1
    lsr.w   #3,d0
    move.w  (\1,d1.w),d1
    add.w   d0,a1       ; plane address
    add.w   d1,a1       ; plane address
    ENDM


    
Start:
    lea  _custom,a5
    move.b  #0,controller_joypad_1
    
    move.l  4,a6
    lea GRname(PC),a1               ; paramètre pour initialiser OpenLibrary
    clr.l d0
    jsr _LVOOpenLibrary(a6)             ; ouverture de la librairie graphique
    move.l d0,gfxbase
    move.l  d0,a4
    move.l StartList(a4),gfxbase_copperlist
    
;    sub.l   a1,a1
;    move.l  a4,a6
;    jsr (_LVOLoadView,a6)
;    jsr (_LVOWaitTOF,a6)
;    jsr (_LVOWaitTOF,a6)

    
    
    bsr init_interrupts
    
.restart    
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    bsr clear_screen
    
    bsr init_new_play

.new_level  
    bsr init_level
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)

    ; do it first, as the last bonus overwrites bottom left of screen
    bsr draw_bonuses    
    bsr draw_maze
    ; compute tunnel position sprite
    move.w  #TUNNEL_MASK_X,d0
    move.w  #TUNNEL_MASK_Y,d1
    bsr store_sprite_pos
    move.l  d0,tunnel_sprite_control_word
   
    ; for debug
    ;;bsr draw_bounds

    lea	screen_data+SCREEN_PLANE_SIZE*3,a1  ; white
    lea p1_string(pc),a0
    move.w  #232,d0
    move.w  #16,d1
    bsr write_string
    lea score_string(pc),a0
    move.w  #232,d0
    add.w  #8,d1
    bsr write_string
    
    lea high_score_string(pc),a0
    move.w  #232,d0
    move.w  #48,d1
    bsr write_string
    lea score_string(pc),a0
    move.w  #232,d0
    add.w  #8,d1
    bsr write_string
    
    bsr draw_dots

    
    moveq #NB_PLANES,d4
    lea	bitplanes,a0              ; adresse de la Copper-List dans a0
    move.l #screen_data,d1
    move.w #bplpt,d3        ; premier registre dans d3

		; 8 bytes per plane:32 + end + bplcontrol
.mkcl:
    move.w d3,(a0)+           ; BPLxPTH
    addq.w #2,d3              ; next register
    swap d1
    move.w d1,(a0)+           ; 
    move.w d3,(a0)+           ; BPLxPTL
    addq.w #2,d3              ; next register
    swap d1
    move.w d1,(a0)+           ; 
    add.l #SCREEN_PLANE_SIZE,d1       ; next plane of maze

    dbf d4,.mkcl
 
	
;DMA disabled, no multitask
    move.l  4,a6
    jsr _LVOForbid(a6)
    move.w #$03E0,dmacon(A5)

    lea game_palette(pc),a0
    lea _custom+color,a1
    move.w  #31,d0
.copy
    move.w  (a0)+,(a1)+
    dbf d0,.copy
;COPPER init
		
    move.l	#coplist,cop1lc(a5)
    clr.w copjmp1(a5)

;playfield init

    move.w #$3081,diwstrt(a5)             ; valeurs standard pour
    move.w #$30C1,diwstop(a5)             ; la fenêtre écran
    move.w #$0038,ddfstrt(a5)             ; et le DMA bitplane
    move.w #$00D0,ddfstop(a5)
    move.w #$4200,bplcon0(a5) ; 4 bitplanes
    clr.w bplcon1(a5)                     ; no scrolling
    clr.w bplcon2(a5)                     ; pas de priorité
    move.w #0,bpl1mod(a5)                ; modulo de tous les plans = 40
    move.w #0,bpl2mod(a5)

    bsr hide_sprites

    ; init sprite, bitplane, whatever dma (not audio ATM)
    move.w #$83E0,dmacon(a5)
    ; enable copper interrupts, mainly


.new_life
    bsr wait_bof
    bsr init_ghosts
    bsr init_player
    bsr draw_lives
    move.w #$C038,intena(a5)
.mainloop
    btst    #6,$bfe001
    beq.b   .out
    DEF_STATE_CASE_TABLE
    
.playing
.level_completed
    bra.b   .mainloop
.game_over
    bra.b   .mainloop
.next_level
    add.w   #1,level_number
    bra.b   .new_level
.life_lost
    sub.b   #1,nb_lives
    bne.b   .new_life
    move.w  #STATE_GAME_OVER,current_state
    bra.b   .game_over
.out      
    ; quit
    bsr     restore_interrupts
			      

    move.l  gfxbase,a1
    move.l  gfxbase_copperlist,StartList(a1) ; adresse du début de la liste
    move.l  gfxbase_copperlist,cop1lc(a5) ; adresse du début de la liste
    clr.w  copjmp1(a5)
    ;;move.w #$8060,dmacon(a5)        ; réinitialisation du canal DMA
    
    move.l  gfxbase,a1
    jsr _LVOCloseLibrary(a6)
    
    jsr _LVOPermit(a6)                  ; Task Switching autorisé
    moveq.l #0,d0
    rts

wait_bof
	move.l	d0,-(a7)
.wait	move.l	$dff004,d0
	and.l	#$1ff00,d0
	cmp.l	#260<<8,d0
	bne.b	.wait
.wait2	move.l	$dff004,d0
	and.l	#$1ff00,d0
	cmp.l	#260<<8,d0
	beq.b	.wait2
	move.l	(a7)+,d0
	rts    
    
clear_screen
    lea screen_data,a1
    moveq.l #3,d0
.cp
    move.w  #(NB_BYTES_PER_LINE*NB_LINES)/4-1,d1
    move.l  a1,a2
.cl
    clr.l   (a2)+
    dbf d1,.cl
    add.l   #SCREEN_PLANE_SIZE,a1
    dbf d0,.cp
    rts
    
init_new_play:
    ; global init at game start
    move.b  #3,nb_lives
    move.w  #0,level_number
    move.l  #0,score
    move.l  #0,displayed_score
    rts
init_level: 
    ; level
    move.w  level_number,d2
    cmp.w   #21,d2
    bcs.b   .okay
    ; maxed out
    move.w  #20,d2
.okay
    ; elroy threshold
    lea elroy_table(pc),a1
    move.b  (a1,d2.w),d0
    move.b  d0,elroy_threshold_1
    lsr.w   #1,d0   ; half
    move.b  d0,elroy_threshold_2
    
    add.w   d2,d2

    lea bonus_level_score(pc),a0
    move.w  (a0,d2.w),fruit_score
    lea bonus_level_table(pc),a0
    move.w  (a0,d2.w),fruit_score_index
    move.b  #0,nb_dots_eaten
    
    ; speed table
    add.w   d2,d2
    lea speed_table(pc),a1
    move.l  (a1,d2.w),a1    ; global speed table
    move.l  a1,global_speed_table
    
    rts
 
hide_sprites:
    move.w  #7,d1
    lea  sprites,a0
    lea empty_sprite,a1
.emptyspr

    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
    addq.l  #8,a0
    dbf d1,.emptyspr
    rts
    
hide_ghost_sprites:
    move.w  #3,d1
    lea  sprites+8,a0
    lea empty_sprite,a1
.emptyspr

    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
    add.w  #16,a0
    dbf d1,.emptyspr
    rts

; < A0: ghost structure
set_normal_ghost_palette
    move.l  a1,-(a7)
    move.l  color_register(a0),a1
    ; set/reset palette
    move.l  palette(a0),(a1)+
    move.l  palette+4(a0),(a1)
    move.l  (a7)+,a1
    rts
    
; Pinky's dot limit is always set to zero, causing him to leave home immediately when every level begins. 
; For the first level, Inky has a limit of 30 dots, and Clyde has a limit of 60. This results in Pinky exiting immediately which, in turn,
; activates Inky's dot counter. His counter must then reach or exceed 30 dots before he can leave the house. Once Inky starts to leave,
; Clyde's counter (which is still at zero) is activated and starts counting dots.
; 
; When his counter reaches or exceeds 60, he may exit.
; On the second level, Inky's dot limit is changed from 30 to zero, while Clyde's is changed from 60 to 50.
; Inky will exit the house as soon as the level begins from now on.
;
; Starting at level three, all the ghosts have a dot limit of zero for the remainder of the game and
; will leave the ghost house immediately at the start of every level.

init_ghosts
    lea ghosts(pc),a0
    lea ghost_sprites,a1   ; the sprite part of the copperlist, sprite 1-7 are the ghost sprites
    lea .behaviour_table(pc),a2
    lea game_palette+32(pc),a3  ; the sprite part of the color palette 16-31
    ; shared settings
    moveq.w #3,d7
    lea _custom+color+32,a4
    
    ; init ghost dot count table
    move.l  #ghosts+2*Ghost_SIZEOF,ghost_which_counts_dots
    ; init pen tile
    move.l  #(RED_XSTART_POS>>3)<<16+(OTHERS_YSTART_POS>>3),pen_tile_xy
    
.igloop
    ; copy all 4 colors (back them up)
    move.l (a3)+,palette(a0)
    move.l (a3)+,palette+4(a0)
    move.l  a4,color_register(a0)
    ; set/reset palette
    bsr set_normal_ghost_palette
    
    addq.l  #8,a4   ; next color register range
    move.l  a1,copperlist_address(a0)
    add.l   #16,a1
    
    clr.w   mode_counter(a0)
    clr.w   speed_table_index(a0)
    clr.w   pen_timer(a0)
    clr.w   pen_nb_dots(a0)
    clr.b   reverse_flag(a0)

    clr.w   h_speed(a0)
    clr.w   v_speed(a0)
    clr.w   target_xtile(a0)
    clr.w   target_ytile(a0)
    clr.w   home_corner_xtile(a0)
    clr.w   home_corner_ytile(a0)
    
    bsr     update_ghost_mode_timer
    move.w  #MODE_SCATTER,mode(a0)
    move.l  (a2)+,behaviour(a0)
	move.w	#OTHERS_YSTART_POS,ypos(a0)
    
    add.l   #Ghost_SIZEOF,a0
    dbf d7,.igloop
    
    ; dot counters
    
    moveq.w #3,d7
    lea ghosts(pc),a0
    move.w  level_number(pc),d0
    bne.b   .no_first_level
    lea     .dot_counter_table_level_1(pc),a2
    bra.b   .igloop2
.no_first_level
    cmp.w   #1,d0
    bne.b   .skip_dot_init
    lea     .dot_counter_table_level_2(pc),a2
    bra.b   .igloop2
.no_second_level


.igloop2
    move.w  (a2)+,pen_nb_dots(a0)
    add.l   #Ghost_SIZEOF,a0
    dbf d7,.igloop2
.skip_dot_init    
    
    
    ; specific settings
    lea ghosts(pc),a0
    ; red ghost
	move.w  #RED_XSTART_POS,xpos(a0)
	move.w	#RED_YSTART_POS,ypos(a0)
    move.w  #LEFT,direction(a0)
    move.w  #8,frame(a0)
    move.l  #'BLIN',character_id(a0)

    move.l  #red_ghost_frame_table,frame_table(a0)
    move.l  #red_frightened_ghost_white_frame_table,frightened_ghost_white_frame_table(a0)
    move.l  #red_frightened_ghost_blue_frame_table,frightened_ghost_blue_frame_table(a0)
    move.l  #red_ghost_eye_frame_table,eye_frame_table(a0)
    move.w  #(NB_TILES_PER_LINE-6),home_corner_xtile(a0)
    bsr     update_ghost_target
    move.w  #-1,h_speed(a0)
    ; pink ghost
    add.l   #Ghost_SIZEOF,a0
    move.l  #'PINK',character_id(a0)
	move.w  #RED_XSTART_POS,xpos(a0)
    move.w  #DOWN,direction(a0)
    move.w  #0,frame(a0)
    move.w  #2,home_corner_xtile(a0)
    move.l  #pink_ghost_frame_table,frame_table(a0)
    move.l  #pink_frightened_ghost_white_frame_table,frightened_ghost_white_frame_table(a0)
    move.l  #pink_frightened_ghost_blue_frame_table,frightened_ghost_blue_frame_table(a0)
    move.l  #pink_ghost_eye_frame_table,eye_frame_table(a0)
    bsr     update_ghost_target
    move.w  #0,home_corner_ytile(a0)
    move.w  #1,v_speed(a0)

    add.l   #Ghost_SIZEOF,a0
    ; cyan ghost
    move.l  #'INKY',character_id(a0)
	move.w  #(RED_XSTART_POS-16),xpos(a0)
    move.w  #UP,direction(a0)
    move.w  #0,frame(a0)
    move.l  #cyan_ghost_frame_table,frame_table(a0)
    move.l  #cyan_frightened_ghost_white_frame_table,frightened_ghost_white_frame_table(a0)
    move.l  #cyan_frightened_ghost_blue_frame_table,frightened_ghost_blue_frame_table(a0)
    move.w  #(NB_TILES_PER_LINE-4),home_corner_xtile(a0)
    move.w  #(NB_TILE_LINES+1),home_corner_ytile(a0) 
    move.l  #cyan_ghost_eye_frame_table,eye_frame_table(a0)
    bsr update_ghost_target
    move.w  #-1,v_speed(a0)
    ; orange ghost
    add.l   #Ghost_SIZEOF,a0
    move.l  #'CLYD',character_id(a0)

	move.w  #(RED_XSTART_POS+16),xpos(a0)
    move.w  #UP,direction(a0)
    move.w  #0,frame(a0)
    move.l  #orange_ghost_frame_table,frame_table(a0)
    move.l  #orange_frightened_ghost_white_frame_table,frightened_ghost_white_frame_table(a0)
    move.l  #orange_frightened_ghost_blue_frame_table,frightened_ghost_blue_frame_table(a0)
    move.l  #orange_ghost_eye_frame_table,eye_frame_table(a0)
    move.w  #(NB_TILE_LINES+1),home_corner_ytile(a0)
    bsr update_ghost_target
    move.w  #-1,v_speed(a0)

    rts
    
.dot_counter_table_level_1
    dc.w    0,0,30,60
.dot_counter_table_level_2
    dc.w    0,0,0,50
    

.behaviour_table
    dc.l    red_chase,pink_chase,cyan_chase,orange_chase

GET_PLAYER_TILE:MACRO

    ENDM

; all 4 "chase" functions have the same in/out params    
; < A0: ghost structure
; < A1: player structure
; < D0: player tile x
; < D1: player tile y
; > D0, D1: target tile
; trashes: none
red_chase
    ; player tile is the target, just return
    rts

pink_chase
    ; according to pac-man direction, add an offset of 4 tiles
    move.l a2,-(a7)
    lea     pinky_direction_offset_table(pc),a2
    add.w   direction(a1),a2   ; each direction is an enum 0,4,8,12 already
    add.w   (a2)+,d0
    add.w   (a2)+,d1
    move.l (a7)+,a2
    rts
cyan_chase
    ; according to pac-man direction, add an offset of 2 tiles to the temp tile
    movem.l d2/a2,-(a7)
    lea     inky_direction_offset_table(pc),a2
    add.w   direction(a1),a2   ; each direction is an enum 0,4,8,12 already
    add.w   (a2)+,d0
    add.w   (a2)+,d1
    
    ; now the position of the red ghost comes to play
    lea red_ghost(pc),a2
    ; algebraic distance for X and Y between temp target & red ghost tile
    move.w  xpos(a2),d2
    sub.w   d0,d2
    ; sub that distance to create symmetrical point
    sub.w   d2,d0
    ; for Y
    move.w  ypos(a2),d2
    sub.w   d1,d2
    sub.w   d2,d1
    ; that's it! simple, yet effective
    movem.l (a7)+,d2/a2
    rts
    
orange_chase
    movem.l a0/d2-d6,-(a7)      ; uses an awful lot of registers
    ; compute distance between clyde and pacman
    move.w  d0,d2       ; pacman XY tile
    move.w  d1,d3
    move.w  xpos(a0),d4
    move.w  ypos(a0),d5
    lsr.w   #3,d4   ; ghost XY tile
    lsr.w   #3,d5
    bsr compute_square_distance
    ; euclidian distance threshold is 8 tiles
    cmp.l   #8*8,d6
    movem.l (a7)+,a0/d2-d6      ; restore registers now, we need a0 back
    bcc.b   .simple     ; run to pacman tile if far away enough
    ; close to pacman: target is the home tile, like scatter mode
    move.w  home_corner_xtile(a0),d0
    move.w  home_corner_xtile(a1),d1
.simple
    rts
    
pinky_direction_offset_table
    dc.w    4,0     ; right
    dc.w    -4,0    ; left
    dc.w    -4,4    ; up: left offset is added, this is an original game bug
    dc.w    4,0     ; down
inky_direction_offset_table
    dc.w    2,0     ; right
    dc.w    -2,0    ; left
    dc.w    -2,2    ; up: left offset is added, this is an original game bug
    dc.w    2,0     ; down

; < A0: ghost structure
; trashes: A1, D0 & D1
update_ghost_target
    move.w  mode(a0),d0
    cmp.w   #MODE_SCATTER,d0
    beq.b   .scatter
    cmp.w   #MODE_CHASE,d0
    beq.b   .chase
    cmp.w   #MODE_EYES,d0
    beq.b   .run_home
    ; fright: no update
    rts
.scatter:
    move.l  home_corner_xtile(a0),target_xtile(a0)  ; hack copy x & y at once
    rts
.run_home:
    move.l  pen_tile_xy,target_xtile(a0)  ; hack copy x & y at once
    rts
.chase:
    lea player(pc),a1
    pea .next(pc)
    move.w  xpos(a1),d0
    move.w  ypos(a1),d1
    lsr.w   #3,d0
    lsr.w   #3,d1
    move.l  behaviour(a0),-(a7)
    rts
.next
    move.w  d0,target_xtile(a0)
    move.w  d1,target_ytile(a0)
    rts
    
; < A0: ghost structure
; trashes: nothing
update_ghost_mode_timer
    movem.l d0/a1,-(a7)
    move.w  level_number,d0
    cmp.w   #4,d0
    bcs.b   .low
    move.w  #4,d0
.low
    add.w   d0,d0
    lea     timer_table(pc),a1
    move.l  (a1,d0.w),a1
    move.w   mode_counter(a0),d0
    add.w   d0,d0
    move.w  (a1,d0.w),mode_timer(a0)
    movem.l (a7)+,d0/a1

    rts
    
init_player
    clr.w   bonus_clear_message

    lea player(pc),a0
    move.l  #'PACM',character_id(a0)
	move.w  #RED_XSTART_POS,xpos(a0)
	move.w	#188+Y_START,ypos(a0)
	move.w 	#LEFT,direction(a0)
    clr.w  speed_table_index(a0)
    move.w  #-1,h_speed(a0)
    clr.w   v_speed(a0)
    clr.w   prepost_turn(a0)
    clr.b   still_timer(a0)
    move.w  #0,frame(a0)
    move.w  #50,ready_timer
    clr.l   previous_random
    move.w  #-1,player_killed_timer
    move.w  #STATE_PLAYING,current_state
    move.l  screen_data+2*NB_BYTES_PER_LINE+SCREEN_PLANE_SIZE,previous_pacman_address  ; valid address for first clear
	rts	    

DEBUG_X = 8     ; 232+8
DEBUG_Y = 8

ghost_debug
    lea ghosts(pc),a2
    move.w  #DEBUG_X,d0
    move.w  #DEBUG_Y+100,d1
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1 
    lea .gx(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w xpos(a2),d2
    move.w  #5,d3
    bsr write_decimal_number
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    move.l  d0,d4
    lea .gy(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w ypos(a2),d2
    move.w  #3,d3
    bsr write_decimal_number

    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .timer(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  mode_timer(a2),d2
    divu    #50,d2
    swap    d2
    clr.w   d2
    swap    d2
    move.w  #2,d3
    bsr write_decimal_number
    
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .mode(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  mode(a2),d2
    move.w  #0,d3
    bsr write_decimal_number
    
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .dir(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  direction(a2),d2
    move.w  #0,d3
    bsr write_decimal_number

    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .pdir(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  possible_directions,d2
    move.w  #4,d3
    bsr write_hexadecimal_number
    rts
.gx
        dc.b    "GX ",0
.gy
        dc.b    "GY ",0
.timer
        dc.b    "MTIM ",0
.mode
        dc.b    "MODE ",0
.dir
        dc.b    "DIR ",0
.pdir
        dc.b    "PDIR ",0
        even
possible_directions
        dc.w    0
        
draw_debug
    lea player(pc),a2
    move.w  #DEBUG_X,d0
    move.w  #DEBUG_Y,d1
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1 
    lea .px(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w xpos(a2),d2
    move.w  #5,d3
    bsr write_decimal_number
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    move.l  d0,d4
    lea .py(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w ypos(a2),d2
    move.w  #3,d3
    bsr write_decimal_number
    move.l  d4,d0
    ;;
    add.w  #8,d1
    lea .dots(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.b nb_dots_eaten,d2
    move.w  #0,d3
    bsr write_decimal_number
    ;;
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .bonus(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w bonus_timer,d2
    move.w  #3,d3
    bsr write_decimal_number

    bsr ghost_debug

    rts
    
.px
        dc.b    "PX ",0
.py
        dc.b    "PY ",0
.dots
        dc.b    "DOTS ",0
.bonus
        dc.b    "BT ",0

        even

draw_ghosts:
    tst.w  ghost_eaten_timer
    bmi.b   .no_ghost_eat
    bsr hide_ghost_sprites
    
    ; store score
    lea player(pc),a4
    move.l  score_frame(pc),a0
    move.w  xpos(a4),d0
    sub.w   #24,d0
    move.w  ypos(a4),d1
    sub.w   #30,d1
    bsr store_sprite_pos      
    move.l  d0,(a0)
    lea score_sprite_entry,a1
    move.l  a0,d2
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)
    ; change color for score, ghost has disappeared anyway

    move.w  #$00ff,_custom+color+32+8+2
    ; don't place sprites
    rts
.no_ghost_eat
    move.w  player_killed_timer(pc),d6
    bmi.b   .normal
    cmp.w   #PLAYER_KILL_TIMER-NB_TICKS_PER_SEC,d6
    bcc.b   .normal
    ; clear the ghosts sprites after 1 second when pacman is killed
    bsr.b hide_sprites
    rts
.normal
    ; enable tunnel mask
    move.l  tunnel_sprite_control_word(pc),black_sprite
    lea     sprites,a0
    move.l  #black_sprite,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
     
    lea ghosts(pc),a0
    moveq.l #3,d7
.gloop    
    move.w  xpos(a0),d0
    move.w  ypos(a0),d1
    ; center => top left
    sub.w  #8+X_START,d0
    sub.w  #4+Y_START,d1    ; not 8, because maybe table is off?
    bsr store_sprite_pos    
    move.w  mode(a0),d3 ; scatter/chase/fright/return base
    cmp.w   #MODE_EYES,d3
    beq.b   .eyes

    move.l  frame_table(a0),a1
    move.w  frame(a0),d2
    lsr.w   #2,d2   ; 8 divide to get 0,1, divide by 4 then mask after adding direction
    cmp.w   #MODE_FRIGHT,d3
    bne.b   .no_fright
    ; change palette for that sprite
    move.w  mode_timer(a0),d4

    cmp.w   flash_timer(a0),d4
    bcc.b   .no_flashing        ; flashing if mode_timer is below flash_timer
    ; now check the flash toggle
    move.w  flash_toggle_timer(a0),d4
    addq.w  #1,d4
    cmp.w   #NB_FLASH_FRAMES,d4
    bne.b   .no_toggle
    clr.w   d4
    eor.b   #1,flashing_as_white(a0)
.no_toggle
    move.w  d4,flash_toggle_timer(a0)
.no_flashing
    move.l  color_register(a0),a3
    lea     frightened_ghosts_blue_palette(pc),a2
    ; select proper palette (blue/white)
    tst.b   flashing_as_white(a0)
    beq.b   .no_white
    ; white flashing
    lea     frightened_ghosts_white_palette(pc),a2
.no_white
    ; directly change color registers for that sprite
    move.l  (a2)+,(a3)+
    move.l  (a2)+,(a3)+
    bra.b   .fright
.no_fright
    add.w   direction(a0),d2     ; add 0,4,8,12 depending on direction
    bra.b   .no_white2
.fright
    ; select proper fright sprite
    move.l  frightened_ghost_blue_frame_table(a0),a1
    tst.b   flashing_as_white(a0)
    beq.b   .no_white2
    move.l  frightened_ghost_white_frame_table(a0),a1    
.no_white2
    bclr    #0,d2   ; even
    add.w   d2,d2       ; times 2
.end_anim
    move.l  (a1,d2.w),a1
    move.l  d0,(a1)     ; store control word
    move.l  a1,d2    
    move.l  copperlist_address(a0),a1
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)    
    
    add.l   #Ghost_SIZEOF,a0
    dbf d7,.gloop
    rts

.eyes
    move.l eye_frame_table(a0),a1
    move.w   direction(a0),d2
    lea ghost_eyes(pc),a2       ; palette
    move.l  color_register(a0),a3
    move.l  (a2)+,(a3)+
    move.l  (a2)+,(a3)+
    ; TODO set black tunnel sprite proper palette in copperlist
    bra.b   .end_anim
     
draw_all
    DEF_STATE_CASE_TABLE
    
.level_completed
.life_lost
.next_level
    ; don't do anything
    rts
    
.game_over
    move.w  #72,d0
    move.w  #136,d1
    move.w  #$0f00,d2   ; red
    lea game_over_string(pc),a0
    bsr write_color_string
    bra.b   .draw_complete
.playing
    tst.w   ready_timer
    beq.b   .ready_off
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  #88,d0
    move.w  #136,d1
    cmp.w   #1,ready_timer
    bne.b   .still_ready
    ; remove "READY!" message
    move.w  #14,d2   ; 96
    bsr clear_plane_any
    bra.b   .ready_off
.still_ready
    move.w  #$0ff0,d2
    lea ready_string(pc),a0
    bsr write_color_string
    ;;bra.b   .draw_complete
.ready_off
    bsr   draw_ghosts

    lea player(pc),a2
    ; draw_pacman
    tst.w  ghost_eaten_timer
    bmi.b   .normal_pacdraw
    lea     pac_dead+64*12,a0       ; empty
    bra.b   .pacblit
.normal_pacdraw
    tst.w  player_killed_timer
    bmi.b   .normal
    lea     pac_dead,a0
    move.w  death_frame_offset(pc),d0
    add.w   d0,a0       ; proper frame to blit
    bra.b   .pacblit

.normal    
    move.w  direction(a2),d0
    lea  pac_dir_table(pc),a0
    move.l  (a0,d0.w),a0
    move.w  frame(a2),d0
    add.w   d0,d0
    add.w   d0,d0
    move.l  (a0,d0.w),a0
.pacblit
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  xpos(a2),d0
    move.w  ypos(a2),d1
    ; center => top left
    moveq.l #-1,d2 ; mask
    sub.w  #8+Y_START,d1
    sub.w  #8+X_START,d0
    bpl.b   .no_left
    ; d0 is negative
    neg.w   d0
    lsr.l   d0,d2
    neg.w   d0
    add.w   #NB_BYTES_PER_LINE*8,d0
    subq.w  #1,d1
    bra.b   .pdraw
.no_left
    ; check mask to the right
    move.w  d0,d4    
    sub.w   #X_MAX-24-X_START,d4
    bmi.b   .pdraw
    lsl.l   d4,d2
    swap    d2
    clr.w   d2
.pdraw
    ; first roughly clear some pacman zones that can remain. We don't test the directions
    ; just clear every possible pixel that could remain whatever the direction was/is
    move.l  previous_pacman_address(pc),a3
    clr.l   (a3)
    clr.l   (NB_BYTES_PER_LINE,a3)
    clr.l   (NB_BYTES_PER_LINE*2,a3)
    clr.l   (NB_BYTES_PER_LINE*3,a3)
    clr.l   (NB_BYTES_PER_LINE*5,a3)
    clr.l   (NB_BYTES_PER_LINE*6,a3)
    clr.l   (NB_BYTES_PER_LINE*7,a3)
    clr.l   (NB_BYTES_PER_LINE*8,a3)
    clr.l   (NB_BYTES_PER_LINE*9,a3)
    clr.l   (NB_BYTES_PER_LINE*15,a3)
    clr.l   (NB_BYTES_PER_LINE*16,a3)

    bsr blit_plane
    ; A1 is start of dest, use it to clear upper part and lower part
    ; and possibly shifted to the left/right
;;    move.l  a1,d0
;;    btst    #0,d0
;;    beq.b   .ok
;;    subq.l  #1,a1   ; even address, always!
;;.ok
    move.l  a1,previous_pacman_address

    
    
    ; timer not running, animate
    move.w  power_dot_timer(pc),d0
    bne.b   .nospd
    lea  powerdots(pc),a0
    moveq.l #3,d0
.drawpdloop
    move.l  (a0)+,d1 
    beq.b   .zap_draw
    move.l  d1,a1
    bsr draw_power_dot
.zap_draw    
    dbf d0,.drawpdloop
    bra.b   .powerdot_done
.nospd
    cmp.w   #BLINK_RATE/2,d0
    bne.b   .powerdot_done
    lea  powerdots(pc),a0
    moveq.l #3,d0
.clrpdloop
    move.l  (a0)+,d1
    beq.b   .zap_clear
    move.l  d1,a1
    bsr clear_power_dot
.zap_clear    
    dbf d0,.clrpdloop
.powerdot_done
    cmp.w   #BONUS_SCORE_TIMER_VALUE,bonus_score_timer
    bne.b   .no_bonus_score_appear
    move.w  fruit_score_index(pc),d0
    bsr draw_bonus_score
.no_bonus_score_appear
    tst.w   bonus_clear_message
    beq.b   .no_bonus_score_disappear
    clr.w   bonus_clear_message
    move.w  #BONUS_X_POS,d0
    move.w  #BONUS_Y_POS,d1
    move.w  #4,d2
    lea	screen_data,a1
    move.w  #3,d3
    bsr clear_plane_any
    add.l  #SCREEN_PLANE_SIZE*3,a1
    bsr clear_plane_any
.no_bonus_score_disappear    
    ; bonus
    cmp.w   #BONUS_TIMER_VALUE,bonus_timer
    bne.b   .no_fruit_appear
    ; blit fruit
    move.w  #BONUS_X_POS,d0
    move.w  #BONUS_Y_POS,d1
    move.w  level_number,d2
    bsr draw_bonus
.no_fruit_appear
    tst.w   bonus_clear_message
    beq.b   .no_fruit_disappear
    clr.w   bonus_clear_message
    
    move.w  #BONUS_X_POS,d0
    move.w  #BONUS_Y_POS,d1
    move.w  #4,d2
    lea	screen_data,a1
    move.w  #3,d3
.cloop
    bsr clear_plane_any
    add.l  #SCREEN_PLANE_SIZE,a1
    dbf d3,.cloop
.no_fruit_disappear
    ; score
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1  ; white
    
    move.l  score,d0
    cmp.l   displayed_score,d0
    beq.b   .no_score_update
    move.l  d0,d2
    move.l  d0,displayed_score

    move.w  #232+16,d0
    move.w  #24,d1
    move.w  #6,d3
    bsr write_decimal_number
    
    move.l  high_score,d4
    cmp.l   d2,d4
    bcc.b   .no_score_update
    ; high score
    move.l  d2,high_score
    move.w  #232+16,d0
    move.w  #24+32,d1
    move.w  #6,d3
    bsr write_decimal_number
.no_score_update
.draw_complete
    rts

random:
    move.l  previous_random(pc),d0
	;;; EAB simple random generator
    ; thanks meynaf
    mulu #$a57b,d0
    addi.l #$bb40e62d,d0
    rol.l #6,d0
    move.l  d0,previous_random
    rts

; < D0: 0,1,2,... score index 100,300,500,700 ...

draw_bonus_score:
    move.w  d0,d3
    cmp.w   #4,d0
    bcs.b   .under_thousand
    sub.w   #4,d0
.under_thousand
    lsl.w   #6,d0       ; *64
    lea bonus_scores,a0
    add.w   d0,a0
    lea	screen_data+BONUS_X_POS/8+BONUS_Y_POS*NB_BYTES_PER_LINE,a1
    move.l  a1,a2
    ; change x when >= 1000
    moveq.w  #10,d0
    cmp.w   #4,d3
    bcs.b   .no_extra_zero1
    ; extra zero: x is more to the left
    moveq.l  #6,d0    
.no_extra_zero1
    moveq.l  #0,d1
    moveq.l #-1,d2
    bsr blit_plane
    lea (SCREEN_PLANE_SIZE*3,a2),a1
    moveq.l  #10,d0
    cmp.w   #4,d3
    bcs.b   .no_extra_zero2
    moveq.l  #6,d0    
.no_extra_zero2
    moveq.l  #0,d1
    moveq.l #-1,d2
    bsr blit_plane
    
    cmp.w   #4,d3
    bcs.b   .no_extra_zero
    bsr wait_blit       ; wait else blit is going to write concurrently
    add.l  #NB_BYTES_PER_LINE*4,a2
    ; add an extra zero character to the right
    move.l  #%0011000000000000000,d0
    move.l  #%0100100000000000000,d1
    moveq.w #1,d2
.orloop
    or.l    d0,(a2)
    or.l    d1,(NB_BYTES_PER_LINE,a2)
    or.l    d1,(NB_BYTES_PER_LINE*2,a2)
    or.l    d1,(NB_BYTES_PER_LINE*3,a2)
    or.l    d1,(NB_BYTES_PER_LINE*4,a2)
    or.l    d1,(NB_BYTES_PER_LINE*5,a2)
    or.l    d0,(NB_BYTES_PER_LINE*6,a2)
    lea (SCREEN_PLANE_SIZE*3,a2),a2
    dbf d2,.orloop
.no_extra_zero
    
    rts
    
; what: clears a plane of any width (ATM not using blitter)
; args:
; < A1: dest
; < D0: X (multiple of 8)
; < D1: Y
; < D2: blit width in bytes (+2)
; trashes: none

clear_plane_any
    movem.l d0-D2/a0-a2,-(a7)
    lsr.w   #3,d0
    add.w   d0,a1
    lea mul40_table(pc),a2
    add.w   d1,d1    
    move.w  (a2,d1.w),d1
    add.w   d1,a1
    move.l  a1,a0
    move.w  #15,d0
.yloop
    move.w  d2,d1
    addq.w  #1,d1   ; 2-1
.xloop
    clr.b   (a0)+
    dbf d1,.xloop
    ; next line
    add.l   #NB_BYTES_PER_LINE,a1
    move.l  a1,a0
    dbf d0,.yloop
.out
    movem.l (a7)+,d0-D2/a0-a2
    rts
    
draw_lives:
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.l #NB_BYTES_PER_MAZE_LINE*8,d0
    moveq.l #0,d1
    move.l  #8,d2
    bsr clear_plane_any

    lea pac_lives,a0
    move.b  nb_lives(pc),d7
    ext     d7
    subq.w  #2,d7
    bmi.b   .out
    move.w #NB_BYTES_PER_MAZE_LINE*8,d3
    moveq.l #-1,d2  ; mask
.lloop
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  d3,d0
    moveq.l #0,d1
    bsr blit_plane
    add.w   #16,d3
    dbf d7,.lloop
.out

draw_bonuses:
    move.w #NB_BYTES_PER_MAZE_LINE*8,d0
    move.w #248-32,d1
    move.w  level_number,d2
    move.w  #1,d4
.dbloopy
    move.w  #5,d3
.dbloopx
    bsr draw_bonus
    subq.w  #1,d2
    bmi.b   .outb
    add.w   #16,d0
    dbf d3,.dbloopx
    move.w #NB_BYTES_PER_MAZE_LINE*8,d0
    add.w   #16,d1
    dbf d4,.dbloopy
.outb
    
    rts
    
; < D0: X
; < D1: Y
; < D2: level number

draw_bonus
    movem.l d0-d3/a0-a1,-(a7)
    lea	screen_data,a1
    cmp.w   #12,d2
    bcs.b   .ok
    move.w  #12,d2  ; maxed 
.ok
    add.w   d2,d2
    lea bonus_level_table,a0
    move.w  (a0,d2.w),d3      ; bonus index * 320
    mulu.w  #10,d3
    swap    d3
    clr     d3
    swap    d3
    lsl.l   #5,d3
    lea bonus_pics,a0
    add.l   d3,a0    ; bonus bitplanes
    bsr blit_4_planes
    movem.l (a7)+,d0-d3/a0-a1
    rts
    
draw_maze:
    lea game_palette(pc),a0
    move.w  (2,a0),maze_color     ; save original maze color
    move.w  #MAZE_BLINK_TIME,maze_blink_timer
    move.b  #8,maze_blink_nb_times
    ; copy maze data in bitplanes
    lea maze_data(pc),a0
    lea screen_data,a1
    move.w  #NB_LINES-1,d0
.copyline
    move.w  #6,d1
.copylong
    move.l  (a0)+,(a1)+
    dbf d1,.copylong
    add.l  #12,a1
    ; init planes in copperlist (after colors)
    dbf d0,.copyline
    rts    

; debug function
draw_bounds
    lea wall_table,a0
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1
    
    move.w  #NB_TILE_LINES-1,d0    
.loopy
    move.w  #NB_TILES_PER_LINE-1,d1
.loopx
    move.b  (a0)+,d2
    beq.b   .next
    cmp.b   #1,d2
    ; draw small dot
    bne.b   .pen
    move.b  #%10101010,(NB_BYTES_PER_LINE*1,a1)
    move.b  #%10101010,(NB_BYTES_PER_LINE*3,a1)
    move.b  #%10101010,(NB_BYTES_PER_LINE*5,a1)
    move.b  #%10101010,(NB_BYTES_PER_LINE*7,a1)
    bra.b   .next
.pen
    move.b  #%11111111,(NB_BYTES_PER_LINE*1,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*3,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*5,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*7,a1)

.next
    addq.l  #1,a1
    dbf d1,.loopx
    add.l  #NB_BYTES_PER_LINE-NB_TILES_PER_LINE,a1
    add.l   #NB_BYTES_PER_LINE*7,a1
    dbf d0,.loopy
    rts
draw_dots:
    ; draw pen gate
    lea	screen_data+(RED_YSTART_POS-15)*NB_BYTES_PER_LINE+(RED_XSTART_POS-X_START)/8-1,a1
    moveq.l #-1,d0
    move.w  d0,(a1)
    move.w  d0,(NB_BYTES_PER_LINE,a1)
    add.l   #SCREEN_PLANE_SIZE*3,a1
    move.w  d0,(a1)
    move.w  d0,(NB_BYTES_PER_LINE,a1)


    ; init dots
    lea     powerdots(pc),a2
    lea dot_table_read_only(pc),a0
    lea dot_table,a1
    move.l  #NB_TILE_LINES*NB_TILES_PER_LINE-1,d0
.copy
    move.b  (a0)+,(a1)+
    dbf d0,.copy

    ; start with an offset (skip the fake 3 first rows
    lea dot_table+(Y_START/8)*NB_TILES_PER_LINE,a0
    
    lea	screen_data+DOT_PLANE_OFFSET,a1
    
    move.w  #NB_TILE_LINES-1-(Y_START/8),d0    
.loopy
    move.w  #NB_TILES_PER_LINE-1,d1
.loopx
    move.b  (a0)+,d2
    beq.b   .next
    cmp.b   #1,d2
    ; draw small dot
    bne.b   .big
    move.b  #%0011000,(NB_BYTES_PER_LINE*3,a1)
    move.b  #%0011000,(NB_BYTES_PER_LINE*4,a1)
    bra.b   .next
.big
    move.l  a1,(a2)+        ; store powerdot address
    bsr draw_power_dot

.next
    addq.l  #1,a1
    dbf d1,.loopx
    add.l  #NB_BYTES_PER_LINE-NB_TILES_PER_LINE,a1
    add.l   #NB_BYTES_PER_LINE*7,a1
    dbf d0,.loopy
    rts

; < A1 address
draw_power_dot
    move.b  #%00111100,(a1)
    move.b  #%01111110,(NB_BYTES_PER_LINE*1,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*2,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*3,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*4,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*5,a1)
    move.b  #%01111110,(NB_BYTES_PER_LINE*6,a1)
    move.b  #%00111100,(NB_BYTES_PER_LINE*7,a1)
    rts
    
; < A1 address
clear_power_dot
    clr.b  (a1)
    clr.b  (NB_BYTES_PER_LINE*1,a1)
    clr.b  (NB_BYTES_PER_LINE*2,a1)
    clr.b  (NB_BYTES_PER_LINE*3,a1)
    clr.b  (NB_BYTES_PER_LINE*4,a1)
    clr.b  (NB_BYTES_PER_LINE*5,a1)
    clr.b  (NB_BYTES_PER_LINE*6,a1)
    clr.b  (NB_BYTES_PER_LINE*7,a1)
    rts

; < A1 address
clear_dot
    clr.b  (NB_BYTES_PER_LINE*3,a1)
    clr.b  (NB_BYTES_PER_LINE*4,a1)
    rts
    
init_interrupts
    ; assuming VBR at 0
    sub.l   a0,a0
    lea saved_vectors(pc),a1
    move.l  ($68,a0),(a1)+
    move.l  ($6C,a0),(a1)+
    move.l  ($78,a0),(a1)+
    
    lea level3_interrupt(pc),a1
    move.l  a1,($6C,a0)
    

    ; init phx ptplayer, needs a6 as custom, a0 as vbr (which is zero)

    moveq.l #1,d0
    lea _custom,a6
    jsr _mt_install_cia
    
    move.w  (dmaconr,a5),saved_dmacon
    move.w  (intenar,a5),saved_intena
    
    rts
    
restore_interrupts:
    ; assuming VBR at 0
    sub.l   a0,a0
    
    lea saved_vectors(pc),a1
    move.l  (a1)+,($68,a0)
    move.l  (a1)+,($6C,a0)
    move.l  (a1)+,($78,a0)

    move.w  saved_dmacon,d0
    bset    #15,d0
    move.w  d0,(dmacon,a5)
    move.w  saved_intena,d0
    bset    #15,d0
    move.w  d0,(intena,a5)

    ; this crashes
    ;lea _custom,a6
    ;jsr _mt_remove_cia

    rts
    
saved_vectors
        dc.l    0   ; keyboard
        dc.l    0   ; vblank
        dc.l    0   ; cia b
saved_dmacon
    dc.w    0
saved_intena
    dc.w    0

; what: level 3 interrupt (vblank/copper)
; args: none
; trashes: none
    
level3_interrupt:
    movem.l d0-a6,-(a7)
    lea  _custom,a5
    move.w  (intreqr,a5),d0
    btst    #5,d0
    bne.b   .vblank
    move.w  (intreqr,a5),d0
    btst    #4,d0
    beq.b   .blitter
    ; copper
    bsr draw_all
    ;bsr draw_debug
    bsr update_all
    move.w  vbl_counter,d0
    addq.w  #1,d0
    cmp.w   #5,d0
    bne.b   .normal
    ; update a second time, simulate 60Hz
    bsr update_all
    moveq.w #0,d0    
.normal
    move.w  d0,vbl_counter
    move.w  #$0010,(intreq,a5) 
    movem.l (a7)+,d0-a6
    rte    
.vblank
    moveq.l #1,d0
    bsr _read_joystick
    move.l  d0,joystick_state
    move.w  #$0020,(intreq,a5)
    movem.l (a7)+,d0-a6
    rte
.blitter
    move.w  #$0040,(intreq,a5) 
    movem.l (a7)+,d0-a6
    rte

vbl_counter:
    dc.w    0
mouse:	
	;move.w	#$0F0,$DFF180
	BTST #6,$BFE001
	BNE  mouse
	rts

; what: updates game state
; args: none
; trashes: potentially all registers

update_all
    DEF_STATE_CASE_TABLE

.life_lost
    rts  ; bra update_power_dot_flashing

.level_completed
    subq.w  #1,maze_blink_timer
    bne.b   .no_change
    move.w  #MAZE_BLINK_TIME,maze_blink_timer
    subq.b  #1,maze_blink_nb_times
    beq.b   .next_level
    eor.b   #1,.color_blue
    beq.b   .orig
    move.w  maze_color,d0
    bra.b   .chcol
.orig
    move.w  #$FFF,d0
.chcol
    move.w  d0,_custom+color+2
.no_change
    rts
.color_blue
    dc.w    0
    
.next_level
     move.w  #STATE_NEXT_LEVEL,current_state
     move.w #50,ready_timer
     rts
     
.game_over
    rts
.playing
    tst.w   ready_timer
    beq.b   .ready_off
    subq.w  #1,ready_timer
    rts
.ready_off
    bsr update_power_dot_flashing
    move.w  ghost_eaten_timer(pc),d6
    bmi.b   .update_pac_and_ghosts
    subq.w  #1,d6
    move.w  d6,ghost_eaten_timer
    rts
.update_pac_and_ghosts
    bsr update_pac
    bsr update_ghosts
    bsr check_pac_ghosts_collisions
    rts
remove_bonus
    move.b  #0,dot_table+BONUS_OFFSET
    move.w  #1,bonus_clear_message      ; tell draw routine to clear
    clr.w   bonus_timer
    rts
remove_bonus_score
    move.w  #1,bonus_score_clear_message      ; tell draw routine to clear
    clr.w   bonus_score_timer
    rts
update_power_dot_flashing
    ; power dot blink timer
    subq.w  #1,power_dot_timer
    bpl.b   .no_reload
    move.w  #BLINK_RATE,power_dot_timer
.no_reload
    rts
    
; is done after both pacman & ghosts have been updated, maybe allowing for the
; "pass-through" bug at higher levels
check_pac_ghosts_collisions
    tst.w   player_killed_timer
    bmi.b   .check
    rts
.check
    lea player(pc),a3
    move.w  xpos(a3),d0
    move.w  ypos(a3),d1
    lsr.w   #3,d0
    lsr.w   #3,d1
    
    lea ghosts(pc),a4
    moveq.w #3,d7
.gloop
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3
    lsr.w   #3,d2
    lsr.w   #3,d3
    cmp.w   d2,d0
    bne.b   .nomatch
    cmp.w   d3,d1
    beq.b   .collision
.nomatch
    add.w   #Ghost_SIZEOF,a4
    dbf d7,.gloop
    rts
.collision
    ; is the ghost frightened?
    move.w  mode(a4),d0
    cmp.w   #MODE_FRIGHT,d0
    beq.b   .pac_eats_ghost
    cmp.w   #MODE_EYES,d0
    beq.b   .nomatch        ; ignore eyes
    ; pacman is killed
    move.w  #PLAYER_KILL_TIMER,player_killed_timer
    rts
    
.pac_eats_ghost:
    move.w  #MODE_EYES,mode(a4)
    ; test display score with the proper color (reusing pink sprite palette)
    move.w  #GHOST_KILL_TIMER,ghost_eaten_timer

    move.w  next_ghost_score(pc),d0
    add.w   #1,next_ghost_score
    add.w   d0,d0
    add.w   d0,d0
    lea  score_value_table(pc),a0
    move.l   (a0,d0.w),d1
    add.l  d1,score
    lea  score_frame_table(pc),a0
    move.l  (a0,d0.w),d1
    move.l  d1,score_frame
    
    ; exits as soon as a collision is found
    rts

update_ghosts:
    lea ghosts(pc),a4
    moveq.w #3,d7
    move.w  player_killed_timer(pc),d6
    bmi.b   .gloop
    subq.w  #1,player_killed_timer
    bne.b   .glkill
    ; end current life & restart
    move.w  #STATE_LIFE_LOST,current_state
    rts
.glkill
    ; player killed, just update ghost animations but don't move
    move.w  frame(a4),d1
    addq.w  #1,d1
    and.w   #$F,d1
    move.w  d1,frame(a4)
    add.w   #Ghost_SIZEOF,a4
    dbf d7,.glkill
    rts
    
.gloop
    move.w  d7,-(a7)
    ; decrease mode timer
    
    cmp.w   #MODE_EYES,mode(a4)
    beq.b   .eyes
    move.w  mode_timer(a4),d0
    subq.w  #1,d0
    beq.b   .new_mode
    move.w  d0,mode_timer(a4)
.mode_done
    move.l  a4,a0
    bsr get_ghost_move_speed
    subq.w   #1,d0          ; sub for dbf
    bmi.b   .ghost_done     ; 0: no move for now
    ; now ghost can move once or twice
    bra.b   .move_loop
.eyes
    moveq.w #1,d0   ;constant speed, super fast
.move_loop
    move.w  d0,-(a7)
    move.w  frame(a4),d1
    addq.w  #1,d1
    and.w   #$F,d1
    move.w  d1,frame(a4)

    ; check if ghost is in the pen
    move.w  xpos(a4),d0
    move.w  ypos(a4),d1
    move.w  d0,d2
    move.w  d1,d3

    cmp.w   #MODE_EYES,mode(a4)
    beq.b   .no_pen
    
    ; check if ghost is in the pen, part 1 quickcheck for the pen exit zone
    ; (which doesn't trigger the pen tile test when exiting)
    cmp.w   #RED_XSTART_POS,d0
    bne.b   .test_pen_tile

    cmp.w   #RED_YSTART_POS,d1
    bcs.b   .no_pen     ; above red start: not in pen
    bne.b   .in_pen_test_2
    ; check if was in the pen by checking direction
    cmp.w   #UP,direction(a4)
    bne.b   .no_pen     ; was just passing by
    ; now just exited the pen: go left (unless some reverse flag is set)
    move.w  #LEFT,direction(a4)
    move.w  #-1,h_speed(a4)
    move.w  #0,v_speed(a4)
    bra.b   .no_pen
.in_pen_test_2
    ; below red start, see if currenty-48 is above red start
    sub.w   #16*3,d1
    cmp.w   #RED_YSTART_POS,d1
    bcs.b   .in_pen     ; in the middle part of pen or exiting it
    ; check inside pen
    move.w  d3,d1
.test_pen_tile
    ; check if ghost is in the pen, part 2
    bsr collides_with_maze
    cmp.b   #P,d0
    bne.b   .no_pen
.in_pen
    ; see if number of dots is 0 in which case exit
    bsr .can_exit_pen
    tst d0
    bne.b   .align_sequence
    
    ; just moves up and down if not in exit sequence
    move.w  direction(a4),d6
    cmp.w   #UP,d6
    beq.b   .pen_up
    cmp.w   #DOWN,d6
    beq.b   .pen_down
    bra.b   .next_ghost
.pen_down    
    cmp.w   #OTHERS_YSTART_POS+5,d3
    bne.b   .move_pen_down
    ; toggle direction
    move.w  #UP,direction(a4)
    sub.w   #1,ypos(a4)
    bra.b   .next_ghost
.move_pen_down:
    add.w   #1,ypos(a4)
    bra.b   .next_ghost
.pen_up
    cmp.w   #OTHERS_YSTART_POS-6,d3
    bne.b   .move_pen_up
    move.w  #DOWN,direction(a4)
    bra.b   .next_ghost
.move_pen_up:
    sub.w   #1,ypos(a4)
    bra.b   .next_ghost
    
.align_sequence
    cmp.w   #RED_XSTART_POS,xpos(a4)
    beq.b   .pen_x_aligned

    ; is it y-aligned
    cmp.w   #OTHERS_YSTART_POS,ypos(a4)
    beq.b   .pen_y_aligned
    bcc.b   .pen_align_from_down
    move.w  #DOWN,direction(a4)
    addq.w  #1,ypos(a4)
    bra.b   .next_ghost
.pen_align_from_down
    subq.w  #1,ypos(a4)
    bra.b   .next_ghost
.pen_y_aligned
    cmp.w   #RED_XSTART_POS,xpos(a4)
    bcc.b   .pen_left
    ; center from right
    move.w  #RIGHT,direction(a4)
    add.w   #1,xpos(a4)
    bra.b   .next_ghost
.pen_left
    ; center from left
    sub.w   #1,xpos(a4)
    move.w  #LEFT,direction(a4)
    bra.b   .next_ghost
.pen_x_aligned
    move.w  #UP,direction(a4)
    sub.w   #1,ypos(a4)
    bra.b   .next_ghost

.no_pen
    move.w  d2,d0
    move.w  d3,d1
    lsr.w   #3,d2       ; 8 divide, now this is the tile
    lsr.w   #3,d3   ; 8 divide

    add.w  v_speed(a4),d1   
    add.w  h_speed(a4),d0
    bpl.b   .positive
    ; handle tunnel warp for ghosts
    move.w  #X_MAX,d0
    bra.b   .setx
.positive
    cmp.w   #X_MAX,d0
    bcs.b   .setx
    clr.w   d0   ; warp to left
.setx 
    move.w  d0,d4
    move.w  d1,d5
    lsr.w   #3,d4       ; 8 divide, now this is the tile
    lsr.w   #3,d5   ; 8 divide
   
    move.w  d0,xpos(a4)
    move.w  d1,ypos(a4)
    
    cmp.w   d2,d4
    bne.b   .tile_change
    cmp.w   d3,d5
    bne.b   .tile_change
.tile_change_done
    bsr.b   .set_speed_vector
.next_ghost
    move.w  (a7)+,d0
    dbf d0,.move_loop
.ghost_done
    move.w  (a7)+,d7
    add.l   #Ghost_SIZEOF,a4
    dbf d7,.gloop
    rts

; < A4: ghost struture    
.can_exit_pen
    tst.w   pen_nb_dots(a4)
    beq.b   .exit_pen_ok
.no_exit_pen    
    clr.l   d0
    rts
.exit_pen_ok
    moveq #1,d0
    rts
    
.tile_change

    cmp.w   #MODE_EYES,mode(a4)
    bne.b   .no_eyes
    ; has reached target?
    cmp.w   pen_tile_xy(pc),d4
    bne.b   .nothing
    cmp.w   pen_tile_xy+2(pc),d5
    bne.b   .nothing
    ; reached target tile: respawn
    move.l  previous_mode(a4),mode(a4)  ; hack also restores mode
    move.l  a4,a0
    bsr     set_normal_ghost_palette
.nothing
    bra.b   .no_reverse
.no_eyes
    ; now the tricky bit: decide where to go
    ; first check for "reverse flag" signal
    tst.b   reverse_flag(a4)
    beq.b   .no_reverse
    clr.b   reverse_flag(a4)    ; ack
    neg.w  h_speed(a4)
    neg.w  v_speed(a4)
    bsr    .set_direction_from_speed
    bra.b   .next_ghost
.no_reverse
    ; direction NOT changed, check objective if not fright mode
    bsr .compute_possible_directions
    move.w  d0,d3           ; save possible directions in D3
    move.w  mode(a4),d0
    cmp.w   #MODE_FRIGHT,d0
    bne.b   .no_fright
    bsr random
    and.w   #3,d0   ; 4 directions to pick from

    lea     .direction_clockwise_table(pc),a0
    add.w   d0,d0
    add.w   d0,a0
    moveq.w #3,d0
    moveq.l #0,d1
.try_dir
    move.b  (a0),d1
    btst    d1,d3
    bne.b   .free_direction
    addq.l  #2,a0
    dbf     d0,.try_dir
    illegal     ; should not have 4 directions blocked, not possible
.free_direction
    moveq   #0,d0
    move.b  (1,a0),d0      ; translate to actual direction enumerate
    move.w  d0,direction(a4)
    bra.b   .tile_change_done
.no_fright:
    move.l  a4,a0
    bsr update_ghost_target
    ; optimization: where there's only one possible direction, skip
    lea no_direction_choice_table(pc),a0
    move.b   (a0,d3.w),d4
    bpl.b   .one_direction_only
;    cmp.w   #MODE_CHASE,mode(a4)
;    bne .noch
;    blitz
;    nop
;.noch
    ; ghost can chose several paths/routes
    ; try to select the best direction to reach the target
    move.w  d3,d0       ; get possible directions back
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3
    lsr.w   #3,d2
    lsr.w   #3,d3       ; current tile
    movem.w d2-d3,-(a7)
    ; now it's time to check the possible tiles
    ; and their distance to target
    ; priority: up, left, down, right
    moveq.l #-1,d7      ; max distance
    btst    #DIRB_UP,d0
    beq.b   .no_test_up
    move.w  target_xtile(a4),d4
    move.w  target_ytile(a4),d5
    subq.w  #1,d3   ; tile up
    bsr compute_square_distance
    move.l   d6,d7              ; store min distance (only one distance computed)
    move.w  #UP,direction(a4)
.no_test_up
    btst    #DIRB_LEFT,d0
    beq.b   .no_test_left
    move.w  target_xtile(a4),d4
    move.w  target_ytile(a4),d5
    movem.w (a7),d2-d3
    subq.w  #1,d2   ; tile left
    bsr compute_square_distance
    cmp.l   d6,d7
    bcs.b   .no_test_left
    move.w  #LEFT,direction(a4)
    move.l  d6,d7
.no_test_left
    btst    #DIRB_DOWN,d0
    beq.b   .no_test_down
    move.w  target_xtile(a4),d4
    move.w  target_ytile(a4),d5
    movem.w (a7),d2-d3
    addq.w  #1,d3   ; tile down
    bsr compute_square_distance
    cmp.l   d6,d7
    bcs.b   .no_test_down
    move.w  #DOWN,direction(a4)
    move.l  d6,d7
.no_test_down
    btst    #DIRB_RIGHT,d0
    beq.b   .no_test_right
    move.w  target_xtile(a4),d4
    move.w  target_ytile(a4),d5
    movem.w (a7),d2-d3
    addq.w  #1,d2   ; tile right
    bsr compute_square_distance
    cmp.l   d6,d7
    bcs.b   .no_test_right
    move.w  #RIGHT,direction(a4)
.no_test_right
    addq.l  #4,a7
    bra.b   .tile_change_done
    
.one_direction_only
    ext.w   d4
    move.w  d4,direction(a4)
    bra.b   .tile_change_done
    
; 2 times the clockwise cycle so we can pick
; a clockwise sequence randomly by picking a 0-3 random number
.direction_clockwise_table
    REPT    2
    dc.b    DIRB_RIGHT,RIGHT
    dc.b    DIRB_DOWN,DOWN
    dc.b    DIRB_LEFT,LEFT
    dc.b    DIRB_UP,UP
    ENDR
   
.is_in_pen
    dc.b    0

        even
    
.set_speed_vector
    lea grid_align_table(pc),a2
    
    move.w  xpos(a4),d1
    ; set speed vector when aligned with grid
    ; direction is already set properly, no need to check walls again
    ; (ghost faces the direction it's going to take)
    ; are we x-aligned?
    move.w  d1,d2
    add.w   d1,d1
    move.w  (a2,d1.w),d0
    cmp.w   d0,d2
    bne.b   .no_dirchange
.no_vmove    
    move.w  ypos(a4),d1
    ; are we y-aligned?
    move.w  d1,d2
    add.w   d1,d1
    move.w  (a2,d1.w),d0
    cmp.w   d0,d2
    bne.b   .no_dirchange   ; not aligned: don't change direction yet
.change_direction
    lea direction_speed_table(pc),a2
    move.w  direction(a4),d0
    move.l  (a2,d0.w),h_speed(a4)   ; change h & v speed from set direction
.no_dirchange
    rts

; what: compute possible directions
; elimiating the direction we're coming from
; < A4: ghost structure
; > D0: possible direction mask, DIR_RIGHT|DIR_LEFT...
; trashes: D1-D4
.compute_possible_directions
    moveq.l #0,d4
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3
    tst.w   h_speed(a4)
    bmi.b   .skip_right
    ; h speed is 0 or > 0
    ; test tile to the right
    move.w  d2,d0
    move.w  d3,d1
    addq.w  #8,d0
; W = 4   ; wall
; P = 3   ; pen space (pac block)
; T = 2   ; tunnel
; B = 1   ; ghost block
; O = 0   ; empty
    bsr collides_with_maze
    tst.b d0    ; 'O'
    beq.b   .can_move_right
    cmp.b #W,d0    ; wall, frequent, optim
    beq.b   .skip_right
    cmp.b #B,d0    ; ghost up block
    beq.b   .can_move_right
    cmp.b #T,d0    ; pen
    beq.b   .can_move_right
    ; now if down, can move but only if already
    ; inside the pen (cannot enter the pen once out unless in
    ; "eyes" mode
    bsr.b   .pen_tile_check
    bne.b   .skip_right
.can_move_right
    or.w    #DIRF_RIGHT,d4
.skip_right
    tst.w   h_speed(a4)
    beq.b   .test_left
    bpl.b   .skip_left
.test_left
    ; h speed is 0 or < 0
    ; test tile to the left
    move.w  d2,d0
    move.w  d3,d1
    subq.w  #8,d0
; W = 4   ; wall
; P = 3   ; pen space (pac block)
; T = 2   ; tunnel
; B = 1   ; ghost block
; O = 0   ; empty
    bsr collides_with_maze
    tst.b d0    ; 'O'
    beq.b   .can_move_left
    cmp.b #W,d0    ; wall, frequent, optim
    beq.b   .skip_left
    cmp.b #B,d0    ; ghost up block
    beq.b   .can_move_left
    cmp.b #T,d0    ; tunnel
    beq.b   .can_move_left
    bsr.b   .pen_tile_check
    bne.b   .skip_left
.can_move_left
    or.w    #DIRF_LEFT,d4
.skip_left
    tst.w   v_speed(a4)
    bmi.b   .skip_down
    ; v speed is 0 or > 0
    ; test bottom tile 
    move.w  d2,d0
    move.w  d3,d1
    addq.w  #8,d1
; W = 4   ; wall
; P = 3   ; pen space (pac block)
; T = 2   ; tunnel
; B = 1   ; ghost block
; O = 0   ; empty
    bsr collides_with_maze
    tst.b d0    ; 'O'
    beq.b   .can_move_down
    cmp.b #W,d0    ; wall, frequent, optim
    beq.b   .skip_down
    cmp.b #T,d0    ; tunnel
    beq.b   .can_move_down
    ; now if down, can move
    cmp.b #B,d0    ; ghost up block
    beq.b   .can_move_down
    bsr.b   .pen_tile_check
    bne.b   .skip_down
.can_move_down
    or.w    #DIRF_DOWN,d4
.skip_down
    tst.w   v_speed(a4)
    beq.b   .test_up
    bpl.b   .test_done
.test_up
    ; v speed is 0 or > 0
    ; test bottom tile 
    move.w  d2,d0
    move.w  d3,d1
    subq.w  #8,d1
; W = 4   ; wall
; P = 3   ; pen space (pac block)
; T = 2   ; tunnel
; B = 1   ; ghost block
; O = 0   ; empty
    bsr collides_with_maze
    tst.b d0    ; 'O'
    beq.b   .can_move_up
    cmp.b #W,d0    ; wall, frequent, optim
    beq.b   .test_done
    cmp.b #T,d0    ; tunnel
    beq.b   .can_move_up
    ; now if up, can move, only if fright mode
    move.w  mode(a4),d2
    cmp.w   #MODE_FRIGHT,d2
    beq.b   .skip_block_test
    cmp.b #B,d0    ; ghost up block
    bne.b   .test_done
.skip_block_test
    bsr.b   .pen_tile_check
    bne.b   .test_done
.can_move_up    
    or.w    #DIRF_UP,d4
.test_done
    move.l  d4,d0
    rts

; what: check if pen tile is valid
; - when already in the pen, can move inside it
; - when in "eyes" mode
; > D0,D1 (x,y),A4 (ghost struct)
; < Z=0 if not possible to walk on a pen tile
.pen_tile_check
    cmp.w   #MODE_EYES,mode(a4)
    beq.b   .no_pen_tile
    cmp.b   #P,d0
    bne.b   .no_pen_tile
    ; pen tile: are we in the pen already
    move.w  d2,d0
    move.w  d3,d1
; W = 4   ; wall
; P = 3   ; pen space (pac block)
; T = 2   ; tunnel
; B = 1   ; ghost block
; O = 0   ; empty
    bsr collides_with_maze
    cmp.b   #P,d0    
.no_pen_tile
    rts
    
; what: change ghost direction according speed
; there can only be vertical OR horizontal speed
; < A4: pointer on ghost structure
; trashes: nothing
.set_direction_from_speed
    tst.w   h_speed(a4)
    bmi.b   .left
    beq.b   .vertical
    move.w  #RIGHT,direction(a4)
    rts
.left
    move.w  #LEFT,direction(a4)
    rts
.vertical    
    tst.w   h_speed(a4)
    bmi.b   .up
    move.w  #DOWN,direction(a4)
    rts
.up
    move.w  #UP,direction(a4)
    rts
    
.new_mode
    move.w  mode(a4),d0 ; old mode
    cmp.w   #MODE_FRIGHT,d0
    beq.b   .from_fright
    
    move.w   mode_counter(a4),d1
    cmp.w   #6,d1
    beq.b   .maxed


    cmp.w   #MODE_SCATTER,d0
    beq.b   .chase
    cmp.w   #MODE_CHASE,d0
    beq.b   .scatter

.chase
    move.w  #MODE_CHASE,D0
    
.switch_mode
    ; change mode
    move.w  d0,mode(a4)
    ; next timer in table   
    add.w   #1,d1
    move.w  d1,mode_counter(a4)

    ; reload new mode timer
    move.l  a4,a0
    bsr update_ghost_mode_timer
    move.b  #1,reverse_flag(a4)
    bra.b   .mode_done
.scatter
    move.l  a4,a0
    bsr is_elroy
    tst.w   d0
    beq.b   .ok_scatter
    move.w  #MODE_CHASE,d0
    bra.b   .switch_mode
.ok_scatter:
    move.w  #MODE_SCATTER,D0
    bra.b   .switch_mode
.maxed
    ; end of sequence: chase mode all the time
    move.w  #MODE_CHASE,mode(a4)
    ; just to avoid timer overflow but could be ignored
    bsr update_ghost_mode_timer
    bra.b   .mode_done
    
.from_fright
    ; first, restore normal color palette (frames are restored by the animation)
    lea  palette(a4),a0
    move.l  color_register(a4),a1
    move.l  (a0)+,(a1)+
    move.l  (a0)+,(a1)+
    ; then resume sequence & target
    move.l  previous_mode_timer(a4),mode_timer(a4)  ; hack also restores mode
    move.l  a4,a0
    bsr update_ghost_target
    bra.b   .mode_done

; what: computes square distance using square
; tables (faster than multiplying)
; < D2: XT1
; < D3: YT1
; < D4: XT2
; < D5: YT2
; > D6: square distance
; trashes: A0, D4, D5
; the register trashing looks ugly but actually this is done
; quite frequently when ghosts choose directions so it's adapted to that

compute_square_distance
    lea  square_table(pc),a0
    moveq.l   #0,d6
    sub.w   d2,d4
    bpl.b   .pos1
    neg.w   d4
.pos1
    add.w   d4,d4
    move.w  (a0,d4.w),d6    ; still < 65536
    sub.w   D3,D5
    bpl.b   .pos2
    neg.w   d5
.pos2
    add.w  D5,D5
    move.w  (a0,d5.w),d5
    swap    d5
    clr.w   d5
    swap    d5
    add.l   d5,d6   ; may be > 65536
    rts
    
; what: sets game state when a power pill has been taken
; trashes: A0,A1,D0,D1
power_pill_taken
    move.l  d2,-(a7)
    ; resets next ghost eaten score
    clr.w  next_ghost_score
    lea ghosts(pc),a0
    moveq.w  #3,d0
.gloop
    cmp.w  #MODE_EYES,mode(a0)      ; don't fright the eyes
    beq.b   .next
    
    move.l  mode_timer(a0),previous_mode_timer(a0)  ; hack also saves mode
    move.w  #MODE_FRIGHT,mode(a0)
    ; set proper fright mode according to current level
    move.w  #NB_FLASH_FRAMES-1,flash_toggle_timer(a0)
    move.w  level_number(pc),d1
    cmp.w   #18,d1
    bcc.b   .no_time
    add.w   d1,d1
    add.w   d1,d1
    lea     fright_table(pc),a1
    move.w  (a1)+,d2
    move.w  d2,mode_timer(a0)
    move.w  (a1)+,flash_timer(a0)
    clr.b   flashing_as_white(a0)
    move.b  #1,reverse_flag(a0)
    bra.b   .next
.no_time
    clr.w  mode_timer(a0)
    clr.w  flash_timer(a0)
    clr.w   flash_toggle_timer(a0)
.next
    add.l   #Ghost_SIZEOF,a0
    dbf d0,.gloop
    
    ; also store global fright timer for pacman speed
    move.w  d2,fright_timer
    move.l (a7)+,d2
    rts
    
update_pac
    move.w  player_killed_timer(pc),d6
    bmi.b   .alive
    moveq.w #0,d0
    move.w  #PLAYER_KILL_TIMER-NB_TICKS_PER_SEC,d5
    sub.w   d6,d5
    bcs.b   .frame_done     ; frame 0
    ; d5 is the timer starting from 0
    lea player_kill_anim_table(pc),a0
    move.b  (a0,d5.w),d0
.frame_done
    lsl.w   #6,d0   ; times 64
    move.w  d0,death_frame_offset
    rts
.alive
    tst.w   fright_timer
    beq.b   .no_fright1
    sub.w   #1,fright_timer
.no_fright1
    tst.w   bonus_timer
    beq.b   .no_fruit
    subq.w  #1,bonus_timer
    bne.b   .no_fruit
    ; timeout: make fruit disappear, no score
    bsr remove_bonus
.no_fruit 
    tst.w   bonus_score_timer    
    beq.b   .no_fruit_score
    subq.w  #1,bonus_score_timer
    bne.b   .no_fruit_score
    ; timeout: make fruit score disappear
    bsr remove_bonus_score
.no_fruit_score

    ; player
    lea player(pc),a4
    tst.b   still_timer(a4)
    beq.b   .okmove
    subq.b  #1,still_timer(a4)
.skip_move
    ; return without doing nothing!!
    rts
    
.okmove    
    move.w  speed_table_index(a4),d1
    add.w   #1,d1
    cmp.w   #16,d1
    bne.b   .nowrap
    moveq   #0,d1
.nowrap
    ; store
    move.w  d1,speed_table_index(a4)
    move.l  global_speed_table(pc),a1
    ; faster when at least one ghost is in fright mode
    tst.w   fright_timer
    beq.b   .no_fright
    add.w   #32,d1      ; pacman is faster when ghosts are frightened
.no_fright
    move.b  (a1,d1.w),d0    ; speed
    beq.b   .skip_move      ; if zero skip move
    ext.w   d0
    ; store current speed
    move.w  d0,player_speed
    
    ; pre turn timer
    tst.w  prepost_turn(a4)
    beq.b   .ptzero
    subq.w  #1,prepost_turn(a4)
.ptzero
    move.l  joystick_state(pc),d0

    btst    #JPB_BTN_RIGHT,d0
    beq.b   .no_right
    move.w  player_speed(pc),h_speed(a4)
    bra.b   .vertical
.no_right
    btst    #JPB_BTN_LEFT,d0
    beq.b   .vertical
    move.w  player_speed(pc),d1
    neg.w   d1
    move.w  d1,h_speed(a4)  
.vertical
    btst    #JPB_BTN_UP,d0
    beq.b   .no_up
    move.w  player_speed(pc),d1
    neg.w   d1
    move.w  d1,v_speed(a4)
    bra.b   .out
.no_up
    btst    #JPB_BTN_DOWN,d0
    beq.b   .no_down
    move.w  player_speed(pc),v_speed(a4)
.no_down    
.out
    ; cache xy in regs / save them
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3

    move.w  direction(a4),d6

    cmp.w   #UP,d6
    bcc.b   .horiz_first
    ; priority to vertical move (direction change)
    ; if pre/post turn in progress don't try to turn
    tst.w   prepost_turn(a4)
    bne.b .novtest1

    bsr.b .vtest    
.novtest1
    bsr.b .htest
    bra.b   .pills
.horiz_first
    ; priority to horizontal move
    tst.w   prepost_turn(a4)
    bne.b .nohtest1
    bsr .htest
.nohtest1    
    bsr.b .vtest
.pills
    move.w  xpos(a4),d0
    move.w  ypos(a4),d1
    bsr is_on_bonus
    move.b   d0,d2
    beq.b   .end_pac

    lea	screen_data+DOT_PLANE_OFFSET,a1
    move.w  xpos(a4),d0
    move.w  ypos(a4),d1
    ; are we y-aligned?
    and.w   #$1F8,d1
    sub.w   #Y_START,d1     ; phantom 3 rows of tiles at start
    ADD_XY_TO_A1    a0

    cmp.b   #1,d2
    beq.b   .simple
    cmp.b   #2,d2
    beq.b   .power
    ; bonus (fruit)
    bra.b   .score
.power
    ; save A1
    move.l  A1,A2
    bsr power_pill_taken
    move.l  A2,A1
    
    move.b  #3,still_timer(a4)      ; still during 3 frames
    ; linear search find the relevant powerdot
    lea  powerdots(pc),a0
    moveq.l #3,d0
.clrpdloop
    move.l  (a0)+,d1
    beq.b   .zap_clear
    cmp.l  d1,a1
    bne.b   .clrpdloop
    bsr clear_power_dot
    clr.l   (-4,a0)
    bra.b   .score  ; found
.zap_clear
    dbf d0,.clrpdloop
    ; should not happen!!
    bra.b   .score
.simple
    move.b  #1,still_timer(a4)      ; still during 1 frame
    bsr clear_dot
.score
    and.w   #$FF,d2
    cmp.w   #3,d2
    bcc.b   .bonus_eaten
    ; dot
    move.l  ghost_which_counts_dots(pc),a3
.retry
    cmp.l   #ghosts+4*Ghost_SIZEOF,a3
    beq.b   .no_need_to_count_dots
    tst.w   pen_nb_dots(a3)
    bne.b   .do_ghost_count
    add.l   #Ghost_SIZEOF,a3
    move.l  a3,ghost_which_counts_dots
    bra.b   .retry
.do_ghost_count
    sub.w   #1,pen_nb_dots(a3)
.no_need_to_count_dots
    move.b  nb_dots_eaten,d4
    addq.b  #1,d4
    tst.w   bonus_timer
    bne.b   .skip_fruit_test

    cmp.b   #70,d4
    beq.b   .show_fruit
    cmp.b   #170,d4
    beq.b   .show_fruit
.skip_fruit_test
    cmp.b   #TOTAL_NUMBER_OF_DOTS,d4
    bne.b   .other
    ; no more dots: win
    move.w  #STATE_LEVEL_COMPLETED,current_state
.other
    move.b  d4,nb_dots_eaten
    lea score_table(pc),a0
    add.w   d2,d2
    move.w  (a0,d2.w),d0
    ext.l   d0
    add.l   d0,score
.end_pac
    rts
.bonus_eaten:
    bsr remove_bonus
    ; show score
    move.w  #BONUS_SCORE_TIMER_VALUE,bonus_score_timer
    bra.b   .other

.show_fruit
    move.b  #3,dot_table+BONUS_OFFSET
    move.w  #BONUS_TIMER_VALUE,bonus_timer
    bra.b   .other
    
.vtest
    ; vertical move
    ; re-set coords
    move.w  d2,d0
    move.w  d3,d1
    move.w  v_speed(a4),d4
    ; now check if speeds are applicable to player
    beq.b   .no_vmove
    ; are we x-aligned?
    and.w   #$F8,d0
    add.w   #4,d0

    ; are we y-aligned?
    move.w  d0,d5   ; save d1 (aligned) into d5
    sub.w   d2,d0
    addq.w  #4,d0
    ; d1 must be between 0 and 7 for pre-post turn
    bmi.b   .no_vmove
    cmp.w   #8,d0
    bcc.b   .no_vmove

    move.w  d5,d0   ; restore d0
    
    tst d4
    bmi.b   .to_up
    move.w  #DOWN,d6
    add.w  #4,d1
    bra.b   .contv
.to_up
    move.w  #UP,d6
    sub.w  #5,d1
.contv
    bsr pacman_collides_with_maze
    tst.b d0
    beq.b   .can_move_vertically
    ; cancel speed, note the turn
    clr.w   v_speed(a4)
    bra.b   .no_vmove
.can_move_vertically
    move.w  d6,direction(a4)
        
    cmp.w   xpos(a4),d5
    beq.b   .ddv
    move.w  #PREPOST_TURN_LOCK,prepost_turn(a4)
.ddv
    
    add.w   d4,d3
    move.w  d5,xpos(a4)
    move.w  d3,ypos(a4)

    bsr .animate
    clr.w   h_speed(a4)
.no_vmove
    rts
    
.htest
    move.w  d2,d0
    move.w  d3,d1
    move.w  h_speed(a4),d4
    ; now check if speeds are applicable to player
    beq.b   .no_hmove
    ; are we y-aligned?
    and.w   #$1F8,d1
    add.w   #4,d1
    move.w  d1,d5   ; save d1 (aligned) into d5
    sub.w   d3,d1
    addq.w  #4,d1
    ; d1 must be between 0 and 7 for pre-post turn
    bmi.b   .no_hmove
    cmp.w   #8,d1
    bcc.b   .no_hmove

    move.w  d5,d1   ; restore d1
    tst d4
    bmi.b   .to_left
    move.w  #RIGHT,d6
    add.w  #4,d0
    bra.b   .conth
.to_left
    move.w  #LEFT,d6
    sub.w  #5,d0
.conth
    bsr pacman_collides_with_maze
    tst.b d0
    beq.b   .can_move_horizontally
    ; cancel speed
    clr.w   h_speed(a4)
    bra.b   .no_hmove
.can_move_horizontally
    ; set direction
    move.w  d6,direction(a4)
    
    ; set aligned value in y (corner cut)
    cmp.w   ypos(a4),d5
    beq.b   .dd
    move.w  #PREPOST_TURN_LOCK,prepost_turn(a4)
.dd
    ; handle tunnel
    add.w   d4,d2
    bpl.b   .positive
    ; warp to right
    move.w  #X_MAX,d2
    bra.b   .setx    
.positive
    cmp.w   #X_MAX,d2
    bcs.b   .setx
    clr.w   d2   ; warp to left
.setx
    move.w  d2,xpos(a4)
    move.w  d5,ypos(a4)
    bsr .animate
    clr.w   v_speed(a4)

.no_hmove
    rts

; called when pacman moves
.animate
    addq.w  #1,frame(a4)
    cmp.w   #(pac_anim_left_end-pac_anim_left)/4,frame(a4)
    bne.b   .no_floop
    clr.w   frame(a4)
.no_floop
    rts

        
; < d0.w: x
; < d1.w: y
; > d0.L: control word
store_sprite_pos
    movem.l  d1/a0/a1,-(a7)

    lea	HW_SpriteXTable(pc),a0
    lea	HW_SpriteYTable(pc),a1

    add.w	d0,d0
    add.w	d0,d0
    move.l	(a0,d0.w),d0
    add.w	d1,d1
    add.w	d1,d1
    or.l	(a1,d1.w),d0
    movem.l  (a7)+,d1/a0/a1
    rts

player_speed
    dc.w    1
direction_speed_table
    ; right
    dc.w    1,0
    ; left
    dc.w    -1,0
    ; up
    dc.w    0,-1
    ; down
    dc.w    0,1
    
grid_align_table
    REPT    320
    dc.w    (REPTN&$1F8)+4
    ENDR
    
HW_SpriteXTable
  rept 320
x   set REPTN+$80
    dc.b  0, x>>1, 0, x&1
  endr


HW_SpriteYTable
  rept 260
ys  set REPTN+$2c
ye  set ys+16       ; size = 16
    dc.b  ys&255, 0, ye&255, ((ys>>6)&%100) | ((ye>>7)&%10)
  endr

    
; what: checks if x,y has a dot/fruit/power pill 
; args:
; < d0 : x (screen coords)
; < d1 : y
; > d0 : nonzero (1,2,3) if collision (dot,power pill,fruit), 0 if no collision
; trashes: a0,a1,d1

is_on_bonus:
    lea dot_table,a0
    ; apply x,y offset
    lsr.w   #3,d1       ; 8 divide
    lsl.w   #5,d1       ; times 32
    add.w   d1,a0
    lsr.w   #3,d0   ; 8 divide
    add.w   d0,a0
    move.b  (a0),d0
    bne.b   .pill
    rts
.pill
    ; only once!
    clr.b   (a0)
    rts

; what: checks if x,y collides with maze 
; args:
; < d0 : x (screen coords)
; < d1 : y
; > d0 : nonzero if collision (wall/pen gate), 0 if no collision
; trashes: a0,a1,d1

pacman_collides_with_maze
    bsr collides_with_maze
    cmp.b   #P,d0
    bcc.b   .wall
    moveq.l #0,d0
    rts
.wall
    moveq.l #1,d0
    rts
    

; what: checks if x,y collides with maze 
; args:
; < d0 : x (screen coords)
; < d1 : y
; > d0 : nonzero (1,2) if collision (wall/pen gate), 0 if no collision
; W = 4   ; wall
; P = 3   ; pen space (pac block)
; T = 2   ; tunnel
; B = 1   ; ghost block
; O = 0   ; empty
; trashes: a0,a1,d1

collides_with_maze:
    lea wall_table,a0
    ; apply x,y offset
    lsr.w   #3,d1       ; 8 divide
    lsl.w   #5,d1       ; times 32
    add.w   d1,a0
    lsr.w   #3,d0   ; 8 divide
    move.b  (a0,d0.w),d0
    rts

   

; what: blits 16x16 data on one plane
; args:
; < A0: data (16x16)
; < A1: plane
; < D0: X
; < D1: Y
; < D2: blit mask
; trashes: D0-D1
; returns: A1 as start of destination (A1 = orig A1+40*D1+D0/8)

blit_plane
    movem.l d2-d4/a2-a5,-(a7)
    move.l  d2,d3
    move.w  #4,d2       ; 16 pixels + 2 shift bytes
    bsr blit_plane_any_internal
    movem.l (a7)+,d2-d4/a2-a5
    rts
    
; what: blits (any width)x16 data on one plane
; args:
; < A0: data (widthx16)
; < A1: plane
; < D0: X
; < D1: Y
; < D2: blit width in bytes (+2)
; < D3: blit mask
; trashes: D0-D1, A1
;
; if A1 is already computed with X/Y offset and no shifting, an optimization
; skips the XY offset computation

blit_plane_any:
    movem.l d2-d4/a2-a5,-(a7)
    bsr blit_plane_any_internal
    movem.l (a7)+,d2-d4/a2-a5
    rts

blit_plane_any_internal:

blith = 16

    lea $DFF000,A5
	move.l d3,bltafwm(a5)	;no masking of first/last word
    ; pre-compute the maximum shit here
    lea mul40_table(pc),a2
    add.w   d1,d1
    beq.b   .d1_zero    ; optim
    move.w  (a2,d1.w),d1
    swap    d1
    clr.w   d1
    swap    d1
.d1_zero
    move.l  #$09f00000,d4    ;A->D copy, ascending mode
    move    d0,d3
    beq.b   .d0_zero
    and.w   #$F,D3
    and.w   #$1F0,d0
    lsr.w   #3,d0
    add.w   d0,d1
    add.l   d1,a1       ; plane position

    swap    d3
    clr.w   d3
    lsl.l   #8,d3
    lsl.l   #4,d3
    or.l   d3,d4            ; add shift
.d0_zero    

	move.w #NB_BYTES_PER_LINE,d0
    sub.w   d2,d0       ; blit width

    move.w #blith*64,d3
    lsr.w   #1,d2
    add.w   d2,d3

    ; always the same settings (ATM)
	move.w #0,bltamod(a5)		;A modulo=bytes to skip between lines
	move.l d4,bltcon0(a5)	

    ; now just wait for blitter ready to write all registers
	bsr	wait_blit
    
    ; blitter registers set
    move.w  d0,bltdmod(a5)	;D modulo
	move.l a0,bltapt(a5)	;source graphic top left corner
	move.l a1,bltdpt(a5)	;destination top left corner
	move.w  d3,bltsize(a5)	;rectangle size, starts blit
    rts

; what: blits 16(32)x16 data on 4 planes (for bonuses)
; args:
; < A0: data (16x16)
; < A1: plane
; < D0: X
; < D1: Y
; trashes: D0-D1

blit_4_planes
    movem.l d2/d4-d5/a0-a1/a5,-(a7)
    moveq.l #3,d5
.loop
    movem.l d0-d1/a1,-(a7)
    move.w  #4,d2       ; 16 pixels + 2 shift bytes
    moveq   #-1,d3      ; full mask
    bsr blit_plane_any_internal
    movem.l (a7)+,d0-d1/a1
    add.l   #SCREEN_PLANE_SIZE,a1
    add.l   #64,a0      ; 32 but shifting!
    dbf d5,.loop
    movem.l (a7)+,d2/d4-d5/a0-a1/a5
    rts
    
wait_blit
	TST.B	$BFE001
.wait
	BTST	#6,dmaconr+$DFF000
	BNE.S	.wait
	rts

; what: writes an hexadecimal number (or BCD) in a single plane
; args:
; < A1: plane
; < D0: X (multiple of 8)
; < D1: Y
; < D2: number value
; < D3: number of padding zeroes
; > D0: number of characters written

write_hexadecimal_number

    movem.l A0/D2-d5,-(a7)
    cmp.w   #7,d3
    bcs.b   .padok
    move.w  #7,d3
.padok
    bsr     .write_num
    movem.l (a7)+,A0/D2-d5
    rts
.write_num
    lea .buf+8(pc),a0

    
.loop
    subq    #1,d3    
    move.b  d2,d5
    and.b   #$F,d5
    cmp.b   #10,d5
    bcc.b   .letter
    add.b   #'0',d5
    bra.b   .ok
.letter
    add.b   #'A'-10,d5
.ok
    move.b  d5,-(a0)
    lsr.l   #4,d2
    beq.b   .write
    bra.b   .loop
.write
    tst.b   d3
    beq.b   .w
    bmi.b   .w
    subq    #1,d3
.pad
    move.b  #' ',-(a0)
    dbf d3,.pad
.w
    bra write_string
.buf
    ds.b    8
    dc.b    0
    even
    
; what: writes an decimal number in a single plane
; args:
; < A1: plane
; < D0: X (multiple of 8)
; < D1: Y
; < D2: number value
; < D3: number of padding zeroes
; > D0: number of characters written
    
write_decimal_number
    movem.l A0/D2-d5,-(a7)
    cmp.w   #18,d3
    bcs.b   .padok
    move.w  #18,d3
.padok
    cmp.l   #655361,d2
    bcs.b   .one
    sub.l   #4,d3
    move.w  d0,d5
    ; first write high part    
    divu    #10000,d2
    swap    d2
    moveq.l #0,d4
    move.w   d2,d4
    clr.w   d2
    swap    d2
    bsr     .write_num
    lsl.w   #3,d0
    add.w   d5,d0   ; new xpos
    
    move.l  d4,d2
    moveq   #4,d3   ; pad to 4
.one
    bsr     .write_num
    movem.l (a7)+,A0/D2-d5
    rts
.write_num
    lea .buf+20(pc),a0
    tst.w   d2
    beq.b   .zero
.loop
    divu    #10,d2
    swap    d2
    add.b   #'0',d2
    subq    #1,d3
    move.b  d2,-(a0)
    clr.w   d2
    swap    d2
    tst.w   d2
    beq.b   .write
    bra.b   .loop
.zero
    subq    #1,d3
    move.b  #'0',-(a0)
.write
    tst.b   d3
    beq.b   .w
    bmi.b   .w
    subq    #1,d3
.pad
    move.b  #' ',-(a0)
    dbf d3,.pad
.w
    bra write_string
.buf
    ds.b    20
    dc.b    0
    even
    
; what: writes a text in a given color
; args:
; < A0: c string
; < D0: X (multiple of 8)
; < D1: Y
; < D2: RGB4 color (must be in palette!)
; > D0: number of characters written
; trashes: none

write_color_string
    movem.l D1-D5/A1,-(a7)
    lea game_palette(pc),a1
    moveq   #15,d3
    moveq   #0,d5
.search
    move.w  (a1)+,d4
    cmp.w   d4,d2
    beq.b   .color_found
    addq.w  #1,d5
    dbf d3,.search
    moveq   #0,d0   ; nothing written
    bra.b   .out
.color_found
    ; d5: color index
    lea screen_data,a1
    moveq   #3,d3
    move.w  d0,d4
.plane_loop
; < A0: c string
; < A1: plane
; < D0: X (multiple of 8)
; < D1: Y
; > D0: number of characters written
    btst    #0,d5
    beq.b   .skip_plane
    move.w  d4,d0
    bsr write_string
.skip_plane
    lsr.w   #1,d5
    add.l   #SCREEN_PLANE_SIZE,a1
    dbf d3,.plane_loop
.out
    movem.l (a7)+,D1-D5/A1
    rts
    
; what: writes a text in a single plane
; args:
; < A0: c string
; < A1: plane
; < D0: X (multiple of 8 else it's rounded)
; < D1: Y
; > D0: number of characters written
; trashes: none

write_string
    movem.l A0-A2/d1-D2,-(a7)
    clr.w   d2
    ADD_XY_TO_A1    a2
    moveq.l #0,d0
.loop
    move.b  (a0)+,d2
    beq.b   .end
    addq.l  #1,d0
    cmp.b   #'!',d2
    bne.b   .noexcl
    lea exclamation(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noexcl
    cmp.b   #' ',d2
    bne.b   .nospace
    lea space(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nospace
    cmp.b   #'0',d2
    bcs.b   .next
    cmp.b   #'9'+1,d2
    bcc.b   .try_letters
    ; digits
    lea digits(pc),a2
    sub.b   #'0',d2
    bra.b   .wl
    
.try_letters: 
    cmp.b   #'A',d2
    bcs.b   .next
    cmp.b   #'Z'+1,d2
    bcc.b   .next
    lea letters(pc),a2
    sub.b   #'A',d2
    bra.b   .wl
    bra.b   .next
.wl
    lsl.w   #3,d2   ; *8
    add.w   d2,a2
    move.b  (a2)+,(a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*2,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*3,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*4,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*5,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*6,a1)
    move.b  (a2)+,(NB_BYTES_PER_LINE*7,a1)
.next   
    addq.l  #1,a1
    bra.b   .loop
.end
    movem.l (a7)+,A0-A2/d1-D2
    rts
    
    include ReadJoyPad.s
    ; variables
    
previous_random
    dc.l    0
joystick_state
    dc.l    0
ready_timer
    dc.w    0
level_blink_timer
    dc.w    0
current_state
    dc.w    0
power_dot_timer:
    dc.w    BLINK_RATE
score
    dc.l    0
displayed_score
    dc.l    0
high_score
    dc.l    0
maze_color
    dc.w    0
maze_blink_timer
    dc.w    0
bonus_level_table:
    dc.w    0,1,2,2,3,3,4,4,5,5,6,6,7
bonus_level_score:  ; *10
    dc.w    10,30,50,50,70,70,100,100,200,200,300,300,500
bonus_timer
    dc.w    0
fruit_score_index:
    dc.w    0
next_ghost_score
    dc.w    0
previous_pacman_address
    dc.l    0
ghost_which_counts_dots
    dc.l    0
tunnel_sprite_control_word
    dc.l    0
score_frame
    dc.l    0
global_speed_table
    dc.l    0
; 0: level 1
level_number:
    dc.w    0
player_killed_timer:
    dc.w    -1
ghost_eaten_timer:
    dc.w    -1
pen_tile_xy:
    dc.l    0
bonus_score_timer:
    dc.w    0
fright_timer:
    dc.w    0
death_frame_offset
    dc.w    0
maze_blink_nb_times
    dc.b    0
nb_lives
    dc.b    0
nb_dots_eaten
    dc.b    0
elroy_threshold_1
    dc.b    0
elroy_threshold_2
    dc.b    0
    even

bonus_clear_message
    dc.w    0
bonus_score_clear_message
    dc.w    0
    
score_table
    dc.w    0,1,5
fruit_score     ; must follow score_table
    dc.w    10
    
    
    
player_kill_anim_table:
    REPT    NB_TICKS_PER_SEC/2
    dc.b    1
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    2
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    3
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    4
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    5
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    6
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    7
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    8
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    9
    ENDR
    REPT    NB_TICKS_PER_SEC/8
    dc.b    10
    ENDR
    REPT    NB_TICKS_PER_SEC/4
    dc.b    11
    ENDR
    REPT    NB_TICKS_PER_SEC
    dc.b    12
    ENDR
    even
    
    even

    
gfxbase
    dc.l    0
gfxbase_copperlist
    dc.l    0
GRname:   dc.b "graphics.library",0
    even

pac_dir_table
    dc.l    pac_anim_right,pac_anim_left,pac_anim_up,pac_anim_down
    
PAC_ANIM_TABLE:MACRO
pac_anim_\1
    ; original shows 1 frame each 1/30s. We can't do that here but we
    ; can shorten some frames
    dc.l    pac_dead,pac_dead,pac_\1_0,pac_\1_1,pac_\1_1,pac_\1_0
pac_anim_\1_end
    ENDM
    
    PAC_ANIM_TABLE  right
    PAC_ANIM_TABLE  left
    PAC_ANIM_TABLE  up
    PAC_ANIM_TABLE  down


    

    
    
maze_data:
    incbin  "maze.bin"

digits:
    incbin  "0.bin"
    incbin  "1.bin"
    incbin  "2.bin"
    incbin  "3.bin"
    incbin  "4.bin"
    incbin  "5.bin"
    incbin  "6.bin"
    incbin  "7.bin"
    incbin  "8.bin"
    incbin  "9.bin"
letters
    incbin	"A.bin"
    incbin	"B.bin"
    incbin	"C.bin"
    incbin	"D.bin"
    incbin	"E.bin"
    incbin	"F.bin"
    incbin	"G.bin"
    incbin	"H.bin"
    incbin	"I.bin"
    incbin	"J.bin"
    incbin	"K.bin"
    incbin	"L.bin"
    incbin	"M.bin"
    incbin	"N.bin"
    incbin	"O.bin"
    incbin	"P.bin"
    incbin	"Q.bin"
    incbin	"R.bin"
    incbin	"S.bin"
    incbin	"T.bin"
    incbin	"U.bin"
    incbin	"V.bin"
    incbin	"W.bin"
    incbin	"X.bin"
    incbin	"Y.bin"
    incbin	"Z.bin"    
exclamation
    incbin  "exclamation.bin"
space
    ds.b    8,0
    
high_score_string
    dc.b    " HIGH SCORE",0
p1_string
    dc.b    "     1UP",0
score_string
    dc.b    "       00",0
game_over_string
    dc.b    "GAME  OVER",0
ready_string
    dc.b    "READY!",0
    even

    MUL_TABLE   40

square_table:
	rept	256
	dc.w	REPTN*REPTN
	endr
   
; truth table to avoid testing for several directions where there's only once choice
; (one bit set)
no_direction_choice_table:
    dc.b    $ff   ; 0=not possible
    dc.b    RIGHT   ; 1
    dc.b    DOWN   ; 2
    dc.b    $ff   ; 3=composite
    dc.b    LEFT   ; 4=UP
    dc.b    $ff   ; 5=composite
    dc.b    $ff   ; idem
    dc.b    $ff   ; idem
    dc.b    UP   ; 8
    ; all the rest is composite or invalid
    REPT    7
    dc.b    $ff
    ENDR
    even
    
    ; fright time + number of flashes. Each flash is 14 frames long
    ; 4 words: total number of frames for fright mode,
    ;          number of frames to start flashing
DEF_FRIGHT_ENTRY:MACRO
    dc.w    NB_TICKS_PER_SEC*\1,NB_FLASH_FRAMES*\2*2
    ENDM
    
fright_table
    DEF_FRIGHT_ENTRY    6,5
    DEF_FRIGHT_ENTRY    5,5
    DEF_FRIGHT_ENTRY    4,5
    DEF_FRIGHT_ENTRY    3,5
    DEF_FRIGHT_ENTRY    2,5
    DEF_FRIGHT_ENTRY    5,5
    DEF_FRIGHT_ENTRY    2,5
    DEF_FRIGHT_ENTRY    2,5
    DEF_FRIGHT_ENTRY    1,3
    DEF_FRIGHT_ENTRY    5,5
    DEF_FRIGHT_ENTRY    2,5
    DEF_FRIGHT_ENTRY    1,3
    DEF_FRIGHT_ENTRY    1,3
    DEF_FRIGHT_ENTRY    3,5
    DEF_FRIGHT_ENTRY    1,3
    DEF_FRIGHT_ENTRY    1,3
    DEF_FRIGHT_ENTRY    0,0
    DEF_FRIGHT_ENTRY    1,3    ; level 18

timer_table:
    dc.l    level_1
    dc.l    level2_to_4
    dc.l    level2_to_4
    dc.l    level2_to_4
    dc.l    level5_and_higher
    
level_1:
    dc.w    NB_TICKS_PER_SEC*7	
    dc.w    NB_TICKS_PER_SEC*20	
    dc.w    NB_TICKS_PER_SEC*7	
    dc.w    NB_TICKS_PER_SEC*20	
    dc.w    NB_TICKS_PER_SEC*5	
    dc.w    NB_TICKS_PER_SEC*20	
    dc.w    NB_TICKS_PER_SEC*5  ; scatter	

level2_to_4:
    dc.w    NB_TICKS_PER_SEC*7	  
    dc.w    NB_TICKS_PER_SEC*20	  
    dc.w    NB_TICKS_PER_SEC*7	  
    dc.w    NB_TICKS_PER_SEC*20	  
    dc.w    NB_TICKS_PER_SEC*5	  
    dc.w    NB_TICKS_PER_SEC*1037  
    dc.w    1	  

level5_and_higher:
    dc.w    NB_TICKS_PER_SEC*5
    dc.w    NB_TICKS_PER_SEC*20
    dc.w    NB_TICKS_PER_SEC*5
    dc.w    NB_TICKS_PER_SEC*20
    dc.w    NB_TICKS_PER_SEC*5
    dc.w    NB_TICKS_PER_SEC*1037
    dc.w    1
 

score_frame_table:
    dc.l    score_200,score_400,score_800,score_1600
score_value_table
    dc.l    20,40,80,160
    
; what: get current move speed for ghost
; < A0: ghost structure
; > D0.W: 0,1,2 depending on level, mode, etc...
; trashes: nothing
get_ghost_move_speed:   
    movem.l  d1-d2/a1/a4,-(a7)
    move.l  a0,a4
    move.w  speed_table_index(a4),d1
    add.w   #1,d1
    cmp.w   #16,d1
    bne.b   .nowrap
    moveq   #0,d1
.nowrap
    ; store
    move.w  d1,speed_table_index(a4)

    ; ok but are we in a tunnel or pen
    move.w  xpos(a4),d0
    move.w  ypos(a4),d1
    bsr collides_with_maze
    cmp.b   #T,d0
    beq.b   .tunnel
    cmp.b   #P,d0
    bne.b   .no_tunnel
.tunnel
    move.w  #4*16,d2
    bra.b   .table_computed
.no_tunnel    
    move.w  mode(a4),d2 ; old mode
    cmp.w   #MODE_FRIGHT,d2
    beq.b   .fright
    ; now it depends on the ghost type and elroy mode
; scatter/chase same speed: normal
    move.l  a4,a0
    bsr get_elroy_level
    tst d0
    beq.b   .no_elroy
    cmp.w   #1,d0
    beq.b   .elroy_level_1
    ; level 2
    move.w  #6*16,d2
    bra.b   .table_computed
.elroy_level_1    
    move.w  #6*16,d1
    bra.b   .table_computed
.no_elroy
    move.w  #1*16,d2    ; table 1: normal
    bra.b   .table_computed
.fright:
    move.w  #3*16,d2
    
.table_computed
    move.l  global_speed_table(pc),a1
    move.w  speed_table_index(a4),d0
    add.w   d2,d0
    move.b  (a1,d0.w),d0            ; get speed index
    ext.w   d0
    movem.l (a7)+,d1-d2/a1/a4
    rts

; < A0: ghost structure
; > D0: 0 if not elroy, 1 if elroy level 1, 2 if elroy level 2    
get_elroy_level:
    moveq   #0,d0
    cmp.l   #ghosts,a0
    bne.b   .out
    move.b  #TOTAL_NUMBER_OF_DOTS,d1
    sub.b   nb_dots_eaten(pc),d1
    cmp.b   elroy_threshold_2(pc),d1
    bcs.b   .level2
    cmp.b   elroy_threshold_1(pc),d1
    bcs.b   .level1
.out
    rts
.level1
   move.w  #$F0,$dff180
    moveq   #1,d0
    rts
.level2
    move.w  #$F00,$dff180
    moveq   #2,d0
    rts
; < A0: ghost structure
; > D0: 0 if not elroy, 1 if elroy
is_elroy:
    moveq   #0,d0
    cmp.l   #ghosts,a0
    bne.b   .out
    move.b  #TOTAL_NUMBER_OF_DOTS,d1
    sub.b   nb_dots_eaten(pc),d1
    cmp.b   elroy_threshold_1(pc),d1
    bcs.b   .level1
.out
    rts
.level1
    moveq   #1,d0
    rts

    
; number of dots remaining, triggers elroy1 mode
elroy_table:
    dc.b    20
    dc.b    30
    dc.b    40
    dc.b    40
    dc.b    40
    dc.b    50
    dc.b    50
    dc.b    50
    dc.b    60
    dc.b    60
    dc.b    60
    dc.b    80
    dc.b    80
    dc.b    80
    dc.b    100
    dc.b    100
    dc.b    100
    dc.b    100
    dc.b    120
    dc.b    120
    dc.b    120

 ; speed table at 60 Hz. Not compatible with 50 Hz if we want roughly same speed
speed_table:  ; lifted from https://github.com/shaunlebron/pacman/blob/master/src/Actor.js
    dc.l    speeds_level1,speeds_level2_4,speeds_level2_4,speeds_level2_4
    dc.l    speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20
    dc.l    speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20
    dc.l    speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level5_20,speeds_level21
    
speeds_level1:
                                            ; LEVEL 1
    dc.b   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ; pac-man (normal)
    dc.b   0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ; ghosts (normal)
    dc.b   1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1 ; pac-man (fright)
    dc.b   0,1,1,0,1,1,0,1,0,1,1,0,1,1,0,1 ; ghosts (fright)
    dc.b   0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1 ; ghosts (tunnel)
    dc.b   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 ; elroy 1
    dc.b   1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1 ; elroy 2
speeds_level2_4:
                                           ; LEVELS 2-4
    dc.b   1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1 ; pac-man (normal)
    dc.b   1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1 ; ghosts (normal)
    dc.b   1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1 ; pac-man (fright)
    dc.b   0,1,1,0,1,1,0,1,1,0,1,1,0,1,1,1 ; ghosts (fright)
    dc.b   0,1,1,0,1,0,1,0,1,1,0,1,0,1,0,1 ; ghosts (tunnel)
    dc.b   1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1 ; elroy 1
    dc.b   1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1 ; elroy 2
                                           ;
speeds_level5_20                                          
                                           ; LEVELS 5-20
    dc.b   1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1 ; pac-man (normal)
    dc.b   1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1 ; ghosts (normal)
    dc.b   1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1 ; pac-man (fright) (N/A for levels 17, 19 & 20)
    dc.b   0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1 ; ghosts (fright)  (N/A for levels 17, 19 & 20)
    dc.b   0,1,1,0,1,1,0,1,0,1,1,0,1,1,0,1 ; ghosts (tunnel)
    dc.b   1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1 ; elroy 1
    dc.b   1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1 ; elroy 2
                                           ;
speeds_level21                                          
                                           ; LEVELS 21+
    dc.b   1,1,1,1,2,1,1,1,1,1,1,1,2,1,1,1 ; pac-man (normal)
    dc.b   1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1 ; ghosts (normal)
    dc.b   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; pac-man (fright) N/A
    dc.b   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ; ghosts (fright)  N/A
    dc.b   0,1,1,0,1,1,0,1,0,1,1,0,1,1,0,1 ; ghosts (tunnel)
    dc.b   1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1 ; elroy 1
    dc.b   1,1,2,1,1,2,1,1,2,1,1,2,1,1,2,1; elroy 2


    
wall_table:
    REPT    3
    dc.b    W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W
    ENDR
    ; ------OUT ------------------------- MID ------------------------- OUT
    dc.b    W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W
    dc.b    W,W,W,O,O,O,O,O,O,O,O,O,O,O,O,W,W,O,O,O,O,O,O,O,O,O,O,O,O,W,W,W
    REPT    3
    dc.b    W,W,W,O,W,W,W,W,O,W,W,W,W,W,O,W,W,O,W,W,W,W,W,O,W,W,W,W,O,W,W,W
    ENDR
    dc.b    W,W,W,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,W,W,W
    dc.b    W,W,W,O,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,O,W,W,W
    dc.b    W,W,W,O,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,O,W,W,W
    dc.b    W,W,W,O,O,O,O,O,O,W,W,O,O,O,O,W,W,O,O,O,O,W,W,O,O,O,O,O,O,W,W,W
    dc.b    W,W,W,W,W,W,W,W,O,W,W,W,W,W,O,W,W,O,W,W,W,W,W,O,W,W,W,W,W,W,W,W
    dc.b    W,W,W,W,W,W,W,W,O,W,W,W,W,W,B,W,W,B,W,W,W,W,W,O,W,W,W,W,W,W,W,W
    dc.b    W,W,W,W,W,W,W,W,O,W,W,O,O,O,O,O,O,O,O,O,O,W,W,O,W,W,W,W,W,W,W,W
    dc.b    W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,P,P,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W
    dc.b    W,W,W,W,W,W,W,W,O,W,W,O,W,P,P,P,P,P,P,W,O,W,W,O,W,W,W,W,W,W,W,W
    dc.b    T,T,T,T,T,T,O,O,O,O,O,O,W,P,P,P,P,P,P,W,O,O,O,O,O,O,T,T,T,T,T,T
    dc.b    W,W,W,W,W,W,W,W,O,W,W,O,W,P,P,P,P,P,P,W,O,W,W,O,W,W,W,W,W,W,W,W
    dc.b    W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W
    dc.b    W,W,W,W,W,W,W,W,O,W,W,O,O,O,O,O,O,O,O,O,O,W,W,O,W,W,W,W,W,W,W,W
    dc.b    W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W
    dc.b    W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W
    dc.b    W,W,W,O,O,O,O,O,O,O,O,O,O,O,O,W,W,O,O,O,O,O,O,O,O,O,O,O,O,W,W,W
    dc.b    W,W,W,O,W,W,W,W,O,W,W,W,W,W,O,W,W,O,W,W,W,W,W,O,W,W,W,W,O,W,W,W
    dc.b    W,W,W,O,W,W,W,W,O,W,W,W,W,W,B,W,W,B,W,W,W,W,W,O,W,W,W,W,O,W,W,W
    dc.b    W,W,W,O,O,O,W,W,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,W,W,O,O,O,W,W,W
    dc.b    W,W,W,W,W,O,W,W,O,W,W,O,W,W,W,W,W,W,W,W,O,W,W,O,W,W,O,W,W,W,W,W
    dc.b    W,W,W,W,W,O,W,W,O,W,W,O,W,W,W,W,W,W,W,W,O,W,W,O,W,W,O,W,W,W,W,W    
    dc.b    W,W,W,O,O,O,O,O,O,W,W,O,O,O,O,W,W,O,O,O,O,W,W,O,O,O,O,O,O,W,W,W
    dc.b    W,W,W,O,W,W,W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W,W,W,O,W,W,W
    dc.b    W,W,W,O,W,W,W,W,W,W,W,W,W,W,O,W,W,O,W,W,W,W,W,W,W,W,W,W,O,W,W,W
    dc.b    W,W,W,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,W,W,W
    dc.b    W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W,W
    
    ; 32x34  -------------------------------------------------------OUT
dot_table_read_only:
    REPT    3
    dc.b    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    ENDR
    dc.b    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    dc.b    0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0
    dc.b    0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0
    dc.b    0,0,0,2,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,2,0,0,0
    dc.b    0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0
    dc.b    0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0
    dc.b    0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0,0
    dc.b    0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0,0
    dc.b    0,0,0,1,1,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,1,1,0,0,0
    REPT    11
    dc.b    0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0
    ENDR
    dc.b    0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0
    dc.b    0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0
    dc.b    0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0
    dc.b    0,0,0,2,1,1,0,0,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,0,0,1,1,2,0,0,0
    dc.b    0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0
    dc.b    0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0
    dc.b    0,0,0,1,1,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,1,1,0,0,0
    dc.b    0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0
    dc.b    0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0
    dc.b    0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0
    dc.b    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

    even
powerdots
    ds.l    4

; palette is different for frightened ghosts & eyes
frightened_ghosts_blue_palette
    include "frightened_ghost_blue.s"
frightened_ghosts_white_palette
    include "frightened_ghost_white.s"
ghost_eyes
    include "ghost_eyes.s"
game_palette
    include "palette.s"
    
player:
    ds.b    Player_SIZEOF

ghosts:
red_ghost:      ; needed by cyan ghost
    ds.b    Ghost_SIZEOF
pink_ghost:      ; needed by cyan ghost
    ds.b    Ghost_SIZEOF
    ds.b    Ghost_SIZEOF
    ds.b    Ghost_SIZEOF

; BSS --------------------------------------
    SECTION  S2,BSS
HWSPR_TAB_XPOS:	
	ds.l	512			

HWSPR_TAB_YPOS:
	ds.l	512
    

dot_table
    ds.b    NB_TILES_PER_LINE*NB_TILE_LINES
    SECTION  S3,CODE
    include ptplayer.s

    SECTION  S4,DATA,CHIP
; main copper list
coplist
bitplanes:
   dc.l  $01080000
   dc.l  $010a0000
   dc.l  $00e00000
   dc.l  $00e20000
   dc.l  $00e40000
   dc.l  $00e60000
   dc.l  $00e80000
   dc.l  $00ea0000
   dc.l  $00ec0000
   dc.l  $00ee0000
;   dc.l  $00f00000
;   dc.l  $00f20000

tunnel_color_reg = color+38

colors:
   dc.w color,0     ; fix black (so debug can flash color0)
sprites:
    ; tunnel mask
    dc.w    sprpt,0
    dc.w    sprpt+2,0
ghost_sprites:
    ; red ghost
    dc.w    sprpt+4,0
    dc.w    sprpt+6,0
    ; empty
    dc.w    sprpt+8,0
    dc.w    sprpt+10,0
score_sprite_entry:
    ; pink ghost / score for ghost eaten
    dc.w    sprpt+12,0
    dc.w    sprpt+14,0
    ; empty
    dc.w    sprpt+16,0
    dc.w    sprpt+18,0
    ; cyan ghost
    dc.w    sprpt+20,0
    dc.w    sprpt+22,0
    ; empty
    dc.w    sprpt+24,0
    dc.w    sprpt+26,0
    ; orange ghost
    dc.w    sprpt+28,0
    dc.w    sprpt+30,0

    IFEQ 1
    dc.w    (TUNNEL_MASK_Y+44)<<8+1,$FFFE
tunnel_copperlist:
    REPT    16
; color change for tunnel mask
    ; reset
    dc.w    color+38,$0F00
    dc.w    1,(TUNNEL_MASK_X+24)<<1
    ; set tunnel mask color (black)
    dc.w    color+38,0
    ; nexr line
    dc.w    (TUNNEL_MASK_Y+45+REPTN)<<8+1,$FFFE
    ENDR
    dc.w    color+38,$0F0
    ENDC
;tunnel_sprite_color:
;   dc.w tunnel_color_reg,0  ; set proper red ghost palette (trashed by tunnel) at start

end_color_copper:
   dc.w  diwstrt,$3081            ;  DIWSTRT
   dc.w  diwstop,$28c1            ;  DIWSTOP
   ; proper sprite priority: above bitplanes
   dc.w  $0102,$0000            ;  BPLCON1 := 0x0000
   dc.w  $0104,$0024            ;  BPLCON2 := 0x0024
   dc.w  $0092,$0038            ;  DDFSTRT := 0x0038
   dc.w  $0094,$00d0            ;  DDFSTOP := 0x00d0
   dc.w  $FFDF,$FFFE            ; PAL wait (256)
   dc.w  $2201,$FFFE            ; PAL extra wait (around 288)
   dc.w intreq,$8010            ; generate copper interrupt
    dc.l    -2




; add small safety in case some blit goes beyond screen
screen_data:
    ds.b    SCREEN_PLANE_SIZE*NB_PLANES+NB_BYTES_PER_LINE,0
	
    
    
pac_left_0
    incbin  "pac_left_0.bin"
pac_left_1    
    incbin  "pac_left_1.bin"
pac_right_0
    incbin  "pac_right_0.bin"
pac_right_1    
    incbin  "pac_right_1.bin"
pac_up_0
    incbin  "pac_up_0.bin"
pac_up_1
    incbin  "pac_up_1.bin"
pac_down_0
    incbin  "pac_down_0.bin"
pac_down_1
    incbin  "pac_down_1.bin"
pac_dead
    ; each has 64 bytes
    incbin  "pac_dead_0.bin"
    incbin  "pac_dead_1.bin"
    incbin  "pac_dead_2.bin"
    incbin  "pac_dead_3.bin"
    incbin  "pac_dead_4.bin"
    incbin  "pac_dead_5.bin"
    incbin  "pac_dead_6.bin"
    incbin  "pac_dead_7.bin"
    incbin  "pac_dead_8.bin"
    incbin  "pac_dead_9.bin"
    incbin  "pac_dead_10.bin"
    incbin  "pac_dead_11.bin"
    ds.b    64,0        ; empty frame
pac_lives
    incbin  "pac_lives_0.bin"
bonus_pics:
    incbin  "cherry.bin"
    incbin  "strawberry.bin"
    incbin  "peach.bin"
    incbin  "apple.bin"
    incbin  "grapes.bin"
    incbin  "galaxian.bin"
    incbin  "bell.bin"
    incbin  "key.bin" 
bonus_scores:
    incbin  "bonus_scores_0.bin"    ; 100
    incbin  "bonus_scores_1.bin"    ; 300
    incbin  "bonus_scores_2.bin"    ; 500
    incbin  "bonus_scores_3.bin"    ; 700

black_sprite:
    dc.l    0   ; control word
    REPT    16
    dc.l    $FFFFFFFF
    ENDR
    

    
    
DECL_GHOST:MACRO
\1_ghost_frame_table:
    dc.l    \1_ghost_0
    dc.l    \1_ghost_1
    dc.l    \1_ghost_2
    dc.l    \1_ghost_3
    dc.l    \1_ghost_4
    dc.l    \1_ghost_5
    dc.l    \1_ghost_6
    dc.l    \1_ghost_7
\1_ghost_end_frame_table:
\1_frightened_ghost_blue_frame_table
    dc.l    \1_frightened_ghost_blue_0
    dc.l    \1_frightened_ghost_blue_1
\1_frightened_ghost_white_frame_table
    dc.l    \1_frightened_ghost_white_0
    dc.l    \1_frightened_ghost_white_1
\1_ghost_eye_frame_table
    dc.l    \1_ghost_eyes_0
    dc.l    \1_ghost_eyes_1
    dc.l    \1_ghost_eyes_2
    dc.l    \1_ghost_eyes_3
    
    ; all ghosts share the same graphics, only the colors are different
    ; but we need to replicate the graphics 8*4 times because of sprite control word
\1_ghost_0
    dc.l    0
    incbin  "ghost_0.bin"
    dc.l    0
\1_ghost_1
    dc.l    0
    incbin  "ghost_1.bin"
    dc.l    0
\1_ghost_2
    dc.l    0
    incbin  "ghost_2.bin"
    dc.l    0
\1_ghost_3
    dc.l    0
    incbin  "ghost_3.bin"
    dc.l    0
\1_ghost_4
    dc.l    0
    incbin  "ghost_4.bin"
    dc.l    0
\1_ghost_5
    dc.l    0
    incbin  "ghost_5.bin"
    dc.l    0
\1_ghost_6
    dc.l    0
    incbin  "ghost_6.bin"
    dc.l    0
\1_ghost_7
    dc.l    0
    incbin  "ghost_7.bin"
    dc.l    0
\1_frightened_ghost_blue_0
    dc.l    0
    incbin  "frightened_ghost_blue_0.bin"
    dc.l    0
\1_frightened_ghost_blue_1
    dc.l    0
    incbin  "frightened_ghost_blue_1.bin"
    dc.l    0
\1_frightened_ghost_white_0
    dc.l    0
    incbin  "frightened_ghost_white_0.bin"
    dc.l    0
\1_frightened_ghost_white_1
    dc.l    0
    incbin  "frightened_ghost_white_1.bin"
    dc.l    0
\1_ghost_eyes_0
    dc.l    0
    incbin  "ghost_eyes_0.bin"
    dc.l    0
\1_ghost_eyes_1
    dc.l    0
    incbin  "ghost_eyes_1.bin"
    dc.l    0
\1_ghost_eyes_2
    dc.l    0
    incbin  "ghost_eyes_2.bin"
    dc.l    0
\1_ghost_eyes_3
    dc.l    0
    incbin  "ghost_eyes_3.bin"
    dc.l    0
    ENDM
        
    DECL_GHOST  red
    DECL_GHOST  pink
    DECL_GHOST  cyan
    DECL_GHOST  orange
    
score_200:
    dc.l    0
    incbin  "scores_0.bin"      ; 64 bytes each, palette from pink sprite
    dc.l    0
score_400:
    dc.l    0
    incbin  "scores_1.bin"
    dc.l    0
score_800:
    dc.l    0
    incbin  "scores_2.bin"
    dc.l    0
score_1600:
    dc.l    0
    incbin  "scores_3.bin"
    dc.l    0

killed_sound
    incbin  "pacman_killed.raw"
    even
killed_sound_end
    
empty_sprite
    dc.l    0,0

    
    	