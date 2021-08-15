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

	
;CIA-A registre port A (bouton souris)

CIAAPRA = $BFE001

	STRUCTURE	Character,0
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
    APTR     behaviour
    APTR     frame_table
    APTR     copperlist_address
    UWORD    home_corner_xtile
    UWORD    home_corner_ytile
    UWORD    target_xtile
    UWORD    target_ytile
    UWORD    mode_timer     ; number of 1/50th to stay in the current mode
    UWORD    mode           ; current mode
    UWORD    mode_counter   ; 0: first scatter, 1: first attack, etc.
    UBYTE    direction_change   ; direction change flag
    UBYTE    pad2
	LABEL	Ghost_SIZEOF
    
    ;Exec Library Base Offsets


;graphics base

StartList = 38

;autres labels

Execbase  = 4

MODE_SCATTER = 10
MODE_CHASE = 20
MODE_FRIGHT = 30

; TODO: set 60
NB_TICKS_PER_SEC = 50

; wall tile types
W = 4   ; wall
P = 3   ; pen space (pac block)
T = 2   ; tunnel
B = 1   ; ghost up block
O = 0   ; empty


NB_BYTES_PER_LINE = 40
NB_BYTES_PER_MAZE_LINE = 28
MAZE_PLANE_SIZE = NB_BYTES_PER_LINE*NB_LINES
SCREEN_PLANE_SIZE = 40*NB_LINES
NB_PLANES   = 4

NB_TILES_PER_LINE = 2+28+2    ; 2 fake tiles in the start & end
NB_TILE_LINES = 31+3    ; 3 fake tiles before the maze to simulate ghosts targets
NB_LINES = NB_TILE_LINES*8

MAZE_BLINK_TIME = NB_TICKS_PER_SEC/2


X_START = 16
Y_START = 24
; tunnel max
X_MAX = (NB_TILES_PER_LINE-1)*8

RED_YSTART_POS = 92+Y_START
RED_XSTART_POS = 112+X_START
OTHERS_XSTART_POS = RED_YSTART_POS+24

BONUS_X_POS = RED_XSTART_POS-24
BONUS_Y_POS = RED_YSTART_POS+16
BONUS_OFFSET = $28F  ;(NB_TILES_PER_LINE*20)+14
BONUS_TIMER_VALUE = NB_TICKS_PER_SEC*10

BLINK_RATE = 2*(NB_TICKS_PER_SEC/5) ; for powerdots
PREPOST_TURN_LOCK = 4

DOT_PLANE_OFFSET = SCREEN_PLANE_SIZE*2-(X_START/8)

; follows order of ghosts
RIGHT = 0
LEFT = 1<<2
UP = 2<<2
DOWN = 3<<2

; possible directions, clockwise


DIRB_RIGHT = 0
DIRB_DOWN = 1
DIRB_LEFT = 2
DIRB_UP = 3
DIRF_RIGHT = 1
DIRF_DOWN = 1<<1
DIRF_LEFT = 1<<2
DIRF_UP = 1<<3

; states, 2 by 2, starting by 0

STATE_PLAYING = 0
STATE_GAME_OVER = 1*2
STATE_LEVEL_COMPLETED = 2*2
STATE_NEXT_LEVEL = 3*2

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
    lea GRname(PC),a1               ; param�tre pour initialiser OpenLibrary
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
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    bsr init_player

    ; do it first, as the last bonus overwrites bottom left of screen
    bsr draw_lives_and_bonuses    
    bsr draw_maze
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

    bsr init_ghosts
    
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

;COPPER init
		
    move.l	#coplist,cop1lc(a5)
    clr.w copjmp1(a5)

;playfield init

    move.w #$3081,diwstrt(a5)             ; valeurs standard pour
    move.w #$30C1,diwstop(a5)             ; la fen�tre �cran
    move.w #$0038,ddfstrt(a5)             ; et le DMA bitplane
    move.w #$00D0,ddfstop(a5)
    move.w #$4200,bplcon0(a5) ; 4 bitplanes
    clr.w bplcon1(a5)                     ; no scrolling
    clr.w bplcon2(a5)                     ; pas de priorit�
    move.w #0,bpl1mod(a5)                ; modulo de tous les plans = 40
    move.w #0,bpl2mod(a5)

    bsr hide_sprites

    ; init sprite, bitplane, whatever dma (not audio ATM)
    move.w #$83E0,dmacon(a5)
    ; enable copper interrupts, mainly

    move.w #$C038,intena(a5)
    
.mainloop
    btst    #6,$bfe001
    beq.b   .out
;    tst.w   current_state
;    beq.b   .mainloop
    cmp.w   #STATE_NEXT_LEVEL,current_state
    bne.b   .no_new_level
    add.w   #1,level_number
    bra.b   .new_level
.no_new_level
    bra.b   .mainloop
.out
      
    ; quit
    bsr     restore_interrupts
			      

    move.l  gfxbase,a1
    move.l  gfxbase_copperlist,StartList(a1) ; adresse du d�but de la liste
    move.l  gfxbase_copperlist,cop1lc(a5) ; adresse du d�but de la liste
    clr.w  copjmp1(a5)
    move.w #$8060,dmacon(a5)        ; r�initialisation du canal DMA
    
    move.l  gfxbase,a1
    jsr _LVOCloseLibrary(a6)
    
    jsr _LVOPermit(a6)                  ; Task Switching autoris�
    moveq.l #0,d0
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
    
    ; level
    move.w  level_number,d2
    add.w   d2,d2
    lea bonus_level_score(pc),a0
    move.w  (a0,d2.w),fruit_score
    move.b  #0,nb_dots_eaten
    rts
    
hide_sprites:
    move.w  #7,d1
.emptyspr
    lea  sprites,a0
    lea empty_sprite,a1
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
    addq.l  #8,a0
    dbf d1,.emptyspr
    rts

init_ghosts
    lea ghosts(pc),a0
    lea sprites,a1   ; the sprite part of the copperlist
    lea behaviour_table(pc),a2
    ; shared settings
    moveq.w #3,d7
.igloop
    move.l  a1,copperlist_address(a0)
    add.l   #16,a1
    
    clr.w   mode_counter(a0)
    clr.w   speed_table_index(a0)
    clr.b   direction_change(a0)

    clr.w   h_speed(a0)
    clr.w   v_speed(a0)
    clr.w   target_xtile(a0)
    clr.w   target_ytile(a0)
    
    bsr     update_ghost_mode_timer
    move.w  #MODE_SCATTER,mode(a0)
    move.l  (a2)+,behaviour(a0)
	move.w	#OTHERS_XSTART_POS,ypos(a0)
    
    add.l   #Ghost_SIZEOF,a0
    dbf d7,.igloop
    
    ; specific settings
    lea ghosts(pc),a0
    ; red ghost
	move.w  #RED_XSTART_POS,xpos(a0)
	move.w	#RED_YSTART_POS,ypos(a0)
    move.w  #LEFT,direction(a0)
    move.w  #8,frame(a0)
    move.l  #red_ghost_frame_table,frame_table(a0)
    move.w  #2,home_corner_xtile(a0)
    move.w  #0,home_corner_ytile(a0)
    bsr     update_ghost_target
    move.w  #-1,h_speed(a0)
    ; pink ghost
    add.l   #Ghost_SIZEOF,a0
	move.w  #RED_XSTART_POS,xpos(a0)
    move.w  #DOWN,direction(a0)
    move.w  #0,frame(a0)
    move.l  #pink_ghost_frame_table,frame_table(a0)
    move.w  #(NB_TILES_PER_LINE-6),home_corner_xtile(a0)
    bsr     update_ghost_target
    move.w  #0,home_corner_ytile(a0)
    move.w  #1,v_speed(a0)

    add.l   #Ghost_SIZEOF,a0
    ; cyan ghost
	move.w  #(RED_XSTART_POS-16),xpos(a0)
    move.w  #UP,direction(a0)
    move.w  #0,frame(a0)
    move.l  #cyan_ghost_frame_table,frame_table(a0)
    move.w  #0,home_corner_xtile(a0)
    move.w  #(NB_TILE_LINES+1),home_corner_ytile(a0) 
    bsr update_ghost_target
    move.w  #-1,v_speed(a0)
    ; orange ghost
    add.l   #Ghost_SIZEOF,a0

	move.w  #(RED_XSTART_POS+16),xpos(a0)
    move.w  #UP,direction(a0)
    move.w  #0,frame(a0)
    move.l  #orange_ghost_frame_table,frame_table(a0)
    move.w  #(NB_TILES_PER_LINE-4),home_corner_xtile(a0)
    move.w  #(NB_TILE_LINES+1),home_corner_ytile(a0)
    bsr update_ghost_target
    move.w  #-1,v_speed(a0)

    rts

behaviour_table
    dc.l    red_chase,pink_chase,cyan_chase,orange_chase

; < A0: ghost structure
; < A1: player structure
red_chase
    ; simple: get player tile
    move.w  xpos(a1),d0
    move.w  ypos(a1),d1
    lsr.w   #3,d0
    lsr.w   #3,d1
    move.w  d0,target_xtile(a0)
    move.w  d1,target_ytile(a1)
    rts
pink_chase
    rts
orange_chase
    rts
cyan_chase
    rts
    
; < A0: ghost structure
; trashes: D0
update_ghost_target
    move.w  mode(a0),d0
    cmp.w   #MODE_SCATTER,d0
    beq.b   .scatter
    cmp.w   #MODE_CHASE,d0
    beq.b   .chase
    ; fright: no update
    rts
.scatter:
    move.l  home_corner_xtile(a0),target_xtile(a0)  ; hack copy x & y at once
    rts
.chase:
    pea .next(pc)
    move.l  behaviour(a0),-(a7)
    rts
.next
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
    lea player(pc),a0
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
    move.w  #STATE_PLAYING,current_state
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
    move.w  #4,d3
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
    lea .dir(pc),a0
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
        
draw_all    
    move.w  current_state(pc),d0
    lea     .case_table(pc),a0
    move.l     (a0,d0.w),a0
    jmp (a0)
.case_table
    dc.l    .playing
    dc.l    .game_over
    dc.l    .level_completed
    dc.l    .next_level
.next_level
    ; don't do anything
    rts
    
.game_over
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  #72,d0
    move.w  #136,d1
    lea game_over_string(pc),a0
    bsr write_string
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1
    move.w  #72,d0
    move.w  #136,d1
    bsr write_string
    bra.b   .draw_complete
.level_completed
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
    lea ready_string(pc),a0
    bsr write_string
    ;;bra.b   .draw_complete
.ready_off
    ; draw pacman
    lea player(pc),a2
    move.w  direction(a2),d0
    lea  pac_dir_table(pc),a0
    move.l  (a0,d0.w),a0
    move.w  frame(a2),d0
    add.w   d0,d0
    add.w   d0,d0
    move.l  (a0,d0.w),a0

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
    bsr blit_plane
    ; A1 is start of dest, use it to clear upper part and lower part
    ; and possibly shifted to the left/right
    move.l  a1,d0
    btst    #0,d0
    beq.b   .ok
    subq.l  #1,a1   ; even address, always!
.ok
    clr.l   (-NB_BYTES_PER_LINE,a1)
    clr.l   (-NB_BYTES_PER_LINE*2,a1)
    clr.l   (-NB_BYTES_PER_LINE*3,a1)
    clr.l   (NB_BYTES_PER_LINE*16,a1)


    ; test display score with the proper color (reusing pink sprite palette)
    IFEQ    1
    move.l  score_frame_table+12,a0
    move.w  #20,d0
    move.w  #24,d1
    bsr store_sprite_pos      
    move.l  d0,(a0)
    lea sprites+24,a1
    move.l  a0,d2
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)    
    ; set proper positions for ghosrs
    ENDC
    
    lea ghosts(pc),a0
    moveq.l #3,d7
.gloop    
    move.w  xpos(a0),d0
    move.w  ypos(a0),d1
    ; center => top left
    sub.w  #8+X_START,d0
    sub.w  #4+Y_START,d1    ; not 8, because maybe table is off?
    bsr store_sprite_pos    
    
    move.l  frame_table(a0),a1
    move.w  frame(a0),d2
    lsr.w   #2,d2   ; 8 divide to get 0,1, divide by 4 then mask after adding direction
    add.w   direction(a0),d2     ; add 0,4,8,12 depending on direction
    bclr    #0,d2   ; even
    add.w   d2,d2       ; times 2
    move.l  (a1,d2.w),a1
    move.l  d0,(a1)     ; store control word
    move.l  a1,d2    
    move.l  copperlist_address(a0),a1
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)    
    
    add.l   #Ghost_SIZEOF,a0
    dbf d7,.gloop
    
    
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
    
draw_lives_and_bonuses:
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.l #NB_BYTES_PER_MAZE_LINE*8,d0
    moveq.l #0,d1
    move.l  #8,d2
    bsr clear_plane_any

    lea pac_lives,a0
    move.b  nb_lives(pc),d7
    ext     d7
    subq.w  #2,d7
    beq.b   .out
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
    ; TEMP
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.l #NB_BYTES_PER_MAZE_LINE*8,d0
    moveq.l #0,d1
    move.l  #20,d2
    ;bsr clear_plane_any
    
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
    move.w  colors+6,maze_color     ; save original maze color
    move.w  #MAZE_BLINK_TIME,maze_blink_timer
    move.b  #4,maze_blink_nb_times
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
    add.l   #SCREEN_PLANE_SIZE,a1
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
    
    lea level3_interrupt(pc),a1
    move.l  a1,($6C,a0)
    
    move.w  (dmaconr,a5),saved_dmacon
    move.w  (intenar,a5),saved_intena
    
    rts
    
restore_interrupts:
    ; assuming VBR at 0
    sub.l   a0,a0
    lea saved_vectors(pc),a1
    move.l  (a1)+,($68,a0)
    move.l  (a1)+,($6C,a0)

    move.w  saved_dmacon,d0
    bset    #15,d0
    move.w  d0,(dmacon,a5)
    move.w  saved_intena,d0
    bset    #15,d0
    move.w  d0,(intena,a5)

    rts
    
saved_vectors
        dc.l    0   ; keyboard
        dc.l    0   ; vblank
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
    bsr draw_debug
    bsr update_all

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

mouse:	
	;move.w	#$0F0,$DFF180
	BTST #6,$BFE001
	BNE  mouse
	rts

; what: updates game state
; args: none
; trashes: potentially all registers

update_all
    move.w  current_state(pc),d0
    lea     .case_table(pc),a0
    move.l     (a0,d0.w),a0
    jmp (a0)
.case_table
    dc.l    .playing
    dc.l    .game_over
    dc.l    .level_completed
.level_completed
    subq.w  #1,maze_blink_timer
    bne.b   .no_change
    move.w  #MAZE_BLINK_TIME,maze_blink_timer
    subq.b  #1,maze_blink_nb_times
    beq.b   .next_level
    cmp.w  #$FFF,colors+6
    beq.b   .orig
    move.w  maze_color,d0
    bra.b   .chcol
.orig
    move.w  #$FFF,d0
.chcol
    move.w  d0,colors+6
.no_change
    rts
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
    bsr update_pac
    bsr update_ghosts
    rts
remove_bonus
    move.b  #0,dot_table+BONUS_OFFSET
    move.w  #1,bonus_clear_message      ; tell draw routine to clear
    clr.w   bonus_timer
    rts

update_ghosts
    lea ghosts(pc),a4
    ; only red ATM
    
    ; decrease mode timer
    move.w  mode_timer(a4),d0
    subq.w  #1,d0
    beq.b   .new_mode
    move.w  d0,mode_timer(a4)
.mode_done
    move.l  a4,a0
    bsr get_ghost_move_speed
    tst   d0
    beq.b   .ghost_done     ; no move for now
    ; now ghost can move once or twice
    move.w  d0,d7
    subq.w  #1,d7
.move_loop
    move.w  frame(a4),d1
    addq.w  #1,d1
    and.w   #$F,d1
    move.w  d1,frame(a4)
 
    move.w  xpos(a4),d0
    move.w  ypos(a4),d1
    move.w  d0,d2
    move.w  d1,d3
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
.next_ghost
    dbf d7,.move_loop
.ghost_done
    rts
    
.tile_change
    ; now the tricky bit: decide where to go
    ; first check for "direction change" signal
    tst.b   direction_change(a4)
    beq.b   .no_dirchange
    clr.b   direction_change(a4)    ; ack
    neg.w  h_speed(a4)
    neg.w  v_speed(a4)
    bsr    .set_direction_from_speed
    bra.b   .next_ghost
.no_dirchange

    ; direction NOT changed, check objective if not fright mode
    bsr .compute_possible_directions
    move.w  d0,possible_directions  ; DEBUG
    move.w  mode(a4),d0
    cmp.w   #MODE_FRIGHT,d0
    bne.b   .no_fright
    ; fright mode: TODO
    blitz
    bra.b   .set_speed_vector
.no_fright:
    bsr update_ghost_target
    ; try to select the best direction to reach the target
    move.w  xpos(a4),d2
    move.w  ypos(a4),d2
    lsr.w   #3,d2
    lsr.w   #3,d3
    ; now it's time to check the possible tiles
    ; and their distance to target
    ; priority: up, left, down, right
    move.w  target_xtile(a4),d4
    move.w  target_ytile(a4),d5
    moveq.l #-1,d7      ; max distance
    btst    #DIRB_UP,d0
    beq.b   .no_test_up
    subq.w  #1,d5   ; tile up
    bsr .compute_square_distance
    move.l   d6,d7              ; store min distance (only one distance computed)
    move.w  #UP,direction(a4)
.no_test_up
    move.w  target_xtile(a4),d4
    move.w  target_ytile(a4),d5
    btst    #DIRB_LEFT,d0
    beq.b   .no_test_left
    subq.w  #1,d4   ; tile left
    bsr .compute_square_distance
    cmp.l   d6,d7
    bcs.b   .no_test_left
    move.w  #LEFT,direction(a4)
    move.l  d6,d7
.no_test_left
    move.w  target_xtile(a4),d4
    move.w  target_ytile(a4),d5
    btst    #DIRB_DOWN,d0
    beq.b   .no_test_down
    addq.w  #1,d5   ; tile down
    bsr .compute_square_distance
    cmp.l   d6,d7
    bcs.b   .no_test_down
    move.w  #DOWN,direction(a4)
    move.l  d6,d7
.no_test_down
    move.w  target_xtile(a4),d4
    move.w  target_ytile(a4),d5
    btst    #DIRB_RIGHT,d0
    beq.b   .no_test_right
    addq.w  #1,d4   ; tile right
    bsr .compute_square_distance
    cmp.l   d6,d7
    bcs.b   .no_test_right
    move.w  #RIGHT,direction(a4)
.no_test_right
    rts

; < D2: XT1
; < D3: YT1
; < D4: XT2
; < D5: YT2
; > D6: square distance
.compute_square_distance   
    moveq.l   #0,d4
    sub.w   d2,d4
    muls.w  d4,d4
    moveq.l   #0,d5
    sub.w   D3,D5
    muls.w  D5,D5
    move.l  d5,d6
    add.l   d4,d6
    rts
.set_speed_vector
    move.w  xpos(a4),d2
    move.w  ypos(a4),d3
    ; set speed vector when aligned with grid
    ; direction is already set properly, no need to check walls again
    ; (ghost faces the direction it's going to take)
    ; if speed is vertical, check if aligned horizontally with grid
    move.w  d2,d0
    move.w  d3,d1
    lea direction_speed_table(pc),a2
    move.w  v_speed(a4),d4
    ; now check if speeds are applicable to ghost
    beq.b   .no_vmove
    ; are we x-aligned?
    and.w   #$F8,d0
    add.w   #4,d0
    cmp.w   d0,d2
    beq.b   .change_direction   ; not aligned: don't change direction yet
.no_vmove    
    ; if speed is horizontal, check if aligned vertically with grid
    move.w  h_speed(a4),d4
    ; now check if speeds are applicable to ghost
    beq.b   .no_vmove
    ; are we y-aligned?
    and.w   #$1F8,d1
    add.w   #4,d1
    cmp.w   d1,d3
    bne.b   .no_hmove   ; not aligned: don't change direction yet
.change_direction
    move.w  direction(a4),d0
    move.l  (a2,d0.l),h_speed(a4)   ; change h & v speed from set direction
.no_hmove
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
    ; now if down, can move
    cmp.b #P,d0    ; pen
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
    cmp.b #T,d0    ; pen
    beq.b   .can_move_left
    ; now if down, can move
    cmp.b #P,d0    ; pen
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
    cmp.b #P,d0    ; pen
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
    cmp.b #P,d0    ; pen
    bne.b   .test_done
.can_move_up    
    or.w    #DIRF_UP,d4
.test_done
    move.l  d4,d0
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
    move.w   mode_counter(a4),d0
    cmp.w   #6,d0
    beq.b   .maxed
    add.w   #1,d0
    move.w  d0,mode_counter(a4)
.maxed
    ; reload new mode timer
    move.l  a4,a0
    bsr update_ghost_mode_timer
    move.b  #1,direction_change(a4)
    bra.b   .mode_done
.scatter
    move.w  #MODE_SCATTER,D0
    bra.b   .switch_mode

    
.from_fright
    blitz
    ; TODO
    bra.b   .mode_done
.prev_tile_x
        dc.w    0
.prev_tile_y
        dc.w    0
        
update_pac
    tst.w   bonus_timer
    beq.b   .no_fruit
    subq.w  #1,bonus_timer
    bne.b   .no_fruit
    ; timeout: make fruit disappear, no score
    bsr remove_bonus
.no_fruit 
    ; power dot blink timer
    subq.w  #1,power_dot_timer
    bpl.b   .no_reload
    move.w  #BLINK_RATE,power_dot_timer
.no_reload
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
    bsr get_global_speed_table
    ; TODO faster when at least one ghost is in fright mode
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
    move.b  #1,still_timer(a4)      ; still during 3 frames
    bsr clear_dot
.score
    and.w   #$FF,d2
    cmp.w   #3,d2
    bcc.b   .bonus_eaten
    ; dot
    move.b  nb_dots_eaten,d4
    addq.b  #1,d4
    tst.w   bonus_timer
    bne.b   .skip_fruit_test

    cmp.b   #70,d4
    beq.b   .show_fruit
    cmp.b   #170,d4
    beq.b   .show_fruit
.skip_fruit_test
    cmp.b   #244,d4
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
    
HW_SpriteXTable
  rept 320
x   set REPTN+$80
    dc.b  0, x>>1, 0, x&1
  endr


HW_SpriteYTable
  rept 256
ys  set REPTN+$2c
ye  set ys+16       ; size = 16
    dc.b  ys&255, 0, ye&255, ((ys>>6)&%100) | ((ye>>7)&%10)
  endr

direction_speed_table
    ; right
    dc.w    1,0
    ; left
    dc.w    -1,0
    ; up
    dc.w    0,-1
    ; down
    dc.w    -1,0
    
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

    
    MUL_TABLE   40

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
    move.w  (a2,d1.w),d1
    swap    d1
    clr.w   d1
    swap    d1
    move    d0,d3

    and.w   #$F,D3
    and.w   #$1F0,d0
    lsr.w   #3,d0
    add.w   d0,d1
    add.l   d1,a1       ; plane position
    
    swap    d3
    clr.w   d3
    lsl.l   #8,d3
    lsl.l   #4,d3
    
    move.l  #$09f00000,d4    ;A->D copy, ascending mode
    or.l   d3,d4            ; add shift

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
    
    
; what: writes a text in a single plane
; args:
; < A0: c string
; < A1: plane
; < D0: X (multiple of 8)
; < D1: Y
; > D0: number of characters written

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
; 0: level 1
level_number
    dc.w    0
maze_blink_nb_times
    dc.b    0
nb_lives
    dc.b    0

bonus_clear_message
    dc.w    0
    
score_table
    dc.w    0,1,5
fruit_score     ; must follow score_table
    dc.w    10
    
    
nb_dots_eaten
    dc.b    0

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
    dc.w    NB_TICKS_PER_SEC*5	

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
    ; now it depends on the ghost type and elroy mode
    ; TODO handle elroy
    move.w  mode(a4),d2 ; old mode
    cmp.w   #MODE_FRIGHT,d2
    beq.b   .fright
; scatter/chase same speed: normal
    move.w  #1*16,d2    ; table 1: normal
    bra.b   .tunnel_test
.fright:
    move.w  #3*16,d2
    
.tunnel_test
    ; ok but are we in a tunnel?
    move.w  xpos(a4),d0
    move.w  xpos(a4),d1
    bsr collides_with_maze
    cmp.b   #T,d0
    bne.b   .no_tunnel
    move.w  #4*16,d2
    
.no_tunnel
    bsr get_global_speed_table
    move.w  speed_table_index(a4),d0
    move.b  (a1,d0.w),d0            ; get speed index
    ext.w   d0
    movem.l (a7)+,d1-d2/a1/a4
    rts
    
; > A1: speed table for the current level
; (used for ghosts and also for pacman depending on the row)
get_global_speed_table
    move.w  level_number,d0
    cmp.w   #20,d0
    bcs.b   .below
    move.w  #20,d0
.below
    add.w   d0,d0
    add.w   d0,d0
    lea speed_table(pc),a1
    move.l  (a1,d0.w),a1    ; global speed table
    rts
    

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
    
    SECTION  S3,DATA,CHIP

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

colors:
   include "palette_clist.s"
sprites:
    ; red ghost
    dc.w    sprpt,0
    dc.w    sprpt+2,0
    ; empty
    dc.w    sprpt+4,0
    dc.w    sprpt+6,0
    ; pink ghost
    dc.w    sprpt+8,0
    dc.w    sprpt+10,0
    ; empty / score for ghost eaten
    dc.w    sprpt+12,0
    dc.w    sprpt+14,0
    ; cyan ghost
    dc.w    sprpt+16,0
    dc.w    sprpt+18,0
    ; empty
    dc.w    sprpt+20,0
    dc.w    sprpt+22,0
    ; orange ghost
    dc.w    sprpt+24,0
    dc.w    sprpt+26,0
    ; empty
    dc.w    sprpt+28,0
    dc.w    sprpt+30,0

 
end_color_copper:
   dc.w  diwstrt,$3081            ;  DIWSTRT
   dc.w  diwstop,$28c1            ;  DIWSTOP
   dc.w  $0102,$0000            ;  BPLCON1 := 0x0000
   dc.w  $0104,$0024            ;  BPLCON2 := 0x0024
   dc.w  $0092,$0038            ;  DDFSTRT := 0x0038
   dc.w  $0094,$00d0            ;  DDFSTOP := 0x00d0
   dc.w  240,$FFFE              ; wait until y=240
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
    incbin  "pac_dead_0.bin"
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
    ; all ghosts share the same graphics, only the colors are different
    ; but we need to replicate the graphics 8*4 times because of sprite control word
\1_ghost_0
    dc.l    0
    incbin  "ghost_0.bin"
\1_ghost_1
    dc.l    0
    incbin  "ghost_1.bin"
\1_ghost_2
    dc.l    0
    incbin  "ghost_2.bin"
\1_ghost_3
    dc.l    0
    incbin  "ghost_3.bin"
\1_ghost_4
    dc.l    0
    incbin  "ghost_4.bin"
\1_ghost_5
    dc.l    0
    incbin  "ghost_5.bin"
\1_ghost_6
    dc.l    0
    incbin  "ghost_6.bin"
\1_ghost_7
    dc.l    0
    incbin  "ghost_7.bin"
    ENDM
        
    DECL_GHOST  red
    DECL_GHOST  pink
    DECL_GHOST  cyan
    DECL_GHOST  orange
    
score_200:
    dc.l    0
    incbin  "scores_0.bin"      ; 64 bytes each, palette from pink sprite
score_400:
    dc.l    0
    incbin  "scores_1.bin"
score_800:
    dc.l    0
    incbin  "scores_2.bin"
score_1600:
    dc.l    0
    incbin  "scores_3.bin"


empty_sprite
    dc.l    0
    
    	