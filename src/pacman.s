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
	
    
    include "whdload.i"
    include "whdmacros.i"

    incdir "../sprites"
    incdir "../sounds"


INTERRUPTS_ON_MASK = $E038

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
    APTR     tunnel_frame
    APTR     target_frame
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
    UWORD    pen_xtile
    UWORD    pen_ytile
	UWORD	 xpen
	UWORD	 ypen
    UBYTE    pen_nb_dots
    UBYTE    pen_dot_limit  ; 0, 7, 17, 32 depending on which ghost
    UBYTE    reverse_flag   ; direction change flag
    UBYTE    flashing_as_white
    UBYTE    pen_exit_override_flag
    UBYTE    pad2
	LABEL	 Ghost_SIZEOF
    
    ;Exec Library Base Offsets


;graphics base

StartList = 38

;autres labels

Execbase  = 4

MODE_SCATTER = 10
MODE_CHASE = 20
MODE_FRIGHT = 30
MODE_EYES = 40

FIRST_INTERMISSION_LEVEL = 2
SECOND_INTERMISSION_LEVEL = 5
THIRD_INTERMISSION_LEVEL = 9
FOURTH_INTERMISSION_LEVEL = 13

; ---------------debug/adjustable variables

; uncomment to test intermission screen
;;INTERMISSION_TEST = THIRD_INTERMISSION_LEVEL

; temp if nonzero, then records game input, intro music doesn't play
; and when one life is lost, blitzes and a0 points to move record table
; a1 points to the end of the table
; 100 means 100 seconds of recording at least (not counting the times where
; the player (me :)) isn't pressing any direction at all.
;RECORD_INPUT_TABLE_SIZE = 100*ORIGINAL_TICKS_PER_SEC

EXTRA_LIFE_SCORE = 10000/10

START_LEVEL = 1   ; +FIRST_INTERMISSION_LEVEL

; --------------- end debug/adjustable variables

; actual nb ticks (PAL)
NB_TICKS_PER_SEC = 50
; game logic ticks
ORIGINAL_TICKS_PER_SEC = 60

; wall tile types
W = 4   ; wall
P = 3   ; pen space (pac block)
T = 2   ; tunnel
B = 1   ; ghost up block
O = 0   ; empty

TOTAL_NUMBER_OF_DOTS = 244

NB_BYTES_PER_LINE = 40
NB_BYTES_PER_MAZE_LINE = 28
MAZE_PLANE_SIZE = NB_BYTES_PER_MAZE_LINE*NB_LINES
SCREEN_PLANE_SIZE = 40*NB_LINES
NB_PLANES   = 4

TUNNEL_MASK_X = NB_BYTES_PER_MAZE_LINE*8
TUNNEL_MASK_Y = OTHERS_YSTART_POS-27

; messages from update routine to display routine
MSG_NONE = 0
MSG_SHOW = 1
MSG_HIDE = 2

NB_TILES_PER_LINE = 2+28+2    ; 2 fake tiles in the start & end
NB_MAZE_ROWS = 31
NB_TILE_LINES = NB_MAZE_ROWS+3    ; 3 fake tiles before the maze to simulate ghosts targets
NB_LINES = NB_TILE_LINES*8
NB_MAZE_LINES = NB_MAZE_ROWS*8

MAZE_BLINK_TIME = NB_TICKS_PER_SEC/4

NB_FLASH_FRAMES = 14

; matches the pac kill animation
PLAYER_KILL_TIMER = NB_TICKS_PER_SEC+NB_TICKS_PER_SEC/2+(NB_TICKS_PER_SEC/8)*9+NB_TICKS_PER_SEC/4+NB_TICKS_PER_SEC
GHOST_KILL_TIMER = (NB_TICKS_PER_SEC*5)/6

X_START = 16
Y_START = 24
; tunnel max
X_MAX = (NB_TILES_PER_LINE-1)*8
; tunnel min (pacman)
; used to be 0 but now for some reason it doesn't work
; so 4 is all right ATM
X_MIN = 4
RED_YSTART_POS = 92+Y_START
RED_XSTART_POS = 112+X_START
OTHERS_YSTART_POS = RED_YSTART_POS+24

BONUS_X_POS = RED_XSTART_POS-24
BONUS_Y_POS = RED_YSTART_POS+16
BONUS_OFFSET = $28F  ;(NB_TILES_PER_LINE*20)+14
BONUS_TIMER_VALUE = NB_TICKS_PER_SEC*10     ; ORIGINAL_TICKS_PER_SEC?
BONUS_SCORE_TIMER_VALUE = NB_TICKS_PER_SEC*2     ; ORIGINAL_TICKS_PER_SEC?
BLINK_RATE = ORIGINAL_TICKS_PER_SEC/2 ; for powerdots
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
STATE_INTRO_SCREEN = 5*4
STATE_GAME_START_SCREEN = 6*4
STATE_INTERMISSION_SCREEN = 7*4

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
    dc.l    .intro_screen
    dc.l    .game_start_screen
    dc.l    .intermission_screen
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
        ; if D0 contains "WHDL"
        ; A0 contains resload
        
    cmp.l   #'WHDL',D0
    bne.b   .standard
    move.l a0,_resload
    move.b  d1,_keyexit
    ;move.l  a0,a2
    ;lea	_tags(pc),a0
    ;jsr	resload_Control(a2)
    bsr load_highscores
    
    bra.b   .startup
.standard
    ; open dos library, graphics library
    move.l  $4.W,a6
    lea dosname(pc),a1
    moveq.l #0,d0
    jsr _LVOOpenLibrary(a6)
    move.l  d0,_dosbase
    lea graphicsname(pc),a1
    moveq.l #0,d0
    jsr _LVOOpenLibrary(a6)
    move.l  d0,_gfxbase
    move.l	d0,a6
    
    bsr load_highscores
    
    move.l  _dosbase(pc),a6
    move.l   #floppy_file,d1
    move.l  #MODE_OLDFILE,d2
    jsr     _LVOOpen(a6)
    move.l  d0,d1
    beq.b   .no_floppy
    
    ; "floppy" file found
    jsr     _LVOClose(a6)
    ; wait 2 seconds for floppy drive to switch off
    move.l  #100,d1
    jsr     _LVODelay(a6)
.no_floppy
	; stop cdtv device if found, avoids that cd device
	; sends spurious interrupts
    move.l  #CMD_STOP,d0
    bsr send_cdtv_command

.startup
    lea  _custom,a5
    move.b  #0,controller_joypad_1
    

; no multitask
    tst.l   _resload
    bne.b   .no_forbid
    move.l  _gfxbase(pc),a4
    move.l StartList(a4),gfxbase_copperlist
	move.l	36(a4),old_actiview		; gb_ActiView
	
    move.l  4,a6
    jsr _LVOForbid(a6)
	sub.l	A1,A1
	jsr	_LVOFindTask(a6)		;find ourselves
	move.l	D0,A0
	move.l	#-1,pr_WindowPtr(A0)	;�no more system requesters (insert volume, write protected...)
    
    
    sub.l   a1,a1
    move.l  a4,a6
    jsr (_LVOLoadView,a6)
    jsr (_LVOWaitTOF,a6)
    jsr (_LVOWaitTOF,a6)
.no_forbid

    move.w  #STATE_INTRO_SCREEN,current_state
    
    
    IFND    RECORD_INPUT_TABLE_SIZE
    ; uncomment to test demo mode right now
    ;;st.b    demo_mode
    ENDC
    
    bsr init_sound
    
    ; shut off dma
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    move.w  #$7FFF,(intreq,a5)
    move.w #$03E0,dmacon(A5)

    bsr init_interrupts
    ; intro screen
    
    
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
    

    lea game_palette,a0
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
    move.w #$30C1,diwstop(a5)             ; la fen�tre �cran
    move.w #$0038,ddfstrt(a5)             ; et le DMA bitplane
    move.w #$00D0,ddfstop(a5)
    move.w #$4200,bplcon0(a5) ; 4 bitplanes
    clr.w bplcon1(a5)                     ; no scrolling
    clr.w bplcon2(a5)                     ; pas de priorit�
    move.w #0,bpl1mod(a5)                ; modulo de tous les plans = 40
    move.w #0,bpl2mod(a5)

intro:
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    move.w  #$7FFF,(intreq,a5)

    ; small random to change ghosts names from time to time (1 out of 5 after a game over)
    lea character_text_table_en(pc),a0
    cmp.w    #1,vbl_counter
    bne.b   .en
    lea character_text_table_jp(pc),a0
.en
    move.l  a0,character_text_table
    
    bsr stop_sounds    
    
    bsr hide_sprites

    bsr clear_screen
    
    bsr draw_score

    clr.w  state_timer
    clr.w  vbl_counter

   
    bsr wait_bof
    ; init sprite, bitplane, whatever dma
    move.w #$83E0,dmacon(a5)
    move.w #INTERRUPTS_ON_MASK,intena(a5)    ; enable level 6!!
    


.intro_loop    
    cmp.w   #STATE_INTRO_SCREEN,current_state
    bne.b   .out_intro
    tst.b   quit_flag
    bne.b   .out
    move.l  joystick_state(pc),d0
    btst    #JPB_BTN_RED,d0
    beq.b   .intro_loop
    clr.b   demo_mode
.out_intro    
    clr.w   state_timer
    move.w  #STATE_GAME_START_SCREEN,current_state
    
.release
    move.l  joystick_state(pc),d0
    btst    #JPB_BTN_RED,d0
    bne.b   .release

    tst.b   demo_mode
    bne.b   .no_credit
    lea credit_sound(pc),a0
    bsr play_fx

.game_start_loop
    move.l  joystick_state(pc),d0
    tst.b   quit_flag
    bne.b   .out
    btst    #JPB_BTN_RED,d0
    beq.b   .game_start_loop

.no_credit
    
.restart    
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    
    bsr init_new_play

    IFD INTERMISSION_TEST
    move.w  #INTERMISSION_TEST,level_number
    ENDC

.new_level  
    bsr clear_screen
    bsr draw_score    
    bsr init_level
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)

    
    ; on some levels, there's an intermission sequence

    cmp.w  #FIRST_INTERMISSION_LEVEL,level_number
    beq.b   .intermission
    cmp.w  #SECOND_INTERMISSION_LEVEL,level_number
    beq.b   .intermission
    cmp.w  #THIRD_INTERMISSION_LEVEL,level_number
    beq.b   .intermission
    cmp.w  #FOURTH_INTERMISSION_LEVEL,level_number
    bne.b   .no_intermission
.intermission
    
    bsr wait_bof

    subq.w  #1,level_number
    bsr draw_bonuses
    addq.w  #1,level_number

    clr.w  state_timer

    move.w  #STATE_INTERMISSION_SCREEN,current_state
    move.w #INTERRUPTS_ON_MASK,intena(a5)
  
.intermission_loop
    cmp.w   #STATE_INTERMISSION_SCREEN,current_state
    bne.b   .out_intermission
    tst.b   quit_flag
    bne.b   .out
    move.l  joystick_state(pc),d0
    btst    #JPB_BTN_RED,d0
    bne.b   .out_intermission
    bra.b   .intermission_loop   
.out_intermission    
    clr.w   state_timer    
    
    lea _custom,a5
    move.w  #$7FFF,(intena,a5)
    bsr stop_sounds
.no_intermission
    bsr wait_bof
    
    ; do it first, as the last bonus overwrites bottom left of screen
    bsr draw_bonuses    
    bsr draw_maze
   
    ; for debug
    ;;bsr draw_bounds
    
    bsr draw_dots
    

    bsr hide_sprites

    ; enable copper interrupts, mainly
    moveq.l #0,d0
    bra.b   .from_level_start
.new_life
    moveq.l #1,d0
.from_level_start
    bsr init_ghosts
    bsr init_player
    
    bsr wait_bof

    bsr draw_lives
    move.w  #STATE_PLAYING,current_state
    move.w #INTERRUPTS_ON_MASK,intena(a5)
.mainloop
    tst.b   quit_flag
    bne.b   .out
    DEF_STATE_CASE_TABLE
    
.game_start_screen
.intro_screen       ; not reachable from mainloop
    bra.b   intro
.intermission_screen
.playing
.level_completed
    bra.b   .mainloop
.game_over
    bra.b   .mainloop
.next_level
    add.w   #1,level_number
    bra.b   .new_level
.life_lost
    IFD    RECORD_INPUT_TABLE_SIZE
    lea record_input_table,a0
    move.l  record_data_pointer(pc),a1
    ; pause so debugger can grab data
    blitz
    ENDC

    tst.b   demo_mode
    beq.b   .no_demo
    ; lose one life in demo mode: return to intro
    move.w  #STATE_GAME_OVER,current_state
    move.w  #1,state_timer
    bra.b   .game_over
.no_demo
    ; life lost, make next start a little easier by
    ; locking elroy mode
    st.b    elroy_mode_lock
    ; note down that the ghost release system changes now
    ; that a life was lost
    st.b    a_life_was_lost
    ; global dot counter for ghosts to exit pen is reset
    clr.b   ghost_release_dot_counter
    
    subq.b   #1,nb_lives
    bne.b   .new_life

    ; save highscores if whdload
    tst.l   _resload
    beq.b   .no_save
    bsr     save_highscores
.no_save
    ; 3 seconds
    move.w  #ORIGINAL_TICKS_PER_SEC*3,state_timer
    move.w  #STATE_GAME_OVER,current_state
    bra.b   .game_over
.out      
    ; quit
    tst.l   _resload
    beq.b   .normal_end
    
    ; quit whdload
	pea	TDREASON_OK
	move.l	_resload(pc),-(a7)
	addq.l	#resload_Abort,(a7)
	rts
    
.normal_end
    bsr     finalize_sound
    bsr     restore_interrupts
    bsr     wait_blit
	; restart CDTV device
    move.l  #CMD_START,d0
    bsr send_cdtv_command
    bsr     save_highscores

    lea _custom,a5
    move.l  gfxbase_copperlist,cop1lc(a5) ; adresse du d�but de la liste
    move.l  _gfxbase,a6
	move.l	old_actiview,a1
	jsr	_LVOLoadView(a6)

    ;;move.w #$8060,dmacon(a5)        ; r�initialisation du canal DMA
    
    move.l  4.W,A6
    move.l  _gfxbase,a1
    jsr _LVOCloseLibrary(a6)
    move.l  _dosbase,a1
    jsr _LVOCloseLibrary(a6)
    
    jsr _LVOPermit(a6)                  ; Task Switching autoris�
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
    

clear_debug_screen
    movem.l d0-d1/a1,-(a7)
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1 
    move.w  #NB_LINES-1,d1
.c0
    move.w  #NB_BYTES_PER_MAZE_LINE/4-1,d0
.cl
    clr.l   (a1)+
    dbf d0,.cl
    add.w   #NB_BYTES_PER_LINE-NB_BYTES_PER_MAZE_LINE,a1
    dbf d1,.c0
    movem.l (a7)+,d0-d1/a1
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

; < A1: plane start    
clear_maze_plane
    move.l #NB_LINES-1,d0
.cp
    move.w  #NB_BYTES_PER_MAZE_LINE/4-1,d1
    move.l  a1,a0
.cl
    clr.l   (a0)+
    dbf d1,.cl
    add.l   #NB_BYTES_PER_LINE,a1
    dbf d0,.cp
    rts
    
init_new_play:
    ; global init at game start
    move.l  #demo_moves,record_data_pointer
    clr.l   replayed_input_state
    move.b  #4,nb_lives
    clr.b   extra_life_awarded
    clr.b    music_played
    move.w  #START_LEVEL-1,level_number
    clr.l   score
    clr.l   displayed_score
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
    clr.b  nb_dots_eaten
    clr.b   elroy_mode_lock
    clr.b   a_life_was_lost
    
    ; sound loop
    clr.w   loop_index

    ; global dot counter for ghosts to exit pen
    clr.b   ghost_release_dot_counter
    
    ; speed table
    add.w   d2,d2
    lea speed_table(pc),a1
    move.l  (a1,d2.w),a1    ; global speed table
    move.l  a1,global_speed_table

    ; timer just in case pacman doesn't eat any dots
    move.w  #4*ORIGINAL_TICKS_PER_SEC,d0
    cmp.w   #5,level_number
    bcs.b   .below_level_5
    move.w  #3*ORIGINAL_TICKS_PER_SEC,d0        ; 3 seconds from level 5
.below_level_5    
    move.w  d0,ghost_release_override_max_time
    clr.w   ghost_release_override_timer
    
    rts

; draw score with titles and extra 0
draw_score:
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
    
    ; extra 0
    lea score_string(pc),a0
    move.w  #232,d0
    add.w  #8,d1
    bsr write_string

    move.l  score(pc),d2
    bsr     draw_current_score
    
    move.l  high_score(pc),d2
    bsr     write_high_score

    lea level_string(pc),a0
    move.w  #232,d0
    move.w  #48+24,d1
    bsr write_string

    moveq.l #1,d2
    add.w  level_number(pc),d2
    move.w  #232+48,d0
    move.w  #48+24+8,d1
    move.w  #3,d3
    bra write_decimal_number

    rts
    
; < D2 score
; trashes D0-D3
draw_current_score:
    move.w  #232+16,d0
    move.w  #24,d1
    move.w  #6,d3
    bra write_decimal_number
    
    
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
    lea  sprites,a0
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

; < D0: 0 if start of a level, 1 if a life has been lost

PEN_XY_PINKY = (RED_XSTART_POS>>3)<<16+(OTHERS_YSTART_POS>>3)

init_ghosts
    move.b  d0,d4
    lea ghosts(pc),a0
    lea ghost_sprites,a1   ; the sprite part of the copperlist, sprite 1-7 are the ghost sprites
    lea .behaviour_table(pc),a2
    lea game_palette+32(pc),a3  ; the sprite part of the color palette 16-31
    ; shared settings
    moveq.w #3,d7
    lea _custom+color+32,a4
    
    ; init ghost dot count table, start with pinky
    ; pinky exits right away unless a life is lost
    move.l  #ghosts+1*Ghost_SIZEOF,ghost_which_counts_dots

    
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
    
    tst.b   d4
    bne.b   .no_reset

    clr.w   speed_table_index(a0)
    clr.b   pen_nb_dots(a0)
.no_reset    
    clr.w   mode_counter(a0)
    clr.w   pen_timer(a0)
    clr.b   reverse_flag(a0)
    clr.b   pen_exit_override_flag(a0)
    clr.w   h_speed(a0)
    clr.w   v_speed(a0)
    clr.w   target_xtile(a0)
    clr.w   target_ytile(a0)
    clr.w   home_corner_xtile(a0)
    clr.w   home_corner_ytile(a0)
    clr.b   flashing_as_white(a0)
    
    move.l  #PEN_XY_PINKY,pen_xtile(a0)     ; default pen position: pinky
    move.w  #RED_XSTART_POS,xpen(a0)
    move.w  #OTHERS_YSTART_POS,ypen(a0)
    
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
    move.b  (a2)+,pen_nb_dots(a0)
    add.l   #Ghost_SIZEOF,a0
    dbf d7,.igloop2
.skip_dot_init    
    
    
    ; specific settings
    lea ghosts(pc),a0
    ; red ghost
	move.w  #RED_XSTART_POS,xpos(a0)
	move.w	#RED_YSTART_POS,ypos(a0)
    move.w  #LEFT,direction(a0)
    move.w  #0,frame(a0)
    move.l  #'BLIN',character_id(a0)

    clr.b   pen_dot_limit(a0)   
    move.l  #red_ghost_frame_table,frame_table(a0)
    move.l  #red_frightened_ghost_white_frame_table,frightened_ghost_white_frame_table(a0)
    move.l  #red_frightened_ghost_blue_frame_table,frightened_ghost_blue_frame_table(a0)
    move.l  #red_ghost_eye_frame_table,eye_frame_table(a0)
    move.l  #red_tunnel_frame,tunnel_frame(a0)
    move.l  #red_target_frame,target_frame(a0)
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
    move.b  #7,pen_dot_limit(a0)    
    move.l  #pink_ghost_frame_table,frame_table(a0)
    move.l  #pink_frightened_ghost_white_frame_table,frightened_ghost_white_frame_table(a0)
    move.l  #pink_frightened_ghost_blue_frame_table,frightened_ghost_blue_frame_table(a0)
    move.l  #pink_ghost_eye_frame_table,eye_frame_table(a0)
    move.l  #pink_tunnel_frame,tunnel_frame(a0)
    move.l  #pink_target_frame,target_frame(a0)
    bsr     update_ghost_target
    move.w  #0,home_corner_ytile(a0)
    move.w  #1,v_speed(a0)

    add.l   #Ghost_SIZEOF,a0
    ; cyan ghost
    move.l  #'INKY',character_id(a0)
	move.w  #(RED_XSTART_POS-16),xpos(a0)
    move.w  #UP,direction(a0)
    move.w  #0,frame(a0)
    move.b  #17,pen_dot_limit(a0)
    subq.w  #1,pen_xtile(a0)
    move.w  xpos(a0),xpen(a0)
    move.l  #cyan_ghost_frame_table,frame_table(a0)
    move.l  #cyan_frightened_ghost_white_frame_table,frightened_ghost_white_frame_table(a0)
    move.l  #cyan_frightened_ghost_blue_frame_table,frightened_ghost_blue_frame_table(a0)
    move.w  #(NB_TILES_PER_LINE-4),home_corner_xtile(a0)
    move.w  #(NB_TILE_LINES+1),home_corner_ytile(a0) 
    move.l  #cyan_tunnel_frame,tunnel_frame(a0)
    move.l  #cyan_ghost_eye_frame_table,eye_frame_table(a0)
    move.l  #cyan_target_frame,target_frame(a0)
    bsr update_ghost_target
    move.w  #-1,v_speed(a0)
    ; orange ghost
    add.l   #Ghost_SIZEOF,a0
    move.l  #'CLYD',character_id(a0)

    move.b  #32,pen_dot_limit(a0)    
    addq.w  #1,pen_xtile(a0)
	move.w  #(RED_XSTART_POS+16),xpos(a0)
    move.w  xpos(a0),xpen(a0)
    move.w  #UP,direction(a0)
    move.w  #0,frame(a0)
    move.l  #orange_ghost_frame_table,frame_table(a0)
    move.l  #orange_frightened_ghost_white_frame_table,frightened_ghost_white_frame_table(a0)
    move.l  #orange_frightened_ghost_blue_frame_table,frightened_ghost_blue_frame_table(a0)
    move.l  #orange_ghost_eye_frame_table,eye_frame_table(a0)
    move.l  #orange_tunnel_frame,tunnel_frame(a0)
    move.l  #orange_target_frame,target_frame(a0)
    move.w  #(NB_TILE_LINES+1),home_corner_ytile(a0)
    bsr update_ghost_target
    move.w  #-1,v_speed(a0)

    rts
    
.dot_counter_table_level_1
    dc.b    0,0,30,60
.dot_counter_table_level_2
    dc.b    0,0,0,50
    even

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
    ; inky
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
    lsr.w   #3,d2   ; ghost X tile
    sub.w   d0,d2
    ; sub that distance to create symmetrical point
    sub.w   d2,d0
    ; for Y
    move.w  ypos(a2),d2
    lsr.w   #3,d2   ; ghost Y tile
    sub.w   d1,d2
    sub.w   d2,d1
    ; that's it! simple, yet effective
    movem.l (a7)+,d2/a2
    rts
    
; clyde/orange ghost behaviour

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
    move.w  home_corner_ytile(a0),d1
.simple
    rts
    
pinky_direction_offset_table
    dc.w    4,0     ; right
    dc.w    -4,0    ; left
    dc.w    -4,-4    ; up: left offset is added, this is an original game bug
    dc.w    0,4     ; down
inky_direction_offset_table
    dc.w    2,0     ; right
    dc.w    -2,0    ; left
    dc.w    -2,-2    ; up: left offset is added, this is an original game bug
    dc.w    0,2     ; down

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
    move.l  pen_xtile(a0),target_xtile(a0)  ; hack copy x & y at once
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
    move.w  level_number(pc),d0
    cmp.w   #4,d0
    bcs.b   .low
    move.w  #4,d0
.low
    add.w   d0,d0
    add.w   d0,d0
    lea     timer_table(pc),a1
    move.l  (a1,d0.w),a1
    move.w   mode_counter(a0),d0
    add.w   d0,d0
    move.w  (a1,d0.w),mode_timer(a0)
    movem.l (a7)+,d0/a1

    rts
    
init_player
    clr.b   loop_playing
    ; in case player was killed / level completed 
    ; when bonus was active
    bsr remove_bonus
    bsr remove_bonus_score
	
    lea player(pc),a0
    move.l  #'PACM',character_id(a0)
    ; added +1 to be 100% exact vs original positionning
	move.w  #RED_XSTART_POS+1,xpos(a0)
	move.w	#188+Y_START,ypos(a0)
	move.w 	#LEFT,direction(a0)
    clr.w  speed_table_index(a0)
    move.w  #-1,h_speed(a0)
    clr.w   v_speed(a0)
    clr.w   prepost_turn(a0)
    clr.b   still_timer(a0)
    move.w  #0,frame(a0)
    move.w  #ORIGINAL_TICKS_PER_SEC,D0   
    tst.b   music_played
    bne.b   .played
    st.b    music_played
    moveq.l #0,d0
    move.w  #262,d1     ; seems okay, matches intro music length
    move.w  d1,d0
    move.w  d0,first_ready_timer
    lsr.w   #1,d1
    move.w  d1,half_first_ready_timer
    IFD    RECORD_INPUT_TABLE_SIZE
    move.l  #ORIGINAL_TICKS_PER_SEC,d0 ; no start music when recording
    ELSE
    tst.b   demo_mode
    beq.b   .no_demo
    move.l  #ORIGINAL_TICKS_PER_SEC,d0 ; no start music when demo
.no_demo
    ENDC
.played
    IFD    RECORD_INPUT_TABLE_SIZE
    move.l  #record_input_table,record_data_pointer ; start of table
    clr.l   prev_record_joystick_state
    ENDC

    clr.w   record_input_clock                      ; start of time
    
    
    move.w  d0,ready_timer
    clr.l   previous_random
    move.w  #-1,player_killed_timer
    move.w  #-1,ghost_eaten_timer
    clr.w   next_ghost_score
    clr.w   fright_timer
    clr.b   eat_toggle
    
    move.w  #MSG_SHOW,ready_display_message
    
    bra get_bonus_pac_plane
    	    

DEBUG_X = 8     ; 232+8
DEBUG_Y = 8

ghost_debug
    lea ghosts(pc),a2
    move.w  #DEBUG_X,d0
    move.w  #DEBUG_Y+100,d1
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1 

    bsr .debug_ghost

    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .elroy(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.l  a2,a0
    move.l  d0,-(a7)
    bsr get_elroy_level
    move.w  d0,d2
    move.l  (a7)+,d0
    move.w  #0,d3
    bsr write_decimal_number
    
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .orange(pc),a0
    bsr write_string

    add.l   #Ghost_SIZEOF*3,a2
    bsr .debug_ghost

    
;    move.w  #DEBUG_X,d0
;    add.w  #8,d1
;    lea .dir(pc),a0
;    bsr write_string
;    lsl.w   #3,d0
;    add.w  #DEBUG_X,d0
;    clr.l   d2
;    move.w  direction(a2),d2
;    move.w  #0,d3
;    bsr write_decimal_number
;
;    move.w  #DEBUG_X,d0
;    add.w  #8,d1
;    lea .pdir(pc),a0
;    bsr write_string
;    lsl.w   #3,d0
;    add.w  #DEBUG_X,d0
;    clr.l   d2
;    move.w  possible_directions,d2
;    move.w  #4,d3
;    bsr write_hexadecimal_number
    rts
.debug_ghost
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .gx(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  target_xtile(a2),d2
    move.w  #3,d3
    bsr write_decimal_number
    add.w  #8,d1
    
    move.w  #DEBUG_X,d0
    lea .gy(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  target_ytile(a2),d2
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
    lea .modec(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.w  mode_counter(a2),d2
    move.w  #3,d3
    bra write_decimal_number
    
.timer
        dc.b    "MTIM ",0
.mode
        dc.b    "MODE ",0
.modec
        dc.b    "MODEC ",0
.elroy:
    dc.b    "ELROY ",0
.pink
    dc.b    "PINK",0
.orange
    dc.b    "CLYDE",0
.gx
        dc.b    "GX ",0
.gy
        dc.b    "GY ",0
        even

        
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
    move.b nb_dots_eaten(pc),d2
    move.w  #3,d3
    bsr write_decimal_number
    ; ---
    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .dots2(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    move.l  d0,d3
    bsr count_dots
    move.l  #TOTAL_NUMBER_OF_DOTS,d2
    sub.b d0,d2
    move.l  d3,d0   
    move.w  #3,d3
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

    move.w  #DEBUG_X,d0
    add.w  #8,d1
    lea .dottable(pc),a0
    bsr write_string
    lsl.w   #3,d0
    add.w  #DEBUG_X,d0
    clr.l   d2
    move.l #dot_table,d2
    move.w  #8,d3
    bsr write_hexadecimal_number

    rts
    
.px
        dc.b    "PX ",0
.py
        dc.b    "PY ",0

.dots
        dc.b    "DOTC1 ",0
.dots2
        dc.b    "DOTC2 ",0
.bonus
        dc.b    "BT ",0
.dottable:
        dc.b    "DTA !",0
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
    bra.b hide_sprites
.normal
    lea ghosts(pc),a0
    moveq.l #3,d7
    tst.w   draw_ghost_as_repaired
    beq.b   .gloop
    moveq.l #0,d7   ; draw only first ghost
.gloop
    move.w  xpos(a0),d0
    ; too on the right, don't draw sprite
    cmp.w   #X_MAX,d0
    bcs.b   .do_display
    moveq.l #0,d0
    bra.b   .ssp
.do_display
    move.w  d0,d6
    sub.w   #X_MAX-16,d6
    bcs.b   .no_tunnel_right
    ; D6 = 1-15: sprite shift to the right
    move.w  #X_MAX-16,d0    ; but fixed display position
    bra.b   .tunnel   ; D6 is computed, skip
.no_tunnel_right
    ; tunnel left?
    move.w  d0,d6
    sub.w   #8+X_START,d6
    bcc.b   .no_tunnel_left
    ; tunnel left: leave X at 0
    ; D6 = number of shift to the left, negated
    moveq.w #8+X_START,d0
    bra.b   .tunnel   ; D6 is computed, skip
.no_tunnel_left
    clr.w   d6      ; reset to 0
.tunnel
    move.w  ypos(a0),d1
    ; center => top left
    sub.w  #8+X_START,d0
    sub.w  #4+Y_START,d1    ; not 8, because maybe table is off?
    bsr store_sprite_pos
.ssp
    move.w  mode(a0),d3 ; scatter/chase/fright/return base
    cmp.w   #MODE_EYES,d3
    beq.b   .eyes

    lea     palette(a0),a2      ; normal ghost colors

    move.l  frame_table(a0),a1
    tst.w   draw_ghost_as_repaired
    beq.b   .no_repaired
    ; special case intermission screen
    lea     repaired_frame_table(pc),a1
.no_repaired    
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
    lea     frightened_ghosts_blue_palette(pc),a2
    ; select proper palette (blue/white)
    tst.b   flashing_as_white(a0)
    beq.b   .no_white
    ; white flashing
    lea     frightened_ghosts_white_palette(pc),a2
.no_white
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
    ; directly change color registers for that sprite
    move.l  color_register(a0),a3
    move.l  (a2)+,(a3)+
    move.l  (a2)+,(a3)+

    ; get proper frame from proper frame set
    move.l  (a1,d2.w),a1
    ; now if D6 is non-zero, handle shift
    tst.w   d6
    beq.b   .no_sprite_shifting
    ; copy current frame in temp ghost frame

    move.l  (tunnel_frame,a0),a2
    addq.l  #4,a1   ; skip control word
    addq.l  #4,a2   ; skip control word

    bpl.b   .shift_right
.shift_left
    move.w  #15,d2
    neg.w   d6
.scopy_left:
    move.l  (a1)+,d3
    lsl.w   d6,d3
    swap    d3
    lsl.w   d6,d3
    swap    d3
    move.l  d3,(a2)+
    dbf     d2,.scopy_left
    move.l  (tunnel_frame,a0),a1
    bra.b   .no_sprite_shifting
.shift_right
    move.w  #15,d2
.scopy_right:
    move.l  (a1)+,d3
    lsr.w   d6,d3
    swap    d3
    lsr.w   d6,d3
    swap    d3
    move.l  d3,(a2)+
    dbf     d2,.scopy_right
    move.l  (tunnel_frame,a0),a1    ; temp frame
.no_sprite_shifting    
    move.l  d0,(a1)     ; store control word
    move.l  a1,d2    
    move.l  copperlist_address(a0),a1
    move.w  d2,(6,a1)
    swap    d2
    move.w  d2,(2,a1)    
    
    tst.b   debug_flag
    bne.b   .debug_targets
.next_ghost
    add.l   #Ghost_SIZEOF,a0
    dbf d7,.gloop
    rts
    
.debug_targets    
    ; draw targets (debug mode)
    ; A1-8: other sprite of the same palette (ghosts use 1-7)
    move.w  target_xtile(a0),d0
    lsl.w   #3,d0
    move.w  target_ytile(a0),d1
    lsl.w   #3,d1
    sub.w  #8+X_START,d0
    bpl.b   .posx
    moveq   #0,d0
.posx
    cmp.w   #268,d1
    bcs.b   .ylow
    sub.w   #16,d1
.ylow
    sub.w  #4+Y_START,d1    ; not 8, because maybe table is off?
    bpl.b   .posy
    moveq   #0,d1
.posy
    bsr     store_sprite_pos
    move.l  (target_frame,a0),a1
    move.l  d0,(a1)
    move.l  a1,d2
    move.l  copperlist_address(a0),a1
    move.w  d2,(6+8,a1)
    swap    d2
    move.w  d2,(2+8,a1)    
    bra.b   .next_ghost

.eyes
    move.l eye_frame_table(a0),a1
    move.w   direction(a0),d2
    lea ghost_eyes(pc),a2       ; palette

    ; TODO set black tunnel sprite proper palette in copperlist
    bra.b   .end_anim
     
draw_all
    DEF_STATE_CASE_TABLE

; draw intro screen
.intro_screen
    bra.b   draw_intro_screen
; draw intro screen
.intermission_screen
    cmp.w   #FIRST_INTERMISSION_LEVEL,level_number
    beq.b   draw_intermission_screen_level_2
    cmp.w   #SECOND_INTERMISSION_LEVEL,level_number
    beq.b   draw_intermission_screen_level_5
    cmp.w   #THIRD_INTERMISSION_LEVEL,level_number
    beq.b   draw_intermission_screen_level_9
    cmp.w   #FOURTH_INTERMISSION_LEVEL,level_number
    beq.b   draw_intermission_screen_level_9
    rts
    
.game_start_screen
    tst.w   state_timer
    beq.b   draw_start_screen
    rts
    
.level_completed
.life_lost
.next_level

    ; don't do anything
    rts
PLAYER_ONE_X = 72
PLAYER_ONE_Y = 102-14

    
.game_over
    bsr clear_bonus
    bsr write_game_over
    bra.b   .draw_complete
.playing
    ; update sound loops here, loop frequency is based on VBL
    bsr update_extra_life_sound_loop

    move.w  ready_timer(pc),d0
    bmi.b   .ready_end
    ; should we clear one life & remove "player one" message ?
    cmp.w   #MSG_SHOW,player_one_and_life_display_message
    bne.b   .no_draw_player_one
    clr.w   player_one_and_life_display_message
    move.w  #PLAYER_ONE_X,d0
    move.w  #PLAYER_ONE_Y,d1
    move.w  #$00ff,d2
    lea player_one_string(pc),a0
    bsr write_color_string
    bra.b   .no_clr_player_one
.no_draw_player_one

    cmp.w   #MSG_HIDE,player_one_and_life_display_message
    bne.b   .no_clr_player_one
    clr.w   player_one_and_life_display_message
    bsr draw_lives          ; remove last life / place pac on screen
    ; remove "PLAYER ONE" message
    move.w  #PLAYER_ONE_X,d0
    move.w  #PLAYER_ONE_Y,d1
    move.w  #$00ff,d2
    lea player_one_string_clear(pc),a0
    bsr write_color_string
    
.no_ready_clr        
.no_clr_player_one
    move.w  ready_timer(pc),d0
    ; should we display ghosts/pacman ?
    cmp.w   half_first_ready_timer(pc),d0
    bcc.b   .after_draw
.ready_end    
    bsr draw_ghosts
    bsr draw_pacman
.after_draw
        
    ; timer not running, animate
    bsr animate_power_pills

    cmp.w   #MSG_SHOW,extra_life_message
    bne.b   .no_extra_life
    clr.w   extra_life_message
    bsr     draw_last_life
.no_extra_life
    cmp.w   #MSG_HIDE,bonus_score_display_message
    bne.b   .no_bonus_score_disappear
    clr.w   bonus_score_display_message
    move.w  #BONUS_X_POS-8,d0
    move.w  #BONUS_Y_POS,d1
    move.w  #5,d2
    ; we know that this color is only on 2 planes
    ; (convieniently 2 planes where pacman isn't drawn!)
    lea	screen_data,a1
    move.w  #3,d3
    bsr clear_plane_any
    add.l  #SCREEN_PLANE_SIZE*3,a1
    bsr clear_plane_any
.no_bonus_score_disappear    
    ; bonus
    cmp.w   #MSG_SHOW,bonus_display_message
    bne.b   .no_fruit_appear
    clr.w   bonus_display_message
    ; blit fruit
    move.w  #BONUS_X_POS,d0
    move.w  #BONUS_Y_POS,d1
    move.w  level_number(pc),d2
    bsr draw_bonus
    bra.b   .no_fruit_disappear
.no_fruit_appear
    cmp.w   #MSG_HIDE,bonus_display_message
    bne.b   .no_fruit_disappear
    clr.w   bonus_display_message
    bsr clear_bonus
.no_fruit_disappear
    cmp.w   #MSG_SHOW,bonus_score_display_message
    bne.b   .no_bonus_score_appear
    clr.w   bonus_score_display_message
    move.w  fruit_score_index(pc),d0
    bsr draw_bonus_score
.no_bonus_score_appear

    bsr handle_ready_text

    ; score
    lea	screen_data+SCREEN_PLANE_SIZE*3,a1  ; white
    
    move.l  score(pc),d0
    move.l  displayed_score(pc),d1
    cmp.l   d0,d1
    beq.b   .no_score_update
    
    move.l  d0,displayed_score

    move.l  d0,d2
    bsr draw_current_score
    
    ; handle highscore in draw routine eek
    move.l  high_score(pc),d4
    cmp.l   d2,d4
    bcc.b   .no_score_update
    ; high score
    st.b    highscore_needs_saving
    
    move.l  d2,high_score
    bsr write_high_score
.no_score_update
    tst.w   bonus_timer
    beq.b   .no_bonus_redraw
    cmp.w   #BONUS_Y_POS+Y_START+8,player+ypos
    bne.b   .no_bonus_redraw
    bsr wait_blit       ; wait for pacman to draw
    bsr draw_bonus_pac_plane
.no_bonus_redraw
    tst.b   demo_mode
    beq.b   .no_demo
    ;;bsr wait_blit       ; wait for pacman to draw
    bsr write_game_over
.no_demo
.draw_complete
    rts

stop_loop_fx:
    ; stop loop, not really required with the new ptplayer
    ; version though
    tst.b   loop_playing
    beq.b   .no_loop
    clr.b   loop_playing
    moveq.l #0,d0
    bra     _mt_stopfx      ; stop sound loop
.no_loop
    rts
stop_sounds
	clr.b	music_playing		; indicates to pause test flag that music isn't playing
    bsr stop_loop_fx
    lea _custom,a6
    bra _mt_end

handle_ready_text
    cmp.w   #MSG_HIDE,ready_display_message
    bne.b   .no_ready_clr
    clr.w   ready_display_message
    ; remove "READY!" message
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  #88,d0
    move.w  #136,d1
    move.w  #14,d2   ; 96
    bsr clear_plane_any
.no_ready_clr    
    tst.w   ready_timer
    bmi.b   .ready_off

    tst.b   demo_mode
    bne.b   .ready_off
    
    cmp.w   #MSG_SHOW,ready_display_message
    bne.b   .ready_off
    clr.w   ready_display_message
    move.w  #88,d0
    move.w  #136,d1
    move.w  #$0ff0,d2
    lea ready_string(pc),a0
    bsr write_color_string
.ready_off
    rts
    
; < D2: highscore
write_high_score
    move.w  #232+16,d0
    move.w  #24+32,d1
    move.w  #6,d3
    bra write_decimal_number
    
write_game_over
    move.w  #72,d0
    move.w  #136,d1
    move.w  #$0f00,d2   ; red
    lea game_over_string(pc),a0
    bra write_color_string
    
animate_power_pills    
    move.w  power_pill_timer(pc),d0
    cmp.w   #1,d0
    bcc.b   .nospd
    lea  powerdots(pc),a0
    moveq.l #3,d0
.drawpdloop
    move.l  (a0)+,d1 
    beq.b   .zap_draw
    move.l  d1,a1
    bsr draw_power_pill
.zap_draw    
    dbf d0,.drawpdloop
    bra.b   .powerdot_done
.nospd
    cmp.w   #BLINK_RATE/2+2,d0
    beq.b   .toggle
    cmp.w   #BLINK_RATE/2+1,d0
    beq.b   .toggle
    cmp.w   #BLINK_RATE/2,d0
    bne.b   .powerdot_done
.toggle
    lea  powerdots(pc),a0
    moveq.l #3,d0
.clrpdloop
    move.l  (a0)+,d1
    beq.b   .zap_clear
    move.l  d1,a1
    bsr clear_power_pill
.zap_clear    
    dbf d0,.clrpdloop
.powerdot_done
    rts
    
; < D0: score (/10)
add_to_score:
    add.l   d0,score
    tst.b  extra_life_awarded
    bne.b   .no_play
    ; was below, check new score
    cmp.l   #EXTRA_LIFE_SCORE,score    ; is current score above xtra life score
    bcs.b   .no_play        ; not yet
    
    move.b  #1,extra_life_awarded
    move.w  #MSG_SHOW,extra_life_message
    addq.b   #1,nb_lives
    move.l A0,-(a7)
    move.w  #10,extra_life_sound_counter
    clr.w   extra_life_sound_timer
    move.l  (a7)+,a0
.no_play
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

X_TEXT = 56
Y_TEXT = 16
GHOST_DESC_HEIGHT = 24

X_DOT = 120
Y_DOT = 160

Y_PAC_ANIM = 136
X_DEMO_POWER_PILL = 48
DEMO_PACMAN_TIMER = NB_TICKS_PER_SEC*14
DEMO_DOT_SCORE_TIMER = NB_TICKS_PER_SEC*12
DEMO_POWER_PILL_TIMER = NB_TICKS_PER_SEC*13

DRAW_GHOST_INFO:MACRO
    cmp.w   #NB_TICKS_PER_SEC*(\2*3+1),state_timer
    bne.b   .no_show_\1
    moveq.l #\2,d0
    bra.b .draw_ghost_bob
.no_show_\1
    cmp.w   #NB_TICKS_PER_SEC*(\2*3+2),state_timer
    bne.b   .no_show_\1_text
    moveq.l #\2,d0
    bsr .draw_ghost_text
    move.w  d0,.nb_written
    rts
.no_show_\1_text
    cmp.w   #NB_TICKS_PER_SEC*(\2*3+2)+(NB_TICKS_PER_SEC/2)+1,state_timer
    bne.b   .no_show_\1_text_2
    moveq.l #\2,d0
    bra .draw_ghost_text
.no_show_\1_text_2
    ENDM
    
draw_start_screen
    bsr hide_sprites
    bsr clear_screen
    lea .psb_string(pc),a0
    move.w  #48,d0
    move.w  #100,d1
    move.w  #$0fb5,d2
    bsr write_color_string
    lea .opo_string(pc),a0
    move.w  #48+16,d0
    move.w  #136,d1
    move.w  #$00ff,d2
    bsr write_color_string
    lea .bp1_string(pc),a0
    move.w  #16,d0
    move.w  #172,d1
    move.w  #$FBB,d2
    bsr write_color_string
    
    ; namco logo
    lea namco,a0
    lea screen_data,a1
    move.w  #48+16+16,d0
    move.w  #172+36,d1
    move.w  #10,d2
    moveq.l #-1,d3
    move.w  #8,d4
    bsr blit_plane_any
    lea screen_data+3*SCREEN_PLANE_SIZE,a1
    move.w  #48+16+16,d0
    move.w  #172+36,d1
    bsr blit_plane_any
    bsr wait_blit
    
    lea .namco_string(pc),a0
    move.w  #48+16,d0
    move.w  #172+36,d1
    move.w  #$0fbf,d2
    bsr write_color_string
    
    rts
.psb_string
    dc.b    "PUSH START BUTTON",0
.opo_string:
    dc.b    "1 PLAYER ONLY",0
.bp1_string
    dc.b    "BONUS PACMAN FOR 10000 pts",0
.namco_string
    dc.b    "c########1980",0
    even
draw_intro_screen
    tst.w   state_timer
    bne.b   .no_first
    lea character_nickname_string(pc),a0
    move.w  #X_TEXT,d0
    move.w  #Y_TEXT,d1
    move.w  #$fff,d2
    bra.b write_color_string
.no_first
    
    DRAW_GHOST_INFO red,0
    DRAW_GHOST_INFO pink,1
    DRAW_GHOST_INFO cyan,2
    DRAW_GHOST_INFO orange,3

    cmp.w   #DEMO_DOT_SCORE_TIMER,state_timer
    bne.b   .no_dot_score
    lea	screen_data+DOT_PLANE_OFFSET+X_DOT/8+Y_DOT*NB_BYTES_PER_LINE,a1
    bsr draw_dot
    move.w  #X_DOT,d0
    move.w  #Y_DOT,d1
    lea     ten_pts_string(pc),a0
    move.w  #$FFF,d2
    bsr write_color_string
   
    
    lea	screen_data+DOT_PLANE_OFFSET+X_DOT/8+(Y_DOT+16)*NB_BYTES_PER_LINE,a1
    lea  powerdots+4(pc),a0
    move.l  a1,(a0)+
    bsr draw_power_pill
    ; 2 other power dots invalidated
    clr.l   (a0)+
    clr.l   (a0)+
    move.w  #X_DOT,d0
    move.w  #Y_DOT+16,d1
    lea     fifty_pts_string(pc),a0
    move.w  #$FFF,d2
    bsr write_color_string
.no_dot_score

    cmp.w   #DEMO_POWER_PILL_TIMER,state_timer
    bne.b   .no_power_pill
    lea	screen_data+DOT_PLANE_OFFSET+X_DEMO_POWER_PILL/8+Y_PAC_ANIM*NB_BYTES_PER_LINE,a1
    lea  powerdots(pc),a0
    move.l  a1,(a0)+
    bsr draw_power_pill
        
    ; namco logo
    lea namco,a0
    lea screen_data,a1
    move.w  #X_DOT,d0
    move.w  #224,d1
    move.w  #10,d2
    moveq.l #-1,d3
    move.w  #8,d4
    bsr blit_plane_any
    lea screen_data+3*SCREEN_PLANE_SIZE,a1
    move.w  #X_DOT,d0
    move.w  #224,d1
    bsr blit_plane_any
.no_power_pill
    
    cmp.w   #DEMO_PACMAN_TIMER,state_timer
    bcs.b   .dont_draw_characs
    ; blit pacman
    bsr draw_ghosts
    bsr draw_pacman
    
    ; animate dots
    bsr animate_power_pills
.dont_draw_characs    
    rts
    
.nb_written
    dc.w    0
    
.draw_ghost_bob
    lea ghost_bob_table,a1
    move.w  d0,d3
    move.w  d0,d2
    add.w   d2,d2
    add.w   d2,d2
    move.l  (a1,d2.w),a0    ; ghost bob    
    move.w  #X_TEXT-24,d0
    move.w  #Y_TEXT+12,d1
    mulu.w  #GHOST_DESC_HEIGHT,d3
    add.w   d3,d1
    
    bra blit_4_planes
    
.draw_ghost_text
    move.l character_text_table(pc),a0
    move.w  d0,d3
    move.w  d0,d4
    lsl.w   #4,d3
    lea  (a0,d3.w),a0    ; text table
    move.l  (8,a0),d2   ; color
    move.w  .nb_written(pc),d0
    beq.b   .no_text_2
    addq.l  #4,a0
.no_text_2
    clr.w   .nb_written
    lsl.w   #3,d0
    add.w  #X_TEXT+8,d0
    move.w  #Y_TEXT+16,d1
    move.l  (a0),a0 ; text
    mulu.w  #GHOST_DESC_HEIGHT,d4
    add.w   d4,d1

    bra write_color_string



    
; < D0: 0,1,2,... score index 100,200 (not possible),300,500,700,1000,2000,3000,5000 ...

THOUSAND_LIMIT = 5

draw_bonus_score:
    move.w  d0,d3
    cmp.w   #THOUSAND_LIMIT,d0
    bcs.b   .under_thousand
    sub.w   #THOUSAND_LIMIT,d0
.under_thousand
    lsl.w   #6,d0       ; *64
    lea bonus_scores,a0
    add.w   d0,a0
    lea	screen_data+BONUS_X_POS/8+BONUS_Y_POS*NB_BYTES_PER_LINE,a1
    move.l  a1,a2
    ; change x when >= 1000
    moveq.w  #10,d0
    cmp.w   #THOUSAND_LIMIT,d3
    bcs.b   .no_extra_zero1
    ; extra zero: x is more to the left
    moveq.l  #6,d0    
.no_extra_zero1
    moveq.l  #0,d1
    moveq.l #-1,d2
    bsr blit_plane
    lea (SCREEN_PLANE_SIZE*3,a2),a1
    moveq.l  #10,d0
    cmp.w   #THOUSAND_LIMIT,d3
    bcs.b   .no_extra_zero2
    moveq.l  #6,d0    
.no_extra_zero2
    moveq.l  #0,d1
    moveq.l #-1,d2
    bsr blit_plane
    
    cmp.w   #THOUSAND_LIMIT,d3
    bcs.b   .no_extra_zero
    bsr wait_blit       ; wait else blit is going to write concurrently
    add.l  #NB_BYTES_PER_LINE*4,a2
    ; add an extra zero character to the right
    move.l  #%00110000000,d0
    move.l  #%01001000000,d1
    moveq.w #1,d2
    addq.w  #1,a2   ; else a2 is odd: crashes on 68000/010
.orloop
    or.w    d0,(a2)
    or.w    d1,(NB_BYTES_PER_LINE,a2)
    or.w    d1,(NB_BYTES_PER_LINE*2,a2)
    or.w    d1,(NB_BYTES_PER_LINE*3,a2)
    or.w    d1,(NB_BYTES_PER_LINE*4,a2)
    or.w    d1,(NB_BYTES_PER_LINE*5,a2)
    or.w    d0,(NB_BYTES_PER_LINE*6,a2)
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
    
draw_last_life:
    lea pac_lives,a0
    move.b  nb_lives(pc),d3
    ext     d3
    subq.w  #2,d3
    bmi.b   .out
    lsl.w   #4,d3
    add.w #NB_BYTES_PER_MAZE_LINE*8,d3
    moveq.l #-1,d2  ; mask

    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  d3,d0
    moveq.l #0,d1
    bsr blit_plane
.out
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
    rts
    
draw_bonuses:
    move.w #NB_BYTES_PER_MAZE_LINE*8,d0
    move.w #248-32,d1
    move.w  level_number(pc),d2
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
    
; what: prepare aligned longwords to restore
; bonus plane eaten by pacman
; we'll use cpu OR, and not blitter
get_bonus_pac_plane:
    movem.l d0-d3/a0-a1,-(a7)

    move.w  level_number(pc),d0
    cmp.w   #12,d0
    bcs.b   .ok
    move.w  #12,d0  ; maxed 
.ok
    add.w   d0,d0
    lea bonus_level_table(pc),a0
    move.w  (a0,d0.w),d3      ; bonus index * 320

    ; fixed fruit pos
    lea bonus_plane_cached+1(pc),a1
    
    mulu.w  #10,d3
    swap    d3
    clr     d3
    swap    d3
    lsl.l   #5,d3
    lea bonus_pics+64,a0
    add.l   d3,a0    ; bonus second bitplane

    ; 2 byte writes to avoid odd word write
    REPT    16
    move.b  (REPTN*4,a0),(4*REPTN,a1)
    move.b  (REPTN*4+1,a0),(4*REPTN+1,a1)
    ENDR
    movem.l (a7)+,d0-d3/a0-a1
    rts

bonus_plane_cached
    ds.l    16,0
        
; what: restore bonus plane (or'ing) when pacman is around

draw_bonus_pac_plane
    movem.l d0/a0-a1,-(a7)

    ; fixed fruit pos
    lea bonus_plane_cached(pc),a0
    lea screen_data+SCREEN_PLANE_SIZE+BONUS_X_POS/8+BONUS_Y_POS*NB_BYTES_PER_LINE-1,a1  ; -1: even
    
    REPT    16
    move.l  (REPTN*4,a0),d0
    or.l  d0,(NB_BYTES_PER_LINE*REPTN,a1)
    ENDR
    movem.l (a7)+,d0/a0-a1
    rts
    
clear_bonus:
    move.w  #BONUS_X_POS,d0
    move.w  #BONUS_Y_POS,d1
    move.w  #4,d2
    lea	screen_data,a1
    move.w  #3,d3
.cloop
    bsr clear_plane_any
    add.l  #SCREEN_PLANE_SIZE,a1
    dbf d3,.cloop
    rts
    
; < D0: X
; < D1: Y
; < D2: level number

draw_bonus:
    movem.l d0-d3/a0-a1,-(a7)
    cmp.w   #12,d2
    bcs.b   .ok
    move.w  #12,d2  ; maxed 
.ok
    add.w   d2,d2
    lea bonus_level_table(pc),a0
    move.w  (a0,d2.w),d3      ; bonus index
    lea mul40_table(pc),a1
    add.w   d3,d3
    move.w  (a1,d3.w),d3    ; *40
    lsl.w   #3,d3           ; *8 => * 320
    lea bonus_pics,a0
    add.w   d3,a0    ; bonus bitplanes
    bsr blit_4_planes
    movem.l (a7)+,d0-d3/a0-a1
    rts
    
draw_maze:    
    lea screen_data+SCREEN_PLANE_SIZE,a1
    bsr clear_playfield_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bsr clear_playfield_plane
    add.w   #SCREEN_PLANE_SIZE,a1
    bsr clear_playfield_plane

    lea screen_data,a1
draw_maze_plane:
    ; copy maze data in bitplanes
    lea maze_data(pc),a0
    move.w  #NB_MAZE_LINES-1,d0
.copyline
    move.w  #6,d1
.copylong
    move.l  (a0)+,(a1)+
    dbf d1,.copylong
    add.l  #12,a1
    ; init planes in copperlist (after colors)
    dbf d0,.copyline
    

    rts    

; < A1: plane start
clear_playfield_plane
    movem.l d0-d1/a0-a1,-(a7)
    move.w #NB_LINES-1,d0
.cp
    move.w  #NB_BYTES_PER_MAZE_LINE/4-1,d1
    move.l  a1,a0
.cl
    clr.l   (a0)+
    dbf d1,.cl
    add.l   #NB_BYTES_PER_LINE,a1
    dbf d0,.cp
    movem.l (a7)+,d0-d1/a0-a1
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

; what: debug function to scan dot table & count dots
; We have counter but just in case there's a problem...    
; > D0: number of dots
; trashes: none

count_dots:
    movem.l d1-d3/a0,-(a7)
    moveq.l #0,d3
    ; start with an offset (skip the fake 3 first rows)
    lea dot_table+(Y_START/8)*NB_TILES_PER_LINE,a0
      
    move.w  #NB_TILE_LINES-1-(Y_START/8),d0    
.loopy
    move.w  #NB_TILES_PER_LINE-1,d1
.loopx
    move.b  (a0)+,d2
    beq.b   .next
    cmp.b   #1,d2
    ; draw small dot
    bne.b   .big
    addq.l  #1,d3
    bra.b   .next
.big
    addq.l  #1,d3
.next
    dbf d1,.loopx
    dbf d0,.loopy
    
    move.l  d3,d0
    movem.l (a7)+,d1-d3/a0
    rts
    
draw_dots:
    ; draw pen gate
    lea	screen_data+(RED_YSTART_POS-15)*NB_BYTES_PER_LINE+(RED_XSTART_POS-X_START)/8-1,a1
    moveq.l #-1,d0
    move.b  d0,(a1)
    move.b  d0,(NB_BYTES_PER_LINE,a1)
    move.b  d0,(1,a1)
    move.b  d0,(NB_BYTES_PER_LINE+1,a1)
    add.l   #SCREEN_PLANE_SIZE*3,a1
    move.b  d0,(a1)
    move.b  d0,(NB_BYTES_PER_LINE,a1)
    move.b  d0,(1,a1)
    move.b  d0,(NB_BYTES_PER_LINE+1,a1)

    ; init dots
    lea     powerdots(pc),a2
    lea dot_table_read_only(pc),a0
    lea dot_table,a1
    move.l  #NB_TILE_LINES*NB_TILES_PER_LINE-1,d0
.copy
    move.b  (a0)+,(a1)+
    dbf d0,.copy

    ; start with an offset (skip the fake 3 first rows)
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
    bsr draw_dot
    bra.b   .next
.big
    move.l  a1,(a2)+        ; store powerdot address
    bsr draw_power_pill

.next
    addq.l  #1,a1
    dbf d1,.loopx
    add.l  #NB_BYTES_PER_LINE-NB_TILES_PER_LINE,a1
    add.l   #NB_BYTES_PER_LINE*7,a1
    dbf d0,.loopy
    rts

; < A1 address
; trashes: none
draw_power_pill
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
clear_power_pill
    clr.b  (a1)
    clr.b  (NB_BYTES_PER_LINE*1,a1)
    clr.b  (NB_BYTES_PER_LINE*2,a1)
    clr.b  (NB_BYTES_PER_LINE*3,a1)
    clr.b  (NB_BYTES_PER_LINE*4,a1)
    clr.b  (NB_BYTES_PER_LINE*5,a1)
    clr.b  (NB_BYTES_PER_LINE*6,a1)
    clr.b  (NB_BYTES_PER_LINE*7,a1)
    rts

draw_dot:
    move.b  #%0011000,(NB_BYTES_PER_LINE*3,a1)
    move.b  #%0011000,(NB_BYTES_PER_LINE*4,a1)
    rts
    
; < A1 address
clear_dot
    clr.b  (NB_BYTES_PER_LINE*3,a1)
    clr.b  (NB_BYTES_PER_LINE*4,a1)
    rts
    
init_sound
    ; init phx ptplayer, needs a6 as custom, a0 as vbr (which is zero)
    sub.l   a0,a0
    moveq.l #1,d0
    lea _custom,a6
    jsr _mt_install_cia
    rts
    
init_interrupts
    lea _custom,a6
    sub.l   a0,a0

    move.w  (dmaconr,a6),old_dmacon
    move.w  (intenar,a6),old_intena

    sub.l   a0,a0
    ; assuming VBR at 0
    lea saved_vectors(pc),a1
    move.l  ($8,a0),(a1)+
    move.l  ($c,a0),(a1)+
    move.l  ($10,a0),(a1)+
    move.l  ($68,a0),(a1)+
    move.l  ($6C,a0),(a1)+

    lea   exc8(pc),a1
    move.l  a1,($8,a0)
    lea   excc(pc),a1
    move.l  a1,($c,a0)
    lea   exc10(pc),a1
    move.l  a1,($10,a0)
    
    lea level2_interrupt(pc),a1
    move.l  a1,($68,a0)
    
    lea level3_interrupt(pc),a1
    move.l  a1,($6C,a0)
    
    
    rts
    
exc8
    blitz
    nop
    rte
excc
    blitz
    nop
    nop
    rte
exc10
    blitz
    nop
    nop
    nop
    rte
    
finalize_sound
    ; assuming VBR at 0
    bsr stop_sounds
    sub.l   a0,a0
    lea _custom,a6
    bsr _mt_remove_cia
    rts
    
restore_interrupts:
    ; assuming VBR at 0
    sub.l   a0,a0
    
    lea saved_vectors(pc),a1
    move.l  (a1)+,($8,a0)
    move.l  (a1)+,($c,a0)
    move.l  (a1)+,($10,a0)
    move.l  (a1)+,($68,a0)
    move.l  (a1)+,($6C,a0)


	lea	_custom,a6
	move.w	old_intena,d0			; get old val
	and.w	#$3fff,d0
	eor.w	#$3fff,d0
	move.w	d0,intena(a6)		; clr bits
	eor.w	#$3fff,d0
	or.w	#$c000,d0		
	move.w	d0,intena(a6)		; set bits and main

	move.w	old_dmacon,d0			; get old val
	and.w	#$3fff,d0
	eor.w	#$3fff,d0


	move.w	d0,dmacon(a6)		; clr bits



    rts
    
saved_vectors
        dc.l    0,0,0   ; some exceptions
        dc.l    0   ; keyboard
        dc.l    0   ; vblank
        dc.l    0   ; cia b
old_dmacon
    dc.w    0
old_intena
    dc.w    0

; what: level 2 interrupt (keyboard)
; args: none
; trashes: none
    
level2_interrupt:
	movem.l	D0/A0/A5,-(a7)
	LEA	$00BFD000,A5
	MOVEQ	#$08,D0
	AND.B	$1D01(A5),D0
	BEQ	.nokey
	MOVE.B	$1C01(A5),D0
	NOT.B	D0
	ROR.B	#1,D0		; raw key code here
    lea keyboard_table(pc),a0
    and.w   #$FF,d0
    bclr    #7,d0
    seq (a0,d0.w)       ; updates keyboard table
    bne.b   .no_playing     ; we don't care about key release
    ; cheat key activation sequence
    move.l  cheat_sequence_pointer(pc),a0
    cmp.b   (a0)+,d0
    bne.b   .reset_cheat
    move.l  a0,cheat_sequence_pointer
    tst.b   (a0)
    bne.b   .cheat_end
    move.w  #$0FF,_custom+color    
    st.b    cheat_keys
.reset_cheat
    move.l  #cheat_sequence,cheat_sequence_pointer
.cheat_end
    
    cmp.b   #$45,d0
    bne.b   .no_esc
    cmp.w   #STATE_INTERMISSION_SCREEN,current_state
    beq.b   .no_esc
    cmp.w   #STATE_INTRO_SCREEN,current_state
    beq.b   .no_esc
    cmp.w   #STATE_GAME_START_SCREEN,current_state
    beq.b   .no_esc
    move.w  #ORIGINAL_TICKS_PER_SEC*2,state_timer
    move.w  #STATE_GAME_OVER,current_state
.no_esc
    
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_playing
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_playing
    tst.b   demo_mode
    bne.b   .no_pause
    cmp.b   #$19,d0
    bne.b   .no_pause
    tst.b   music_playing
    bne.b   .no_pause
    eor.b   #1,pause_flag
.no_pause    
    tst.w   cheat_keys
    beq.b   .no_playing
        
    cmp.b   #$50,d0
    bne.b   .no_lskip
    bsr     level_completed
.no_lskip
    cmp.b   #$51,d0
    bne.b   .no_invincible
    eor.b   #1,invincible_cheat_flag
    move.b  invincible_cheat_flag(pc),d0
    beq.b   .x
    move.w  #$F,d0
.x
    and.w   #$FF00,d0
    or.w  #$0F0,d0
    move.w  d0,_custom+color
    bra.b   .no_playing
.no_invincible
    cmp.b   #$52,d0
    bne.b   .no_teleport
    ; teleport some ghosts to tunnels entrances
    move.l  a0,-(a7)
    lea ghosts(pc),a0
    ; red
    move.w  #X_MAX-16,xpos(a0)
    move.w  #OTHERS_YSTART_POS,ypos(a0)
    move.w  #RIGHT,direction(a0)
    move.w  #1,h_speed(a0)
    clr.w   v_speed(a0)
    ; pink
    ;;add.l   #Ghost_SIZEOF,a0
    ;;move.w  #0,xpos(a0)
    ;;move.w  #OTHERS_YSTART_POS,ypos(a0)
    ;;move.w  #RIGHT,direction(a0)
    ;;move.w  #1,h_speed(a0)
    ;;clr.w   v_speed(a0)
    
    move.l  (a7)+,a0
    bra.b   .no_playing
.no_teleport
    cmp.b   #$53,d0     ; F4
    bne.b   .no_debug
    ; show/hide debug info
    eor.b   #1,debug_flag
    ; clear left part of white plane screen
    bsr     clear_debug_screen
    bra.b   .no_playing
.no_debug
    cmp.b   #$54,d0     ; F5
    bne.b   .no_bonus
    move.b  #3,dot_table+BONUS_OFFSET
    move.w  #BONUS_TIMER_VALUE,bonus_timer
    move.w  #MSG_SHOW,bonus_display_message
    bra.b   .no_playing
.no_bonus

.no_playing

    cmp.b   _keyexit(pc),d0
    bne.b   .no_quit
    st.b    quit_flag
.no_quit

	BSET	#$06,$1E01(A5)
	move.l	#2,d0
	bsr	beamdelay
	BCLR	#$06,$1E01(A5)	; acknowledge key

.nokey
	movem.l	(a7)+,d0/a0/a5
	move.w	#8,_custom+intreq
	rte
    
; < D0: numbers of vertical positions to wait
beamdelay
.bd_loop1
	move.w  d0,-(a7)
    move.b	$dff006,d0	; VPOS
.bd_loop2
	cmp.b	$dff006,d0
	beq.s	.bd_loop2
	move.w	(a7)+,d0
	dbf	d0,.bd_loop1
	rts

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
    tst.b   pause_flag
    bne.b   .outcop
.no_pause
    ; copper
    bsr draw_all
    tst.b   debug_flag
    beq.b   .no_debug
    bsr draw_debug
.no_debug
    bsr update_all
    move.w  vbl_counter(pc),d0
    addq.w  #1,d0
    cmp.w   #5,d0
    bne.b   .normal
    ; update a second time, simulate 60Hz
    bsr update_all
    moveq.w #0,d0    
.normal
    move.w  d0,vbl_counter
.outcop    
    move.w  #$0010,(intreq,a5) 
    movem.l (a7)+,d0-a6
    rte    
.vblank
    move.l  joystick_state(pc),d2
    moveq.l #1,d0
    bsr _read_joystick
    btst    #JPB_BTN_BLU,d0
    beq.b   .no_second
    btst    #JPB_BTN_BLU,d2
    bne.b   .no_second
    
    ; no pause if not in game or music is playing
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_second
    tst.b   demo_mode
    bne.b   .no_second
    tst.b   music_playing
    bne.b   .no_second
    
    eor.b   #1,pause_flag
.no_second
    lea keyboard_table(pc),a0
    tst.b   ($40,a0)    ; up key
    beq.b   .no_fire
    bset    #JPB_BTN_RED,d0
.no_fire 
    tst.b   ($4C,a0)    ; up key
    beq.b   .no_up
    bset    #JPB_BTN_UP,d0
    bra.b   .no_down
.no_up    
    tst.b   ($4D,a0)    ; down key
    beq.b   .no_down
	; set DOWN
    bset    #JPB_BTN_DOWN,d0
.no_down    
    tst.b   ($4F,a0)    ; left key
    beq.b   .no_left
	; set LEFT
    bset    #JPB_BTN_LEFT,d0
    bra.b   .no_right   
.no_left
    tst.b   ($4E,a0)    ; right key
    beq.b   .no_right
	; set RIGHT
    bset    #JPB_BTN_RIGHT,d0
.no_right        
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


; what: updates game state
; args: none
; trashes: potentially all registers

update_all

    DEF_STATE_CASE_TABLE

.intro_screen
    bra update_intro_screen
.intermission_screen
    cmp.w   #FIRST_INTERMISSION_LEVEL,level_number
    beq.b   update_intermission_screen_level_2
    cmp.w   #SECOND_INTERMISSION_LEVEL,level_number
    beq.b   update_intermission_screen_level_5
    cmp.w   #THIRD_INTERMISSION_LEVEL,level_number
    beq.b   update_intermission_screen_level_9
    cmp.w   #FOURTH_INTERMISSION_LEVEL,level_number
    beq.b   update_intermission_screen_level_9
    ; other levels
    rts
    
.game_start_screen
    tst.w   state_timer
    bne.b   .out
    add.w   #1,state_timer
.out    
    rts
    
.life_lost
    rts  ; bra update_power_pill_flashing

.level_completed
	bsr		clear_bonus
    subq.w  #1,maze_blink_timer
    bne.b   .no_change
    move.w  #MAZE_BLINK_TIME,maze_blink_timer
    subq.b  #1,maze_blink_nb_times
    beq.b   .next_level
    lea     screen_data,a2
    lea     screen_data+3*SCREEN_PLANE_SIZE,a1
    eor.b   #1,.color_blue
    beq.b   .chcol
.orig
    exg a1,a2
.chcol
    ;move.w  d0,_custom+color+2
    bsr     clear_maze_plane
    move.l  a2,a1
    bsr     draw_maze_plane
.no_change
    rts
.color_blue
    dc.w    0
    
.next_level
     move.w  #STATE_NEXT_LEVEL,current_state
     move.w #ORIGINAL_TICKS_PER_SEC,ready_timer
     rts
     
.game_over
    bsr stop_sounds

    tst.w   state_timer
    bne.b   .cont
    move.w  #STATE_INTRO_SCREEN,current_state
.cont
    subq.w  #1,state_timer
    rts
.playing
    ; for demo mode
    addq.w  #1,record_input_clock

    move.w   first_ready_timer(pc),d0
    cmp.w   ready_timer(pc),d0
    bne.b   .no_first_tick
    move.w  #MSG_SHOW,player_one_and_life_display_message
    moveq.l #0,d0
    bsr play_music
.no_first_tick


    tst.w   ready_timer
    bmi.b   .ready_off
    bne.b   .dec
    ; 0
    move.w  #MSG_HIDE,ready_display_message
    ; start music
    bsr stop_sounds
    bsr start_background_loop
.dec
    move.w  half_first_ready_timer(pc),d0
    cmp.w   ready_timer(pc),d0
    bne.b   .no_half
    move.w  #MSG_HIDE,player_one_and_life_display_message
    subq.b  #1,nb_lives     ; artificially
.no_half

    subq.w  #1,ready_timer
    rts
.ready_off

    bsr update_power_pill_flashing
    move.w  ghost_eaten_timer(pc),d6
    bmi.b   .update_pac_and_ghosts
    subq.w  #1,d6    
    move.w  d6,ghost_eaten_timer
    bne.b   .no_sound_loop_change
    lea     loop_eyes_sound(pc),a0
    bsr     play_loop_fx
.no_sound_loop_change    
    rts
.update_pac_and_ghosts
    ; collisions are checked more often to avoid the infamous
    ; "pass through" bug. I would have loved to keep it, but it
    ; seems that it happens a lot more with my version for an unknown reason
    ; so since I'm not too short in CPU I'm performing the check twice as often
    ; as soon as either pacman or the ghosts move
    bsr update_pac
    bsr check_pac_ghosts_collisions
    bsr update_ghosts
    bra check_pac_ghosts_collisions

remove_bonus
    move.b  #0,dot_table+BONUS_OFFSET
    move.w  #MSG_HIDE,bonus_display_message      ; tell draw routine to clear
    clr.w   bonus_timer
    rts
remove_bonus_score
    move.w  #MSG_HIDE,bonus_score_display_message      ; tell draw routine to clear
    clr.w   bonus_score_timer
    rts
update_power_pill_flashing
    ; power dot blink timer
    subq.w  #1,power_pill_timer
    bpl.b   .no_reload
    move.w  #BLINK_RATE,power_pill_timer
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
    tst.b   invincible_cheat_flag
    bne.b   .nomatch    
    move.w  #PLAYER_KILL_TIMER,player_killed_timer
    bra stop_sounds

    
.pac_eats_ghost:
a_ghost_was_eaten:
    move.w  #MODE_EYES,mode(a4)
    ; test display score with the proper color (reusing pink sprite palette)
    move.w  #GHOST_KILL_TIMER,ghost_eaten_timer
    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_sound
    lea     ghost_eaten_sound(pc),a0
    bsr     play_fx
.no_sound
    
    move.w  next_ghost_score(pc),d0
    add.w   #1,next_ghost_score
    add.w   d0,d0
    add.w   d0,d0
    lea  score_value_table(pc),a0
    move.l  d0,-(a7)
    move.l   (a0,d0.w),d0
    bsr add_to_score
    move.l  (a7)+,d0
    lea  score_frame_table(pc),a0
    move.l  (a0,d0.w),d0
    move.l  d0,score_frame
    
    ; exits as soon as a collision is found
    rts
update_intro_screen
    add.w   #1,state_timer
    cmp.w   #4,next_ghost_score
    bne.b   .remaining_ghosts
    ; wait a while and go in press button to start game
    move.w  state_timer(pc),d1
    sub.w   last_ghost_eaten_state_timer(pc),d1
    cmp.w   #NB_TICKS_PER_SEC,d1
    bcs.b   .wait
    ; change state
    clr.w   state_timer
    move.w  #STATE_GAME_START_SCREEN,current_state
    ; in demo mode
    st.b    demo_mode
    
.wait
    rts
.remaining_ghosts
    cmp.w   #DEMO_PACMAN_TIMER,state_timer
    bne.b   .no_pac_demo_anim_init
    clr.w   .skip_3_frames
    bsr init_player
    moveq.l #0,d0
    bsr init_ghosts
    lea player(pc),a2
    clr.w   .move_period
    move.w  #X_MAX,xpos(a2)
    move.w  #Y_PAC_ANIM+28,ypos(a2)
    lea ghosts(pc),a3
    moveq   #3,d0
    moveq.w #0,d1
.ginit
    move.w  #X_MAX+24,xpos(a3)
    add.w   d1,xpos(a3)
    add.w   #16,d1
    move.w  ypos(a2),ypos(a3)
    move.w  #LEFT,direction(a3)
    move.w  #-1,h_speed(a3)
    add.l   #Ghost_SIZEOF,a3
    dbf d0,.ginit
.no_pac_demo_anim_init
    cmp.w   #DEMO_PACMAN_TIMER,state_timer
    bcs.b   .no_pac_demo_anim
    
    bsr update_power_pill_flashing
    move.w  ghost_eaten_timer(pc),d6
    bmi.b   .update_pac_and_ghosts
    subq.w  #1,d6
    move.w  d6,ghost_eaten_timer
    bra.b   .no_pac_demo_anim
    
.update_pac_and_ghosts    
    lea     player(pc),a4
    lea     ghosts(pc),a3
    move.w  h_speed(a4),d0      ; speed
    move.w  h_speed(a3),d1      ; speed
    add.w   #1,.move_period

    move.w  .move_period(pc),d5
    cmp.w   #-1,h_speed(a3)
    beq.b   .ghost_attack
    move.w  d5,d6
    and.w   #1,d6
    bne.b   .ghost_attack
    clr.w   d1  ; slower ghosts
.ghost_attack
    
    cmp.w   #16,d5
    bne.b   .no_move_reset
    clr.w   .move_period
    cmp.w   #-1,h_speed(a3)
    bne.b   .ghosts_slow
    ; pacman doesn't move this time
    clr.w   d0
    bra.b   .no_move_reset
.ghosts_slow
    ; ghosts don't move
    ; pacman moves 1 more
    addq.w  #1,d0
    clr.w   d1
.no_move_reset
    tst.w   d0
    beq.b   .no_pac_anim
    bsr animate_pacman
    
.no_pac_anim
    tst.w   d1
    beq.b   .no_ghost_anim
    moveq.w #3,d7
.ganim
    ; update ghost animations but don't move
    ; apply speed on ghosts
    add.w   d1,(xpos,a3)
    move.w  frame(a3),d2
    addq.w  #1,d2
    and.w   #$F,d2
    move.w  d2,frame(a3)
    add.w   #Ghost_SIZEOF,a3
    dbf d7,.ganim
.no_ghost_anim
    ; check if pacman is stopped
    tst.w   .skip_3_frames
    beq.b   .okmove
    subq.w  #1,.skip_3_frames
    moveq   #0,d0
.okmove
    move.w  (xpos,a4),d2
    add.w   d0,d2 ; pac
    cmp.w   #LEFT,direction(a4)
    bne.b   .pacman_chasing

    lea ghosts(pc),a3
    cmp.w   #1,h_speed(a3)
    beq.b   .storex ; powerpill taken already
    cmp.w   #X_DEMO_POWER_PILL+8,d2
    bne.b   .storex
    ; eat dot, don't turn around immediately
    move.w  #3,.skip_3_frames
    lea  powerdots(pc),a0
    tst.l   (a0)
    beq.b   .noclr
    move.l  (a0),a1
    clr.l   (a0)
    bsr clear_power_pill
.noclr
    
    lea ghosts(pc),a3
    move.w  #3,d7
.floop
    move.l  color_register(a3),a1
    lea     frightened_ghosts_blue_palette(pc),a2
    ; directly change color registers for that sprite
    move.l  (a2)+,(a1)+
    move.l  (a2)+,(a1)+
    move.w  #MODE_FRIGHT,mode(a3)
    move.w  #1,h_speed(a3)
    add.l   #Ghost_SIZEOF,a3
    dbf d7,.floop
  
    lea ghosts(pc),a3
    move.w  #RIGHT,d3
    move.w  d3,(direction,a3)
    move.w  d3,(direction+Ghost_SIZEOF,a3)
    move.w  d3,(direction+Ghost_SIZEOF*2,a3)
    move.w  d3,(direction+Ghost_SIZEOF*3,a3)
    
    bra.b   .storex
.pacman_chasing
    ; check collisions
    move.w  d2,d3
    lsr.w   #3,d3
    moveq.w #3,d7
    
.cloop
    cmp.w   #MODE_EYES,mode(a3)
    beq.b   .no_eat

    move.w  xpos(a3),d4
    lsr.w   #3,d4
    cmp.w   d4,d3
    bne.b   .no_eat
    move.w  #X_MAX*2,xpos(a3)   ; hidden
    exg a3,a4       ; routine expects ghost structure in a4, not a3
    bsr a_ghost_was_eaten
    exg a3,a4
    move.w  state_timer,last_ghost_eaten_state_timer
    bra.b   .storex
.no_eat
    add.w   #Ghost_SIZEOF,a3
    dbf d7,.cloop
.storex
    cmp.w   #X_DEMO_POWER_PILL+4,d2
    bcc.b  .storex2
    ; turn around now
    move.w  #1,h_speed(a4)
    move.w  #RIGHT,direction(a4)
    
.storex2
    move.w  d2,(xpos,a4)
.no_pac_demo_anim
    rts
.skip_3_frames
    dc.w    0
.move_period
    dc.w    0

; - intermission sequences (timing from YT longplay):
;  * 3"20 (after level 2): ghost chases pacman, big pacman chases back => DONE
;  * 7"50 (after level 5): ghost chases pacman, tears his drape on a nail => DONE
;  * 13"00 (after level 9): ghost chases pacman with repaired drape, but returns almost naked => DONE
;  * 18"00 (after level 13): same as before
    
; sorry for the very bad coding on non-interactive sequences
; (intermission/intro). Since it runs OK and always the same,
; code is hackish but guaranteed to do the same thing everytime
; (and coding those sequences is REALLY tedious)

NAIL_DRAPE_Y_OFFSET = 29
X_NAIL_HOOKED = X_MAX/2+26

draw_intermission_screen_level_5:
    tst.w   state_timer
    beq.b   .outd
    
    ; place nail
    lea  player(pc),a2
    move.w  #X_MAX/2,d0
    move.w  ypos(a2),d1
    sub.w   #NAIL_DRAPE_Y_OFFSET,d1
    bsr     store_sprite_pos
    
    tst.w   show_leg
    bne.b   .show_torn_drape
    
    move.w  nail_timer(pc),d1
    lsr.w   #3,d1
    cmp.w   #4,d1
    bcc.b   .no_drape_update
    add.w   d1,d1
    add.w   d1,d1
    lea red_ghost_drape_table(pc),a1
    move.l  (a1,d1.w),a0        ; nail/drape frame
    bra.b   .ssp
.show_torn_drape
    lea torn_drape,a0
.ssp
    move.l  d0,(a0)

    ; use sprite 1 for nail/drape
    lea     nail_sprite,a1
    move.l  a0,d0
    move.w  d0,(6,a1)
    swap    d0
    move.w  d0,(2,a1)
.no_drape_update
    
    ; show_leg
    
    tst.w   show_leg
    bne.b   .show_leg_sprite
    bsr draw_ghosts
    bra.b   .cont
.show_leg_sprite
    bsr hide_ghost_sprites
    lea ghosts(pc),a2
    move.w  xpos(a2),d0
    move.w  ypos(a2),d1
    ; center => top left
    sub.w  #10+X_START,d0
    sub.w  #8+Y_START,d1
    ; blit
    lea red_ghost_with_leg_left,a0
    cmp.w   #1,show_leg
    beq.b   .ok
    lea red_ghost_with_leg_up,a0
    
.ok
    bsr blit_4_planes
    
.cont
    tst.w   xpos(a2)
    bmi.b   .outd
    bsr draw_pacman
.outd    
    rts

draw_ghost_as_repaired:
    dc.w    0
    
; only left (both first values aren't used)
repaired_frame_table
    dc.l    repaired_0,repaired_1,repaired_0,repaired_1
    
red_ghost_drape_table:
        dc.l    nail,red_ghost_drape_1,red_ghost_drape_2,red_ghost_drape_3

update_intermission_screen_level_5
    tst.w   state_timer
    bne.b   .no_pac_demo_anim_init
    
    
    moveq.l  #1,d0
    bsr play_music
    
    clr.w   nail_timer
    clr.w   show_leg
    
    bsr init_player
    moveq.l #0,d0
    bsr init_ghosts
        
    lea player(pc),a2

    move.w  #X_MAX,xpos(a2)
    move.w  #Y_PAC_ANIM+28,ypos(a2)
    lea ghosts(pc),a3
    moveq   #3,d0
    moveq.w #0,d1
.ginit
    move.w   #400,xpos(a3)
    move.w  ypos(a2),ypos(a3)
    move.w  #LEFT,direction(a3)
    move.w  #0,h_speed(a3)
    add.l   #Ghost_SIZEOF,a3
    dbf d0,.ginit
    
    ; only red moves / is visible but is far away
    lea     ghosts(pc),a3
    move.w  #-1,h_speed(a3)    
    move.w  #X_MAX+128,xpos(a3)
    
    
.no_pac_demo_anim_init
    bsr handle_music_2_replay

    add.w   #1,state_timer
      
    lea     player(pc),a4
    lea     ghosts(pc),a3

    bsr animate_pacman

    ; update ghost animations but don't move
    ; apply speed on ghosts
    move.w  frame(a3),d2
    addq.w  #1,d2
    and.w   #$F,d2
    move.w  d2,frame(a3)


    move.w  (xpos,a4),d2
    sub.w   #1,d2 ; pac
    bmi.b   .nopm
    move.w  d2,(xpos,a4)
.nopm
    lea ghosts(pc),a3
    move.w  (xpos,a3),d2
    cmp.w   #X_NAIL_HOOKED,d2
    bcc.b   .full_speed

    move.w  nail_timer(pc),d0
    addq.w  #1,d0
    move.w  d0,nail_timer
    
    cmp.w   #X_NAIL_HOOKED-10,d2
    bcs.b   .nogm

    and.w   #7,d0       ; reduces speed
    bne.b   .nogm
.full_speed
    sub.w   #1,d2
.storex
    move.w  d2,(xpos,a3)
.nogm
    move.w  nail_timer(pc),d0
    cmp.w   #ORIGINAL_TICKS_PER_SEC*2,d0
    bne.b   .no_tearing
    move.w  #1,show_leg
.no_tearing    
    cmp.w   #ORIGINAL_TICKS_PER_SEC*3,d0
    bne.b   .no_tearing2
    move.w  #2,show_leg
.no_tearing2
    cmp.w   #$1B8,d0        ; end of second repeat of music
    bne.b   .no_end
    clr.w   state_timer
    move.w  #STATE_PLAYING,current_state
.no_end
    rts

show_leg:
    dc.w    0
nail_timer:
    dc.w    0
    
DEMO_TUNE_LEN = $13C
handle_music_2_replay
    move.w  state_timer(pc),d0
    cmp.w   #DEMO_TUNE_LEN*2+2,d0
    beq.b   stop_sounds
    cmp.w   #DEMO_TUNE_LEN,d0
    bne.b   .no_music_stop
    bra stop_sounds
.no_music_stop
    cmp.w   #DEMO_TUNE_LEN+2,d0
    bne.b   .no_music_replay
    moveq.l  #1,d0
    bra play_music
.no_music_replay
    rts
    
draw_intermission_screen_level_9:
    tst.w   state_timer
    beq.b   .outd
    
    lea ghosts(pc),a2
    
.cont
    cmp.w   #LEFT,direction(a2)
    bne.b   .right
    bsr draw_ghosts
    bsr draw_pacman
    bra.b   .outd
.right
    bsr hide_ghost_sprites
    move.w  xpos(a2),d0
    move.w  ypos(a2),d1
    ; center => top left
    sub.w  #8+Y_START,d1
    ; blit
    lea red_ghost_naked_0,a0
    cmp.w   #8,frame(a2)
    bcc.b   .dn
    lea red_ghost_naked_1,a0
.dn
    tst.w   d0
    bpl.b   .xpositive
    ; cheat
    add.w   #NB_BYTES_PER_LINE*8,d0
    subq.w  #1,d1    
.xpositive
    bsr .blit_naked_ghost
    bsr wait_blit
    ; clip start/end (too lazy to use masks here...)
    lea     screen_data+X_MAX/8-2+Y_PAC_ANIM*NB_BYTES_PER_LINE,a1
    lea     screen_data+Y_PAC_ANIM*NB_BYTES_PER_LINE-4,a3
    moveq   #3,d0
.cy    
    moveq   #13,d1
    move.l  a1,a2
    move.l  a3,a4
.cx
    clr.l   (a2)
    clr.l   (a4)
    add.w   #NB_BYTES_PER_LINE,a2
    add.w   #NB_BYTES_PER_LINE,a4
    dbf d1,.cx
    add.l   #SCREEN_PLANE_SIZE,a1
    add.l   #SCREEN_PLANE_SIZE,a3
    dbf d0,.cy
.outd
    rts
    
.blit_naked_ghost
    movem.l d2-d6/a0-a1/a5,-(a7)
    lea $DFF000,A5
    lea     screen_data,a1
    moveq.l #3,d6
.loop
    movem.l d0-d1/a1,-(a7)
    move.w  #6,d2       ; 32 pixels + 2 shift bytes
    moveq.l #-1,d3
    move.w  #16,d4      ; height
    bsr blit_plane_any_internal
    movem.l (a7)+,d0-d1/a1
    add.l   #SCREEN_PLANE_SIZE,a1
    add.l   #96,a0      ; 64 but shifting!
    dbf d6,.loop
.endblit
    movem.l (a7)+,d2-d6/a0-a1/a5
    rts
    
update_intermission_screen_level_9
    tst.w   state_timer
    bne.b   .no_pac_demo_anim_init
    move.w  #1,draw_ghost_as_repaired
    
    moveq.l #1,d0
    bsr play_music
    
    
    bsr init_player
    moveq.l #0,d0
    bsr init_ghosts
        
    lea player(pc),a2

    move.w  #X_MAX,xpos(a2)
    move.w  #Y_PAC_ANIM+28,ypos(a2)
    lea ghosts(pc),a3
    moveq   #3,d0
    moveq.w #0,d1
.ginit
    move.w   #400,xpos(a3)
    move.w  ypos(a2),ypos(a3)
    move.w  #LEFT,direction(a3)
    move.w  #0,h_speed(a3)
    add.l   #Ghost_SIZEOF,a3
    dbf d0,.ginit
    
    ; only red moves / is visible but is far away
    lea     ghosts(pc),a3
    move.w  #-1,h_speed(a3)    
    move.w  #X_MAX+96,xpos(a3)
    
    
.no_pac_demo_anim_init
    bsr handle_music_2_replay

    add.w   #1,state_timer
      
    lea     player(pc),a4

    bsr animate_pacman

    lea     ghosts(pc),a3
    ; update ghost animations but don't move

    move.w  frame(a3),d2
    addq.w  #1,d2
    and.w   #$F,d2
    move.w  d2,frame(a3)

    move.w  (xpos,a4),d2
    sub.w   #1,d2 ; pac
    bmi.b   .nopm
    move.w  d2,(xpos,a4)
.nopm

    tst.w   (h_speed,a3)
    beq.b   .going_left
    
.going_left
    move.w  (xpos,a3),d2
    tst.w   d2
    bpl.b   .storex
    ; reverse
    cmp.w   #RIGHT,(direction,a3)
    beq.b   .storex
    move.w  #1,(h_speed,a3)
    move.w  #RIGHT,(direction,a3)
    move.w #-32,d2
.storex
    add.w   (h_speed,a3),d2
    cmp.w   #RIGHT,(direction,a3)
    bne.b   .dostore
    tst.w   d2
    bmi.b   .dostore
    cmp.w   #X_MAX-20,d2
    bcc.b   .nogm
.dostore
    move.w  d2,(xpos,a3)
.nogm

    cmp.w   #$2B0,state_timer        ; end of second repeat of music
    bne.b   .no_end
    clr.w   state_timer
    clr.w   draw_ghost_as_repaired
    move.w  #STATE_PLAYING,current_state
.no_end
    rts

    
draw_intermission_screen_level_2:
    tst.w   state_timer
    beq.b   .outd
    
    ; blit pacman
    bsr draw_ghosts
    lea player(pc),a2
    cmp.w   #LEFT,direction(a2)
    beq.b   .small
    bsr draw_big_pacman
    bra.b   .outd
.small
    bsr draw_pacman
.outd    
    rts
    
update_intermission_screen_level_2
    tst.w   state_timer
    bne.b   .no_pac_demo_anim_init
    
    moveq.l #1,d0
    bsr play_music

    bsr init_player   
    
    moveq.l #0,d0
    bsr init_ghosts
        
    lea player(pc),a2
    clr.w   .move_period
    move.w  #X_MAX,xpos(a2)
    move.w  #Y_PAC_ANIM+28,ypos(a2)
    lea ghosts(pc),a3
    moveq   #3,d0
    moveq.w #0,d1
.ginit
    move.w   #400,xpos(a3)
    move.w  ypos(a2),ypos(a3)
    move.w  #LEFT,direction(a3)
    move.w  #0,h_speed(a3)
    add.l   #Ghost_SIZEOF,a3
    dbf d0,.ginit
    
    ; only red moves / is visible
    lea     ghosts(pc),a3
    move.w  #-1,h_speed(a3)    
    move.w  #X_MAX+24,xpos(a3)
    
    move.w  #ORIGINAL_TICKS_PER_SEC+ORIGINAL_TICKS_PER_SEC/2,.pac_wait_timer

.no_pac_demo_anim_init
    bsr handle_music_2_replay

    add.w   #1,state_timer
      
    lea     player(pc),a4
    lea     ghosts(pc),a3
    move.w  h_speed(a4),d0      ; speed
    move.w  h_speed(a3),d1      ; speed
    add.w   #1,.move_period

    move.w  .move_period(pc),d5
    cmp.w   #8,d5
    beq.b   .no_pac_demo_anim       ; nothing moves
    cmp.w   #-1,h_speed(a3)
    beq.b   .ghost_attack
    move.w  d5,d6
    and.w   #3,d6
    bne.b   .ghost_attack
    clr.w   d1  ; slower ghosts
.ghost_attack
    
    cmp.w   #16,d5
    bne.b   .no_move_reset
    clr.w   .move_period
    cmp.w   #-1,h_speed(a3)
    bne.b   .ghosts_slow
    ; pacman doesn't move this time
    clr.w   d0
    bra.b   .no_move_reset
.ghosts_slow
    ; ghosts don't move
    ; pacman moves 1 more
    addq.w  #1,d0
    clr.w   d1
.no_move_reset
    tst.w   d0
    beq.b   .no_pac_anim
    bsr animate_pacman
    
.no_pac_anim
    tst.w   d1
    beq.b   .no_ghost_anim

    ; update ghost animations but don't move
    ; apply speed on ghosts
    add.w   d1,(xpos,a3)
    move.w  frame(a3),d2
    addq.w  #1,d2
    and.w   #$F,d2
    move.w  d2,frame(a3)

.no_ghost_anim
    move.w  (xpos,a4),d2
    add.w   d0,d2 ; pac
    cmp.w   #LEFT,direction(a4)
    bne.b   .pacman_chasing

    lea ghosts(pc),a3
    cmp.w   #1,h_speed(a3)
    beq.b   .storex ; turn taken already
    cmp.w   #8,d2
    bne.b   .storex
    
    lea ghosts(pc),a3

    move.l  color_register(a3),a1
    lea     frightened_ghosts_blue_palette(pc),a2
    ; directly change color registers for that sprite
    move.l  (a2)+,(a1)+
    move.l  (a2)+,(a1)+
    move.w  #MODE_FRIGHT,mode(a3)
    move.w  #1,h_speed(a3)
  
    lea ghosts(pc),a3
    move.w  #RIGHT,d3
    move.w  d3,(direction,a3)
    bra.b   .storex2
.storex
    cmp.w   #4,d2
    bcc.b  .storex2
    tst.w   .pac_wait_timer
    beq.b   .reverse
    subq.w  #1,.pac_wait_timer
    bra.b   .no_pac_demo_anim
.reverse
    sub.w   #16,ypos(a4)        ; upper
    move.w  #1,h_speed(a4)
    move.w  #RIGHT,direction(a4)
    move.w  #0,xpos(a4)
    rts
    
.storex2
    move.w  d2,(xpos,a4)
.no_pac_demo_anim
    rts
.pacman_chasing
    cmp.w   #X_MAX+19,d2
    bcs.b   .storex2
    clr.w   state_timer
    move.w  #STATE_PLAYING,current_state
    rts
.move_period
     dc.w    0
.pac_wait_timer
     dc.w    0
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
    beq.b   .new_mode
    subq.w  #1,d0
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
    cmp.l   #orange_ghost,a4
    bne.b   .no_clyde
    ; clyde exits pen, unlock elroy mode
    clr.b   elroy_mode_lock
.no_clyde
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
    
    ; handle global timer to release ghosts (after a life was lost)
    tst.b   a_life_was_lost
    beq.b   .no_life_lost
    ; increase timer
    
    move.w  ghost_release_override_timer(pc),d0
    cmp.w   ghost_release_override_max_time(pc),d0
    beq.b   .max_time_reached
    addq.w  #1,d0
    move.w  d0,ghost_release_override_timer
    bra.b   .no_life_lost
    
.max_time_reached
    move.l  ghost_which_counts_dots(pc),a4  ; also the next ghost to leave pen
    clr.w   ghost_release_override_timer
    ; next ghost count dots using global counter
    add.l   #Ghost_SIZEOF,a4
    move.l  a4,ghost_which_counts_dots
    cmp.l   #ghosts+4*Ghost_SIZEOF,a4
    bcs.b   .no_last_ghost
    ; all ghosts are out: clear flag, elroy resumes as normal
    clr.b   elroy_mode_lock
.no_last_ghost
    ; release the next ghost (always valid, even if ghost_which_counts_dots
    ; points at the end of the list)
    st.b    (pen_exit_override_flag-Ghost_SIZEOF,a4)
    
.no_life_lost
    rts

; < A4: ghost structure    
.can_exit_pen
    cmp.l   #ghosts,a4
    beq.b   .exit_pen_ok        ; blinky cannot be trapped in the pen
    tst.b   pen_exit_override_flag(a4)
    bne.b   .exit_pen_ok
    tst.b   a_life_was_lost
    bne.b   .no_exit_pen
    tst.b   pen_nb_dots(a4)
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
    ; has reached target? (depends on ghost type)
    cmp.w   pen_xtile(a4),d4
    bne.b   .no_reverse
    cmp.w   pen_ytile(a4),d5
    bne.b   .no_reverse
    ; more accurate x test would be required to avoid jumpy eye to ghost
    ; but what the heck! it's not as easy as it should be :)
    ; align fully
    move.l  xpen(a4),xpos(a4)   ; x and y
    move.w  #UP,direction(a4)   ; force direction where ghost can move
    move.w  #-1,v_speed(a4)   ; force speed where ghost can move
    clr.w   h_speed(a4)
    ; reached target tile: respawn
    move.l  previous_mode_timer(a4),mode_timer(a4)  ; hack also restores mode
    clr.b   pen_exit_override_flag(a4)              ; else would exit immediately    
    ; now set "ghost_which_counts_dots" to that ghost unless variable points to
    ; some more prioritary ghost (ex: if clyde enters pen, but inky is already the next
    ; ghost in the sequence, don't do anything)
    move.l  ghost_which_counts_dots(pc),a0
    cmp.l   a0,a4
    bcc.b   .not_before     ; current has lower priority: do nothing
    move.l  a4,ghost_which_counts_dots      ; used to check timeout counter too
.not_before
    
    move.l  a4,a0
    bsr     set_normal_ghost_palette
    ; check if other ghosts are in "eyes" mode
    bsr     some_ghosts_are_eyes
    tst.w   d0
    bne.b   .nothing    ; still some eyes
    ; no more eyes
    bsr resume_sound_loop
.nothing
    bra.b   .next_ghost
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
    ; no free direction, probably cornercase with in-pen/exiting pen
    ; ignore
    bra.b   .next_ghost
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
; a clockwise sequence randomly by starting by
; a 0-3 random number
.direction_clockwise_table
    REPT    2
    dc.b    DIRB_RIGHT,RIGHT
    dc.b    DIRB_DOWN,DOWN
    dc.b    DIRB_LEFT,LEFT
    dc.b    DIRB_UP,UP
    ENDR


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
    ; should not reach here!
    blitz
    nop
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
    move.l  a4,a0
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
    
some_ghosts_are_eyes:
    move.l  a0,-(a7)
    lea ghosts(pc),a0
    move.w  #3,d0
.chkloop
    cmp.w   #MODE_EYES,mode(a0)
    beq.b   .found
    add.w   #Ghost_SIZEOF,a0
    dbf d0,.chkloop
    moveq.l #0,d0
.out
    move.l  (a7)+,a0
    rts

.found
    moveq.l #1,d0
    bra.b   .out
    
resume_sound_loop:
    tst.w   fright_timer
    beq.b   .normal
    lea loop_fright_sound(pc),a0
    bra play_loop_fx
.normal
    bra start_background_loop
    
play_loop_fx
    tst.b   demo_mode
    bne.b   .demo
	; stopping loop fx just before playing makes
	; some loops fail. Better not do it, now that ptplayer
	; has been fixed
    ;;bsr stop_loop_fx
    st.b    loop_playing
    lea _custom,a6
    bra _mt_loopfx
.demo
    rts
    
; what: sets game state when a power pill has been taken
; trashes: A0,A1,D0,D1
power_pill_taken
    move.l  d2,-(a7)
    ; resets next ghost eaten score
    clr.w  next_ghost_score

    cmp.w   #STATE_PLAYING,current_state
    bne.b   .no_sound
    lea loop_fright_sound(pc),a0
    bsr play_loop_fx
.no_sound
    
    lea ghosts(pc),a0
    moveq.w  #3,d0
.gloop
    cmp.w  #MODE_EYES,mode(a0)      ; don't fright the eyes
    beq.b   .next

    cmp.w   #MODE_FRIGHT,mode(a0)
    beq.b   .already_fright     ; don't save previous mode if fright!!!
    move.l  mode_timer(a0),previous_mode_timer(a0)  ; hack also saves mode
    move.w  #MODE_FRIGHT,mode(a0)
.already_fright    
    ; set proper fright mode according to current level
    move.w  #NB_FLASH_FRAMES-1,flash_toggle_timer(a0)
    move.w  level_number(pc),d1
    move.b  #1,reverse_flag(a0)
    cmp.w   #18,d1
    bcc.b   .no_time
    add.w   d1,d1
    add.w   d1,d1
    lea     fright_table(pc),a1
    add.w   d1,a1       ; select the level
    move.w  (a1)+,d2
    move.w  d2,mode_timer(a0)
    move.w  (a1)+,flash_timer(a0)
    clr.b   flashing_as_white(a0)
    bra.b   .next
.no_time
    clr.w  mode_timer(a0)
    clr.w  flash_timer(a0)
    clr.w  flash_toggle_timer(a0)
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
    cmp.w   #PLAYER_KILL_TIMER,d6
    bne.b   .no_just_dead
    bsr.b   stop_sounds
.no_just_dead
    move.w  #PLAYER_KILL_TIMER-NB_TICKS_PER_SEC,d5
    sub.w   d6,d5
    bne.b   .no_sound
    lea killed_sound(pc),a0
    bsr play_fx
    bra.b   .zz
.no_sound
    bcs.b   .frame_done     ; frame 0
.zz
    ; d5 is the timer starting from 0
    lea player_kill_anim_table(pc),a0
	clr.w	d0		; forgot to clear that => nasty gfx bug
    move.b  (a0,d5.w),d0
.frame_done
    lsl.w   #6,d0   ; times 64
    move.w  d0,death_frame_offset
    rts
.alive
    tst.w   fright_timer
    beq.b   .no_fright1
    sub.w   #1,fright_timer
    bne.b   .no_fright1
    ; fright mode just ended: resume normal sound loop
    bsr start_background_loop
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
    ; pre turn timer
    tst.w  prepost_turn(a4)
    beq.b   .ptzero
    subq.w  #1,prepost_turn(a4)
.ptzero
    move.l  joystick_state(pc),d0
    IFD    RECORD_INPUT_TABLE_SIZE
    bsr     record_input
    ENDC
    tst.b   demo_mode
    beq.b   .no_demo
    ; if fire is pressed, end demo, goto start screen
    btst    #JPB_BTN_RED,d0
    beq.b   .no_demo_end
    clr.b   demo_mode
    move.w  #STATE_GAME_START_SCREEN,current_state
    rts
.no_demo_end
    clr.l   d0  ; cancel all input
    ; demo running
    ; read next timestamp
    move.l  record_data_pointer(pc),a0
    cmp.l   #demo_moves_end,a0
    bcc.b   .no_demo        ; no more input
    move.b  (a0),d2
    lsl.w   #8,d2
    move.b  (1,a0),d2
    ;;add.b   #3,d2   ; correction???
    cmp.w  record_input_clock(pc),d2
    bne.b   .no_demo        ; don't do anything now
    ; new event
    move.b  (2,a0),d2
    addq.w  #3,a0
    move.l  a0,record_data_pointer
    btst    #LEFT>>2,d2
    beq.b   .no_auto_left
    bset    #JPB_BTN_LEFT,d0
    bra.b   .no_auto_right
.no_auto_left
    btst    #RIGHT>>2,d2
    beq.b   .no_auto_right
    bset    #JPB_BTN_RIGHT,d0
.no_auto_right
    btst    #UP>>2,d2
    beq.b   .no_auto_up
    bset    #JPB_BTN_UP,d0
    bra.b   .no_auto_down
.no_auto_up
    btst    #DOWN>>2,d2
    beq.b   .no_auto_down
    bset    #JPB_BTN_DOWN,d0
.no_auto_down
    ; set replayed input state
    move.l  d0,replayed_input_state
    
    ; read live or recorded controls
.no_demo
    tst.l   d0
    beq.b   .out        ; nothing is currently pressed: optimize
    btst    #JPB_BTN_RIGHT,d0
    beq.b   .no_right
    move.w  #1,h_speed(a4)
    bra.b   .vertical
.no_right
    btst    #JPB_BTN_LEFT,d0
    beq.b   .vertical
    move.w  #-1,h_speed(a4)  
.vertical
    btst    #JPB_BTN_UP,d0
    beq.b   .no_up
    move.w  #-1,v_speed(a4)
    bra.b   .out
.no_up
    btst    #JPB_BTN_DOWN,d0
    beq.b   .no_down
    move.w  #1,v_speed(a4)
.no_down    
.out:
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
    beq.b   .end_pac    ; nothing

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
    bra.b   .bonus_eaten
.power
    ; save A1
    move.l  A1,A2
    bsr power_pill_taken
    bsr .dot_eaten
    
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
    bsr clear_power_pill
    clr.l   (-4,a0)
    bra.b   .score  ; found
.zap_clear
    dbf d0,.clrpdloop
    ; should not happen!!
    bra.b   .score
.simple
    eor.b   #1,eat_toggle
    beq.b   .s2
    lea eat_1_sound(pc),a0
    bra.b   .scont
.s2
    lea eat_2_sound(pc),a0
.scont
    bsr play_fx
    move.b  #1,still_timer(a4)      ; still during 1 frame
    bsr clear_dot
    bsr .dot_eaten
.score
    ; dot
    move.l  ghost_which_counts_dots(pc),a3  ; also the next ghost to leave pen
.retry
    cmp.l   #ghosts+4*Ghost_SIZEOF,a3
    bcc.b   .no_need_to_count_dots
    
    ; we need to check if current ghost is in the pen, else we can't consider it
    move.w  (xpos,a3),d0
    move.w  (ypos,a3),d1
    bsr collides_with_maze
    cmp.b   #P,d0
    beq.b   .ghost_is_in_pen
    
    add.l   #Ghost_SIZEOF,a3
    bra.b   .retry

.ghost_is_in_pen    
    ; a3 points on the next ghost, which IS in the pen
    tst.b   a_life_was_lost
    beq.b   .normal_dot_ghost_count
    ; when a life was lost (until counter resets), an alternate way is enabled
    ; to control when ghosts can exit from pen
    ; (note that we don't actually check if the ghosts are in the pen, we don't care
    ; except for clyde)
    move.b  pen_dot_limit(a3),d0
    cmp.b   ghost_release_dot_counter(pc),d0        ; exact number (7,17,32)
    bne.b   .no_need_to_count_dots
    ; force ghost to exit pen immediately
    st.b    pen_exit_override_flag(a3)
    ; next ghost count dots using global counter
    add.l   #Ghost_SIZEOF,a3
    move.l  a3,ghost_which_counts_dots

    ; if this was the last ghost (clyde), then cancel global flag
    ; allowing elroy mode to resume if was locked
    cmp.l   #orange_ghost+Ghost_SIZEOF,a3
    bne.b   .no_need_to_count_dots
    clr.b   elroy_mode_lock
    ; clears the flag that forces use of global counter
    ; only if clyde is actually in the pen

    clr.b   a_life_was_lost
    bra.b   .no_need_to_count_dots
.normal_dot_ghost_count
    tst.b   pen_nb_dots(a3)
    bne.b   .do_ghost_count
    ; zero: ghost is out of here
    add.l   #Ghost_SIZEOF,a3
    move.l  a3,ghost_which_counts_dots
    bra.b   .no_need_to_count_dots  ; we used to retry now we don't anymore
.do_ghost_count
    sub.b   #1,pen_nb_dots(a3)
.no_need_to_count_dots
    move.b  nb_dots_eaten(pc),d4
    tst.w   bonus_timer
    bne.b   .skip_fruit_test

    cmp.b   #70,d4
    beq.b   .show_fruit
    cmp.b   #170,d4
    beq.b   .show_fruit
.skip_fruit_test
    
    ; empric/random limits to change loop freq
    move.b  d4,d0
    and.b   #$F,d0
    bne.b   .no_sound_loop_increase ; optim
    
    cmp.b   #$40,d4
    beq.b   .sound_loop_increase
    cmp.b   #$80,d4
    beq.b   .sound_loop_increase
    cmp.b   #$C0,d4
    beq.b   .sound_loop_increase
    cmp.b   #$F0,d4
    bne.b   .no_sound_loop_increase
.sound_loop_increase    
    add.w   #1,loop_index
    ; now change loop only if no eyes and no fright
    tst.w   fright_timer
    bne.b   .no_sound_loop_increase
    bsr start_background_loop
.no_sound_loop_increase
    cmp.b   #TOTAL_NUMBER_OF_DOTS,d4
    bne.b   .other
    ; no more dots: win
    bsr level_completed
.other
   
    lea score_table(pc),a0
    add.w   d2,d2
    move.w  (a0,d2.w),d0
    ext.l   d0
    bsr     add_to_score
.end_pac
    rts
.bonus_eaten:
    bsr remove_bonus
    lea bonus_eaten_sound(pc),a0
    bsr play_fx
    ; show score
    move.w  #BONUS_SCORE_TIMER_VALUE,bonus_score_timer
    move.w  #MSG_SHOW,bonus_score_display_message      ; tell draw routine to show score
    bra.b   .other

.show_fruit
    move.b  #3,dot_table+BONUS_OFFSET
    move.w  #BONUS_TIMER_VALUE,bonus_timer
    move.w  #MSG_SHOW,bonus_display_message    
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
    add.w   d4,d3       ; new move y too
    move.w  #PREPOST_TURN_LOCK,prepost_turn(a4)
.ddv
    
    add.w   d4,d3
    ; check if speed is double
    bsr .compute_player_speed
    tst d0
    beq.b   .vsingle
    ; double speed
    add.w   d4,d3    
.vsingle
    move.w  d5,xpos(a4)
    move.w  d3,ypos(a4)

    bsr animate_pacman
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
    add.w   d4,d2       ; new move x too
    move.w  #PREPOST_TURN_LOCK,prepost_turn(a4)
.dd
    ; handle tunnel
    add.w   d4,d2
    bsr .compute_player_speed
    tst d0
    beq.b   .hsingle
    ; double speed
    add.w   d4,d2    
.hsingle
    
    cmp.w   #X_MIN+1,d2
    bcc.b   .positive
    ; warp to right
    move.w  #X_MAX,d2
    bra.b   .setx    
.positive
    cmp.w   #X_MAX,d2
    bcs.b   .setx
    move.w   #X_MIN,d2   ; warp to left
.setx
    move.w  d2,xpos(a4)
    move.w  d5,ypos(a4)
    bsr animate_pacman
    clr.w   v_speed(a4)

.no_hmove
    rts

; < A4: player structure
; > D0: 1 if double speed, 0 otherwise
; trashes: A1,D1

.compute_player_speed
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
    subq.w  #1,d0       ; hack so 1 => 0 and 2 => 1
    rts
    
.dot_eaten
    add.b  #1,nb_dots_eaten
    add.b  #1,ghost_release_dot_counter    
    clr.w   ghost_release_override_timer
    rts
    
    IFD    RECORD_INPUT_TABLE_SIZE
record_input:
    tst.l   d0
    bne.b   .store
    ; 0 twice: ignore (saves space)
    cmp.l   prev_record_joystick_state(pc),d0
    beq.b   .no_input
.store
    move.l  d0,prev_record_joystick_state
    clr.b   d1
    ; now store clock & joystick state, "compressed" to 4 bits (up,down,left,right)
    btst    #JPB_BTN_RIGHT,d0
    beq.b   .norr
    bset    #RIGHT>>2,d1
    bra.b   .norl
.norr
    btst    #JPB_BTN_LEFT,d0
    beq.b   .norl
    bset    #LEFT>>2,d1
.norl
    btst    #JPB_BTN_UP,d0
    beq.b   .noru
    bset    #UP>>2,d1
    bra.b   .nord
.noru
    btst    #JPB_BTN_DOWN,d0
    beq.b   .nord
    bset    #DOWN>>2,d1
.nord
    move.l record_data_pointer(pc),a0
    cmp.l   #record_input_table+RECORD_INPUT_TABLE_SIZE-4,a0
    bcc.b   .no_input       ; overflow!!!
    
    ; store clock
    move.b  record_input_clock(pc),(a0)+
    move.b  record_input_clock+1(pc),(a0)+
    move.b  d1,(a0)+
    ; update pointer
    move.l  a0,record_data_pointer
.no_input
    rts
    ENDC
    
; called when pacman moves
; < A4: pac player
animate_pacman
    addq.w  #1,frame(a4)
    cmp.w   #(pac_anim_left_end-pac_anim_left)/4,frame(a4)
    bne.b   .no_floop
    clr.w   frame(a4)
.no_floop
    rts

level_completed:
    clr.w   state_timer
    move.w  #MAZE_BLINK_TIME,maze_blink_timer
    move.b  #8,maze_blink_nb_times    
    move.w  #STATE_LEVEL_COMPLETED,current_state
    bsr     hide_sprites
    bra     stop_sounds    
    
draw_pacman:
    lea     player(pc),a2
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
    bsr wait_blit   ; (just in case the blitter didn't finish writing pacman)
    move.l  previous_pacman_address(pc),a3
    REPT    18
    clr.l   (NB_BYTES_PER_LINE*(REPTN-1),a3)
    ENDR

    bsr blit_plane
    ; A1 is start of dest, use it to clear upper part and lower part
    ; and possibly shifted to the left/right
;;    move.l  a1,d0
;;    btst    #0,d0
;;    beq.b   .ok
;;    subq.l  #1,a1   ; even address, always!
;;.ok
    move.l  a1,previous_pacman_address
    rts

draw_big_pacman:
    lea     player(pc),a2
.normal    
    lea  big_pac_table(pc),a0
    move.w  frame(a2),d0
    add.w   d0,d0
    add.w   d0,d0
    move.l  (a0,d0.w),a0
.pacblit
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  xpos(a2),d0
    move.w  ypos(a2),d1
    ; center => top left
    moveq.l #-1,d3 ; first/last word mask
    sub.w  #8+Y_START,d1
    sub.w  #8+X_START,d0
    bpl.b   .no_left
    ; d0 is negative
    neg.w   d0
    lsr.l   d0,d3
    neg.w   d0
    add.w   #NB_BYTES_PER_LINE*8,d0
    subq.w  #1,d1
    bra.b   .pdraw
.no_left
    ; check mask to the right
;;    move.w  d0,d4    
;;    sub.w   #X_MAX-24-X_START,d4
;;    bmi.b   .pdraw
;;    lsl.l   d4,d3
;;    lsl.l   #8,d3
;;    lsl.l   #8,d3
.pdraw
    ; clear left row
    move.l  previous_pacman_address(pc),a3
    move.w  #31,d4
.c
    clr.b   (a3)
    clr.b   (-1,a3)
    add.w   #NB_BYTES_PER_LINE,a3
    dbf.b   d4,.c
    
    move.w  #6,d2   ; 32+16
    move.w  #32,d4

    bsr blit_plane_any

    move.l  a1,previous_pacman_address    
    ; now hack to clip zones without reducing blit width
    ; this is super-dirty I know. But this is not a game engine...
    
    lea screen_data+SCREEN_PLANE_SIZE+(Y_PAC_ANIM-20)*NB_BYTES_PER_LINE+X_MAX/8-1,a3
    move.w  #31,d4

    bsr wait_blit

.clip
    clr.l   (a3)
    add.w   #NB_BYTES_PER_LINE,a3
    dbf.b   d4,.clip    
    
    lea screen_data+SCREEN_PLANE_SIZE+(Y_PAC_ANIM-20)*NB_BYTES_PER_LINE-4,a3
    move.w  #31,d4
.clip2
    clr.l   (a3)
    add.w   #NB_BYTES_PER_LINE,a3
    dbf.b   d4,.clip2
    
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
    lea wall_table(pc),a0
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
    lea $DFF000,A5
	move.l d2,d3	;no masking of first/last word    
    move.w  #4,d2       ; 16 pixels + 2 shift bytes
    move.w  #16,d4      ; 16 pixels height
    bsr blit_plane_any_internal
    movem.l (a7)+,d2-d4/a2-a5
    rts
    
; what: blits (any width)x(any height) data on one plane
; args:
; < A0: data (width x height)
; < A1: plane
; < D0: X
; < D1: Y
; < D2: blit width in bytes (+2)
; < D3: blit mask
; < D4: blit height
; trashes: D0-D1, A1
;
; if A1 is already computed with X/Y offset and no shifting, an optimization
; skips the XY offset computation

blit_plane_any:
    movem.l d2-d5/a2-a5,-(a7)
    lea $DFF000,A5
    bsr blit_plane_any_internal
    movem.l (a7)+,d2-d5/a2-a5
    rts

; < A5: custom
; < D0,D1: x,y
; < A0: source
; < D2: width in bytes (inc. 2 extra for shifting)
; < D3: bltfwmask
; < D4: height
; blit mask set
; trashes D0-D5, a1
blit_plane_any_internal:
    ; pre-compute the maximum of shit here
    lea mul40_table(pc),a2
    add.w   d1,d1
    beq.b   .d1_zero    ; optim
    move.w  (a2,d1.w),d1
    swap    d1
    clr.w   d1
    swap    d1
.d1_zero
    move.l  #$09f00000,d5    ;A->D copy, ascending mode
    move    d0,d6
    beq.b   .d0_zero
    and.w   #$F,d6
    and.w   #$1F0,d0
    lsr.w   #3,d0
    add.w   d0,d1

    swap    d6
    clr.w   d6
    lsl.l   #8,d6
    lsl.l   #4,d6
    or.l    d6,d5            ; add shift
.d0_zero    
    add.l   d1,a1       ; plane position

	move.w #NB_BYTES_PER_LINE,d0
    sub.w   d2,d0       ; blit width

    lsl.w   #6,d4
    lsr.w   #1,d2
    add.w   d2,d4       ; blit height


    ; now just wait for blitter ready to write all registers
	bsr	wait_blit
    
    ; blitter registers set
	move.l d3,bltafwm(a5)	;no masking of first/last word    
    
	clr.w   bltamod(a5)		;A modulo=bytes to skip between lines
	move.l d5,bltcon0(a5)	
    move.w  d0,bltdmod(a5)	;D modulo
	move.l a0,bltapt(a5)	;source graphic top left corner
	move.l a1,bltdpt(a5)	;destination top left corner
	move.w  d4,bltsize(a5)	;rectangle size, starts blit
    rts

; what: blits 16(32)x16 data on 4 planes (for bonuses), full mask
; args:
; < A0: data (16x16)
; < D0: X
; < D1: Y
; trashes: D0-D1

blit_4_planes
    movem.l d2-d7/a0-a1/a5,-(a7)
    lea $DFF000,A5
    lea     screen_data,a1
    moveq.l #3,d7
.loop
    movem.l d0-d1/a1,-(a7)
    move.w  #4,d2       ; 16 pixels + 2 shift bytes
    moveq.l #-1,d3
    move.w  #16,d4      ; height
    bsr blit_plane_any_internal
    movem.l (a7)+,d0-d1/a1
    add.l   #SCREEN_PLANE_SIZE,a1
    add.l   #64,a0      ; 32 but shifting!
    dbf d7,.loop
    movem.l (a7)+,d2-d7/a0-a1/a5
    rts
    
wait_blit
	TST.B	$BFE001
.wait
	BTST	#6,dmaconr+_custom
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
    cmp.l   #655360,d2
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

write_string:
    movem.l A0-A2/d1-D2,-(a7)
    clr.w   d2
    ADD_XY_TO_A1    a2
    moveq.l #0,d0
.loop
    move.b  (a0)+,d2
    beq.b   .end
    addq.l  #1,d0

    cmp.b   #'0',d2
    bcs.b   .special
    cmp.b   #'9'+1,d2
    bcc.b   .try_letters
    ; digits
    lea digits(pc),a2
    sub.b   #'0',d2
    bra.b   .wl
    
.try_letters: 
    cmp.b   #'A',d2
    bcs.b   .special
    cmp.b   #'Z'+1,d2
    bcc.b   .special
    lea letters(pc),a2
    sub.b   #'A',d2
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
    bra.b   .next
.special
    cmp.b   #' ',d2
    bne.b   .nospace
    lea space(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nospace    
    cmp.b   #'!',d2
    bne.b   .noexcl
    lea exclamation(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noexcl
    cmp.b   #'/',d2
    bne.b   .noslash
    lea slash(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noslash
    cmp.b   #'-',d2
    bne.b   .nodash
    lea dash(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nodash
    cmp.b   #'"',d2
    bne.b   .noquote
    lea quote(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.noquote
    cmp.b   #'c',d2
    bne.b   .nocopy
    lea copyright(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nocopy
    cmp.b   #'p',d2
    bne.b   .nop
    lea pts_0(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nop
    cmp.b   #'t',d2
    bne.b   .nop2
    lea pts_1(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nop2
    cmp.b   #'s',d2
    bne.b   .nop3
    lea pts_2(pc),a2
    moveq.l #0,d2
    bra.b   .wl
.nop3


.next   
    addq.l  #1,a1
    bra.b   .loop
.end
    movem.l (a7)+,A0-A2/d1-D2
    rts
    
load_highscores
    lea scores_name(pc),a0
    move.l  _resload(pc),d0
    beq.b   .standard
    move.l  d0,a2
    jsr (resload_GetFileSize,a2)
    tst.l   d0
    beq.b   .no_file
    ; file is present, read it
    lea scores_name(pc),a0    
    lea high_score(pc),a1
    moveq.l #4,d0   ; size
    moveq.l #0,d1   ; offset
    jmp  (resload_LoadFileOffset,a2)
    
.standard
    move.l  _dosbase(pc),a6
    move.l  a0,d1
    move.l  #MODE_OLDFILE,d2
    jsr     (_LVOOpen,a6)
    move.l  d0,d1
    beq.b   .no_file
    move.l  d1,d4
    move.l  #4,d3
    move.l  #high_score,d2
    jsr (_LVORead,a6)
    move.l  d4,d1
    jsr (_LVOClose,a6)    
.no_file
    rts
    

; < D0: command to send to cdtv 
send_cdtv_command:
	tst.l	_resload
	beq.b	.go
	rts		; not needed within whdload (and will fail)
.go
	movem.l	d0-a6,-(a7)
    move.l  d0,d5
    
	; alloc some mem for IORequest

	MOVEQ	#40,D0			
	MOVE.L	#MEMF_CLEAR|MEMF_PUBLIC,D1
	move.l	$4.W,A6
	jsr	_LVOAllocMem(a6)
	move.l	D0,io_request
	beq	.Quit

	; open cdtv.device

	MOVEA.L	D0,A1
	LEA	cdtvname(PC),A0	; name
	MOVEQ	#0,D0			; unit 0
	MOVE.L	D0,D1			; flags
	jsr	_LVOOpenDevice(a6)
	move.l	D0,D6
	ext	D6
	ext.l	D6
	bne	.Quit		; unable to open

    ; wait a while if CMD_STOP
    cmp.l   #CMD_STOP,d5
    bne.b   .nowait
	move.l	_dosbase(pc),A6
	move.l	#20,D1
	JSR	_LVODelay(a6)		; wait 2/5 second before launching
.nowait
	; prepare the IORequest structure

	MOVEQ	#0,D0
	MOVEA.L	io_request(pc),A0
	MOVE.B	D0,8(A0)
	MOVE.B	D0,9(A0)
	SUBA.L	A1,A1
	MOVE.L	A1,10(A0)
	MOVE.L	A1,14(A0)
	CLR.L	36(A0)

	move.l	io_request(pc),A0

	move.l	A0,A1
	move.w	d5,(IO_COMMAND,a1)
	move.l	$4.W,A6
	JSR		_LVODoIO(a6)

.Quit:
	; close cdtv.device if open

	tst.l	D6
	bne	.Free
	MOVE.L	io_request(pc),D1
	beq	.End
	move.l	D1,A1
	move.l	$4.W,A6
	jsr	_LVOCloseDevice(a6)

.Free:		
	; free the memory

	MOVEQ	#40,D0
	move.l	io_request(pc),A1
	move.l	$4.W,A6
	JSR		_LVOFreeMem(a6)
.End:
	movem.l	(a7)+,d0-a6
	rts
	
save_highscores
    tst.b   highscore_needs_saving
    beq.b   .out
    lea scores_name(pc),a0
    move.l  _resload(pc),d0
    beq.b   .standard
    move.l  d0,a2
    lea scores_name(pc),a0    
    lea high_score(pc),a1
    moveq.l #4,d0   ; size
    jmp  (resload_SaveFile,a2)
    
.standard
    move.l  _dosbase(pc),a6
    move.l  a0,d1
    move.l  #MODE_NEWFILE,d2
    jsr     (_LVOOpen,a6)
    move.l  d0,d1
    beq.b   .out
    move.l  d1,d4
    move.l  #4,d3
    move.l  #high_score,d2
    jsr (_LVOWrite,a6)
    move.l  d4,d1
    jsr (_LVOClose,a6)    
.out
    rts
    
_dosbase
    dc.l    0
_gfxbase
    dc.l    0
_resload
    dc.l    0
io_request:
	dc.l	0
_keyexit
    dc.b    $59
scores_name
    dc.b    "pacman.high",0
highscore_needs_saving
    dc.b    0
graphicsname:
	dc.b "graphics.library",0
dosname
        dc.b    "dos.library",0
cdtvname:
	dc.b	"cdtv.device",0

            even

    include ReadJoyPad.s
    ; variables
gfxbase_copperlist
    dc.l    0
    
previous_random
    dc.l    0
previous_joystick_state
    dc.l    0
joystick_state
    dc.l    0
replayed_input_state
    dc.l    0
record_data_pointer
    dc.l    0
record_input_clock
    dc.w    0    
    IFD    RECORD_INPUT_TABLE_SIZE
prev_record_joystick_state
    dc.l    0

    ENDC    
ready_timer:
    dc.w    0
first_ready_timer:
    dc.w    0
half_first_ready_timer:
    dc.w    0
level_blink_timer
    dc.w    0
current_state
    dc.w    0
power_pill_timer:
    dc.w    BLINK_RATE
score
    dc.l    0
displayed_score
    dc.l    0
high_score
    dc.l    0
maze_blink_timer
    dc.w    0
    ; table up to level 21. After that it's the same
bonus_level_table:  ; 1 is not present, no score 200 in bonuses
    dc.w    0,2,3,3,4,4,5,5,6,6,7,7,8,8,8,8,8,8,8,8,8
bonus_level_score:  ; *10
    dc.w    10,30,50,50,70,70,100,100,200,200,300,300
    dc.w    500,500,500,500,500,500,500,500,500
bonus_timer:
    dc.w    0
; general purpose timer for non-game states (intro, game over...)
state_timer:
    dc.w    0
last_ghost_eaten_state_timer
    dc.w    0
fruit_score_index:
    dc.w    0
next_ghost_score
    dc.w    0
previous_pacman_address
    dc.l    screen_data+2*NB_BYTES_PER_LINE+SCREEN_PLANE_SIZE ; valid address for first clear
ghost_which_counts_dots
    dc.l    0
score_frame
    dc.l    0
global_speed_table
    dc.l    0
extra_life_sound_counter
    dc.w    0
extra_life_sound_timer
    dc.w    0
        
; 0: level 1
level_number:
    dc.w    0
player_killed_timer:
    dc.w    -1
ghost_eaten_timer:
    dc.w    -1
bonus_score_timer:
    dc.w    0
fright_timer:
    dc.w    0
cheat_sequence_pointer
    dc.l    cheat_sequence    
cheat_keys
    dc.w    0
death_frame_offset
    dc.w    0
ghost_release_override_max_time:
    dc.w    0
ghost_release_override_timer:
    dc.w    0
maze_blink_nb_times
    dc.b    0
nb_lives:
    dc.b    0
music_playing:
    dc.b    0
loop_playing:
    dc.b    0
pause_flag
    dc.b    0
quit_flag
    dc.b    0
elroy_mode_lock:
    dc.b    0
a_life_was_lost:
    dc.b    0
ghost_release_dot_counter
    dc.b    0
nb_dots_eaten
    dc.b    0
invincible_cheat_flag
    dc.b    0
debug_flag
    dc.b    0
demo_mode
    dc.b    0
extra_life_awarded
    dc.b    0
music_played
    dc.b    0
eat_toggle
    dc.b    0
elroy_threshold_1
    dc.b    0
elroy_threshold_2
    dc.b    0
    even


old_actiview:
	dc.l	0
	
bonus_display_message:
    dc.w    0
bonus_score_display_message:
    dc.w    0
ready_display_message:
    dc.w    0
extra_life_message:
    dc.w    0
player_one_and_life_display_message
    dc.w    0
    
score_table
    dc.w    0,1,5
fruit_score     ; must follow score_table
    dc.w    10
    
big_pac_table:
    dc.l    big_pac_2,big_pac_2,big_pac_0,big_pac_1,big_pac_1,big_pac_0

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

    

cheat_sequence
    dc.b    $26,$18,$14,$22,0
    even
; table with 2 bytes: 60hz clock, 1 byte: move mask for the demo
demo_moves:
    incbin  "pacman_moves.bin"
demo_moves_end:
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
slash
    incbin  "slash.bin"
dash
    incbin  "dash.bin"
quote
    incbin  "quote.bin"
copyright
    incbin  "copyright.bin"
pts_0
    incbin  "pts_0.bin"
pts_1
    incbin  "pts_1.bin"
pts_2
    incbin  "pts_2.bin"
space
    ds.b    8,0
    
high_score_string
    dc.b    " HIGH SCORE",0
p1_string
    dc.b    "     1UP",0
level_string
    dc.b    "   LEVEL",0
score_string
    dc.b    "       00",0
game_over_string
    dc.b    "GAME##OVER",0
player_one_string
    dc.b    "PLAYER ONE",0
player_one_string_clear
    dc.b    "          ",0
ready_string
    dc.b    "READY!",0
character_nickname_string:
    dc.b    "CHARACTER / NICKNAME",0

fifty_pts_string:
    dc.b    "50 pts",0
ten_pts_string:
    dc.b    "10 pts",0
    even
character_text_table
    dc.l    0
character_text_table_jp
    dc.l    oiake,akabei,$F00,0
    dc.l    machibuse,pinky,$0fbf,0
    dc.l    kimagure,aosuke,$00ff,0
    dc.l    otobuke,guzuta,$0fb5,0
character_text_table_en
    dc.l    shadow,blinky,$F00,0
    dc.l    speedy,pinky,$0fbf,0
    dc.l    bashful,inky,$00ff,0
    dc.l    pokey,clyde,$0fb5,0
oiake:
    dc.b    "OIKAKE----",0
akabei:
    dc.b    '"AKABEI"',0
machibuse:
    dc.b    "MACHIBUSE--",0
pinky:
    dc.b    '"PINKY"',0
kimagure:
    dc.b    "KIMAGURE--",0
aosuke:
    dc.b    '"AOSUKE"',0
otobuke:
    dc.b    "OTOBUKE---",0
guzuta:
    dc.b    '"GUZUTA"',0
    
shadow:
    dc.b    "-SHADOW    ",0
blinky:
    dc.b    '"BLINKY"',0
speedy:
    dc.b    "-SPEEDY    ",0

bashful:
    dc.b    "-BASHFUL   ",0
inky:
    dc.b    '"INKY"',0
pokey:
    dc.b    "-POKEY     ",0
clyde:
    dc.b    '"CLYDE"',0

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
    dc.w    ORIGINAL_TICKS_PER_SEC*\1,NB_FLASH_FRAMES*\2*2
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
; trashes: none
get_elroy_level:
    moveq   #0,d0
    tst.b   elroy_mode_lock
    bne.b   .out
    cmp.l   #ghosts,a0
    bne.b   .out
    move.w  d1,-(a7)
    move.b  #TOTAL_NUMBER_OF_DOTS,d1
    sub.b   nb_dots_eaten(pc),d1
    cmp.b   elroy_threshold_2(pc),d1
    bcs.b   .level2
    cmp.b   elroy_threshold_1(pc),d1
    bcs.b   .level1
    move.w  (a7)+,d1
.out
    rts
.level1
    move.w  (a7)+,d1
    moveq   #1,d0
    rts
.level2
    move.w  (a7)+,d1
    moveq   #2,d0
    rts
; < A0: ghost structure
; > D0: 0 if not elroy, 1 if elroy
; trashes: none
is_elroy:
    moveq   #0,d0
    tst.b   elroy_mode_lock
    bne.b   .out
    cmp.l   #ghosts,a0
    bne.b   .out
    move.w  d1,-(a7)
    move.b  #TOTAL_NUMBER_OF_DOTS,d1
    sub.b   nb_dots_eaten(pc),d1
    cmp.b   elroy_threshold_1(pc),d1
    movem.w  (a7)+,d1
    bcs.b   .level1
.out
    rts
.level1
    moveq   #1,d0
    rts


	STRUCTURE	Sound,0
    ; matches ptplayer
    APTR    ss_data
    UWORD   ss_len
    UWORD   ss_per
    UWORD   ss_vol
    UBYTE   ss_channel
    UBYTE   ss_pri
    
    LABEL   Sound_SIZEOF
    
; < D0: track start number
play_music
    movem.l d0-a6,-(a7)
    lea _custom,a6
    lea music,a0
    sub.l   a1,a1
    bsr _mt_init
    ; set master volume a little less loud
    move.w  #12,d0
    bsr _mt_mastervol
    bsr _mt_start
    st.b    music_playing
    movem.l (a7)+,d0-a6
    rts
    
; < A0: sound struct
play_fx
    tst.b   demo_mode
    bne.b   .no_sound
    lea _custom,a6
    bra _mt_playfx
.no_sound
    rts
    
; custom addition to ptplayer: ability to loop sound several times but not infinitely
update_extra_life_sound_loop:
    tst.w   extra_life_sound_counter
    beq.b   .out    
    tst.w   extra_life_sound_timer
    bne.b   .increase
    ; timer = 0: play
    lea extra_life_sound(pc),a0
    bsr play_fx
    sub.w   #1,extra_life_sound_counter
.increase
    add.w   #1,extra_life_sound_timer
    cmp.w   #9,extra_life_sound_timer
    bne.b   .out
    clr.w   extra_life_sound_timer
.out
    rts

start_background_loop
    move.w  loop_index(pc),d0
    add.w   d0,d0
    add.w   d0,d0
    lea     loop_table(pc),a0
    move.l  (a0,d0.w),a0    ; current loop sound
    bra play_loop_fx

   
    
       
;base addr, len, per, vol, channel<<8 + pri, loop timer, number of repeats (or -1), current repeat, current vbl

FXFREQBASE = 3579564
SOUNDFREQ = 22050

SOUND_ENTRY:MACRO
\1_sound
    dc.l    \1_raw
    dc.w    (\1_raw_end-\1_raw)/2,FXFREQBASE/SOUNDFREQ,64
    dc.b    \2
    dc.b    $01
    ENDM
    
    ; radix,channel (0-3)
    SOUND_ENTRY killed,1
    SOUND_ENTRY credit,1
    SOUND_ENTRY extra_life,1
    SOUND_ENTRY ghost_eaten,2
    SOUND_ENTRY bonus_eaten,3
    SOUND_ENTRY eat_1,3
    SOUND_ENTRY eat_2,3
    SOUND_ENTRY loop_1,0
    SOUND_ENTRY loop_2,0
    SOUND_ENTRY loop_3,0
    SOUND_ENTRY loop_4,0
    SOUND_ENTRY loop_5,0
    SOUND_ENTRY loop_fright,0
    SOUND_ENTRY loop_eyes,0

    dc.l    0
    
loop_table:
    dc.l    loop_1_sound,loop_2_sound,loop_3_sound,loop_4_sound,loop_5_sound
loop_index
    dc.w    0
    
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

    even
 ; speed table at 60 Hz
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
    even
ghosts:
red_ghost:      ; needed by cyan ghost
    ds.b    Ghost_SIZEOF
pink_ghost:      ; needed by cyan ghost
    ds.b    Ghost_SIZEOF
    ds.b    Ghost_SIZEOF
orange_ghost:        ; needed to unlock elroy mode
    ds.b    Ghost_SIZEOF


keyboard_table:
    ds.b    $100,0
    
floppy_file
    dc.b    "floppy",0
    even
    
; BSS --------------------------------------
    SECTION  S2,BSS
HWSPR_TAB_XPOS:	
	ds.l	512			

HWSPR_TAB_YPOS:
	ds.l	512
    
    IFD   RECORD_INPUT_TABLE_SIZE
record_input_table:
    ds.b    RECORD_INPUT_TABLE_SIZE
    ENDC
    
dot_table
    ds.b    NB_TILES_PER_LINE*NB_TILE_LINES
    
    even
    
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
ghost_sprites:
    ; red ghost
    dc.w    sprpt+0,0
    dc.w    sprpt+2,0
nail_sprite
    ; red target / nail / drape
    dc.w    sprpt+4,0
    dc.w    sprpt+6,0
score_sprite_entry:
    ; pink ghost / score for ghost eaten
    dc.w    sprpt+8,0
    dc.w    sprpt+10,0
    ; pink target
    dc.w    sprpt+12,0
    dc.w    sprpt+14,0
    ; cyan ghost
    dc.w    sprpt+16,0
    dc.w    sprpt+18,0
    ; cyan target
    dc.w    sprpt+20,0
    dc.w    sprpt+22,0
    ; orange ghost
    dc.w    sprpt+24,0
    dc.w    sprpt+26,0
    ; orange target
    dc.w    sprpt+28,0
    dc.w    sprpt+30,0


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
	
ghost_bob_table:
    dc.l    .ghost_bobs,.ghost_bobs+320,.ghost_bobs+320*2,.ghost_bobs+320*3
.ghost_bobs:
    incbin  "red_ghost_bob.bin"
    incbin  "pink_ghost_bob.bin"
    incbin  "cyan_ghost_bob.bin"
    incbin  "orange_ghost_bob.bin"

big_pac_0
    incbin  "big_pac_0.bin"
big_pac_1
    incbin  "big_pac_1.bin"
big_pac_2
    incbin  "big_pac_2.bin"
    
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
    incbin  "pac_lives.bin"
namco:
    incbin  "namco.bin"
    
bonus_pics:
    incbin  "cherry.bin"        ; 0
    incbin  "cherry.bin"        ; 1  (gap for nonexistent 200 score)
    incbin  "strawberry.bin"    ; 2
    incbin  "peach.bin"         ; 3
    incbin  "apple.bin"         ; 4
    incbin  "grapes.bin"        ; 5
    incbin  "galaxian.bin"      ; 6
    incbin  "bell.bin"          ; 7
    incbin  "key.bin"           ; 8
bonus_scores:
    incbin  "bonus_scores_0.bin"    ; 100
    incbin  "bonus_score_200.bin"   ; 200 (for 2000)
    incbin  "bonus_scores_1.bin"    ; 300
    incbin  "bonus_scores_2.bin"    ; 500
    incbin  "bonus_scores_3.bin"    ; 700

red_ghost_with_leg_left
    incbin  "red_ghost_with_leg_0.bin"
red_ghost_with_leg_up
    incbin  "red_ghost_with_leg_1.bin"
    
red_ghost_naked_0
    incbin  "red_ghost_naked_0.bin"
red_ghost_naked_1
    incbin  "red_ghost_naked_1.bin"
    even

    

    
    
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
\1_tunnel_frame
    dc.l    0
    ds.l    64,0    ; generated on the fly     
    dc.l    0
\1_target_frame
    dc.l    0
    dc.l    0
    REPT    14
    dc.l    $7FFE0000    ; square with ghost color
    ENDR
    dc.l    0
    dc.l    0
    ENDM
        
    DECL_GHOST  red
    DECL_GHOST  pink
    DECL_GHOST  cyan
    DECL_GHOST  orange

; special sprites for intermissions


repaired_0
    dc.l    0
    incbin  "red_ghost_repaired_0.bin"
    dc.l    0
repaired_1
    dc.l    0
    incbin  "red_ghost_repaired_1.bin"
    dc.l    0
nail:
    dc.l    0
    incbin  "red_ghost_drape_0.bin"
    dc.l    0
torn_drape:
    dc.l    0
    incbin  "red_ghost_drape_4.bin"
    dc.l    0
red_ghost_drape_1:
    dc.l    0
    incbin  "red_ghost_drape_1.bin"
    dc.l    0
red_ghost_drape_2:
    dc.l    0
    incbin  "red_ghost_drape_2.bin"
    dc.l    0
red_ghost_drape_3:
    dc.l    0
    incbin  "red_ghost_drape_3.bin"
    dc.l    0
    
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

killed_raw
    incbin  "pacman_killed.raw"
    even
killed_raw_end
credit_raw
    incbin  "credit.raw"
    even
credit_raw_end

ghost_eaten_raw
    incbin  "ghost_eaten.raw"
    even
ghost_eaten_raw_end


bonus_eaten_raw
    incbin  "bonus_eaten.raw"
    even
bonus_eaten_raw_end
extra_life_raw
    incbin  "extra_life.raw"
    even
extra_life_raw_end
eat_1_raw
    incbin  "eat_1.raw"
    even
eat_1_raw_end
eat_2_raw
    incbin  "eat_2.raw"
    even
eat_2_raw_end
loop_1_raw
    incbin  "loop_1.raw"
    even
loop_1_raw_end
loop_2_raw
    incbin  "loop_2.raw"
    even
loop_2_raw_end
loop_3_raw
    incbin  "loop_3.raw"
    even
loop_3_raw_end
loop_4_raw
    incbin  "loop_4.raw"
    even
loop_4_raw_end
loop_5_raw
    incbin  "loop_5.raw"
    even
loop_5_raw_end
loop_fright_raw
    incbin  "loop_fright.raw"
    even
loop_fright_raw_end
loop_eyes_raw
    incbin  "loop_eyes.raw"
    even
loop_eyes_raw_end

    
empty_sprite
    dc.l    0,0
music
    incbin  "pacman_convert.mod"

    
    	