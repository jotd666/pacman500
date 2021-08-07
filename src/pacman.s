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


	
;CIA-A registre port A (bouton souris)

CIAAPRA = $BFE001

	STRUCTURE	Character,0
	UWORD	xpos
	UWORD	ypos
	UWORD	direction
    UBYTE   frame
	LABEL	Character_SIZEOF

	STRUCTURE	Ghost,0
	STRUCT      BaseCharacter,Character_SIZEOF
    APTR     behaviour
    UWORD    frightened_time
    UWORD    home_corner    
    UBYTE    attacking
    
	LABEL	Ghost_SIZEOF
    
    ;Exec Library Base Offsets


;graphics base

StartList = 38

;autres labels

Execbase  = 4

NB_LINES = 248
NB_BYTES_PER_LINE = 40
NB_BYTES_PER_MAZE_LINE = 28
MAZE_PLANE_SIZE = NB_BYTES_PER_LINE*NB_LINES
SCREEN_PLANE_SIZE = 40*NB_LINES
NB_PLANES   = 4

NB_DOTS_PER_LINE = 26
NB_DOT_LINES = 29

PRECISION = 4

LEFT = 0
RIGHT = 1<<2
UP = 2<<2
DOWN = 3<<2

Start:
    lea  _custom,a5
    ;bsr compute_sprite_xy_table
    bsr compute_collision_table
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

    bsr draw_maze
    bsr draw_dots
    bsr init_player

    bsr init_interrupts
    ;move.w  #$7FFF,(intena,a5)
    
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
    move.w #$30C1,diwstop(a5)             ; la fenêtre écran
    move.w #$0038,ddfstrt(a5)             ; et le DMA bitplane
    move.w #$00D0,ddfstop(a5)
    move.w #$4200,bplcon0(a5) ; 4 bitplanes
    clr.w bplcon1(a5)                     ; no scrolling
    clr.w bplcon2(a5)                     ; pas de priorité
    move.w #0,bpl1mod(a5)                ; modulo de tous les plans = 40
    move.w #0,bpl2mod(a5)

    
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
    
    lea  sprites,a0
    move.w  #10,d0
    move.w  #10,d1
    bsr store_sprite_pos
    lea  red_ghost_2,a1
    move.l  d0,(a1)
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
    
    add.l  #16,a0
    move.w  #30,d0
    move.w  #80,d1
    bsr store_sprite_pos
    lea  pink_ghost_4,a1
    move.l  d0,(a1)
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)

    add.l  #16,a0
    move.w  #48,d0
    move.w  #80,d1
    bsr store_sprite_pos
    lea  cyan_ghost_2,a1
    move.l  d0,(a1)
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)

    add.l  #16,a0
    move.w  #86,d0
    move.w  #80,d1
    bsr store_sprite_pos
    lea  orange_ghost_3,a1
    move.l  d0,(a1)
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)

    ; init sprite, bitplane, whatever dma (not audio ATM)
    move.w #$83E0,dmacon(a5)
    ; init copper interrupts, mainly
    move.w #$8038,intena(a5)
    
    ; start game
; < A0: data (16x16)
; < A1: plane
; < D0: X
; < D1: Y
       
	; now sprite test
	bsr		mouse
    bsr     restore_interrupts
			      

    move.l  gfxbase,a1
    move.l  gfxbase_copperlist,StartList(a1) ; adresse du début de la liste
    move.l  gfxbase_copperlist,cop1lc(a5) ; adresse du début de la liste
    clr.w  copjmp1(a5)
    move.w #$8060,dmacon(a5)        ; réinitialisation du canal DMA
    
    move.l  gfxbase,a1
    jsr _LVOCloseLibrary(a6)
    
    jsr _LVOPermit(a6)                  ; Task Switching autorisé
    moveq.l #0,d0
    rts

init_player
    lea player(pc),a0
	move.w  #104<<PRECISION,xpos(a0)
	move.w	#180<<PRECISION,ypos(a0)
	move.w 	#LEFT,direction(a0)
    move.w  #0,frame(a0)
    move.w  #50,ready_timer
	rts	    

draw_all
    lea player(pc),a2
    move.w  direction(a2),d0
    lea  pac_dir_table(pc),a0
    move.l  (a0,d0.w),a0
    move.w  frame(a2),d0
    move.l  (a0,d0.w),a0

    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  xpos(a2),d0
    lsr.w   #PRECISION,d0
    move.w  ypos(a2),d1
    lsr.w   #PRECISION,d1
    bsr blit_plane

    tst.w   ready_timer
    beq.b   .no_ready
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    lea ready,a0
    move.w  #88,d0
    move.w  #136,d1
    move.w  #14,d2   ; 96
    bsr blit_plane_any
    
.no_ready
    rts
    
draw_maze:    
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

draw_dots:
    ; init dots
    lea dot_table_read_only(pc),a0
    lea dot_table,a1
    move.l  #NB_DOT_LINES*NB_DOTS_PER_LINE-1,d0
.copy
    move.b  (a0)+,(a1)+
    dbf d0,.copy

    lea dot_table,a0
    lea	screen_data+SCREEN_PLANE_SIZE*2+NB_BYTES_PER_LINE*8+1,a1
    
    move.w  #NB_DOT_LINES-1,d0    
.loopy
    move.w  #NB_DOTS_PER_LINE-1,d1
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
    move.b  #%00111100,(a1)
    move.b  #%01111110,(NB_BYTES_PER_LINE*1,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*2,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*3,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*4,a1)
    move.b  #%11111111,(NB_BYTES_PER_LINE*5,a1)
    move.b  #%01111110,(NB_BYTES_PER_LINE*6,a1)
    move.b  #%00111100,(NB_BYTES_PER_LINE*7,a1)

.next
    addq.l  #1,a1
    dbf d1,.loopx
    add.l  #NB_BYTES_PER_LINE-NB_BYTES_PER_MAZE_LINE+2,a1
    add.l   #NB_BYTES_PER_LINE*7,a1
    dbf d0,.loopy
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

	even
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


 
compute_collision_table
    lea maze_data(pc),a0
    lea collision_table,a1
    move.w  #NB_BYTES_PER_MAZE_LINE*NB_LINES-1,d0    ; nb bytes
.loop
    move.b  (a0)+,d1
    moveq.l #7,d2
.bitloop
    btst    #7,d1
    sne (a1)
    addq.l  #1,a1
    lsr.b   #1,d1
    dbf d2,.bitloop
    dbf d0,.loop
    rts

mul224_table
	rept	256
	dc.w	REPTN*224
	endr

; < A0: data (16x16)
; < A1: plane
; < D0: X
; < D1: Y

blit_plane
    move.w  #4,d2       ; 16 pixels + 2 shift bytes

; < A0: data (widthx16)
; < A1: plane
; < D0: X
; < D1: Y
; < D2: blit width in bytes (+2)

blit_plane_any:
Screenwidth = 320

blith = 16


    lea $DFF000,A5
    ; pre-compute the maximum shit here
    mulu    #Screenwidth/8,d1
    move    d0,d3

    and.w   #$F,D3
    and.w   #$F0,d0
    lsr.w   #3,d0
    add.w   d0,d1
    add.l   d1,a1       ; plane position
    
    swap    d3
    clr.w   d3
    lsl.l   #8,d3
    lsl.l   #4,d3
    
    move.l  #$09f00000,d4    ;A->D copy, ascending mode
    or.l   d3,d4            ; add shift

	move.w #Screenwidth/8,d0
    sub.w   d2,d0       ; blit width

    move.w #blith*64,d3
    lsr.w   #1,d2
    add.w   d2,d3

    ; always the same settings (ATM)
	move.w #0,bltamod(a5)		;A modulo=bytes to skip between lines
	move.l #$ffffffff,bltafwm(a5)	;no masking of first/last word
	move.l d4,bltcon0(a5)	

    ; now just wait for blitter ready to write all registers
	bsr	wait_blit
    
    ; blitter registers set
    move.w  d0,bltdmod(a5)	;D modulo
	move.l a0,bltapt(a5)	;source graphic top left corner
	move.l a1,bltdpt(a5)	;destination top left corner
	move.w  d3,bltsize(a5)	;rectangle size, starts blit
    rts
    
wait_blit
	TST.B	$BFE001
.wait
	BTST	#6,dmaconr+$DFF000
	BNE.S	.wait
	rts
 
    include ReadJoyPad.s

joystick_state
    dc.l    0
ready_timer
    dc.w    0
    
gfxbase
    dc.l    0
gfxbase_copperlist
    dc.l    0
GRname:   dc.b "graphics.library",0
    even
    
maze_data:
    incbin  "maze.bin"

    ; 26x29
dot_table_read_only:
    dc.b    1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1
    dc.b    1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1
    dc.b    2,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,2
    dc.b    1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1
    dc.b    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    dc.b    1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1
    dc.b    1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1
    dc.b    1,1,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,1,1
    REPT    11
    dc.b    0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0
    ENDR
    dc.b    1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1
    dc.b    1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1
    dc.b    1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,1
    dc.b    2,1,1,0,0,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,0,0,1,1,2
    dc.b    0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0
    dc.b    0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0
    dc.b    1,1,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,0,0,1,1,1,1,1,1
    dc.b    1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1
    dc.b    1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1
    dc.b    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

player:
    ds.b    Character_SIZEOF

pac_dir_table
    dc.l    pac_anim_left
    dc.l    pac_anim_right
    dc.l    pac_anim_up
    dc.l    pac_anim_down
    
pac_anim_left
    dc.l    pac_dead,pac_left_0,pac_left_1
pac_anim_right
    dc.l    pac_dead,pac_right_0,pac_right_1
pac_anim_up
    dc.l    pac_dead,pac_up_0,pac_up_1
pac_anim_down
    dc.l    pac_dead,pac_down_0,pac_down_1

; BSS --------------------------------------
    SECTION  S2,BSS
HWSPR_TAB_XPOS:	
	ds.l	512			

HWSPR_TAB_YPOS:
	ds.l	512
    
collision_table
    ds.b    NB_BYTES_PER_LINE*NB_LINES*8,0

dot_table
    ds.b    NB_DOTS_PER_LINE*NB_DOT_LINES
    
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
    ; empty
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




screen_data:
    ds.b    SCREEN_PLANE_SIZE*NB_PLANES,0
	

    incdir "../sprites"


ready:
    incbin  "ready_0.bin"

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
    
    
    
DECL_GHOST:MACRO
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

empty_sprite
    dc.l    0
    
    	