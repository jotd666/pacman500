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

;Exec Library Base Offsets


;graphics base

StartList = 38

;autres labels

Execbase  = 4

NB_LINES = 248
NB_BYTES_PER_LINE = 28
MAZE_PLANE_SIZE = NB_BYTES_PER_LINE*NB_LINES
SCREEN_PLANE_SIZE = 40*NB_LINES
NB_PLANES   = 4

Start:
    ;bsr compute_sprite_xy_table
    bsr compute_collision_table

    
    
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
        
        
        moveq #NB_PLANES,d4
        lea	bitplanes,a0              ; adresse de la Copper-List dans a0
        move.l #screen_data,d1
        move.w #bplpt,d3        ; premier registre dans d3

		; 8 bytes per plane:32 + end + bplcontrol
MakeCL:
        move.w d3,(a0)+           ; BPLxPTH
        addq.w #2,d3              ; next register
        swap d1
        move.w d1,(a0)+           ; 
        move.w d3,(a0)+           ; BPLxPTL
        addq.w #2,d3              ; next register
        swap d1
        move.w d1,(a0)+           ; 
        add.l #SCREEN_PLANE_SIZE,d1       ; next plane of maze

        dbf d4,MakeCL
     
	
;DMA activé et Task Switching bloqué
    move.l  4,a6
        jsr _LVOForbid(a6)
        lea $DFF000,a5
        move.w #$03E0,dmacon(A5)

;COPPER initialisé
		
        move.l	#coplist,cop1lc(a5)
        clr.w copjmp1(a5)


;playfield initialisé

        move.w #$3081,diwstrt(a5)             ; valeurs standard pour
        move.w #$30C1,diwstop(a5)             ; la fenêtre écran
        move.w #$0038,ddfstrt(a5)             ; et le DMA bitplane
        move.w #$00D0,ddfstop(a5)
        move.w #$4200,bplcon0(a5) ; 4 bitplanes
        clr.w bplcon1(a5)                     ; no scrolling
        clr.w bplcon2(a5)                     ; pas de priorité
        move.w #0,bpl1mod(a5)                ; modulo de tous les plans = 40
        move.w #0,bpl2mod(a5)


	
    ; bob test
    lea pac_left,a0
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    add.l   #40*6+4,a1
    move.l  #15,d0
.copypac
    move.l  (a0)+,(a1)+
    add.l   #40-4,a1
    dbf d0,.copypac
    

    
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
    lea  red_ghost,a1
    move.l  d0,(a1)
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
    
    add.l  #16,a0
    move.w  #30,d0
    move.w  #80,d1
    bsr store_sprite_pos
    lea  pink_ghost,a1
    move.l  d0,(a1)
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)

    add.l  #16,a0
    move.w  #48,d0
    move.w  #80,d1
    bsr store_sprite_pos
    lea  cyan_ghost,a1
    move.l  d0,(a1)
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)

    add.l  #16,a0
    move.w  #86,d0
    move.w  #80,d1
    bsr store_sprite_pos
    lea  orange_ghost,a1
    move.l  d0,(a1)
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)


    move.w #$83E0,dmacon(a5)
    
; < A0: data (16x16)
; < A1: plane
; < D0: X
; < D1: Y

    lea pac_left,a0
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  #100,d0
    move.w  #100,d1
    bsr blit_plane

    lea pac_left,a0
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    move.w  #102,d0
    move.w  #118,d1
    bsr blit_plane
   

    
	; now sprite test
	bsr		mouse

			        	
;*** fin de programme ***

;liste ancienne du COPPER active

        lea GRname(PC),a1               ; paramètre pour initialiser OpenLibrary
        clr.l d0
        jsr _LVOOpenLibrary(a6)             ; ouverture de la librairie graphique
        move.l d0,a4
        move.l StartList(a4),cop1lc(a5) ; adresse du début de la liste
        clr.w  copjmp1(a5)
        move.w #$8060,dmacon(a5)        ; réinitialisation du canal DMA
        jsr _LVOPermit(a6)                  ; Task Switching autorisé


;mémoire des bitplanes libre


Fin:
        clr.l d0
        rts                       ; quitter le programme

	
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
    move.w  #NB_BYTES_PER_LINE*NB_LINES-1,d0    ; nb bytes
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

blit_plane:
Screenwidth = 320
blitw = 4
blith = 16


    lea $DFF000,A5
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
    
    move.l  #$09f00000,d2    ;A->D copy, ascending mode
    or.l   d3,d2            ; add shift
    
	move.l d2,bltcon0(a5)	
	move.l #$ffffffff,bltafwm(a5)	;no masking of first/last word
	move.w #0,bltamod(a5)		;A modulo=bytes to skip between lines
	move.w #Screenwidth/8-blitw,bltdmod(a5)	;D modulo
.nextp
	; try blit ghost
	move.l a0,bltapt(a5)	;source graphic top left corner
	move.l a1,bltdpt(a5)	;destination top left corner
	move.w #blith*64+blitw/2,bltsize(a5)	;rectangle size, starts blit
	bsr	wait_blit
    rts
    
wait_blit
	TST.B	$BFE001
.wait
	BTST	#6,dmaconr+$DFF000
	BNE.S	.wait
	rts
 
 
GRname:   dc.b "graphics.library",0

maze_data:
    incbin  "maze.bin"


  
    SECTION  S2,BSS
HWSPR_TAB_XPOS:	
	ds.l	512			

HWSPR_TAB_YPOS:
	ds.l	512
    
collision_table
    ds.b    NB_BYTES_PER_LINE*NB_LINES*8,0
    
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
   dc.w  $008e,$3081            ;  DIWSTRT
   dc.w  $0090,$28c1            ;  DIWSTOP
   dc.w  $0102,$0000            ;  BPLCON1 := 0x0000
   dc.w  $0104,$0024            ;  BPLCON2 := 0x0024
   dc.w  $0092,$0038            ;  DDFSTRT := 0x0038
   dc.w  $0094,$00d0            ;  DDFSTOP := 0x00d0
    dc.l    -2




screen_data:
    ds.b    SCREEN_PLANE_SIZE*NB_PLANES,0
	

ghosts:
    dc.l    red_ghost,pink_ghost
pac_left
    incbin  "pac_left.bin"
red_ghost
    dc.l    0
    incbin  "red_ghost.bin"
pink_ghost
    dc.l    0
    incbin  "red_ghost.bin"
cyan_ghost
    dc.l    0
    incbin  "red_ghost.bin"
orange_ghost
    dc.l    0
    incbin  "red_ghost.bin"
empty_sprite
    dc.l    0
    
    	