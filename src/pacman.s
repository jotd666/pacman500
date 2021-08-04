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
	


	
;CIA-A registre port A (bouton souris)

CIAAPRA = $BFE001

;Exec Library Base Offsets


;graphics base

StartList = 38

;autres labels

Execbase  = 4

NB_LINES = 248
MAZE_PLANE_SIZE = 28*NB_LINES
SCREEN_PLANE_SIZE = 40*NB_LINES
NB_PLANES   = 4

Start:
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

        move.w #$83E0,dmacon(a5)

	
    ; bob test
    lea pac_left,a0
    lea	screen_data+SCREEN_PLANE_SIZE,a1
    add.l   #40*6+4,a1
    move.l  #15,d0
.copypac
    move.l  (a0)+,(a1)+
    add.l   #40-4,a1
    dbf d0,.copypac
    
    lea  sprites,a0
    lea  red_ghost,a1
    ;move.l  #0,(a1)     ; TEMP
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)
    addq.l  #8,a0
    lea  pink_ghost,a1
    ;move.l  #0,(a1)     ; TEMP
    move.l  a1,d0
    move.w  d0,(6,a0)
    swap    d0
    move.w  d0,(2,a0)

    
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

wait_blit
	TST.B	$BFE001
.wait
	BTST	#6,dmaconr+$DFF000
	BNE.S	.wait
	rts
	
mouse:	
	;move.w	#$0F0,$DFF180
	BTST #6,$BFE001
	BNE  mouse
	rts
	

;constante

GRname:   dc.b "graphics.library",0

maze_data:
    incbin  "maze.bin"

	even
	
    SECTION  S2,DATA,CHIP

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
    ; pink ghost
    dc.w    sprpt+8,0
    dc.w    sprpt+10,0

 
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
	
SPRITE_POS:MACRO
    DC.W ((\2&$FF)<<8)|(((\1-1)&$1FE)>>1)
	DC.W (((\2+\3)&$FF)<<8)|((\2&$100)>>6)|(((\2+\3)&$100)>>7)|((\1-1)&$1)
    ENDM
    
pac_left
    incbin  "pac_left.bin"
red_ghost
	SPRITE_POS  10,10,16
	dc.w	$30A0,$4090
    incbin  "red_ghost.bin"
pink_ghost
	;SPRITE_POS  100,60,16
	dc.w	$3080,$4090
    incbin  "pink_ghost.bin"

    