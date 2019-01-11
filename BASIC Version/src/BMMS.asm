; Bigger Memory Mapper Selector v0.4 by GDX
;
; Execute BMMS as following: BLOAD"BMMS",R
; or as below to force the mapper selection on Turbo R:
; BLOAD"BMMS": POKE&HD01F,1: DEFUSR=&HD01E: A=USR(0)
;
; Assembled with zasm cross assembler
; http://sourceforge.net/projects/zasm/

;S 15	; Remove the semicolon at the beginning of this line to assemble it with GEN80

; Main-Rom entries

CALSLT:	equ	0001Ch		; Call Slot
ENASLT:	equ	00024h		; Slot select
MSXVER:	equ	0002Dh		; Read MSX version
DISSCR:	equ	00041h		; Screen disable
ENASCR:	equ	00044h		; Screen enable
LDIRMV:	equ	00059h		; VRAM to RAM transfert
LDIRVM:	equ	0005Ch		; RAM to VRAM transfert
INITXT:	equ	0006Ch		; SCREEN0
RSLREG:	equ	00138h		; Read primary slots register

; System variable entries

DISKVE:	equ	0F313h		; Disk-ROM version
RAMAD0:	equ	0F341h		; Main-RAM slot for the bank 0000h~3FFFh
RAMAD1:	equ	0F342h		; Main-RAM slot for the bank 4000h~7FFFh
RAMAD2:	equ	0F343h		; Main-RAM slot for the bank 8000h~BFFFh
RAMAD3:	equ	0F344h		; Main-RAM slot for the bank C000h~FFFFh
KBUF:	equ	0F41Fh		; Crunch Buffer
H.PHYD:	equ	0FFA7h		; Physical Disk I/O Hook
EXPTBL:	equ	0FCC1h		; Main-ROM Slot

	org	0D000h-7

; Binary file header

	db	0FEh
	dw	PRGstart,PRGend,PRGrun

PRGstart:

Prim_SLT:
	db	0
Sec_SLT:
	db	0
Map_Slot:
	db	0
Segments:
	db	0
TMP_Map_Slt:
	db	0
TMP_Segments:
	db	0
PRG_Ver:
	db	"BMMS v0.4 by GDX",0
PRGrun:
	ld	a,0		; LD A,0 must be at the address 0D01Eh
	cp	0
	jr	nz,Not_TR	; Jump if POKE&HD01F,1 was entered

	ld	a,(MSXVER)
	cp	3
	jr	c,Not_TR	; Jump if not Turbo R
	ld	a,(DISKVE)
	or	a
	jr	z,Not_TR	; Jump if Disk-ROM v1.xx
	jp	POKE
Not_TR:	
	call	DISSCR		; Disable the display
	ld	hl,08000h
	ld	de,0
	ld	bc,04000h
	call	LDIRVM		; Store the Bank 2 to VRAM

	di
	ld	a,(Sec_SLT)
	inc	a
MapSel_loop:
	ld	(Sec_SLT),a

	call	Slt_Num_conv

	ld	h,80h
	call	ENASLT		; Select the slot X-X bank 2 (8000h~bfffh)

	ld	a,0ffh
	ld	(08000h),a
	ld	a,(08000h)
	cp	0ffh
	ld	a,0
	jp	nz,Smaller	; Jump if No RAM
	ld	(08000h),a
	ld	a,(08000h)
	cp	0

	call	z,TST_Mapper_Size	; Call if RAM found

	ld	a,(TMP_Segments)
	or	a
	jr	z,Smaller		; Jump if no segment
	ld	b,a
	ld	a,(Segments)
	cp	b
	jr	nc,Smaller		; Jump if fewer segments than the previous time

	call	Slt_Num_conv
	
	ld	a,(TMP_Segments)
	ld	(Segments),a
	ld	a,(TMP_Map_Slt)
	ld	(Map_Slot),a
	
Smaller:
	ld	a,(Sec_SLT)
	cp	3
	jr	nz,PRGrun
	xor	a
	ld	(Sec_SLT),a

	ld	a,(Prim_SLT)
	inc	a
	ld	(Prim_SLT),a
	cp	4
	ld	a,0
	jp	nz,MapSel_loop	; Jump if all slots are not scanned

; Move Main-RAM

	xor	a
	out	(0feh),a	; Select the segment 0 on bank 2 (8000h~bfffh)

	ld	a,(Map_Slot)
	ld	h,80h
	call	ENASLT		; Select the Main-Rom on bank 2 (8000h~bfffh)

	ld	hl,0C000h
	ld	de,08000h
	ld	bc,3FFFh
	ldir

; -> Replace slot numbers in system variables

;	ld	a,(H.PHYD)
;	cp	0c9h
;	jr	z,No_Disk	; Jump if no disk
	ld	a,(Map_Slot)
	ld	hl,RAMAD0-04000h
	ld	(hl),a
	inc	hl
	ld	(hl),a
	inc	hl
	ld	(hl),a
	inc	hl
	ld	(hl),a
No_Disk:
; <-
	ld	a,1
	out	(0feh),a	; Select the segment 1 on bank 2 (8000h~bfffh)
	
	ld	hl,0
	ld	de,08000h
	ld	bc,04000h
	call	LDIRMV		; Restore the Bank 2 with the contents of VRAM
	call	INITXT
	call	ENASCR		; Enable the display

;	xor	a
;	ld	(08000h),a	; First BASIC address to 0

	ld	a,(Map_Slot)
	ld	h,0C0h
	call	ENASLT		; Select the New Main-Rom on bank 2 (c000h~ffffh)
POKE:
	call	RSLREG
	and	0F0h
	jr	z,No_POKE	; Jump if the RAM is in slot 0-X 

	ld	a,(0FFFFh)
	cpl
	and	0F0h
	ld	b,a
	rrca
	rrca
	rrca
	rrca
	or	b
	ld	(0FFFFh),a	; POKE-1,(15-PEEK(-1)\16)*17
No_POKE:
	ld	hl,KBUF
	ld	de,KBUF+1
	ld	bc,318
	ld	(hl),0
	ldir			; Clear the crunch buffer

	ld	a,2
	out	(0fdh),a	; Select the segment 2 on bank 2 (4000h~7fffh)
	inc	a
	out	(0fch),a	; Select the segment 3 on bank 2 (0000h~3fffh)

	ret			; Back to Basic

; -> Test routine of Memory Mapper size

TST_Mapper_Size:

	ld	b,255
	ld	hl,KBUF
	di
Store_Loop:
	ld	a,b
	out	(0feh),a
	ld	a,(08000h)
	ld	(hl),a		; Store the first byte of each page
	inc	hl
	djnz	Store_Loop

	ld	b,255
MM_Size_Loop1:
	ld	a,b
	out	(0feh),a
	ld	(08000h),a
	djnz	MM_Size_Loop1

	ld	b,255
MM_Size_Loop2:
	ld	a,b
	out	(0feh),a
	ld	a,(08000h)
	cp	b
	jr	nc,MM_SIZE	; Jump if detect size is bigger
	djnz	MM_Size_Loop2
	
	xor	a		; No segment

MM_SIZE:
	ld	(TMP_Segments),a	; Store temporarily segments number found

	ld	b,255
	ld	hl,KBUF
Restore_Loop:
	ld	a,b
	out	(0feh),a
	ld	a,(hl)
	ld	(08000h),a	; Restore the first byte of each page
	inc	hl
	djnz	Restore_Loop

	call	Slt_Num_conv
	ld	(TMP_Map_Slt),a		; Store temporarily slot number
	ret
; <-

; -> Slot number conversion
; Entry: Prim_SLT, Sec_SLT
; Output: A = Slot number (FxxxPPSS)
; Modify: A, BC, HL

Slt_Num_conv:
	ld	a,(Prim_SLT)
	ld	c,a
	ld	b,0
	ld	hl,EXPTBL
	add	hl,bc
	ld	a,(hl)
	and	80h
	ld	a,c
	ret	z		; Back if primary slot

	ld	a,(Sec_SLT)
	rlca
	rlca
	or	c
	or	080h
	ret
; <-

PRGend: