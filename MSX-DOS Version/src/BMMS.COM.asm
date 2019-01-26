; Bigger Memory Mapper Selector for MSX-DOS1 v0.4 by GDX
;
; Assembled with zasm cross assembler
; http://sourceforge.net/projects/zasm/

;S 15	; Remove the semicolon at the beginning of this line to assemble it with GEN80

; Main-Rom entries

RDSLT	equ	0000Ch		; Read Slot
CALSLT	equ	0001Ch		; Call Slot
ENASLT	equ	00024h		; Slot select
MSXVER	equ	0002Dh
DISSCR	equ	00041h		; Screen disable
ENASCR	equ	00044h		; Screen enable
LDIRMV	equ	00059h		; VRam to Ram transfert
LDIRVM	equ	0005Ch		; Ram to VRam transfert
INITXT	equ	0006Ch		; SCREEN0
BEEP	equ	000C0h		; Produce a BEEP

; System variable entries

DISKVE	equ	0F313h		; Disk-ROM version
RAMAD0	equ	0F341h		; Main-RAM on bank 0 (0000h~3fffh)
RAMAD1	equ	0F342h		; Main-RAM on bank 1 (4000h~7fffh)
RAMAD2	equ	0F343h		; Main-RAM on bank 2 (8000h~bfffh)
RAMAD3	equ	0F344h		; Main-RAM on bank 3 (c000h~ffffh)
EXPTBL	equ	0FCC1h		; Main-ROM Slot
EXTBIO	equ	0FFCAh		; Extended Bios entry

; System scratch area entries

DMA	equ	00080h		; Parameters address
BDOS	equ	00005h		; MSX-DOS function

	org	0100h

PRGstart:
	ld	de,Title_TXT
	ld	c,9
	call	BDOS		; Print the message

	ld	a,(EXPTBL)
	ld	hl,MSXVER
	call	RDSLT	
	cp	3
	jr	c,No_TurboR		; Jump if no turbo R

	ld	a,(DISKVE)
	or	a
	jr	nz,DiskROM2	; Jump if not Disk-ROM v1.xx
No_TurboR:
	ld	hl,PRG_Source
	ld	de,0c000h
	ld	bc,PRGend - 0c000h
	ldir

	jp	PRGrun
DiskROM2:
	ld	d,4
	ld	e,0
	ld	a,(RAMAD2)
	ld	(Map_Slot),a
	ld	b,a
	ld	hl,Mapper_data
	call	EXTBIO
	ld	a,(Mapper_data+4)
	ld	(Segments),a
	jp	TurboR

Final_Stage:

	ld	a,(Map_Slot)
	ld	h,0C0h
	call	ENASLT		; Select the New Main-Rom on bank 3 (c000h~ffffh)

	di
	ld	a,(Map_Slot)
	ld	(RAMAD0),a
	ld	(RAMAD1),a
	ld	(RAMAD2),a
	ld	(RAMAD3),a

TurboR:
;	ld	hl,0F41Fh
;	ld	de,0F420h
;	ld	bc,318
;	ld	(hl),0
;	ldir			; Clear the crunch buffer

	ei

; ---> HL = HL * 16 (Calculation of mapper size)

	ld	a,(Segments)
	rlca
	rlca
	rlca
	rlca
	and	0f0h
	ld	l,a
	ld	a,(Segments)
	rrca
	rrca
	rrca
	rrca
	and	0fh
	ld	h,a		; HL = (Segments) * 16

	ld	a,(Segments)
	cp	255
	jr	nz,M4096
	ld	de,10h
	add	hl,de		; To get a size of 4096kB instead of 4080kB
M4096:

; <---
	ld	de,1000
	ld	b,0		; B=0 if first digit

	ld	ix,MS_TXT+13
	call	Div16
	ld	a,h
	or	l
	jr	z,No_Thousands	; Jump if digit = "0"
	ld	a,l
	add	a,30h
	ld	(ix+0),a	; Set the thousands digit
	inc	ix
	inc	b
No_Thousands:
	ld	h,d
	ld	l,e
	ld	de,100
	call	Div16
	ld	a,h
	or	l
	or	b
	jr	z,No_Hundreds	; Jump if first digit = "0"
	ld	a,l
	add	a,30h
	ld	(ix+0),a	; Set the hundreds digit
	inc	ix
	inc	b
No_Hundreds:
	ld	h,d
	ld	l,e
	ld	de,10
	call	Div16
	ld	a,h
	or	l
	or	b
	jr	z,No_Tens	; Jump if first digit = "0"
	ld	a,l
	add	a,30h
	ld	(ix+0),a	; Set the tens digit
	inc	ix
No_Tens:
	ld	a,e
	add	a,30h
	ld	(ix+0),a	; Set the ones digit

	ld	de,MS_TXT
	ld	c,9
	call	BDOS

	ld	de,KB_TXT
	ld	c,9
	call	BDOS		; Print "kB"

Print_SLT:
	ld	a,(Map_Slot)
	and	3
	add	30h
	ld	(SLT_TXT+15),a	; Set the primary slot
	ld	a,(Map_Slot)
	and	80h
	jr	z,No_Sec_SLT

	ld	a,"-"
	ld	(SLT_TXT+16),a	; Set

	ld	a,(Map_Slot)
	rrca
	rrca
	and	3
	add	30h
	ld	(SLT_TXT+17),a	; Set the secondary slot
No_Sec_SLT:
	ld	de,SLT_TXT
	ld	c,9
	call	BDOS		; Print the slot number

;	ld	de,RET_TXT
;	ld	c,9
;	call	BDOS		;
		
	ret			; Back to DOS

; --->
; Routine: HL = HL / DE
; DE = Rest of a division

Div16:	push	af
	push	bc
	ld	a,h
	ld	c,l
	ld	hl,0
	ld	b,16
DIV:	rl	c
	rla
	adc	hl,hl
	sbc	hl,de
	jr	nc,NOadd_DIV
	add	hl,de
NOadd_DIV:
	ccf
	djnz	DIV
	rl	c
	rla
	ex	de,hl
	ld	h,a
	ld	l,c
	pop	bc
	pop	af
	ret
; <---

Title_TXT:
	db	"BMMS v0.4 by GDX",10,10,13,24h
MS_TXT:
	db	"Mapper Size: ",24h,24h,24h,24h,24h
KB_TXT:
	db	"kB"
RET_TXT:
	db	10,13,24h
SLT_TXT:
	db	"Main-RAM slot: ",24h,24h,24h,24h
No_Mapper_TXT:
	db	13,"No Memory Mapper!",10,13,24h

PRG_Source:
	.PHASE	0c000h

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

PRGrun:

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
	jp	nz,Smaller	; Jump if No Ram
	ld	(08000h),a
	ld	a,(08000h)
	cp	0

	call	z,TST_Mapper_Size	; Call if Ram

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

	ld	a,(Segments)
	cp	1
	jr	nz,Mapper_OK	; Jump if a mapper is found
	ld	de,No_Mapper_TXT
	ld	c,9
	call	BDOS
	jp	Print_SLT

Mapper_OK:

	ld	a,1
	out	(0feh),a	; Select the segment 1 on bank 2 (8000h~bfffh)

	ld	a,(RAMAD2)
	ld	h,80h
	call	ENASLT		; Select the Main-Rom on bank 2 (8000h~bfffh)

	ld	a,1
	out	(0fdh),a	; Select the segment 1 on bank 1 (4000h~7fffh)

	ld	a,(Map_Slot)
	ld	h,40h
	call	ENASLT		; Select the Mapper Slot on bank 1 (4000h~7fffh)

	ld	hl,08000h
	ld	de,04000h
	ld	bc,04000h
	ldir			; Copy the bank RAMAD2 to page 1 of Map_Slot mapper 
;---
	ld	a,3
	out	(0fdh),a

	ld	hl,00000h
	ld	de,04000h
	ld	bc,04000h
	ldir			; Copy the bank RAMAD0 to page 3 of Map_Slot mapper 
;---
	xor	a
	out	(0fdh),a

	ld	hl,0C000h
	ld	de,04000h
	ld	bc,04000h
	ldir			; Copy the bank RAMAD3 to page 0 of Map_Slot mapper 
;---
	ld	a,(RAMAD1)
	ld	h,40h
	call	ENASLT		; Select the Main-RAM on bank 2 (4000h~7fffh)

	ld	a,2
	out	(0fdh),a

	ld	a,(Map_Slot)
	ld	h,80h
	call	ENASLT		; Select the Mapper slot on bank 2 (8000h~bfffh)

	ld	a,2
	out	(0feh),a	; Select the page 2 on bank 2 (8000h~bfffh)

	ld	hl,04000h
	ld	de,08000h
	ld	bc,04000h
	ldir			; Copy the bank RAMAD1 to page 2 of Map_Slot mapper 
;---
	ld	a,1
	out	(0feh),a

	ld	a,(Map_Slot)
	ld	h,40h
	call	ENASLT		; Select the Mapper slot on bank 1 (4000h~7fffh)

	ld	a,(Map_Slot)
	ld	h,0
	call	ENASLT		; Select the Mapper slot on bank 0 (0000h~3fffh)

	jp	Final_Stage

; -> Test routine of Memory Mapper size

TST_Mapper_Size:

	ld	b,255
	ld	hl,Mapper_data
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
	ld	hl,Mapper_data
Restore_Loop:
	ld	a,b
	out	(0feh),a
	ld	a,(hl)
	ld	(08000h),a	; Restore the first byte of each page
	inc	hl
	djnz	Restore_Loop

	call	Slt_Num_conv
	ld	(TMP_Map_Slt),a	; Store slot number
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

Mapper_data:
	ds	256,0

PRGend:
