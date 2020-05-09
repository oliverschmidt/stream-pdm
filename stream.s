.export     _streamafterexit
.import     done, abort_key

.include    "apple2.inc"
.macpack    apple2
.pc02

src         := $00
dst         := $02
rd_count    := $04
wr_count    := $05
scroll_row  := $06
scroll_col  := $07
scroll_opc  := $08

program     := $100B        ; adressed via jmp_table !
buffer      := $9000
zp_save     := $9100

px          = $8F           ; force page crossing
silence     = $FF
skew        = 3
end_mark    = $FF
rd_mark     = $FE
wr_mark     = $FD
max_row     = 15
max_column  = 39

scroll_ldx  = $AE
scroll_ldy  = $AC
scroll_stx  = $8E
scroll_sty  = $8C

_streamafterexit:
    asl
    asl
    asl
    asl
    tax
    clc
    adc #px
    sta slot
    txa
    ora #%10001111
    sta slot_mask

    lda abort_key
    sta quit_key

    lda #<stream
    ldx #>stream
    sta done+1
    stx done+2
    rts


.segment    "LOWCODE"


slot:
    .res 1

slot_mask:
    .res 1

quit_key:
    .res 1

key_state:
    .res 1

jmp_table:
    .byte $0B, $10, $10     ; second byte used both as high and low !

lowres_lo:
    .byte $00, $80, $00, $80, $00, $80, $28, $A8
    .byte $28, $A8, $28, $A8, $28, $A8, $50

lowres_hi:
    .byte $05, $05, $06, $06, $07, $07, $04, $04
    .byte $05, $05, $06, $06, $07, $07, $04

streaming:
    scrcode "Streaming...       [Space] - Pause     [Esc] - Stop"
    .byte $00

pausing:
    scrcode "Pausing...         [Space] - Stream"
    .byte $00

waiting:
    scrcode "Waiting...                        "
    .byte $00


update_status:
    sta update_status+9
    stx update_status+10
    ldx #$00
chr:lda a:$0000,x           ; patched
    bne :+
    rts
:   tay
    txa
    lsr                     ; even cols go into aux mem
    tax
    tya
    bcs :+
    bit $C055               ; aux mem
:   sta $07D0,x
    bcs :+
    bit $C054               ; main mem
:   txa
    rol
    tax
    inx
    bra chr


stream:
    ; Save zero page
    ldx #$00
:   lda $00,x
    sta zp_save,x
    inx
    bne :-

    ; Crate unrolled loop program
    jsr unroll

    ; Create socket 0 physical read memory lookup table in zero page
    ldx #$00
:   txa
    and #>$1FFF
    ora #>$6000
    sta $00,x
    inx
    bne :-

    ; Init part of ring buffer that is played before first data is available
    ldx #skew-1
    lda #silence
:   sta buffer,x
    dex
    bpl :-

    ; Show status 'Streaming'
    lda #<streaming
    ldx #>streaming
    jsr update_status

    ; Run unrolled loop program
    jsr program

    ; Restore zero page
    ldx #$00
:   lda zp_save,x
    sta $00,x
    inx
    bne :-

    ; Switch to text screen
    bit $C051

    ; Clear screen
    jsr $FC58               ; HOME

    ; Check ProDOS system bit map
    lda $BF6F               ; protection for pages $B8 - $BF
    cmp #%00000001          ; exactly system global page is protected
    beq :+
    jmp DOSWARM

    ; Quit to ProDOS dispatcher
:   jsr $BF00               ; MLI call entry point
    .byte $65               ; quit
    .word par

    ; MLI parameter list for quit
par:.byte $04               ; param_count
    .byte $00               ; quit_type
    .word $0000             ; reserved
    .byte $00               ; reserved
    .word $0000             ; reserved


disconnect:
    ldx slot
    lda #$04                ; socket 0
    sta $C085-px,x
    lda #$01                ; command register
    sta $C086-px,x
    lda #$08                ; DISCON
    sta $C087-px,x
    rts


check_connect:
    ; Show status 'Waiting'
    lda #<waiting
    ldx #>waiting
    jsr update_status

    ; Check if still connected at all
    ldx slot
chk:lda #$04                ; socket 0
    sta $C085-px,x
    lda #$03                ; status register
    sta $C086-px,x
    lda $C087-px,x
    cmp #$17                ; SOCK_ESTABLISHED
    beq :+
    pla                     ; completely ...
    pla                     ; quit ...
    rts                     ; stream

    ; Check if at least one page available
:   lda #$04                ; socket 0
    sta $C085-px,x
    lda #$26                ; received size register
    sta $C086-px,x
    lda $C087-px,x             ; high byte
    beq :+

    ; Show status 'Streaming'
    lda #<streaming
    ldx #>streaming
    jsr update_status
    ldx #$04                ; socket 0
    rts                     ; return to stream

    ; Check keyboard
:   lda $C000
    bpl chk
    bit $C010
    cmp quit_key
    beq :+
    cmp #$9B                ; Esc
    beq :+
    bra chk

:   pla                     ; completely ...
    pla                     ; quit ...
    jmp disconnect          ; stream


check_keyboard:
    stz key_state

key:bit $C010               ; keyboard strobe
    cpx quit_key
    beq :+
    cpx #$9B                ; Esc
    beq :+
    cpx #$A0                ; Space
    beq :++

    ldx key_state
    bne get
    rts                     ; return to stream

:   pla                     ; completely ...
    pla                     ; quit ...
    jmp disconnect          ; stream

:   lda key_state
    beq :+

    ; Show status 'Streaming'
    lda #<streaming
    ldx #>streaming
    jmp update_status       ; return to stream

    ; Show status 'Pausing'
:   lda #<pausing
    ldx #>pausing
    jsr update_status

    ; Wait for keypress
get:ldx $C000               ; keyboard
    bpl :-
    inc key_state
    bra key


unroll:
    lda #<program
    ldx #>program
    sta dst
    stx dst+1

    stz rd_count
    lda #skew
    sta wr_count

    lda #<recv_prolog
    ldy #>recv_prolog
    jsr copy

    ldx #$0100 / 7
:   lda #<recv_loop
    ldy #>recv_loop
    jsr copy
    dex
    bne :-

    lda #<recv_epilog
    ldy #>recv_epilog
    jsr copy

    stz scroll_row
    lda #max_column
    sta scroll_col
    lda #scroll_ldx
    sta scroll_opc
    ldx #$07
scr:cpx #$07
    bne :+
    lda #<speaker_load
    ldy #>speaker_load
    bra :++
:   jsr scroll_patch
    lda #<speaker
    ldy #>speaker
:   jsr copy
    dex
    bpl :+
    ldx #$07
:   lda scroll_row
    cmp #max_row
    bne scr

fil:cpx #$07
    bne :+
    lda #<speaker_load
    ldy #>speaker_load
    bra :++
:   lda #<speaker_nop4
    ldy #>speaker_nop4
:   jsr copy
    dex
    bpl :+
    ldx #$07
:   lda rd_count
    bne fil
    cpx #$01
    bne fil

    lda #<speaker_loop
    ldy #>speaker_loop
    jsr copy
    rts


copy:
    sta src
    sty src+1
    ldy #$00
nxt:lda (src),y
    cmp #end_mark
    bne :++
    tya
    clc
    adc dst
    sta dst
    bcc :+
    inc dst+1
:   rts
:   cmp #rd_mark
    bne :+
    lda rd_count
    inc rd_count
    bra put
:   cmp #wr_mark
    bne :+
    lda wr_count
    inc wr_count
    bra put
:   cmp #$F5                ; slot mask
    bcc put
    and slot_mask
put:sta (dst),y
    iny
    bne nxt
    inc src+1
    inc dst+1
    bra nxt


scroll_patch:
    lda scroll_opc
    sta speaker             ; opcode
    ldy scroll_row
    lda lowres_lo,y
    clc
    adc scroll_col
    sta speaker+1           ; operand low
    lda lowres_hi,y
    sta speaker+2           ; operand high

    lda scroll_opc
    cmp #scroll_ldx
    bne :+
    dec scroll_col
    lda #scroll_ldy
    sta scroll_opc
    rts

:   cmp #scroll_ldy
    bne :+
    inc scroll_col
    lda #scroll_sty
    sta scroll_opc
    rts

:   cmp #scroll_sty
    bne :++
    dec scroll_col
    beq :+
    dec scroll_col
    lda #scroll_ldy
    sta scroll_opc
    rts

:   lda #scroll_stx
    sta scroll_opc
    rts

:   lda #scroll_ldx
    sta scroll_opc
    lda #max_column
    sta scroll_col
    inc scroll_row
    rts


; 2 bytes, 2 cycles
.macro nop2
    bit #$00
.endmacro


; 2 bytes, 3 cycles
.macro nop3
    bit $00
.endmacro


; 3 bytes, 4 cycles
.macro nop4
    bit a:$0000
.endmacro


; 13 bytes, 9 cycles
.macro      spkr
    lsr a                   ;   2
    bcs :+                  ;   2   3
    nop                     ;   2
    bra :++                 ;   3
    .res 4
:   bit $C030               ;       4
:.endmacro


; 9 bytes, 9 cycles
.macro      spkr_short
    lsr a                   ;   2
    bcs :+                  ;   2   3
    nop                     ;   2
    bra :++                 ;   3
:   bit $C030               ;       4
:.endmacro


; 14 bytes, 9 cycles
.macro      spkr_long
    lsr a                   ;   2
    bcs :+                  ;   2   3
    nop                     ;   2
    bra :++                 ;   3
    .res 5
:   bit $C030               ;       4
:.endmacro


; 13 bytes, 9 cycles
.macro      spkr_jsr    addr
    lsr a                   ;   2
    bcs :+                  ;   2   3
    nop                     ;   2
    bra :++                 ;   3
    .res 1
    jsr addr
:   bit $C030               ;       4
:.endmacro


; 16 bytes, 13 cycles
.macro      spkr_load
    lda buffer | rd_mark    ;   4
    lsr a                   ;   2
    bcs :+                  ;   2   3
    nop                     ;   2
    bra :++                 ;   3
    .res 4
:   bit $C030               ;       4
:.endmacro


; 22 bytes, 20 cycles (= 2 * 13 - 6 used by speaker_loop)
.macro      spkr_init
    nop4                    ;           4   $100B (bit 7 is 0)
    bra :+                  ;           3
    nop3                    ;   3           $1010 (bit 7 is 1)
    bit $C030               ;   4
:   lda buffer | rd_mark    ;   4
    lsr a                   ;   2
    bcs :+                  ;   2   3
    nop                     ;   2
    bra :++                 ;   3
:   bit $C030               ;       4
:.endmacro


recv_prolog:
    spkr_init               ; bit 0
    ldx #$04                ; socket 0
    nop
    spkr_short              ; bit 1
    stx $C0F5               ; socket 0
    spkr                    ; bit 2
    ldy #$26                ; received size register
    nop
    spkr                    ; bit 3
    sty $C0F6               ; received size register
    spkr                    ; bit 4
    ldy $C0F7               ; high byte
    spkr                    ; bit 5
    dey                     ; at least one page available
    bmi *+9                 ; branch to jsr
    spkr_jsr check_connect  ; bit 6
    stx $C0F5               ; socket 0
    spkr                    ; bit 7
    spkr_load               ; bit 0
    ldx #$28                ; read pointer register
    nop
    spkr                    ; bit 1
    stx $C0F6               ; read pointer register
    spkr                    ; bit 2
    ldx $C0F7               ; high byte
    spkr                    ; bit 3
    ldy $00,x               ; high byte -> physical
    spkr_long               ; bit 4
    ldx $C0F7               ; low byte
    spkr                    ; bit 5
    sty $C0F5               ; read addr high
    spkr                    ; bit 6
    stx $C0F6               ; read addr low
    spkr                    ; bit 7
    .byte end_mark


recv_loop:
    spkr_load               ; bit 0
    ldx $C0F7               ; data
    spkr                    ; bit 1
    stx buffer | wr_mark
    spkr                    ; bit 2
    ldx $C0F7               ; data
    spkr                    ; bit 3
    stx buffer | wr_mark
    spkr                    ; bit 4
    ldx $C0F7               ; data
    spkr                    ; bit 5
    stx buffer | wr_mark
    spkr                    ; bit 6
    ldx $C0F7               ; data
    spkr                    ; bit 7
    spkr_load               ; bit 0
    stx buffer | wr_mark
    spkr                    ; bit 1
    ldx $C0F7               ; data
    spkr                    ; bit 2
    stx buffer | wr_mark
    spkr                    ; bit 3
    ldx $C0F7               ; data
    spkr                    ; bit 4
    stx buffer | wr_mark
    spkr                    ; bit 5
    ldx $C0F7               ; data
    spkr                    ; bit 6
    stx buffer | wr_mark
    spkr                    ; bit 7
    .byte end_mark


recv_epilog:
    ; $0100 % 7 = 4 -> 4 data bytes left
    spkr_load               ; bit 0
    ldx $C0F7               ; data
    spkr                    ; bit 1
    stx buffer | wr_mark
    spkr                    ; bit 2
    ldx $C0F7               ; data
    spkr                    ; bit 3
    stx buffer | wr_mark
    spkr                    ; bit 4
    ldx $C0F7               ; data
    spkr                    ; bit 5
    stx buffer | wr_mark
    spkr                    ; bit 6
    ldx $C0F7               ; data
    spkr                    ; bit 7
    spkr_load               ; bit 0
    stx buffer | wr_mark

    ; Commit recv
    spkr                    ; bit 1
    ldx #$04                ; socket 0
    nop
    spkr                    ; bit 2
    stx $C0F5               ; socket 0
    spkr                    ; bit 3
    ldy #$28                ; received size register
    nop
    spkr                    ; bit 4
    sty $C0F6               ; received size register
    spkr                    ; bit 5
    ldy $C0F7               ; high byte
    spkr                    ; bit 6
    iny                     ; commit one page
    nop2
    spkr                    ; bit 7
    spkr_load               ; bit 0
    stx $C0F5               ; socket 0
    spkr                    ; bit 1
    ldx #$28                ; received size register
    nop
    spkr                    ; bit 2
    stx $C0F6               ; received size register
    spkr                    ; bit 3
    sty $C0F7               ; high byte
    spkr                    ; bit 4
    ldx #$04                ; socket 0
    nop
    spkr                    ; bit 5
    stx $C0F5               ; socket 0
    spkr                    ; bit 6
    ldx #$01                ; command register
    nop
    spkr                    ; bit 7
    spkr_load               ; bit 0
    stx $C0F6               ; command register
    spkr                    ; bit 1
    ldx #$40                ; RECV
    nop
    spkr                    ; bit 2
    stx $C0F7               ; RECV
    spkr                    ; bit 3

    ; Check keyboard
    ldx $C000               ; keyboard
    spkr                    ; bit 4
    inx                     ; prepare for N flag
    bit #$00                ; 2 byte nop
    spkr                    ; bit 5
    dex                     ; set N flag
    bmi *+9                 ; branch to jsr
    spkr_jsr check_keyboard ; bit 6

    ; Filler
    nop4
    spkr                    ; bit 7
    .byte end_mark


; 16 bytes, 13 cycles
speaker:
    .res 3                  ; patched
    spkr
    .byte end_mark


; 16 bytes, 13 cycles
speaker_load:
    spkr_load
    .byte end_mark


; 16 bytes, 13 cycles
speaker_nop4:
    nop4
    spkr
    .byte end_mark


; 14 bytes, 13 cycles (+ 6 cycles)
speaker_loop:
    lsr a                   ;   2       bit 6
    tax                     ;   2       bit 7
    nop                     ;   2
    bcs :+                  ;   2   3
    nop                     ;   2
    bra :++                 ;   3
:   bit $C030               ;       4
:   jmp (jmp_table,x)       ;   6
    .byte end_mark
