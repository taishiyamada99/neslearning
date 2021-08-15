.segment "HEADER"
    .byte "NES"
    .byte $1a
    .byte $02 ; 2 * 16KByte PRG ROM
    .byte $01 ; 1 * 8KByte CHR ROM
    .byte %00000000 ; mapper and mirrorig
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00, $00, $00, $00, $00 ; filler bytes

.segment "ZEROPAGE"
temp:           .res 2
nmi_count:      .res 1
pos_x:          .res 1
pos_y:          .res 1
player_1_x:     .res 1
player_1_y:     .res 1
player_1_a:     .res 1
player_1_b:     .res 1
world:          .res 2  ;worldmap読み込み時のaddr指定

.segment "STARTUP"
reset:
    sei             ; Disable all interrupts
    cld             ; Disable Decimal mode
    ldx #$40        ; Disable sound IRQ
    stx $4017
    ldx #$FF        ; Initialize the stack register
    txs
    inx             ; #$FF + 1 => #$00
    stx $4010

;PPU初期化
    lda #%00001000  ; NMI Disabled, BG $0000, CHR $1000, VRAMADDR +1
    sta $2000
    lda #%00000000  ; BG/Sprit Disabled
    sta $2001

:                   ; wait for vblank
    bit $2002
    bpl :-
    txa

clear_mem:
    sta $0000, x    ; $0000 => $00FF
    sta $0100, x 
    sta $0300, x
    sta $0400, x 
    sta $0500, x 
    sta $0600, x 
    sta $0700, x
    lda #$FF
    sta $0200, x    ;Sprit Mem
    lda #$00
    inx
    bne clear_mem

:                   ; wait for vblank
    bit $2002
    bpl :-
    lda #$02        ;sprint memの指定 ($0300 - $03FF)
    sta $4014
    nop

;最初のBG描画
    lda #$20        ;nametable $2000
    sta $2006
    lda #$00
    sta $2006

    lda #$C0        ;$03C0 = 960
    sta temp
    lda #$03
    sta temp+1

:
    lda #$FF        ;nametableをすべてFFに
    sta $2007
    dec temp
    bne :-
    lda temp+1
    beq :+
    dec temp+1
    jmp :-
:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mainloop:
    bit $2002       ; wait for vblank
    bpl mainloop

    jmp map_drawing


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
map_drawing:

;まだ途中
;１）現在地座標から、worldmapの読み込む起点を計算
;２）worldmapを32タイル読み込む（横１行分）
;３）Y座標をずらす
;４）２）に戻る。３０行書いたら終わり。

    lda #08
    sta pos_x
    lda #08
    sta pos_y

;１）現在地座標から、worldmapの読み込む起点を計算
    lda #<worldmap          ;worldmapのデータの番地を読ませる
    sta world
    lda #>worldmap
    sta world+1

    ldx pos_y
:
    lda world
    clc
    adc #128         ;64 x 2
    sta world
    lda world+1
    adc #00
    sta world+1
    dex
    bne :-
    clc
    adc pos_x
    sta world
    lda world+1
    adc #00
    sta world+1
    clc
    adc pos_x       ;pos_x x 2 (2倍の値を入れる)
    sta world
    lda world+1
    adc #00
    sta world+1


;２）worldmapを32タイル読み込む（横１行分）

    lda #$20        ;nametable $2000
    sta $2006
    lda #$00
    sta $2006

    ldx #30
:
    ldy #00
:
    lda (world), y
    sta $2007
    iny
    cpy #32
    bne :-

;３）Y座標をずらす
    dex
    beq :+
    lda world
    clc
    adc #64
    sta world
    lda world+1
    adc #00
    sta world+1
    jmp :--
:

    lda #00
    sta $2005       ;x
    lda #00
    sta $2005       ;y

BG_Enabled:
    lda #%00001000  ; NMI Disabled, BG $0000, CHR $1000, VRAMADDR +1
    sta $2000
    lda #%00001110  ; BG Enabled
    sta $2001

loop:
    jmp loop



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
nmi:
    pha         ;スタックへy,x,aを退避
    txa
    pha         
    tya
    pha

    inc nmi_count

sprit_dma:
    lda #$00        ; Sprit DMA転送用
    sta $2003
    lda #$02        ; copy sprite data from $0200 => PPU memory for display (DMA転送)
    sta $4014

    pla             ;スタックからy,x,aへ戻す
    tay
    pla
    tax
    pla
    rti             ;nmi終了

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PaletteData_Sprit:
    .byte $0F,$11,$30,$36,$0F,$36,$25,$16  ;勇者標準、魔法使いーピンク
    .byte $0F,$36,$30,$12,$0F,$36,$19,$0A  ;賢者ー青, 魔法使いー緑
    .byte $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F
    .byte $0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F

PaletteData_BG:
    .byte $0F,$1A,$3A,$0F,$0F,$1A,$3A,$0F ;文字表示用、黒、白、ライトグレー、ダークグレー、  ;草原用
    .byte $0F,$30,$10,$00,$0F,$30,$10,$00
    .byte $0F,$30,$10,$00,$0F,$30,$10,$00
    .byte $0F,$30,$10,$00,$0F,$30,$10,$00

worldmap:
    .incbin "worldmap2.bin"

.segment "VECTORS"
    .word nmi
    .word reset

.segment "CHARS"
    .incbin "set03.chr"