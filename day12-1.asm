;テストのコメント行 Day13

JOYPAD1 = $4016
JOYPAD2 = $4017

DIST_UP = $08
DIST_RIGHT = $10
DIST_DOWN = $00
DIST_LEFT = $18

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
world:          .res 2
buttons:        .res 1
buttons1:       .res 1
chrstyle:       .res 1
count:          .res 1
count2:         .res 1
temp1:          .res 1
last_dist:      .res 1
player1_x:      .res 1
player1_y:      .res 1
player1_tile:   .res 1 ;タイル
player1_dist:   .res 1 ;向き
move_count:     .res 1 ;移動中かどうかの判定フラグ（カウンター）
move_count_last:    .res 1
last_player1_x: .res 1

.segment "STARTUP"
Reset:
    SEI ; Disable all interrupts
    CLD ; Disable Decimal mode

    ; Disable sound IRQ
    LDX #$40
    STX $4017

    ; Initialize the stack register
    LDX #$FF
    TXS
    INX ; #$FF + 1 => #$00

    ; Zero out the PPU registers
    STX $2000
    STX $2001

    STX $4010
:
    BIT $2002
    BPL :-

    TXA

CLEARMEM:
    STA $0000, x ; $0000 => $00FF
    STA $0100, x 
    STA $0300, x
    STA $0400, x 
    STA $0500, x 
    STA $0600, x 
    STA $0700, x
    LDA #$FF
    STA $0200, x ;Sprit Mem
    LDA #$00
    INX
    BNE CLEARMEM
; wait for vblank
:
    BIT $2002
    BPL :-

    LDA #$03
    sta $4014
    nop

;#### Parretsのロード
    lda #$3F        ; $3F00　BackGroud Palletsのロード先
    sta $2006
    lda #$00
    sta $2006

    ldx #$00
LoadPalettes_BG:
    lda PaletteData_BG, x ; PalletsDataの読み込み
    sta $2007
    inx
    cpx #$10
    BNE LoadPalettes_BG

    ldx #$00
LoadPalettes_Sprit:
    lda PaletteData_Sprit, x ; PalletsDataの読み込み
    sta $2007
    inx
    cpx #$10
    BNE LoadPalettes_Sprit

    ; Initialize world to point to world data
    lda #<WorldData
    sta world
    lda #>WorldData
    sta world+1

    ; setup addrss in PPU for nametable data ネームテーブルの生成 PPU $2000
    BIT $2002

    lda #$20
    sta $2006
    lda #$00
    sta $2006

    ldx #$00
    ldy #$00

LoadWorld:
    lda (world), y
    sta $2007
    iny
    cpx #$03
    bne :+
    cpy #$C0
    BEQ DoneLoadingWorld
:
    cpy #$00
    bne LoadWorld
    inx 
    inc world+1
    jmp LoadWorld

DoneLoadingWorld:
    ldx #$00

SetAttributes:
    lda #$55
    sta $2007
    inx
    cpx #$40
    BNE SetAttributes

    ldx #$00
    ldy #$00

SetWindowPosition:
    lda #$00
    sta $2005
    sta $2005

    lda #$00
    sta temp1

;player1 初期化
    lda #$00
    sta player1_x
    sta player1_y
    lda #DIST_DOWN
    sta player1_dist
    lda #$00 ;魔法使い　正面タイル
    sta player1_tile
    lda #%00000001 ;パレットBG-01
    sta $0202   ;スプライト属性（反転など）
    sta $0206   ;スプライト属性（反転など）
    sta $020A   ;スプライト属性（反転など）
    sta $020E   ;スプライト属性（反転など）

    lda #112    ;Y中心の座標 
    sta $0200   ;Y座標
    sta $0204   ;Y座標
    clc
    adc #8
    sta $0208   ;Y座標
    sta $020C   ;Y座標

    lda #120    ;X中心の座標 
    sta $0203   ;X座標
    sta $020B   ;X座標
    clc
    adc #8
    sta $0207   ;X座標
    sta $020F   ;X座標

    jsr chr_style_change

;----------キャラここまで
    lda #$00
    sta count

;PPU初期化
    lda #%10000100  ; enable NMI change BG $0000 Sprit $1000
    sta $2000
    lda #%00000110  ; Sprites: Disabled (bit4) / BG:Enabled (bit3 = true)
 ;  lda #%00011110  ; Enable sprites and BG:Enabled (bit3 = true)
 ;  lda #%00010110  ; Enable sprites and BG:Disabled(bit3 = false)
    sta $2001

; Enable interrupts
    cli

; オープニングループ待ち
:
    lda #60
    cmp count2
    bne :-

;PPU初期化
    lda #%10000100  ; enable NMI change BG $0000 Sprit $1000
    sta $2000
    lda #%00001110  ; Sprites: Disabled (bit4) / BG:Enabled (bit3 = true)
 ;  lda #%00011110  ; Enable sprites and BG:Enabled (bit3 = true)
 ;  lda #%00010110  ; Enable sprites and BG:Disabled(bit3 = false)
    sta $2001

; オープニングループ待ち
:
    lda #160
    cmp count2
    bne :-

;PPU初期化 2
   lda #%10000100  ; enable NMI change BG $0000 Sprit $1000
    sta $2000
 ;  lda #%00001110  ; Sprites: Disabled (bit4) / BG:Enabled (bit3 = true)
   lda #%00011110  ; Enable sprites and BG:Enabled (bit3 = true)
;    lda #%00010110  ; Enable sprites and BG:Disabled(bit3 = false)
    sta $2001

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MainLoop:
    bit $2002
    bpl MainLoop

;joypadの処理
readjoy:
 ;JOYPAD1($4016)に$01,$00の順位入れて準備。そして, buttonsに$01を入れて初期化。
    lda #$01
    sta JOYPAD1 ; $00を入れて
    sta buttons
    lsr a   ; now A is 0
    sta JOYPAD1  ; $01を入れる。

readjoy_loop:
    lda JOYPAD1
    lsr a                   ;右シフト
    rol buttons             ;左ロール
    bcc readjoy_loop
    ;rts

BUTTON_A      = 1 << 7
BUTTON_B      = 1 << 6
BUTTON_SELECT = 1 << 5
BUTTON_START  = 1 << 4
BUTTON_UP     = 1 << 3
BUTTON_DOWN   = 1 << 2
BUTTON_LEFT   = 1 << 1
BUTTON_RIGHT  = 1 << 0

; Calculating Press and Release
    lda buttons
    cmp #BUTTON_RIGHT + BUTTON_UP
    beq UpRight_Down
    cmp #BUTTON_LEFT + BUTTON_UP
    beq UpLeft_Down
    cmp #BUTTON_RIGHT + BUTTON_DOWN
    beq DownRight_Down
    cmp #BUTTON_LEFT + BUTTON_DOWN
    beq DownLeft_Down

    cmp #BUTTON_UP
    beq Up_Down
    cmp #BUTTON_DOWN
    beq Down_Down
    cmp #BUTTON_LEFT
    beq Left_Down
    cmp #BUTTON_RIGHT
    beq Right_Down

    jmp nothing_joypad

Up_Down:
    dec player1_y
    lda #DIST_UP
    sta player1_dist
    jmp do_joypad

Down_Down:
    inc player1_y
    lda #DIST_DOWN
    sta player1_dist
    jmp do_joypad

Left_Down:
    dec player1_x
    lda #DIST_LEFT
    sta player1_dist
    jmp do_joypad

Right_Down:
    inc player1_x
    lda #DIST_RIGHT
    sta player1_dist
    jmp do_joypad

UpRight_Down:
    jmp do_joypad

UpLeft_Down:
    jmp do_joypad

DownRight_Down:
    jmp do_joypad

DownLeft_Down:
    jmp do_joypad

nothing_joypad:   ; 何も押されていないときの処理

    jmp MainLoop

do_joypad:  ;何か押された時の処理
    ;キャラ向き変更発生時は、すぐにキャラ書き換え
    lda player1_dist
    cmp last_dist
    beq :+
    sta last_dist
    jsr chr_style_2
:

;    ;キャラのx軸が変化したら、8dot移動
;    lda player1_x
;    cmp last_player1_x
;    beq :+
;    sta last_player1_x
;    lda #$00
;    sta move_count
;    jsr player_move
;:
    jmp MainLoop

nmi:
    inc move_count
    inc count2
    inc count
    ;キャラ足踏み
    lda #15
    cmp count
    bne sprit_dma
    jsr chr_style_change

sprit_dma:
    lda #$00    ; Sprit DMA転送用
    sta $2003
    lda #$02 ; copy sprite data from $0200 => PPU memory for display (DMA転送)
    sta $4014
    rti

;;;;;;;;;;;;;;;;;;;;;;;ここから、サブルーチン

Player_tile_change:
    ; reg A : キャラのタイル番号を入れる($c0とか)
    ; reg X : キャラの番号($02xxのxx部分)を入れる ($00, $10, $20とか)
    sta $0201, x
    clc
    adc #1
    sta $0205, x
    adc #1
    sta $0209, x
    adc #1
    sta $020D, x
    rts

;;;;;;;;;;;;;;;;サブルーチン2
chr_style_change:
    lda #$00
    sta count
    sta temp1
    lda player1_dist
    sta last_dist

    lda chrstyle
    eor #%00000001
    sta chrstyle
    beq :+
    lda #$04
    sta temp1
:

;タイルを入れ替える
chr_style_2:
    lda player1_tile
    clc
    adc player1_dist
    adc temp1
    ldx #$00
    jsr Player_tile_change
    rts

;BGスクロール(8dot)
player_move:
    lda #$00
    sta move_count_last
    ldx player1_x
:
    lda move_count_last
    cmp move_count
    bne :-
    stx $2005
    inx 
    lda move_count
    cmp #08
    bne :-
    rts

; サウンド　矩形波;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda $4015
    ora #%10000001  ;矩形波チャンネル1を有効にする
    sta $4015

    lda #%00000111  ;Duty Cycle, 再生カウンタ有効無効, 音響選択, ボリューム AABCDDD
    sta $4000
    lda #%10101011
    sta $4001
    lda #%00000100
    sta $4002
	lda #%11111011	; 周波数(下位8ビット)
	sta $4003	; 矩形波チャンネル１周波数レジスタ１

; サウンド ノイズ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda $4015
    ora #%00001000  ;矩形波チャンネル1を有効にする
    sta $4015

    lda #%00000100
    sta $400C

    lda #%00000100
    sta $400E
    lda #%11111000
    sta $400F

	lda #%00000100	; 周波数(下位8ビット)
	sta $4002	; 矩形波チャンネル１周波数レジスタ１
	lda #%11111011	; 再生時間・周波数(上位3ビット)
	sta $4003	; 矩形波チャンネル１周波数レジスタ２

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

WorldData:
    .incbin "map01.bin"

.segment "VECTORS"
    .word nmi
    .word Reset
    ; 
.segment "CHARS"
    .incbin "set03.chr"