' emulador de CPU 6502
' esqueleto pra emulacion APPLE-IIe
' Jepalza 2013 (usando partes de emuladores ya existentes)

' necesario para el MULTIKEY
' ademas, si usamos compilacion FB, se necesita el "USING FB"
#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB 
#endif


Declare SUB adc6502 ()
DECLARE SUB and6502 ()
DECLARE SUB asl6502 ()
DECLARE SUB asla6502 ()
DECLARE SUB bcc6502 ()
DECLARE SUB bcs6502 ()
DECLARE SUB beq6502 ()
DECLARE SUB bit6502 ()
DECLARE SUB bmi6502 ()
DECLARE SUB bne6502 ()
DECLARE SUB bpl6502 ()
DECLARE SUB bra6502 ()
DECLARE SUB brk6502 ()
DECLARE SUB bvc6502 ()
DECLARE SUB bvs6502 ()
DECLARE SUB cmp6502 ()
DECLARE SUB cpx6502 ()
DECLARE SUB cpy6502 ()
DECLARE SUB dea6502 ()
DECLARE SUB dec6502 ()
DECLARE SUB dex6502 ()
DECLARE SUB dey6502 ()
DECLARE SUB eor6502 ()
DECLARE SUB ina6502 ()
DECLARE SUB inc6502 ()
DECLARE SUB inx6502 ()
DECLARE SUB iny6502 ()
DECLARE SUB jsr6502 ()
DECLARE SUB lsr6502 ()
DECLARE SUB lsra6502 ()
DECLARE SUB pha6502 ()
DECLARE SUB php6502 ()
DECLARE SUB phx6502 ()
DECLARE SUB phy6502 ()
DECLARE SUB pla6502 ()
DECLARE SUB plp6502 ()
DECLARE SUB plx6502 ()
DECLARE SUB ply6502 ()
DECLARE SUB rol6502 ()
DECLARE SUB rola6502 ()
DECLARE SUB ror6502 ()
DECLARE SUB rora6502 ()
DECLARE SUB rti6502 ()
DECLARE SUB rts6502 ()
DECLARE SUB sbc6502 ()
DECLARE SUB sec6502 ()
DECLARE SUB sed6502 ()
DECLARE SUB sei6502 ()
DECLARE SUB tax6502 ()
DECLARE SUB tay6502 ()
DECLARE SUB tsx6502 ()
DECLARE SUB txa6502 ()
DECLARE SUB txs6502 ()
DECLARE SUB tya6502 ()
DECLARE SUB indabsx6502 ()
DECLARE SUB indx6502 ()
DECLARE SUB indy6502 ()
DECLARE SUB zpx6502 ()
DECLARE SUB indzp6502 ()
DECLARE SUB zpy6502 ()
DECLARE SUB absy6502 ()
DECLARE SUB indirect6502 ()
DECLARE SUB absx6502 ()

' subrutinas mas importantes
DECLARE SUB adrmode (opcode As UInteger)
DECLARE SUB SetFlags (BYVAL value AS UInteger)
' entrada/salida I/O
DECLARE SUB IOwrite (BYVAL addr AS UINTEGER, BYVAL value AS UInteger)
DECLARE FUNCTION IOread (BYVAL addr AS UINTEGER) As UInteger
' ejecucion e inicializacion
DECLARE SUB exec6502 (nciclos As UInteger)
DECLARE SUB nmi6502 ()
DECLARE SUB init6502 () ' inicializa variables del emulador, no de la CPU
Declare SUB reset6502 () ' inicializa la CPU
' tratamientos de memoria
DECLARE FUNCTION Read6502 (BYVAL addr AS UINTEGER) As UInteger
Declare SUB Write6502 (BYVAL addr AS UINTEGER, BYVAL value AS UInteger)

DIM SHARED a AS UInteger = 0'CPU registers
Dim SHARED p As UInteger = 0'CPU registers
Dim SHARED s AS UInteger = 0'CPU registers
Dim SHARED y As UInteger = 0'CPU registers
Dim SHARED x AS UInteger = 0'CPU registers
DIM SHARED pc As UINTEGER 'and a couple UINTEGER ones too
DIM SHARED savepc As UINTEGER 'and a couple UINTEGER ones too
DIM SHARED totalexec As UINTEGER 'and a couple UINTEGER ones too

' uso de la CPU
DIM SHARED opcode As UInteger
DIM SHARED clockticks AS UInteger
Dim SHARED Ticks(255) AS UInteger
DIM SHARED addrmode(255) AS UInteger
DIM SHARED instruction(255) AS UInteger
Dim SHARED cpurunning As UInteger
DIM SHARED RAM(65535) AS UInteger

' de uso general
DIM SHARED tmpstring AS STRING * 1
DIM SHARED tmpbyte AS STRING * 1
Dim shared keybuff as String
Dim shared n As UInteger
Dim Shared curloc As UInteger
Dim Shared key As String
Dim Shared value As UInteger
Dim Shared temp As UInteger
Dim Shared saveflags As UInteger
Dim Shared sum As UInteger
Dim Shared IniROM As UInteger
Dim Shared ini As UInteger =0
Dim Shared aveces As UInteger = 0

' CARGAMOS UNA ROM "APPLE-II" DE PRUEBAS
Dim romfile As String
romfile = "apple2.rom"

Open romfile for binary as #1
IniROM = 65536 - lof(1) ' inicio de "ROM no escribible"
curloc = IniROM
do until eof(1)
  get #1, , tmpbyte
  RAM(curloc) = ASC(tmpbyte)
  curloc = curloc + 1
Loop
close #1


' inicializamos la CPU
init6502 ' primero las variables y tiempos de ejecucion
reset6502' ahora la cpu

' hacemos un bucle de prueba, para emular un APPLE-II
screen 12
' color verde, para imitar un monitor "viejo" de fosforo verde
color 2

  
	' marco virtual de la pantalla de texto
	Line (7,7)-Step(321,420),15,b

Do
  exec6502(10) ' ejecutamos 100 ciclos la cpu
  clockticks = 0
  key=InKey
  If key<>"" then  keybuff = keybuff + key
Loop Until key=Chr(27)

End

' *****************************************************************************
' *******************  PRECESAMIENTO DE LAS INSTRUCCIONES 6502 ****************
' *****************************************************************************
Sub exec6502 (nciclos As UInteger)

While nciclos>0
 nciclos-=1
 opcode = Read6502(pc)
 pc = pc + 1
 'put idledetect #@ here later... wate wut?
 clockticks = clockticks + Ticks(opcode)
 Select CASE instruction(opcode)
  CASE 0 'brk6502
    brk6502
  CASE 1 'ora6502
    adrmode opcode
    a = a OR Read6502(savepc)
    SetFlags a
  CASE 2 'nop6502
    'nothing here hurr durr
  'CASE 3: tsb6502
  CASE 4: asl6502
  CASE 5: php6502
  CASE 6: asla6502
  CASE 7: bpl6502
  'CASE 8: trb6502
  CASE 9: p = p AND &HFE 'clc6502
  CASE 10: ina6502
  CASE 11: jsr6502
  CASE 12: and6502
  CASE 13: bit6502
  CASE 14: rol6502
  CASE 15: plp6502
  CASE 16: rola6502
  CASE 17: bmi6502
  CASE 18: sec6502
  CASE 19: dea6502
  CASE 20: rti6502
  CASE 21: eor6502
  CASE 22: lsr6502
  CASE 23: pha6502
  CASE 24: lsra6502
  CASE 25' jmp6502
    adrmode opcode
    pc = savepc
  CASE 26: bvc6502
  CASE 27: p = p AND &HFB 'cli6502
  CASE 28: phy6502
  CASE 29: rts6502
  CASE 30: adc6502
  'CASE 31: stz6502
  CASE 32: ror6502
  CASE 33: pla6502
  CASE 34: rora6502
  CASE 35: bvs6502
  CASE 36: sei6502
  CASE 37: ply6502
  CASE 38: bra6502
  CASE 39 'sta6502
    adrmode opcode
    Write6502 savepc, a
  CASE 40 'sty6502
    adrmode opcode
    Write6502 savepc, y
  CASE 41 'stx6502
    adrmode opcode
    Write6502 savepc, x
  CASE 42: dey6502
  CASE 43: txa6502
  CASE 44: bcc6502
  CASE 45: tya6502
  CASE 46: txs6502
  CASE 47 'ldy6502
    adrmode opcode
    y = Read6502(savepc)
    SetFlags y
  CASE 48 'lda6502
    adrmode opcode
    a = Read6502(savepc)
    SetFlags a
  CASE 49 'ldx6502
    adrmode opcode
    x = Read6502(savepc)
    SetFlags x
  CASE 50: tay6502
  CASE 51: tax6502
  CASE 52: bcs6502
  CASE 53: p = p AND &HBF 'clv6502
  CASE 54: tsx6502
  CASE 55: cpy6502
  CASE 56: cmp6502
  CASE 57: dec6502
  CASE 58: iny6502
  CASE 59: dex6502
  CASE 60: bne6502
  CASE 61: p = p AND &HF7 'cld6502
  CASE 62: phx6502
  CASE 63: cpx6502
  CASE 64: sbc6502
  CASE 65: inc6502
  CASE 66: inx6502
  CASE 67: beq6502
  CASE 68: sed6502
  CASE 69: plx6502
 End SELECT
 'totalexec = totalexec + 1
Wend
END Sub


' **********************************************
' VARIABLES INICALES DEL EMULADOR (NO DE LA CPU)
' **********************************************
SUB init6502
Ticks(&H0) = 7: instruction(&H0) = 0: addrmode(&H0) = 0
Ticks(&H1) = 6: instruction(&H1) = 1: addrmode(&H1) = 1
Ticks(&H2) = 2: instruction(&H2) = 2: addrmode(&H2) = 0
Ticks(&H3) = 2: instruction(&H3) = 2: addrmode(&H3) = 0
Ticks(&H4) = 3: instruction(&H4) = 3: addrmode(&H4) = 2
Ticks(&H5) = 3: instruction(&H5) = 1: addrmode(&H5) = 2
Ticks(&H6) = 5: instruction(&H6) = 4: addrmode(&H6) = 2
Ticks(&H7) = 2: instruction(&H7) = 2: addrmode(&H7) = 0
Ticks(&H8) = 3: instruction(&H8) = 5: addrmode(&H8) = 0
Ticks(&H9) = 3: instruction(&H9) = 1: addrmode(&H9) = 3
Ticks(&HA) = 2: instruction(&HA) = 6: addrmode(&HA) = 0
Ticks(&HB) = 2: instruction(&HB) = 2: addrmode(&HB) = 0
Ticks(&HC) = 4: instruction(&HC) = 3: addrmode(&HC) = 4
Ticks(&HD) = 4: instruction(&HD) = 1: addrmode(&HD) = 4
Ticks(&HE) = 6: instruction(&HE) = 4: addrmode(&HE) = 4
Ticks(&HF) = 2: instruction(&HF) = 2: addrmode(&HF) = 0
Ticks(&H10) = 2: instruction(&H10) = 7: addrmode(&H10) = 5
Ticks(&H11) = 5: instruction(&H11) = 1: addrmode(&H11) = 6
Ticks(&H12) = 3: instruction(&H12) = 1: addrmode(&H12) = 7
Ticks(&H13) = 2: instruction(&H13) = 2: addrmode(&H13) = 0
Ticks(&H14) = 3: instruction(&H14) = 8: addrmode(&H14) = 2
Ticks(&H15) = 4: instruction(&H15) = 1: addrmode(&H15) = 8
Ticks(&H16) = 6: instruction(&H16) = 4: addrmode(&H16) = 8
Ticks(&H17) = 2: instruction(&H17) = 2: addrmode(&H17) = 0
Ticks(&H18) = 2: instruction(&H18) = 9: addrmode(&H18) = 0
Ticks(&H19) = 4: instruction(&H19) = 1: addrmode(&H19) = 9
Ticks(&H1A) = 2: instruction(&H1A) = 10: addrmode(&H1A) = 0
Ticks(&H1B) = 2: instruction(&H1B) = 2: addrmode(&H1B) = 0
Ticks(&H1C) = 4: instruction(&H1C) = 8: addrmode(&H1C) = 4
Ticks(&H1D) = 4: instruction(&H1D) = 1: addrmode(&H1D) = 10
Ticks(&H1E) = 7: instruction(&H1E) = 4: addrmode(&H1E) = 10
Ticks(&H1F) = 2: instruction(&H1F) = 2: addrmode(&H1F) = 0
Ticks(&H20) = 6: instruction(&H20) = 11: addrmode(&H20) = 4
Ticks(&H21) = 6: instruction(&H21) = 12: addrmode(&H21) = 1
Ticks(&H22) = 2: instruction(&H22) = 2: addrmode(&H22) = 0
Ticks(&H23) = 2: instruction(&H23) = 2: addrmode(&H23) = 0
Ticks(&H24) = 3: instruction(&H24) = 13: addrmode(&H24) = 2
Ticks(&H25) = 3: instruction(&H25) = 12: addrmode(&H25) = 2
Ticks(&H26) = 5: instruction(&H26) = 14: addrmode(&H26) = 2
Ticks(&H27) = 2: instruction(&H27) = 2: addrmode(&H27) = 0
Ticks(&H28) = 4: instruction(&H28) = 15: addrmode(&H28) = 0
Ticks(&H29) = 3: instruction(&H29) = 12: addrmode(&H29) = 3
Ticks(&H2A) = 2: instruction(&H2A) = 16: addrmode(&H2A) = 0
Ticks(&H2B) = 2: instruction(&H2B) = 2: addrmode(&H2B) = 0
Ticks(&H2C) = 4: instruction(&H2C) = 13: addrmode(&H2C) = 4
Ticks(&H2D) = 4: instruction(&H2D) = 12: addrmode(&H2D) = 4
Ticks(&H2E) = 6: instruction(&H2E) = 14: addrmode(&H2E) = 4
Ticks(&H2F) = 2: instruction(&H2F) = 2: addrmode(&H2F) = 0
Ticks(&H30) = 2: instruction(&H30) = 17: addrmode(&H30) = 5
Ticks(&H31) = 5: instruction(&H31) = 12: addrmode(&H31) = 6
Ticks(&H32) = 3: instruction(&H32) = 12: addrmode(&H32) = 7
Ticks(&H33) = 2: instruction(&H33) = 2: addrmode(&H33) = 0
Ticks(&H34) = 4: instruction(&H34) = 13: addrmode(&H34) = 8
Ticks(&H35) = 4: instruction(&H35) = 12: addrmode(&H35) = 8
Ticks(&H36) = 6: instruction(&H36) = 14: addrmode(&H36) = 8
Ticks(&H37) = 2: instruction(&H37) = 2: addrmode(&H37) = 0
Ticks(&H38) = 2: instruction(&H38) = 18: addrmode(&H38) = 0
Ticks(&H39) = 4: instruction(&H39) = 12: addrmode(&H39) = 9
Ticks(&H3A) = 2: instruction(&H3A) = 19: addrmode(&H3A) = 0
Ticks(&H3B) = 2: instruction(&H3B) = 2: addrmode(&H3B) = 0
Ticks(&H3C) = 4: instruction(&H3C) = 13: addrmode(&H3C) = 10
Ticks(&H3D) = 4: instruction(&H3D) = 12: addrmode(&H3D) = 10
Ticks(&H3E) = 7: instruction(&H3E) = 14: addrmode(&H3E) = 10
Ticks(&H3F) = 2: instruction(&H3F) = 2: addrmode(&H3F) = 0
Ticks(&H40) = 6: instruction(&H40) = 20: addrmode(&H40) = 0
Ticks(&H41) = 6: instruction(&H41) = 21: addrmode(&H41) = 1
Ticks(&H42) = 2: instruction(&H42) = 2: addrmode(&H42) = 0
Ticks(&H43) = 2: instruction(&H43) = 2: addrmode(&H43) = 0
Ticks(&H44) = 2: instruction(&H44) = 2: addrmode(&H44) = 0
Ticks(&H45) = 3: instruction(&H45) = 21: addrmode(&H45) = 2
Ticks(&H46) = 5: instruction(&H46) = 22: addrmode(&H46) = 2
Ticks(&H47) = 2: instruction(&H47) = 2: addrmode(&H47) = 0
Ticks(&H48) = 3: instruction(&H48) = 23: addrmode(&H48) = 0
Ticks(&H49) = 3: instruction(&H49) = 21: addrmode(&H49) = 3
Ticks(&H4A) = 2: instruction(&H4A) = 24: addrmode(&H4A) = 0
Ticks(&H4B) = 2: instruction(&H4B) = 2: addrmode(&H4B) = 0
Ticks(&H4C) = 3: instruction(&H4C) = 25: addrmode(&H4C) = 4
Ticks(&H4D) = 4: instruction(&H4D) = 21: addrmode(&H4D) = 4
Ticks(&H4E) = 6: instruction(&H4E) = 22: addrmode(&H4E) = 4
Ticks(&H4F) = 2: instruction(&H4F) = 2: addrmode(&H4F) = 0
Ticks(&H50) = 2: instruction(&H50) = 26: addrmode(&H50) = 5
Ticks(&H51) = 5: instruction(&H51) = 21: addrmode(&H51) = 6
Ticks(&H52) = 3: instruction(&H52) = 21: addrmode(&H52) = 7
Ticks(&H53) = 2: instruction(&H53) = 2: addrmode(&H53) = 0
Ticks(&H54) = 2: instruction(&H54) = 2: addrmode(&H54) = 0
Ticks(&H55) = 4: instruction(&H55) = 21: addrmode(&H55) = 8
Ticks(&H56) = 6: instruction(&H56) = 22: addrmode(&H56) = 8
Ticks(&H57) = 2: instruction(&H57) = 2: addrmode(&H57) = 0
Ticks(&H58) = 2: instruction(&H58) = 27: addrmode(&H58) = 0
Ticks(&H59) = 4: instruction(&H59) = 21: addrmode(&H59) = 9
Ticks(&H5A) = 3: instruction(&H5A) = 28: addrmode(&H5A) = 0
Ticks(&H5B) = 2: instruction(&H5B) = 2: addrmode(&H5B) = 0
Ticks(&H5C) = 2: instruction(&H5C) = 2: addrmode(&H5C) = 0
Ticks(&H5D) = 4: instruction(&H5D) = 21: addrmode(&H5D) = 10
Ticks(&H5E) = 7: instruction(&H5E) = 22: addrmode(&H5E) = 10
Ticks(&H5F) = 2: instruction(&H5F) = 2: addrmode(&H5F) = 0
Ticks(&H60) = 6: instruction(&H60) = 29: addrmode(&H60) = 0
Ticks(&H61) = 6: instruction(&H61) = 30: addrmode(&H61) = 1
Ticks(&H62) = 2: instruction(&H62) = 2: addrmode(&H62) = 0
Ticks(&H63) = 2: instruction(&H63) = 2: addrmode(&H63) = 0
Ticks(&H64) = 3: instruction(&H64) = 31: addrmode(&H64) = 2
Ticks(&H65) = 3: instruction(&H65) = 30: addrmode(&H65) = 2
Ticks(&H66) = 5: instruction(&H66) = 32: addrmode(&H66) = 2
Ticks(&H67) = 2: instruction(&H67) = 2: addrmode(&H67) = 0
Ticks(&H68) = 4: instruction(&H68) = 33: addrmode(&H68) = 0
Ticks(&H69) = 3: instruction(&H69) = 30: addrmode(&H69) = 3
Ticks(&H6A) = 2: instruction(&H6A) = 34: addrmode(&H6A) = 0
Ticks(&H6B) = 2: instruction(&H6B) = 2: addrmode(&H6B) = 0
Ticks(&H6C) = 5: instruction(&H6C) = 25: addrmode(&H6C) = 11
Ticks(&H6D) = 4: instruction(&H6D) = 30: addrmode(&H6D) = 4
Ticks(&H6E) = 6: instruction(&H6E) = 32: addrmode(&H6E) = 4
Ticks(&H6F) = 2: instruction(&H6F) = 2: addrmode(&H6F) = 0
Ticks(&H70) = 2: instruction(&H70) = 35: addrmode(&H70) = 5
Ticks(&H71) = 5: instruction(&H71) = 30: addrmode(&H71) = 6
Ticks(&H72) = 3: instruction(&H72) = 30: addrmode(&H72) = 7
Ticks(&H73) = 2: instruction(&H73) = 2: addrmode(&H73) = 0
Ticks(&H74) = 4: instruction(&H74) = 31: addrmode(&H74) = 8
Ticks(&H75) = 4: instruction(&H75) = 30: addrmode(&H75) = 8
Ticks(&H76) = 6: instruction(&H76) = 32: addrmode(&H76) = 8
Ticks(&H77) = 2: instruction(&H77) = 2: addrmode(&H77) = 0
Ticks(&H78) = 2: instruction(&H78) = 36: addrmode(&H78) = 0
Ticks(&H79) = 4: instruction(&H79) = 30: addrmode(&H79) = 9
Ticks(&H7A) = 4: instruction(&H7A) = 37: addrmode(&H7A) = 0
Ticks(&H7B) = 2: instruction(&H7B) = 2: addrmode(&H7B) = 0
Ticks(&H7C) = 6: instruction(&H7C) = 25: addrmode(&H7C) = 12
Ticks(&H7D) = 4: instruction(&H7D) = 30: addrmode(&H7D) = 10
Ticks(&H7E) = 7: instruction(&H7E) = 32: addrmode(&H7E) = 10
Ticks(&H7F) = 2: instruction(&H7F) = 2: addrmode(&H7F) = 0
Ticks(&H80) = 2: instruction(&H80) = 38: addrmode(&H80) = 5
Ticks(&H81) = 6: instruction(&H81) = 39: addrmode(&H81) = 1
Ticks(&H82) = 2: instruction(&H82) = 2: addrmode(&H82) = 0
Ticks(&H83) = 2: instruction(&H83) = 2: addrmode(&H83) = 0
Ticks(&H84) = 2: instruction(&H84) = 40: addrmode(&H84) = 2
Ticks(&H85) = 2: instruction(&H85) = 39: addrmode(&H85) = 2
Ticks(&H86) = 2: instruction(&H86) = 41: addrmode(&H86) = 2
Ticks(&H87) = 2: instruction(&H87) = 2: addrmode(&H87) = 0
Ticks(&H88) = 2: instruction(&H88) = 42: addrmode(&H88) = 0
Ticks(&H89) = 2: instruction(&H89) = 13: addrmode(&H89) = 3
Ticks(&H8A) = 2: instruction(&H8A) = 43: addrmode(&H8A) = 0
Ticks(&H8B) = 2: instruction(&H8B) = 2: addrmode(&H8B) = 0
Ticks(&H8C) = 4: instruction(&H8C) = 40: addrmode(&H8C) = 4
Ticks(&H8D) = 4: instruction(&H8D) = 39: addrmode(&H8D) = 4
Ticks(&H8E) = 4: instruction(&H8E) = 41: addrmode(&H8E) = 4
Ticks(&H8F) = 2: instruction(&H8F) = 2: addrmode(&H8F) = 0
Ticks(&H90) = 2: instruction(&H90) = 44: addrmode(&H90) = 5
Ticks(&H91) = 6: instruction(&H91) = 39: addrmode(&H91) = 6
Ticks(&H92) = 3: instruction(&H92) = 39: addrmode(&H92) = 7
Ticks(&H93) = 2: instruction(&H93) = 2: addrmode(&H93) = 0
Ticks(&H94) = 4: instruction(&H94) = 40: addrmode(&H94) = 8
Ticks(&H95) = 4: instruction(&H95) = 39: addrmode(&H95) = 8
Ticks(&H96) = 4: instruction(&H96) = 41: addrmode(&H96) = 13
Ticks(&H97) = 2: instruction(&H97) = 2: addrmode(&H97) = 0
Ticks(&H98) = 2: instruction(&H98) = 45: addrmode(&H98) = 0
Ticks(&H99) = 5: instruction(&H99) = 39: addrmode(&H99) = 9
Ticks(&H9A) = 2: instruction(&H9A) = 46: addrmode(&H9A) = 0
Ticks(&H9B) = 2: instruction(&H9B) = 2: addrmode(&H9B) = 0
Ticks(&H9C) = 4: instruction(&H9C) = 31: addrmode(&H9C) = 4
Ticks(&H9D) = 5: instruction(&H9D) = 39: addrmode(&H9D) = 10
Ticks(&H9E) = 5: instruction(&H9E) = 31: addrmode(&H9E) = 10
Ticks(&H9F) = 2: instruction(&H9F) = 2: addrmode(&H9F) = 0
Ticks(&HA0) = 3: instruction(&HA0) = 47: addrmode(&HA0) = 3
Ticks(&HA1) = 6: instruction(&HA1) = 48: addrmode(&HA1) = 1
Ticks(&HA2) = 3: instruction(&HA2) = 49: addrmode(&HA2) = 3
Ticks(&HA3) = 2: instruction(&HA3) = 2: addrmode(&HA3) = 0
Ticks(&HA4) = 3: instruction(&HA4) = 47: addrmode(&HA4) = 2
Ticks(&HA5) = 3: instruction(&HA5) = 48: addrmode(&HA5) = 2
Ticks(&HA6) = 3: instruction(&HA6) = 49: addrmode(&HA6) = 2
Ticks(&HA7) = 2: instruction(&HA7) = 2: addrmode(&HA7) = 0
Ticks(&HA8) = 2: instruction(&HA8) = 50: addrmode(&HA8) = 0
Ticks(&HA9) = 3: instruction(&HA9) = 48: addrmode(&HA9) = 3
Ticks(&HAA) = 2: instruction(&HAA) = 51: addrmode(&HAA) = 0
Ticks(&HAB) = 2: instruction(&HAB) = 2: addrmode(&HAB) = 0
Ticks(&HAC) = 4: instruction(&HAC) = 47: addrmode(&HAC) = 4
Ticks(&HAD) = 4: instruction(&HAD) = 48: addrmode(&HAD) = 4
Ticks(&HAE) = 4: instruction(&HAE) = 49: addrmode(&HAE) = 4
Ticks(&HAF) = 2: instruction(&HAF) = 2: addrmode(&HAF) = 0
Ticks(&HB0) = 2: instruction(&HB0) = 52: addrmode(&HB0) = 5
Ticks(&HB1) = 5: instruction(&HB1) = 48: addrmode(&HB1) = 6
Ticks(&HB2) = 3: instruction(&HB2) = 48: addrmode(&HB2) = 7
Ticks(&HB3) = 2: instruction(&HB3) = 2: addrmode(&HB3) = 0
Ticks(&HB4) = 4: instruction(&HB4) = 47: addrmode(&HB4) = 8
Ticks(&HB5) = 4: instruction(&HB5) = 48: addrmode(&HB5) = 8
Ticks(&HB6) = 4: instruction(&HB6) = 49: addrmode(&HB6) = 13
Ticks(&HB7) = 2: instruction(&HB7) = 2: addrmode(&HB7) = 0
Ticks(&HB8) = 2: instruction(&HB8) = 53: addrmode(&HB8) = 0
Ticks(&HB9) = 4: instruction(&HB9) = 48: addrmode(&HB9) = 9
Ticks(&HBA) = 2: instruction(&HBA) = 54: addrmode(&HBA) = 0
Ticks(&HBB) = 2: instruction(&HBB) = 2: addrmode(&HBB) = 0
Ticks(&HBC) = 4: instruction(&HBC) = 47: addrmode(&HBC) = 10
Ticks(&HBD) = 4: instruction(&HBD) = 48: addrmode(&HBD) = 10
Ticks(&HBE) = 4: instruction(&HBE) = 49: addrmode(&HBE) = 9
Ticks(&HBF) = 2: instruction(&HBF) = 2: addrmode(&HBF) = 0
Ticks(&HC0) = 3: instruction(&HC0) = 55: addrmode(&HC0) = 3
Ticks(&HC1) = 6: instruction(&HC1) = 56: addrmode(&HC1) = 1
Ticks(&HC2) = 2: instruction(&HC2) = 2: addrmode(&HC2) = 0
Ticks(&HC3) = 2: instruction(&HC3) = 2: addrmode(&HC3) = 0
Ticks(&HC4) = 3: instruction(&HC4) = 55: addrmode(&HC4) = 2
Ticks(&HC5) = 3: instruction(&HC5) = 56: addrmode(&HC5) = 2
Ticks(&HC6) = 5: instruction(&HC6) = 57: addrmode(&HC6) = 2
Ticks(&HC7) = 2: instruction(&HC7) = 2: addrmode(&HC7) = 0
Ticks(&HC8) = 2: instruction(&HC8) = 58: addrmode(&HC8) = 0
Ticks(&HC9) = 3: instruction(&HC9) = 56: addrmode(&HC9) = 3
Ticks(&HCA) = 2: instruction(&HCA) = 59: addrmode(&HCA) = 0
Ticks(&HCB) = 2: instruction(&HCB) = 2: addrmode(&HCB) = 0
Ticks(&HCC) = 4: instruction(&HCC) = 55: addrmode(&HCC) = 4
Ticks(&HCD) = 4: instruction(&HCD) = 56: addrmode(&HCD) = 4
Ticks(&HCE) = 6: instruction(&HCE) = 57: addrmode(&HCE) = 4
Ticks(&HCF) = 2: instruction(&HCF) = 2: addrmode(&HCF) = 0
Ticks(&HD0) = 2: instruction(&HD0) = 60: addrmode(&HD0) = 5
Ticks(&HD1) = 5: instruction(&HD1) = 56: addrmode(&HD1) = 6
Ticks(&HD2) = 3: instruction(&HD2) = 56: addrmode(&HD2) = 7
Ticks(&HD3) = 2: instruction(&HD3) = 2: addrmode(&HD3) = 0
Ticks(&HD4) = 2: instruction(&HD4) = 2: addrmode(&HD4) = 0
Ticks(&HD5) = 4: instruction(&HD5) = 56: addrmode(&HD5) = 8
Ticks(&HD6) = 6: instruction(&HD6) = 57: addrmode(&HD6) = 8
Ticks(&HD7) = 2: instruction(&HD7) = 2: addrmode(&HD7) = 0
Ticks(&HD8) = 2: instruction(&HD8) = 61: addrmode(&HD8) = 0
Ticks(&HD9) = 4: instruction(&HD9) = 56: addrmode(&HD9) = 9
Ticks(&HDA) = 3: instruction(&HDA) = 62: addrmode(&HDA) = 0
Ticks(&HDB) = 2: instruction(&HDB) = 2: addrmode(&HDB) = 0
Ticks(&HDC) = 2: instruction(&HDC) = 2: addrmode(&HDC) = 0
Ticks(&HDD) = 4: instruction(&HDD) = 56: addrmode(&HDD) = 10
Ticks(&HDE) = 7: instruction(&HDE) = 57: addrmode(&HDE) = 10
Ticks(&HDF) = 2: instruction(&HDF) = 2: addrmode(&HDF) = 0
Ticks(&HE0) = 3: instruction(&HE0) = 63: addrmode(&HE0) = 3
Ticks(&HE1) = 6: instruction(&HE1) = 64: addrmode(&HE1) = 1
Ticks(&HE2) = 2: instruction(&HE2) = 2: addrmode(&HE2) = 0
Ticks(&HE3) = 2: instruction(&HE3) = 2: addrmode(&HE3) = 0
Ticks(&HE4) = 3: instruction(&HE4) = 63: addrmode(&HE4) = 2
Ticks(&HE5) = 3: instruction(&HE5) = 64: addrmode(&HE5) = 2
Ticks(&HE6) = 5: instruction(&HE6) = 65: addrmode(&HE6) = 2
Ticks(&HE7) = 2: instruction(&HE7) = 2: addrmode(&HE7) = 0
Ticks(&HE8) = 2: instruction(&HE8) = 66: addrmode(&HE8) = 0
Ticks(&HE9) = 3: instruction(&HE9) = 64: addrmode(&HE9) = 3
Ticks(&HEA) = 2: instruction(&HEA) = 2: addrmode(&HEA) = 0
Ticks(&HEB) = 2: instruction(&HEB) = 2: addrmode(&HEB) = 0
Ticks(&HEC) = 4: instruction(&HEC) = 63: addrmode(&HEC) = 4
Ticks(&HED) = 4: instruction(&HED) = 64: addrmode(&HED) = 4
Ticks(&HEE) = 6: instruction(&HEE) = 65: addrmode(&HEE) = 4
Ticks(&HEF) = 2: instruction(&HEF) = 2: addrmode(&HEF) = 0
Ticks(&HF0) = 2: instruction(&HF0) = 67: addrmode(&HF0) = 5
Ticks(&HF1) = 5: instruction(&HF1) = 64: addrmode(&HF1) = 6
Ticks(&HF2) = 3: instruction(&HF2) = 64: addrmode(&HF2) = 7
Ticks(&HF3) = 2: instruction(&HF3) = 2: addrmode(&HF3) = 0
Ticks(&HF4) = 2: instruction(&HF4) = 2: addrmode(&HF4) = 0
Ticks(&HF5) = 4: instruction(&HF5) = 64: addrmode(&HF5) = 8
Ticks(&HF6) = 6: instruction(&HF6) = 65: addrmode(&HF6) = 8
Ticks(&HF7) = 2: instruction(&HF7) = 2: addrmode(&HF7) = 0
Ticks(&HF8) = 2: instruction(&HF8) = 68: addrmode(&HF8) = 0
Ticks(&HF9) = 4: instruction(&HF9) = 64: addrmode(&HF9) = 9
Ticks(&HFA) = 4: instruction(&HFA) = 69: addrmode(&HFA) = 0
Ticks(&HFB) = 2: instruction(&HFB) = 2: addrmode(&HFB) = 0
Ticks(&HFC) = 2: instruction(&HFC) = 2: addrmode(&HFC) = 0
Ticks(&HFD) = 4: instruction(&HFD) = 64: addrmode(&HFD) = 10
Ticks(&HFE) = 7: instruction(&HFE) = 65: addrmode(&HFE) = 10
Ticks(&HFF) = 2: instruction(&HFF) = 2: addrmode(&HFF) = 0

END Sub

' *************************************
' INSTRUCCIONES 6502 PROPIAMENTE DICHAS 
' *************************************


SUB absx6502
  savepc = Read6502(pc)
  savepc = savepc + (Read6502(pc + 1) * &H100&)
  pc = pc + 2

  IF (Ticks(opcode) = 4) THEN
    IF ((savepc \ &H100&) = ((savepc + x) \ &H100&)) THEN
    ELSE
      clockticks = clockticks + 1
    END IF
  END IF
  savepc = savepc + x
END SUB

SUB absy6502
  savepc = Read6502(pc) + (Read6502(pc + 1) * &H100&)
  pc = pc + 2

  IF (Ticks(opcode) = 4) THEN
    IF ((savepc \ &H100&) = ((savepc + y) \ &H100&)) THEN
    ELSE
      clockticks = clockticks + 1
    END IF
  END IF
  savepc = savepc + y
END SUB

SUB adc6502
    DIM tmp AS UInteger 
    Dim sum As UInteger
    Dim saveflags As UInteger
      
    adrmode opcode
    value = Read6502(savepc)
     
    saveflags = (p AND &H1)

    sum = a
    sum = (sum + value) AND &HFF
    sum = (sum + saveflags) AND &HFF
      
    IF (sum > &H7F) OR (sum < -&H80) THEN
        p = p OR &H40
    ELSE
        p = (p AND &HBF)
    END IF
      
    sum = a + (value + saveflags)
    IF (sum > &HFF) THEN
        p = p OR &H1
    ELSE
        p = (p AND &HFE)
    END IF
      
    a = sum AND &HFF
    IF (p AND &H8) THEN
        p = (p AND &HFE)
        IF ((a AND &HF) > &H9) THEN
            a = (a + &H6) AND &HFF
        END IF
        IF ((a AND &HF0) > &H90) THEN
            a = (a + &H60) AND &HFF
            p = p OR &H1
        END IF
    ELSE
        clockticks = clockticks + 1
    END IF
    SetFlags a
END SUB

Sub and6502
  adrmode opcode
  value = Read6502(savepc)
  a = (a AND value)
  SetFlags a
END SUB

SUB asl6502
  adrmode opcode
  value = Read6502(savepc)
  
  p = (p AND &HFE) OR ((value \ 128) AND &H1)
  value = (value * 2) AND &HFF
  
  Write6502 savepc, (value AND &HFF)
  SetFlags value
END SUB

SUB asla6502
  p = (p AND &HFE) OR ((a \ 128) AND &H1)
  a = (a * 2) AND &HFF
  SetFlags a
END SUB

SUB bcc6502
  IF ((p AND &H1) = 0) THEN
    adrmode opcode
    pc = pc + savepc
    clockticks = clockticks + 1
  ELSE
    pc = pc + 1
  END IF
END SUB

SUB bcs6502
  IF (p AND &H1) THEN
    adrmode opcode
    pc = pc + savepc
    clockticks = clockticks + 1
  ELSE
    pc = pc + 1
  END IF
END SUB

SUB beq6502
  IF (p AND &H2) THEN
    adrmode opcode
    pc = pc + savepc
    clockticks = clockticks + 1
  ELSE
    pc = pc + 1
  END IF
END SUB

SUB bit6502
    adrmode opcode
    value = Read6502(savepc)
  
    IF (value AND a) THEN
        p = (p AND &HFD)
    ELSE
        p = p OR &H2
    END IF
    p = ((p AND &H3F) OR (value AND &HC0))
END SUB

SUB bmi6502
    IF (p AND &H80) THEN
        adrmode opcode
        pc = pc + savepc
        clockticks = clockticks + 1
    ELSE
        pc = pc + 1
    END IF
END SUB

SUB bne6502
    IF ((p AND &H2) = 0) THEN
        adrmode opcode
        pc = pc + savepc
    ELSE
        pc = pc + 1
    END IF
END SUB

SUB bpl6502
    IF ((p AND &H80) = 0) THEN
        adrmode opcode
        pc = pc + savepc
    ELSE
        pc = pc + 1
    END IF
END SUB

SUB bra6502
  adrmode opcode
  pc = pc + savepc
  clockticks = clockticks + 1
END SUB

SUB brk6502
 PC = PC + 1
    Write6502 &H100& + s, (pc \ &H100&) AND &HFF
'    Write6502 &H100& + s, (pc / 2 ^ 8)
    s = (s - 1) AND &HFF
'    s = s - 1
    Write6502 &H100& + s, (pc AND &HFF)
    s = (s - 1) AND &HFF
'    s = s - 1
    Write6502 &H100& + s, p
    s = (s - 1) AND &HFF
'    s = s - 1
    p = p OR &H14
    pc = Read6502(&HFFFE&) + (Read6502(&HFFFF&) * &H100&)
'    pc = Read6502(&HFFFE&) + (Read6502(&HFFFF&) * 2 ^ 8)
END SUB

SUB bvc6502
    IF ((p AND &H40) = 0) THEN
        adrmode opcode
        pc = pc + savepc
        clockticks = clockticks + 1
    ELSE
        pc = pc + 1
    END IF
END SUB

SUB bvs6502
  IF (p AND &H40) THEN
    adrmode opcode
    pc = pc + savepc
    clockticks = clockticks + 1
  ELSE
    pc = pc + 1
  END IF
END SUB

SUB cmp6502
  adrmode opcode
  value = Read6502(savepc)
  
  IF (a + &H100 - value) > &HFF THEN
    p = p OR &H1
  ELSE
    p = (p AND &HFE)
  END IF
  
  value = (a + &H100 - value) AND &HFF
  SetFlags value
END SUB

SUB cpx6502
  adrmode opcode
  value = Read6502(savepc)
      
  IF (x + &H100 - value > &HFF) THEN
    p = p OR &H1
  ELSE
    p = (p AND &HFE)
  END IF
  
  value = (x + &H100 - value) AND &HFF
  SetFlags value
END SUB

SUB cpy6502
  adrmode opcode
  value = Read6502(savepc)
      
  IF (y + &H100 - value > &HFF) THEN
    p = (p OR &H1)
  ELSE
    p = (p AND &HFE)
  END IF
  value = (y + &H100 - value) AND &HFF
  SetFlags value
END SUB

SUB dea6502
  a = (a - 1) AND &HFF
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB dec6502
  adrmode opcode
  Write6502 (savepc), (Read6502(savepc) - 1) AND &HFF
  value = Read6502(savepc)
  IF (value) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (value AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB dex6502
  x = (x - 1) AND &HFF
  IF (x) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (x AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB dey6502
  y = (y - 1) AND &HFF
  IF (y) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (y AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB eor6502
  adrmode opcode
  a = a XOR Read6502(savepc)
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

Sub ina6502
  a = (a + 1) AND &HFF
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB inc6502
  adrmode opcode
  Write6502 (savepc), (Read6502(savepc) + 1) AND &HFF
  value = Read6502(savepc)
  IF (value) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (value AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB indabsx6502
  temp = Read6502(pc) + (Read6502(pc + 1) * &H100&) + x
  savepc = Read6502(temp) + (Read6502(temp + 1) * &H100&)
END SUB

SUB indirect6502
  temp = Read6502(pc) + (Read6502(pc + 1) * &H100&)
  savepc = Read6502(temp) + (Read6502(temp + 1) * &H100&)
  pc = pc + 2
END SUB

SUB indx6502
  value = Read6502(pc) AND &HFF
  value = (value + x) AND &HFF
  pc = pc + 1
  savepc = Read6502(value) + (Read6502(value + 1) * &H100&)
END SUB

SUB indy6502
  value = Read6502(pc)
  pc = pc + 1
      
  savepc = Read6502(value) + (Read6502(value + 1) * &H100&)
  IF (Ticks(opcode) = 5) THEN
    IF ((savepc \ &H100&) = ((savepc + y) \ &H100&)) THEN
    ELSE
      clockticks = clockticks + 1
    END IF
  END IF
  savepc = savepc + y
END SUB

SUB indzp6502
  value = Read6502(pc)
  pc = pc + 1
  savepc = Read6502(value) + (Read6502(value + 1) * &H100&)
END SUB


SUB inx6502
  x = (x + 1) AND &HFF
  IF (x) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (x AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB iny6502
  y = (y + 1) AND &HFF
  IF (y) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (y AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB jsr6502
  pc = pc + 1
  Write6502 s + &H100&, (pc \ &H100&)
  s = (s - 1) AND &HFF
  Write6502 s + &H100&, (pc AND &HFF)
  s = (s - 1) AND &HFF
  pc = pc - 1
  adrmode opcode
  pc = savepc
END SUB

SUB lsr6502
  adrmode opcode
  value = Read6502(savepc)
         
  p = ((p AND &HFE) OR (value AND &H1))
  
  value = (value \ 2) AND &HFF
  Write6502 savepc, (value AND &HFF)
  IF (value) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (value AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB lsra6502
  p = (p AND &HFE) OR (a AND &H1)
  a = (a \ 2) AND &HFF
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB nmi6502
    Write6502 (s + &H100&), (pc \ &H100&)
    s = (s - 1) AND &HFF
    Write6502 (s + &H100&), (pc AND &HFF)
    s = (s - 1) AND &HFF
    Write6502 (s + &H100&), p
    p = p OR &H4
    s = (s - 1) AND &HFF
    pc = Read6502(&HFFFA&) + (Read6502(&HFFFB&) * &H100&)
    clockticks = clockticks + 7
END SUB

SUB ora6502
  adrmode opcode
  a = a OR Read6502(savepc)
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB pha6502
  Write6502 &H100& + s, a
  s = (s - 1) AND &HFF
END SUB

SUB php6502
  Write6502 &H100& + s, p
  s = (s - 1) AND &HFF
END SUB

SUB phx6502
  Write6502 &H100 + s, x
  s = (s - 1) AND &HFF
END SUB

SUB phy6502
  Write6502 &H100 + s, y
  s = (s - 1) AND &HFF
END SUB

SUB pla6502
  s = (s + 1) AND &HFF
  a = Read6502(s + &H100)
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB plp6502
  s = (s + 1) AND &HFF
  p = Read6502(s + &H100) OR &H20
END SUB

SUB plx6502
  s = (s + 1) AND &HFF
  x = Read6502(s + &H100)
  IF (x) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (x AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB ply6502
  s = (s + 1) AND &HFF
  
  y = Read6502(s + &H100)
  IF (y) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (y AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB



SUB rol6502
  saveflags = (p AND &H1)
  adrmode opcode
  value = Read6502(savepc)
      
  p = (p AND &HFE) OR ((value \ 128) AND &H1)
  
  value = (value * 2) AND &HFF
  value = value OR saveflags
  
  Write6502 savepc, (value AND &HFF)
  
  IF (value) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (value AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB rola6502
  saveflags = (p AND &H1)
  p = (p AND &HFE) OR ((a \ 128) AND &H1)
  a = (a * 2) AND &HFF
  a = a OR saveflags
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB ror6502
  saveflags = (p AND &H1)
  adrmode opcode
  value = Read6502(savepc)
      
  p = (p AND &HFE) OR (value AND &H1)
  value = (value \ 2) AND &HFF
  IF (saveflags) THEN
    value = value OR &H80
  END IF
  Write6502 (savepc), value AND &HFF
  IF (value) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (value AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB rora6502
  saveflags = (p AND &H1)
  p = (p AND &HFE) OR (a AND &H1)
  a = (a \ 2) AND &HFF
  
  IF (saveflags) THEN
    a = a OR &H80
  END IF
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB rti6502
  s = (s + 1) AND &HFF
  p = Read6502(s + &H100&) OR &H20
  s = (s + 1) AND &HFF
  pc = Read6502(s + &H100&)
  s = (s + 1) AND &HFF
  pc = pc + (Read6502(s + &H100) * &H100&)
END SUB

SUB rts6502
  s = (s + 1) AND &HFF
  pc = Read6502(s + &H100)
  s = (s + 1) AND &HFF
  pc = pc + (Read6502(s + &H100) * &H100&)
  pc = pc + 1
END SUB

SUB sbc6502
  adrmode opcode
  value = Read6502(savepc) XOR &HFF
  
  saveflags = (p AND &H1)
  
  sum = a
  sum = (sum + value) AND &HFF
  sum = (sum + (saveflags * 16)) AND &HFF
  
  IF ((sum > &H7F) OR (sum <= -&H80)) THEN
    p = p OR &H40
  ELSE
    p = p AND &HBF
  END IF
  
  sum = a + (value + saveflags)
  
  IF (sum > &HFF) THEN
    p = p OR &H1
  ELSE
    p = p AND &HFE
  END IF
  
  a = sum AND &HFF
  IF (p AND &H8) THEN
        a = (a - &H66) AND &HFF
        p = p AND &HFE
    IF ((a AND &HF) > &H9) THEN
      a = (a + &H6) AND &HFF
    END IF
    IF ((a AND &HF0) > &H90) THEN
      a = (a + &H60) AND &HFF
      p = p OR &H1
    END IF
  ELSE
    clockticks = clockticks + 1
  END IF
  IF (a) THEN
    p = p And &HFD
  ELSE
    p = p OR &H2
  END IF
  
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB sec6502
  p = p OR &H1
END SUB

SUB sed6502
  p = p OR &H8
END SUB

SUB sei6502
  p = p OR &H4
END Sub

SUB tax6502
  x = a
  IF (x) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (x AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB tay6502
  y = a
  IF (y) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (y AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB tsx6502
  x = s
  IF (x) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (x AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB txa6502
  a = x
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END SUB

SUB txs6502
  s = x
END SUB

SUB tya6502
  a = y
  IF (a) THEN
    p = p AND &HFD
  ELSE
    p = p OR &H2
  END IF
  IF (a AND &H80) THEN
    p = p OR &H80
  ELSE
    p = p AND &H7F
  END IF
END Sub


SUB zpx6502
  savepc = Read6502(pc)
  savepc = savepc + x
  savepc = savepc AND &HFF
  pc = pc + 1
End SUB

SUB zpy6502
  savepc = Read6502(pc)
  savepc = savepc + y
  pc = pc + 1
END Sub
' ************************
' FIN DE INSTRUCCIONES CPU
' ************************





' ************************************************************************
' FUNCIONES COMUNES MAS USADAS: LEER/ESCRIBIR RAM, PUERTOS I/O, RESET, ETC
' ************************************************************************
Sub adrmode (opcode As UInteger)
 Select CASE addrmode(opcode)
  CASE 0 'implied
  CASE 1 'indx
    indx6502
  CASE 2 'zeropage
    savepc = Read6502(pc): savepc = savepc AND &HFF: pc = pc + 1
  CASE 3 'immediate
    savepc = pc: pc = pc + 1
  CASE 4 'abs
    savepc = Read6502(pc) + (Read6502(pc + 1) * &H100&): pc = pc + 2
  CASE 5 'rel
    savepc = Read6502(pc): pc = pc + 1: IF (savepc AND &H80) THEN savepc = savepc - &H100&
  CASE 6 'indy
    indy6502
  CASE 7 'indzp
    indzp6502
  CASE 8 'zpx
    zpx6502
  CASE 9 'absy
    absy6502
  CASE 10 'absx
    absx6502
  CASE 11 'indirect
    indirect6502
  CASE 12 'indabsx
    indabsx6502
  CASE 13 'zpy
    zpy6502
 End SELECT
END Sub

' *********************************  ESTADO DE LAS "BANDERAS"
SUB SetFlags (BYVAL value AS UInteger)
	
 If value THEN
  p = p AND &HFD
 Else
  p = p OR &H2
 End If
 
 If (value AND &H80) THEN
  p = p OR &H80
 Else
  p = p AND &H7F
 End If
 
END Sub


FUNCTION Read6502 (BYVAL addr AS UINTEGER) As UInteger

 Select case addr
  case &hc000
    if len(keybuff)>0 then
      Read6502 = asc(Left(keybuff,1)) + 128      
    else
      Read6502 = 0
    end if
	case &hc010 to &hc01f
    keybuff = Mid(keybuff, 2)
  case else
    Read6502 = RAM(addr)
 End select

END FUNCTION

' *************************** RESET
SUB reset6502
 cpurunning = 1
 clockticks = 0
 totalexec = 0

 a = 0: x = 0: y = 0: p = &H20
 s = &HFF

 ' punto de inicio de ejecucion de la CPU
 pc = Read6502(&HFFFC&) + (Read6502(&HFFFD&) * 256)

END Sub

' *************************** PUERTOS I/O ************************
FUNCTION IOread (BYVAL addr AS UINTEGER) As UInteger
IOread = &h0 'Int(Rnd(1)*256)
END FUNCTION

SUB IOwrite (BYVAL addr AS UINTEGER, BYVAL value AS UINTEGER)
END Sub



' ----------------------------------------------------------------------------------------------------


' ********************** ESCRITURA/LECTURA EN RAM
' Y APROVECHANDO PARA "MIRAR" LA PANTALLA DEL APPLE-II
SUB Write6502 (BYVAL addr AS UINTEGER, BYVAL value AS UInteger)

' pone la pantalla modo texto del APPLE	
Dim by128 As UInteger
Dim in128 As UInteger
Dim by40 As UInteger
Dim xp As UInteger 
Dim yp As UInteger 


' PANTALLA MODO TEXTO
If addr > &h3ff and addr < &h800 then
  temp = addr - &h400
  by128 = (temp - (temp mod 128)) / 128
  in128 = temp mod 128
  by40 = (in128 - (in128 mod 40)) / 40
  xp = (in128 mod 40) + 1
  yp = 1 + (8 * by40) + by128
  yp+=1:If yp=33 Then yp=1:xp+=1
  locate yp+1, xp+1 ,0 ' el "0" oculta el cursor
  print Chr(value And 127);
end If


' DEPURACION
'aveces+=1
'If aveces>256 Then
' aveces=0
' Locate 1,1:Print "dir:";Hex(ini);"   ";
' If MultiKey(SC_PAGEUP) Then ini =ini+80
' If MultiKey(SC_PAGEDOWN) Then ini =ini-80
' xp=40:yp=1
' For temp=0+ini To 2400+ini  
'  xp+=1:If xp=81 Then xp=41:yp+=1:If yp=28 Then GoTo NO
'  locate yp, xp ,0' el "0" oculta el cursor
'  by40=ram(temp)
'   If by40 > 31 Then Print Chr(RAM(temp)) Else Print " " ' And 127)
' Next
'End IF
'NO:


Select case addr
  case &hc010 to &hc01f
    keybuff = mid(keybuff, 2)
End Select

 'solo escribe en RAM, nunca en ROM
 If addr<IniROM Then RAM(addr) = value 

END SUB