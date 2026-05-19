DEFINT A-Z
DECLARE SUB Bordo2 (ri%, ci%, rf%, cf%)
DECLARE SUB Box (Rig%, Col%, alt%, lar%, mo%)
DECLARE SUB LeggiOggetti ()
DECLARE SUB LeggiOggetto (Vett%(), Num.File%)
DECLARE SUB PosizionaCPU (Num.Navi%, Dim.Navi%, Nave%())
DECLARE SUB PosizionaNavi (Num.Navi%, Dim.Navi%, Nave%())
DECLARE SUB Scelta (Rig%, Col%, Strin$, po%, mo%)
DECLARE SUB SettaIntorno (Vett.bidim%(), x%, y%)

COMMON SHARED xGrid%, yGrid%
COMMON SHARED Off.Xgi%, Off.Ygi%
COMMON SHARED Off.Xne%, Off.Yne%

CONST TRUE = -1
CONST FALSE = 0
CONST xmin.grid% = 8
CONST ymin.grid% = 8
CONST xmax.grid% = 15
CONST ymax.grid% = 15

'ON ERROR GOTO gesterr

RANDOMIZE TIMER

SCREEN 12
WIDTH 80, 60
PALETTE 2, &H2F0000
PALETTE 3, &H2F1808
PALETTE 4, &H2F1F08
PALETTE 5, &H2F2414
PALETTE 1, &H303030
PALETTE 0, &H282828
PALETTE 7, &H202020

xGrid% = 15
yGrid% = 15

yMess% = 8 + yGrid% * 2
hSer% = (58 - yMess%) / 2 - 2
ySer1% = yMess% + 3
ySer2% = ySer1% + hSer% + 2
xTurno% = 36
xEsAvv% = 21
xEsGio% = 52
PRINT
PRINT
PRINT
PRINT "Vuoi giocare contro il computer o contro un altro giocatore (C\H):"
DO
	Avversario$ = UCASE$(INKEY$)
LOOP UNTIL Avversario$ = "C" OR Avversario$ = "H"

IF Avversario$ = "H" THEN
	OPEN "com2:9600,n,8,1" FOR RANDOM AS #2
	PriGio% = 65536 * RND - 32768
	PRINT #2, "&%PR" + RIGHT$("000" + HEX$(PriGio%), 4)
	LineaSer$ = ""
	DO
		WHILE LOC(2) <> 0
			LettoSer$ = INPUT$(1, 2)
			IF LettoSer$ = CHR$(13) THEN
				IF LEFT$(LineaSer$, 4) = "&%PR" THEN
					PriAvv% = VAL("&H" + RIGHT$(LineaSer$, 4))
					IF PriAvv% > PriGio% THEN
						Turno$ = "A"
					ELSE
						Turno$ = "G"
					END IF
					EXIT DO
				ELSE
					LineaSer$ = ""
				END IF
			ELSE
				LineaSer$ = LineaSer$ + LettoSer$
			END IF
		WEND
	LOOP
	PRINT #2, "&%PR" + RIGHT$("000" + HEX$(PriGio%), 4)
	WHILE LOC(2) <> 0
		LettoSer$ = INPUT$(1, 2)
	WEND
	LineaSer$ = ""
ELSE
	Turno$ = "G"
	Prossima$ = ""
	ncc% = 0
END IF

CLS

Off.Xgi% = 41 - (xGrid% + 1) * 2
Off.Ygi% = 5
Off.Xne% = 44
Off.Yne% = 5

n.somm% = 4
n.cacc% = 3
n.Incr% = 2
n.Ammi% = 1

Tot.Navi% = n.somm% + n.cacc% + n.Incr% + n.Ammi% - 1
DIM Rappor$(Tot.Navi%)

DIM SHARED LineaGio$(hSer%)
DIM SHARED LineaAvv$(hSer%)
DIM SHARED GridGio%(xGrid%, yGrid%)
DIM SHARED GridAvv%(xGrid%, yGrid%)
DIM SHARED Onde%(66)
DIM SHARED Mirino%(66)
DIM SHARED Buffer%(258)
DIM SHARED Nave%(1032)
DIM SHARED Ammi%(1032)
DIM SHARED Incr%(1032)
DIM SHARED Cacc%(1032)
DIM SHARED Somm%(1032)
DIM SHARED Mina%(66)
DIM SHARED Espl%(660)
DIM SHARED Vett%(1032)

COLOR 11
PRINT TAB(20); "ATTENDERE LA PREPARAZIONE DEGLI OGGETTI!!"
LeggiOggetti

CLS
Bordo2 1, 1, 60, 80
Box ySer1%, 3, hSer%, 76, 1
Box ySer2%, 3, hSer%, 76, 1
Scelta yMess%, xTurno%, "            ", 0, 1
Scelta yMess%, xEsAvv%, "           ", 0, 1
Scelta yMess%, xEsGio%, "           ", 0, 1
COLOR 15
FOR x% = 0 TO xGrid%
	Scelta Off.Ygi% - 2, Off.Xgi% + x% * 2, CHR$(65 + x%), 0, 1
	Scelta Off.Yne% - 2, Off.Xne% + x% * 2, CHR$(65 + x%), 0, 1
	FOR y% = 0 TO yGrid%
		IF x% = 0 THEN
			Numero$ = RIGHT$(RTRIM$(STR$(y% + 1)), 2)
			Scelta Off.Ygi% + y% * 2, Off.Xgi% - 3, Numero$, 0, 1
			Scelta Off.Yne% + y% * 2, Off.Xne% - 3, Numero$, 0, 1
		END IF
		PUT ((Off.Xgi% - 1) * 8 + x% * 16 - 4, (Off.Ygi% - 1) * 8 + y% * 16 - 4), Onde%
		PUT ((Off.Xne% - 1) * 8 + x% * 16 - 4, (Off.Yne% - 1) * 8 + y% * 16 - 4), Onde%
	NEXT
NEXT
IF Avversario$ = "C" THEN
	PosizionaCPU (n.Ammi%), 4, Ammi%()
	PosizionaCPU (n.Incr%), 3, Incr%()
	PosizionaCPU (n.cacc%), 2, Cacc%()
	PosizionaCPU (n.somm%), 1, Somm%()
END IF
PosizionaNavi (n.Ammi%), 4, Ammi%()
PosizionaNavi (n.Incr%), 3, Incr%()
PosizionaNavi (n.cacc%), 2, Cacc%()
PosizionaNavi (n.somm%), 1, Somm%()

IF Avversario$ = "H" THEN
	LineaSer$ = ""
	PRINT #2, "&%OK"
	DO
		WHILE LOC(2) <> 0
			LettoSer$ = INPUT$(1, 2)
			IF LettoSer$ = CHR$(13) THEN
				IF LineaSer$ = "&%OK" THEN
					EXIT DO
				ELSE
					LineaSer$ = ""
				END IF
			ELSE
				LineaSer$ = LineaSer$ + LettoSer$
			END IF
		WEND
	LOOP
	SLEEP 1
	WHILE LOC(2) <> 0
		LettoSer$ = ""
	WEND
	SLEEP 1
	LineaSer$ = ""
END IF
AttesaEsito% = FALSE
IF Turno$ = "A" THEN
	Scelta yMess%, xTurno%, " Avversario ", 0, 1
ELSE
	Scelta yMess%, xTurno%, " Giocatore  ", 0, 0
END IF
xGio% = 0
yGio% = 0
xMir% = (Off.Xne% - 1) * 8 + xGio% * 16 - 4
yMir% = (Off.Yne% - 1) * 8 + yGio% * 16 - 4
nLinGio% = 0
nLinAvv% = 0
Scelta Off.Yne% - 2, Off.Xne% + xGio% * 2, CHR$(65 + xGio%), 0, 0
Numero$ = RIGHT$(RTRIM$(STR$(yGio% + 1)), 2)
Scelta Off.Yne% + yGio% * 2, Off.Xne% - 3, Numero$, 0, 0
GET (xMir%, yMir%)-STEP(16, 16), Buffer%
PUT (xMir%, yMir%), Mirino%, OR
Modo$ = "Gioco"
DO
	IF Avversario$ = "H" THEN
		WHILE LOC(2) <> 0
			LettoSer$ = INPUT$(1, 2)
			IF LettoSer$ = CHR$(13) THEN
				SELECT CASE LEFT$(LineaSer$, 4)
				CASE "&%MS"
					LineaAvv$(nLinAvv%) = MID$(LineaSer$, 5)
					LineaSer$ = ""
					IF nLinAvv% = hSer% THEN
						nLinAvv% = nLinAvv% - 1
						FOR i% = 1 TO hSer%
							LineaAvv$(i% - 1) = LineaAvv$(i%)
						NEXT
						COLOR 15
						FOR i% = 0 TO hSer% - 1
							LOCATE ySer1% + i%, 3
							PRINT LEFT$(LineaAvv$(i%) + SPACE$(76), 76);
						NEXT
					ELSE
						COLOR 15
						LOCATE ySer1% + nLinAvv%, 3
						PRINT LineaAvv$(nLinAvv%)
					END IF
					nLinAvv% = nLinAvv% + 1
				CASE "&%CB"
					XAvv% = VAL("&h" + MID$(LineaSer$, 5, 2))
					YAvv% = VAL("&h" + MID$(LineaSer$, 7, 2))
					LineaSer$ = ""
					SELECT CASE GridGio%(XAvv%, YAvv%)
					CASE 0, 9
						PUT ((Off.Xgi% - 1) * 8 + XAvv% * 16 - 4, (Off.Ygi% - 1) * 8 + YAvv% * 16 - 4), Mina%, PSET
						GridGio%(XAvv%, YAvv%) = GridGio%(XAvv%, YAvv%) + 10
						PRINT #2, "&%EBV"
						Scelta yMess%, xEsAvv%, "           ", 0, 1
						PRINT #2, "&%EBV"
					CASE 1 TO 4
						FOR v = 0 TO 9
							PUT ((Off.Xgi% - 1) * 8 + XAvv% * 16 - 4, (Off.Ygi% - 1) * 8 + YAvv% * 16 - 4), Espl%(66 * v), PSET
							FOR i = 0 TO 1000 + v * 100: NEXT
						NEXT
						PRINT #2, "&%EBC"
						Scelta yMess%, xEsAvv%, "  Colpito  ", 0, 1
						PRINT #2, "&%EBC"
					END SELECT
					Turno$ = "G"
					Scelta yMess%, xTurno%, " Giocatore  ", 0, 0
				CASE "&%EB"
					Stringa$ = RIGHT$(LineaSer$, LEN(LineaSer$) - 4)
					SELECT CASE Stringa$
					CASE "V"
						PUT (xMir%, yMir%), Buffer%, PSET
						PUT (xMir%, yMir%), Mina%, PSET
						GET (xMir%, yMir%)-STEP(16, 16), Buffer%
						PUT (xMir%, yMir%), Mirino%, OR
						Scelta yMess%, xEsGio%, "           ", 0, 1
					CASE "C"
						PUT (xMir%, yMir%), Buffer%, PSET
						FOR v = 0 TO 9
							PUT (xMir%, yMir%), Espl%(66 * v), PSET
							FOR i = 0 TO 1000 + v * 100: NEXT
						NEXT
						GET (xMir%, yMir%)-STEP(16, 16), Buffer%
						PUT (xMir%, yMir%), Mirino%, OR
						Scelta yMess%, xEsGio%, "  Colpito  ", 0, 1
					END SELECT
					IF GridAvv%(xGio%, yGio%) < 10 THEN GridAvv%(xGio%, yGio%) = GridAvv%(xGio%, yGio%) + 10
					LineaSer$ = ""
					AttesaEsito% = FALSE
				CASE ELSE
					COLOR 12
					LOCATE 55, 5
					PRINT SPACE$(70);
					LOCATE 55, 5
					COLOR 11: PRINT LineaSer$
					LineaSer$ = ""
					AttesaEsito% = FALSE
				END SELECT
			ELSE
				LineaSer$ = LineaSer$ + LettoSer$
			END IF
		WEND
	ELSE
		IF Turno$ = "A" THEN
			SLEEP 1
			IF Prossima$ = "" THEN
				DO
					xa% = xGrid% * RND
					ya% = yGrid% * RND
				LOOP UNTIL GridGio%(xa%, ya%) < 10
			ELSE
				DO
					xa% = xGrid% * RND
					ya% = yGrid% * RND
				LOOP UNTIL GridGio%(xa%, ya%) < 10
			END IF
			SELECT CASE GridGio%(xa%, ya%)
			CASE 0, 9
				Scelta yMess%, xEsAvv%, "           ", 0, 1
				PUT ((Off.Xgi% - 1) * 8 + xa% * 16 - 4, (Off.Ygi% - 1) * 8 + ya% * 16 - 4), Mina%, PSET
				GridGio%(xa%, ya%) = GridGio%(xa%, ya%) + 10
				Turno$ = "G"
				Scelta yMess%, xTurno%, " Giocatore  ", 0, 0
			CASE 1 TO 4
				Scelta yMess%, xEsAvv%, "  Colpito  ", 0, 1
				ncc% = ncc% + 1
				FOR v = 0 TO 9
					PUT ((Off.Xgi% - 1) * 8 + xa% * 16 - 4, (Off.Ygi% - 1) * 8 + ya% * 16 - 4), Espl%(66 * v), PSET
					FOR i = 0 TO 1000 + v * 100: NEXT
				NEXT
				IF GridGio%(xa%, ya%) = ncc% THEN
					ncc% = 0
					IF xa% > 0 THEN
						IF GridGio%(xa% - 1, ya%) < 10 THEN GridGio%(xa% - 1, ya%) = GridGio%(xa% - 1, ya%) + 20
					END IF
					IF xa% < xGrid% THEN
						IF GridGio%(xa% + 1, ya%) < 10 THEN GridGio%(xa% + 1, ya%) = GridGio%(xa% + 1, ya%) + 20
					END IF
					IF ya% > 0 THEN
						IF GridGio%(xa%, ya% - 1) < 10 THEN GridGio%(xa%, ya% - 1) = GridGio%(xa%, ya% - 1) + 20
					END IF
					IF ya% < yGrid% THEN
						IF GridGio%(xa%, ya% + 1) < 10 THEN GridGio%(xa%, ya% + 1) = GridGio%(xa%, ya% + 1) + 20
					END IF
				END IF
				IF GridGio%(xa%, ya%) > ncc% THEN
					IF ncc% = 1 THEN
						xrc% = xa%
						yrc% = ya%
						Prossima$ = "SENO"
					ELSE
					END IF
				END IF
				GridGio%(xa%, ya%) = GridGio%(xa%, ya%) + 10
				IF xa% > 0 AND ya% > 0 THEN
					IF GridGio%(xa% - 1, ya% - 1) < 10 THEN GridGio%(xa% - 1, ya% - 1) = GridGio%(xa% - 1, ya% - 1) + 20
				END IF
				IF xa% < xGrid% AND ya% > 0 THEN
					IF GridGio%(xa% + 1, ya% - 1) < 10 THEN GridGio%(xa% + 1, ya% - 1) = GridGio%(xa% + 1, ya% - 1) + 20
				END IF
				IF xa% < xGrid% AND ya% < yGrid% THEN
					IF GridGio%(xa% + 1, ya% + 1) < 10 THEN GridGio%(xa% + 1, ya% + 1) = GridGio%(xa% + 1, ya% + 1) + 20
				END IF
				IF xa% > 0 AND ya% < yGrid% THEN
					IF GridGio%(xa% - 1, ya% + 1) < 10 THEN GridGio%(xa% - 1, ya% + 1) = GridGio%(xa% - 1, ya% + 1) + 20
				END IF
				Turno$ = "G"
				Scelta yMess%, xTurno%, " Giocatore  ", 0, 0
			END SELECT
		END IF
	END IF
	LettoTas$ = INKEY$
	SELECT CASE LettoTas$
	CASE CHR$(0) + "H", CHR$(28)
		IF NOT (AttesaEsito%) THEN
			IF yGio% > 0 THEN
				Numero$ = RIGHT$(RTRIM$(STR$(yGio% + 1)), 2)
				Scelta Off.Yne% + yGio% * 2, Off.Xne% - 3, Numero$, 0, 1
				PUT (xMir%, yMir%), Buffer%, PSET
				yGio% = yGio% - 1
				yMir% = (Off.Yne% - 1) * 8 + yGio% * 16 - 4
				Numero$ = RIGHT$(RTRIM$(STR$(yGio% + 1)), 2)
				Scelta Off.Yne% + yGio% * 2, Off.Xne% - 3, Numero$, 0, 0
				GET (xMir%, yMir%)-STEP(16, 16), Buffer%
				PUT (xMir%, yMir%), Mirino%, OR
			END IF
		END IF
	CASE CHR$(0) + "M", CHR$(30)
		IF NOT (AttesaEsito%) THEN
			IF xGio% < xGrid% THEN
				PUT (xMir%, yMir%), Buffer%, PSET
				Scelta Off.Yne% - 2, Off.Xne% + xGio% * 2, CHR$(65 + xGio%), 0, 1
				xGio% = xGio% + 1
				xMir% = (Off.Xne% - 1) * 8 + xGio% * 16 - 4
				GET (xMir%, yMir%)-STEP(16, 16), Buffer%
				PUT (xMir%, yMir%), Mirino%, OR
				Scelta Off.Yne% - 2, Off.Xne% + xGio% * 2, CHR$(65 + xGio%), 0, 0
			END IF
		END IF
	CASE CHR$(0) + "P", CHR$(29)
		IF NOT (AttesaEsito%) THEN
			IF yGio% < yGrid% THEN
				PUT (xMir%, yMir%), Buffer%, PSET
				Numero$ = RIGHT$(RTRIM$(STR$(yGio% + 1)), 2)
				Scelta Off.Yne% + yGio% * 2, Off.Xne% - 3, Numero$, 0, 1
				yGio% = yGio% + 1
				yMir% = (Off.Yne% - 1) * 8 + yGio% * 16 - 4
				Numero$ = RIGHT$(RTRIM$(STR$(yGio% + 1)), 2)
				Scelta Off.Yne% + yGio% * 2, Off.Xne% - 3, Numero$, 0, 0
				GET (xMir%, yMir%)-STEP(16, 16), Buffer%
				PUT (xMir%, yMir%), Mirino%, OR
			END IF
		END IF
	CASE CHR$(0) + "K", CHR$(31)
		IF NOT (AttesaEsito%) THEN
			IF xGio% > 0 THEN
				PUT (xMir%, yMir%), Buffer%, PSET
				Scelta Off.Yne% - 2, Off.Xne% + xGio% * 2, CHR$(65 + xGio%), 0, 1
				xGio% = xGio% - 1
				xMir% = (Off.Xne% - 1) * 8 + xGio% * 16 - 4
				GET (xMir%, yMir%)-STEP(16, 16), Buffer%
				PUT (xMir%, yMir%), Mirino%, OR
				Scelta Off.Yne% - 2, Off.Xne% + xGio% * 2, CHR$(65 + xGio%), 0, 0
			END IF
		END IF
	CASE CHR$(0) + "R"
		'Tasto Ins per inserire la banderina
	CASE CHR$(0) + "S"
		'Tasto Canc per cancellare la bandierina
	CASE CHR$(0) + ";"
		IF Modo$ = "Gioco" THEN
			Modo$ = "Messaggio"
			Scelta yMess%, xTurno%, " Messaggio  ", 0, 1
			Box ySer2%, 3, hSer%, 76, 0
		ELSE
			Modo$ = "Gioco"
			IF Turno$ = "G" THEN
				Scelta yMess%, xTurno%, " Giocatore  ", 0, 1
			ELSE
				Scelta yMess%, xTurno%, " Avversario ", 0, 1
			END IF
			Box ySer2%, 3, hSer%, 76, 1
		END IF
	CASE CHR$(13)
		IF Modo$ = "Gioco" THEN
			IF Turno$ = "G" THEN
				IF Avversario$ = "C" THEN
					SELECT CASE GridAvv%(xGio%, yGio%)
					CASE 0, 9
						Scelta yMess%, xEsGio%, "           ", 0, 1
						PUT (xMir%, yMir%), Buffer%, PSET
						PUT (xMir%, yMir%), Mina%, PSET
						GET (xMir%, yMir%)-STEP(16, 16), Buffer%
						PUT (xMir%, yMir%), Mirino%, OR
						GridAvv%(xGio%, yGio%) = GridAvv%(xGio%, yGio%) + 10
						Turno$ = "A"
						Scelta yMess%, xTurno%, " Avversario ", 0, 1
					CASE 1 TO 4
						Scelta yMess%, xEsGio%, "  Colpito  ", 0, 1
						PUT (xMir%, yMir%), Buffer%, PSET
						FOR v = 0 TO 9
							PUT (xMir%, yMir%), Espl%(66 * v), PSET
							FOR i = 0 TO 1000 + v * 100: NEXT
						NEXT
						GET (xMir%, yMir%)-STEP(16, 16), Buffer%
						PUT (xMir%, yMir%), Mirino%, OR
						GridAvv%(xGio%, yGio%) = GridAvv%(xGio%, yGio%) + 10
						Turno$ = "A"
						Scelta yMess%, xTurno%, " Avversario ", 0, 1
					END SELECT
				ELSE
					IF GridAvv%(xGio%, yGio%) <= 10 THEN
						PRINT #2, "&%CB" + RIGHT$("0" + HEX$(xGio%), 2) + RIGHT$("0" + HEX$(yGio%), 2)
						AttesaEsito% = TRUE
						Turno$ = "A"
						Scelta yMess%, xTurno%, " Avversario ", 0, 1
					END IF
				END IF
			END IF
		ELSE
			PRINT #2, "&%MS"; LineaGio$(nLinGio%)
			nLinGio% = nLinGio% + 1
			IF nLinGio% = hSer% THEN
				nLinGio% = nLinGio% - 1
				FOR i% = 1 TO hSer%
					LineaGio$(i% - 1) = LineaGio$(i%)
				NEXT
				COLOR 8
				FOR i% = 0 TO hSer% - 1
					LOCATE ySer2% + i%, 3
					PRINT LEFT$(LineaGio$(i%) + SPACE$(76), 76);
				NEXT
			ELSE
				COLOR 8
				LOCATE ySer2% + nLinGio% - 1, 3
				PRINT LineaGio$(nLinGio% - 1);
			END IF
		END IF
	CASE ELSE
		IF Modo$ = "Messaggio" THEN
			IF LettoTas$ >= " " THEN
				IF LEN(LineaGio$(nLinGio%)) < 77 THEN
					LineaGio$(nLinGio%) = LineaGio$(nLinGio%) + LettoTas$
					COLOR 15: LOCATE ySer2% + nLinGio%, 3
					PRINT LineaGio$(nLinGio%);
				END IF
			END IF
		END IF
	END SELECT
LOOP UNTIL LettoTas$ = CHR$(27)

SCREEN 0
END

gesterr:
'  PRINT "codice errore : ", ERR
SELECT CASE ERR
CASE 68
	PRINT "Errore sulla periferica"
END SELECT
END

DEFSNG A-Z
SUB Bordo2 (ri%, ci%, rf%, cf%) STATIC
	xi% = (ci% - 1) * 8
	yi% = (ri% - 1) * 8
	xf% = (cf% - 1) * 8 + 7
	yf% = (rf% - 1) * 8 + 7
	LINE (xi%, yf%)-(xi% + 1, yi%), 15, B
	LINE (xf%, yi%)-(xf% - 1, yf%), 8, B
	LINE (xi%, yi%)-(xf% - 1, yi% + 1), 15, B
	LINE (xf%, yf%)-(xi% + 1, yf% - 1), 8, B
	PSET (xf% - 1, yi% + 1), 8
	PSET (xi% + 1, yf% - 1), 15
	xi% = xi% + 4
	yi% = yi% + 4
	xf% = xf% - 4
	yf% = yf% - 4
	LINE (xi%, yf%)-(xi% + 1, yi%), 8, B
	LINE (xf%, yi%)-(xf% - 1, yf%), 15, B
	LINE (xi%, yi%)-(xf% - 1, yi% + 1), 8, B
	LINE (xf%, yf%)-(xi% + 1, yf% - 1), 15, B
	PSET (xf% - 1, yi% + 1), 15
	PSET (xi% + 1, yf% - 1), 8
	LINE (xi% + 2, yi% + 2)-(xf% - 2, yf% - 2), 0, BF
END SUB

SUB Box (Rig%, Col%, alt%, lar%, mo%)
	xi% = (Col% - 1) * 8 - 4
	yi% = (Rig% - 1) * 8 - 4
	xf% = xi% + (lar% + 1) * 8 - 1
	yf% = yi% + (alt% + 1) * 8 - 1
	SELECT CASE mo%
	CASE 0
		LINE (xi%, yf%)-(xi% + 1, yi%), 15, B
		LINE (xf%, yi%)-(xf% - 1, yf%), 8, B
		LINE (xi%, yi%)-(xf% - 1, yi% + 1), 15, B
		LINE (xf%, yf%)-(xi% + 1, yf% - 1), 8, B
		PSET (xf% - 1, yi% + 1), 8
		PSET (xi% + 1, yf% - 1), 15
	CASE 1
		LINE (xi%, yf%)-(xi% + 1, yi%), 8, B
		LINE (xf%, yi%)-(xf% - 1, yf%), 15, B
		LINE (xi%, yi%)-(xf% - 1, yi% + 1), 8, B
		LINE (xf%, yf%)-(xi% + 1, yf% - 1), 15, B
		PSET (xf% - 1, yi% + 1), 15
		PSET (xi% + 1, yf% - 1), 8
	END SELECT
END SUB

DEFINT A-Z
SUB LeggiOggetti
	OPEN "objects2.bob" FOR INPUT AS #1
		WHILE NOT (EOF(1))
			Matrice$ = INPUT$(4, 1)
			SELECT CASE Matrice$
			CASE "ONDE"
				LeggiOggetto Onde%(), 1
			CASE "MIRI"
				LeggiOggetto Mirino%(), 1
			CASE "MINA"
				LeggiOggetto Mina%(), 1
			CASE "ESPL"
				LeggiOggetto Espl%(), 1
			CASE "AMMI"
				LeggiOggetto Ammi%(), 1
			CASE "INCR"
				LeggiOggetto Incr%(), 1
			CASE "CACC"
				LeggiOggetto Cacc%(), 1
			CASE "SOMM"
				LeggiOggetto Somm%(), 1
			END SELECT
		WEND
	CLOSE #1
END SUB

SUB LeggiOggetto (Vett%(), Num.File%)
	lungh% = VAL("&H" + INPUT$(4, 1))
	FOR i = 0 TO lungh%
		Vett%(i) = VAL("&H" + INPUT$(4, 1))
	NEXT
END SUB

SUB PosizionaCPU (Num.Navi%, Dim.Navi%, Nave%()) STATIC
	Dim.Navi% = Dim.Navi% - 1
	DO
		DO
			xi% = xGrid% * RND
			yi% = yGrid% * RND
			Direz% = 3 * RND
			SELECT CASE Direz%
			CASE 0
				inc.x% = 0
				inc.y% = 1
				Orie% = 0
			CASE 1
				inc.x% = 1
				inc.y% = 0
				Orie% = 258
			CASE 2
				inc.x% = 0
				inc.y% = -1
				Orie% = 516
			CASE 3
				inc.x% = -1
				inc.y% = 0
				Orie% = 774
			END SELECT
			xc% = (Off.Xne% + xi% * 2) * 8 - 12
			yc% = (Off.Yne% + yi% * 2) * 8 - 12
			x1% = xc% + 16 * (Dim.Navi% * SGN(inc.x% AND &H8000))
			y1% = yc% + 16 * (Dim.Navi% * SGN(inc.y% AND &H8000))
			x2% = x1% + 15 + 16 * (Dim.Navi% * ABS(inc.x%))
			y2% = y1% + 15 + 16 * (Dim.Navi% * ABS(inc.y%))
			Libero = TRUE
			xc% = xi% + Dim.Navi% * inc.x%
			yc% = yi% + Dim.Navi% * inc.y%
			IF xc% < 0 OR xc% > xGrid% OR yc% < 0 OR yc% > yGrid% THEN
				Libero = FALSE
			ELSE
				FOR i% = 0 TO Dim.Navi%
					IF GridAvv%(xi% + i% * inc.x%, yi% + i% * inc.y%) > 0 THEN Libero = FALSE
				NEXT
			END IF
		LOOP UNTIL Libero
		FOR i% = 0 TO Dim.Navi%
			GridAvv%(xi% + i% * inc.x%, yi% + i% * inc.y%) = Dim.Navi% + 1
			xn% = xi% + i% * inc.x% - 1 * inc.y%
			yn% = yi% + i% * inc.y% - 1 * inc.x%
			SettaIntorno GridAvv%(), xi% + i% * inc.x% - 1 * inc.y%, yi% + i% * inc.y% - 1 * inc.x%
			SettaIntorno GridAvv%(), xi% + i% * inc.x% + 1 * inc.y%, yi% + i% * inc.y% + 1 * inc.x%
			IF i% = 0 THEN
				SettaIntorno GridAvv%(), xi% - inc.x% - 1 * inc.y%, yi% - inc.y% - 1 * inc.x%
				SettaIntorno GridAvv%(), xi% - inc.x%, yi% - inc.y%
				SettaIntorno GridAvv%(), xi% - inc.x% + 1 * inc.y%, yi% - inc.y% + 1 * inc.x%
			END IF
			IF i% = Dim.Navi% THEN
				SettaIntorno GridAvv%(), xi% + (Dim.Navi% + 1) * inc.x% - 1 * inc.y%, yi% + (Dim.Navi% + 1) * inc.y% - 1 * inc.x%
				SettaIntorno GridAvv%(), xi% + (Dim.Navi% + 1) * inc.x%, yi% + (Dim.Navi% + 1) * inc.y%
				SettaIntorno GridAvv%(), xi% + (Dim.Navi% + 1) * inc.x% + 1 * inc.y%, yi% + (Dim.Navi% + 1) * inc.y% + 1 * inc.x%
			END IF
		NEXT
'    PUT (x1%, y1%), Nave%(Orie%), PSET
		Num.Navi% = Num.Navi% - 1
	LOOP UNTIL Num.Navi% = 0
END SUB

DEFSNG A-Z
SUB PosizionaNavi (Num.Navi%, Dim.Navi%, Nave%()) STATIC
	Dim.Navi% = Dim.Navi% - 1
	Orie% = 0
	inc.x% = 0
	inc.y% = 1
	xc% = (Off.Xgi% + xi% * 2) * 8 - 12
	yc% = (Off.Ygi% + yi% * 2) * 8 - 12
	x1% = xc% + 16 * (Dim.Navi% * SGN(inc.x% AND &H8000))
	y1% = yc% + 16 * (Dim.Navi% * SGN(inc.y% AND &H8000))
	x2% = x1% + 15 + 16 * (Dim.Navi% * ABS(inc.x%))
	y2% = y1% + 15 + 16 * (Dim.Navi% * ABS(inc.y%))
	GET (x1%, y1%)-(x2%, y2%), Buffer%
	DO
		DO
			PUT (x1%, y1%), Buffer%, PSET
			xc% = (Off.Xgi% + xi% * 2) * 8 - 12
			yc% = (Off.Ygi% + yi% * 2) * 8 - 12
			x1% = xc% + 16 * (Dim.Navi% * SGN(inc.x% AND &H8000))
			y1% = yc% + 16 * (Dim.Navi% * SGN(inc.y% AND &H8000))
			x2% = x1% + 15 + 16 * (Dim.Navi% * ABS(inc.x%))
			y2% = y1% + 15 + 16 * (Dim.Navi% * ABS(inc.y%))
			GET (x1%, y1%)-(x2%, y2%), Buffer%
			PUT (x1%, y1%), Nave%(Orie%), PSET
			k$ = UCASE$(INKEY$)
			SELECT CASE k$
			CASE CHR$(0) + "H", CHR$(28)
				IF inc.y% < 0 THEN
					IF yi% - Dim.Navi% > 0 THEN yi% = yi% - 1
				ELSE
					IF yi% > 0 THEN yi% = yi% - 1
				END IF
			CASE CHR$(0) + "M", CHR$(30)
				IF inc.x% > 0 THEN
					IF xi% + Dim.Navi% < xGrid% THEN xi% = xi% + 1
				ELSE
					IF xi% < xGrid% THEN xi% = xi% + 1
				END IF
			CASE CHR$(0) + "P", CHR$(29)
				IF inc.y% > 0 THEN
					IF yi% + Dim.Navi% < yGrid% THEN yi% = yi% + 1
				ELSE
					IF yi% < yGrid% THEN yi% = yi% + 1
				END IF
			CASE CHR$(0) + "K", CHR$(31)
				IF inc.x% < 0 THEN
					IF xi% - Dim.Navi% > 0 THEN xi% = xi% - 1
				ELSE
					IF xi% > 0 THEN xi% = xi% - 1
				END IF
			CASE "S"
				IF yi% + Dim.Navi% < yGrid% THEN
					inc.x% = 0
					inc.y% = 1
					Orie% = 0
				END IF
			CASE "E"
				IF xi% + Dim.Navi% < xGrid% THEN
					inc.x% = 1
					inc.y% = 0
					Orie% = 258
				END IF
			CASE "N"
				IF yi% - Dim.Navi% > 0 THEN
					inc.x% = 0
					inc.y% = -1
					Orie% = 516
				END IF
			CASE "O"
				IF xi% - Dim.Navi% > 0 THEN
					inc.x% = -1
					inc.y% = 0
					Orie% = 774
				END IF
			END SELECT
		LOOP UNTIL k$ = CHR$(13)
		Libero = TRUE
		FOR i% = 0 TO Dim.Navi%
			IF GridGio%(xi% + i% * inc.x%, yi% + i% * inc.y%) > 0 THEN Libero = FALSE
		NEXT
		IF Libero THEN
			FOR i% = 0 TO Dim.Navi%
				GridGio%(xi% + i% * inc.x%, yi% + i% * inc.y%) = Dim.Navi% + 1
				xn% = xi% + i% * inc.x% - 1 * inc.y%
				yn% = yi% + i% * inc.y% - 1 * inc.x%
				SettaIntorno GridGio%(), xi% + i% * inc.x% - 1 * inc.y%, yi% + i% * inc.y% - 1 * inc.x%
				SettaIntorno GridGio%(), xi% + i% * inc.x% + 1 * inc.y%, yi% + i% * inc.y% + 1 * inc.x%
				IF i% = 0 THEN
					SettaIntorno GridGio%(), xi% - inc.x% - 1 * inc.y%, yi% - inc.y% - 1 * inc.x%
					SettaIntorno GridGio%(), xi% - inc.x%, yi% - inc.y%
					SettaIntorno GridGio%(), xi% - inc.x% + 1 * inc.y%, yi% - inc.y% + 1 * inc.x%
				END IF
				IF i% = Dim.Navi% THEN
					SettaIntorno GridGio%(), xi% + (Dim.Navi% + 1) * inc.x% - 1 * inc.y%, yi% + (Dim.Navi% + 1) * inc.y% - 1 * inc.x%
					SettaIntorno GridGio%(), xi% + (Dim.Navi% + 1) * inc.x%, yi% + (Dim.Navi% + 1) * inc.y%
					SettaIntorno GridGio%(), xi% + (Dim.Navi% + 1) * inc.x% + 1 * inc.y%, yi% + (Dim.Navi% + 1) * inc.y% + 1 * inc.x%
				END IF
			NEXT
			PUT (x1%, y1%), Nave%(Orie%), PSET
			GET (x1%, y1%)-(x2%, y2%), Buffer%
			Num.Navi% = Num.Navi% - 1
		END IF
	LOOP UNTIL Num.Navi% = 0
END SUB

SUB Scelta (Rig%, Col%, Strin$, po%, mo%) STATIC
	LOCATE Rig%, Col%: COLOR 15: PRINT Strin$
	IF po% <> 0 THEN
		LOCATE Rig%, Col% + po% - 1: COLOR 13: PRINT MID$(Strin$, po%, 1)
	END IF
	xi% = (Col% - 1) * 8 - 4
	yi% = (Rig% - 1) * 8 - 4
	xf% = xi% + LEN(Strin$) * 8 + 7
	yf% = yi% + 15
	SELECT CASE mo%
	CASE 0
		LINE (xi%, yf%)-(xi% + 1, yi%), 15, B
		LINE (xf%, yi%)-(xf% - 1, yf%), 8, B
		LINE (xi%, yi%)-(xf% - 1, yi% + 1), 15, B
		LINE (xf%, yf%)-(xi% + 1, yf% - 1), 8, B
		PSET (xf% - 1, yi% + 1), 8
		PSET (xi% + 1, yf% - 1), 15
	CASE 1
		LINE (xi%, yf%)-(xi% + 1, yi%), 8, B
		LINE (xf%, yi%)-(xf% - 1, yf%), 15, B
		LINE (xi%, yi%)-(xf% - 1, yi% + 1), 8, B
		LINE (xf%, yf%)-(xi% + 1, yf% - 1), 15, B
		PSET (xf% - 1, yi% + 1), 15
		PSET (xi% + 1, yf% - 1), 8
	END SELECT
END SUB

DEFINT A-Z
SUB SettaIntorno (Vett.bidim%(), x%, y%) STATIC
	IF x% >= 0 AND x% <= xGrid% AND y% >= 0 AND y% <= yGrid% THEN
		Vett.bidim%(x%, y%) = 9
	END IF
END SUB

