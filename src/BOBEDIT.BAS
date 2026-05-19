DEFINT A-Z
DECLARE SUB CostruisciOggetti ()
DECLARE SUB LeggiOggetti ()
DECLARE SUB LeggiOggetto (Vett%(), Num.File%)
DECLARE SUB ScriviOggetto (Vett%(), n.Elem%, Num.File%)

CONST TRUE = -1
CONST FALSE = 0

RANDOMIZE TIMER

CLS
'ON ERROR GOTO gesterr

DIM SHARED Onde%(66)
DIM SHARED Mirino%(66)
DIM SHARED Nave%(1032)
DIM SHARED Ammi%(1032)
DIM SHARED Incr%(1032)
DIM SHARED Cacc%(1032)
DIM SHARED Somm%(1032)
DIM SHARED Mina%(66)
DIM SHARED Espl%(660)
DIM SHARED Vett%(1032)

SCREEN 12
WIDTH 80, 60
PALETTE 2, &H2F0000
PALETTE 3, &H2F1808
PALETTE 4, &H2F1F08
PALETTE 5, &H2F2414

PALETTE 1, &H303030
PALETTE 0, &H282828
PALETTE 7, &H202020
COLOR 11
PRINT TAB(20); "ATTENDERE LA PREPARAZIONE DEGLI OGGETTI!!"
CostruisciOggetti
'LeggiOggetti

END
SCREEN 0
'screen close 1

END

gesterr:
	PRINT "codice errore : ", ERR
END

DEFSNG A-Z
SUB CostruisciOggetti
'***** Portaerei
'***** Incrociatore
'***** Cacciatorpediniere
'***** Corazzata
'***** Fregata
'***** Corvetta

	FOR i = 0 TO 15
		LINE (i * 20, 50)-(i * 20 + 19, 70), i, BF
	NEXT

'*****  Mirino:
	colore.mirino% = 15
	CIRCLE (107, 107), 5, colore.mirino%, , , 1
	LINE (100, 107)-(114, 107), colore.mirino%
	LINE (107, 100)-(107, 114), colore.mirino%
	CIRCLE (107, 107), 1, 0
	CIRCLE (107, 107), 2, 0
	GET (100, 100)-(115, 115), Mirino%
 
'*****  Onde:
	LINE (100, 100)-(115, 119), 2, BF
	FOR y = 100 TO 116 STEP 4
		LINE (100, y)-(115, y), 3, , &H8888
		LINE (100, y)-(115, y), 4, , &H707
		LINE (100, y + 1)-(115, y + 1), 4, , &H8888
		LINE (100, y + 1)-(115, y + 1), 3, , &H7070
		LINE (100, y + 1)-(115, y + 1), 5, , &H707
		LINE (100, y + 2)-(115, y + 2), 5, , &H8888
		LINE (100, y + 2)-(115, y + 2), 4, , &H7070
		LINE (100, y + 3)-(115, y + 3), 5, , &H7070
		LINE (100, y + 3)-(115, y + 3), 3, , &H707
	NEXT
	GET (100, 100)-(115, 115), Onde%

'*****  Mina:
	LINE (102, 107)-(113, 108), 8
	LINE (108, 102)-(107, 113), 8
	LINE (103, 103)-(112, 112), 8
	LINE (103, 112)-(112, 103), 8
	LINE (107, 104)-(108, 111), 8, BF
	LINE (104, 107)-(111, 108), 8, BF
	LINE (105, 105)-(110, 110), 8, BF
	LINE (107, 105)-(108, 110), 7, BF
	LINE (105, 107)-(110, 108), 7, BF
	LINE (106, 106)-(109, 109), 7, BF
	LINE (106, 107)-(109, 108), 0, BF
	LINE (107, 106)-(108, 109), 0, BF
	LINE (107, 107)-(106, 105), 1
	LINE (107, 108)-(105, 109), 1
	LINE (108, 108)-(110, 109), 1
	LINE (108, 107)-(111, 106), 1
	GET (100, 100)-(115, 115), Mina%
	PUT (100, 130), Mina%

'*****  Esplosione:
	FOR v = 0 TO 9
		PUT (100, 100), Mina%, PSET
		FOR i = 0 TO 2 + (v / 1.5)
			FOR an! = 0 TO 6.28 STEP .3
				x = 107 + i * COS(an! + RND - .5) + RND - .5
				y = 107 + i * SIN(an! + RND - .5) + RND - .5
				PSET (x, y), 12 + i / 2.5
			NEXT
		NEXT
		GET (100, 100)-(115, 115), Espl%(66 * v)
	NEXT
'  FOR v = 0 TO 9
'    PUT (100 + v * 20, 150), Espl%(66 * v), PSET
'  NEXT

'*****  Sommergibile:
	LINE (100, 100)-(115, 115), 0, BF
	LINE (100, 107)-(115, 108), 7, B
	LINE (102, 106)-(112, 109), 7, B
	LINE (102, 107)-(111, 108), 1, B
	LINE (105, 107)-(106, 108), 8, B
	PSET (112, 105), 5
	PSET (112, 110), 5
	PSET (113, 105), 5
	PSET (113, 110), 5
	PSET (114, 104), 5
	PSET (114, 111), 5
	PSET (115, 105), 5
	PSET (115, 110), 5
	PUT (200, 100), Onde%, PSET
	PUT (270, 100), Onde%, PSET
	PUT (290, 100), Onde%, PSET
	PUT (310, 100), Onde%, PSET
	FOR x% = 0 TO 15
		FOR y% = 0 TO 15
			IF POINT(100 + x%, 100 + y%) <> 0 THEN
				PSET (200 + x%, 100 + y%), POINT(100 + x%, 100 + y%)
				PSET (270 + y%, 100 + x%), POINT(100 + x%, 100 + y%)
				PSET (290 + y%, 100 + (15 - x%)), POINT(100 + x%, 100 + y%)
				PSET (310 + (15 - x%), 100 + y%), POINT(100 + x%, 100 + y%)
			END IF
		NEXT
	NEXT
	GET (290, 100)-STEP(15, 15), Somm%(0)
	GET (310, 100)-STEP(15, 15), Somm%(258)
	GET (270, 100)-STEP(15, 15), Somm%(516)
	GET (200, 100)-STEP(15, 15), Somm%(774)

'*****  Cacciatorpediniere:
	LINE (100, 100)-(131, 115), 0, BF
	LINE (100, 107)-(131, 108), 1, B
	LINE (102, 106)-(130, 109), 1, B
	LINE (106, 105)-(128, 110), 1, B
	LINE (102, 107)-(130, 108), 7, B
	LINE (106, 106)-(128, 109), 7, B
	LINE (114, 106)-(115, 109), 15, B
	LINE (116, 107)-(117, 108), 1, B
	LINE (120, 107)-(122, 108), 7, B
	LINE (124, 107)-(126, 108), 8, B
	LINE (108, 107)-(109, 108), 7, B
	PSET (128, 104), 5
	PSET (128, 111), 5
	PSET (129, 104), 5
	PSET (129, 111), 5
	PSET (130, 103), 5
	PSET (130, 112), 5
	PSET (131, 105), 5
	PSET (131, 110), 5
	PUT (200, 100), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	PUT (270, 100), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT (290, 100), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT (310, 100), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	FOR x% = 0 TO 31
		FOR y% = 0 TO 15
			IF POINT(100 + x%, 100 + y%) <> 0 THEN
				PSET (200 + x%, 100 + y%), POINT(100 + x%, 100 + y%)
				PSET (270 + y%, 100 + x%), POINT(100 + x%, 100 + y%)
				PSET (290 + y%, 100 + (31 - x%)), POINT(100 + x%, 100 + y%)
				PSET (310 + (31 - x%), 100 + y%), POINT(100 + x%, 100 + y%)
			END IF
		NEXT
	NEXT
	GET (290, 100)-STEP(15, 31), Cacc%(0)
	GET (310, 100)-STEP(31, 15), Cacc%(258)
	GET (270, 100)-STEP(15, 31), Cacc%(516)
	GET (200, 100)-STEP(31, 15), Cacc%(774)
	
'*****  Incrociatore:
	LINE (100, 100)-(147, 115), 0, BF
	LINE (100, 107)-(147, 108), 1, B
	LINE (102, 106)-(146, 109), 1, B
	LINE (105, 105)-(145, 110), 1, B
	LINE (109, 104)-(143, 111), 1, B
	LINE (102, 107)-(146, 108), 7, B
	LINE (106, 106)-(145, 109), 7, B
	LINE (110, 105)-(142, 110), 7, B
	LINE (117, 105)-(118, 110), 15, B
	LINE (119, 106)-(121, 109), 1, B
	LINE (120, 107)-(122, 108), 7, B
	LINE (127, 106)-(130, 109), 8, BF
	LINE (131, 106)-(133, 106), 8
	LINE (131, 109)-(133, 109), 8
	LINE (137, 106)-(142, 109), 15, B
	LINE (138, 105)-(141, 110), 15, BF
	LINE (138, 106)-(141, 106), 8
	LINE (138, 109)-(141, 109), 8
	LINE (139, 107)-(140, 108), 8, B
	LINE (110, 107)-(111, 108), 7, B
	LINE (112, 106)-(114, 109), 7, B
	PSET (141, 103), 5
	PSET (141, 112), 5
	PSET (143, 103), 5
	PSET (143, 112), 5
	PSET (144, 104), 5
	PSET (144, 111), 5
	PSET (144, 103), 5
	PSET (144, 112), 5
	PSET (145, 104), 5
	PSET (145, 111), 5
	PSET (145, 102), 5
	PSET (145, 113), 5
	PSET (146, 103), 5
	PSET (146, 112), 5
	PSET (147, 105), 5
	PSET (147, 110), 5
	PSET (147, 103), 5
	PSET (147, 112), 5
	PUT (200, 100), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	PUT (270, 100), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT (290, 100), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT (310, 100), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	FOR x% = 0 TO 47
		FOR y% = 0 TO 15
			IF POINT(100 + x%, 100 + y%) <> 0 THEN
				PSET (200 + x%, 100 + y%), POINT(100 + x%, 100 + y%)
				PSET (270 + y%, 100 + x%), POINT(100 + x%, 100 + y%)
				PSET (290 + y%, 100 + (47 - x%)), POINT(100 + x%, 100 + y%)
				PSET (310 + (47 - x%), 100 + y%), POINT(100 + x%, 100 + y%)
			END IF
		NEXT
	NEXT
	GET (290, 100)-STEP(15, 47), Incr%(0)
	GET (310, 100)-STEP(47, 15), Incr%(258)
	GET (270, 100)-STEP(15, 47), Incr%(516)
	GET (200, 100)-STEP(47, 15), Incr%(774)

'*****  Ammiraglia:
	LINE (100, 100)-(163, 115), 0, BF
	LINE (100, 107)-(102, 108), 1, B
	LINE (102, 106)-(163, 109), 1, BF
	LINE (105, 105)-(162, 110), 1, B
	LINE (109, 104)-(161, 111), 1, B
	LINE (114, 103)-(159, 112), 1, B
	LINE (102, 107)-(162, 108), 7, B
	LINE (106, 106)-(161, 109), 7, B
	LINE (110, 105)-(160, 110), 7, B
	LINE (115, 104)-(158, 111), 7, B
	LINE (120, 105)-(121, 110), 15, B
	LINE (122, 106)-(145, 109), 1, BF
	LINE (125, 107)-(142, 108), 7, B
	LINE (147, 106)-(150, 109), 8, BF
	LINE (151, 106)-(153, 106), 8
	LINE (151, 109)-(153, 109), 8
	LINE (130, 107)-(140, 108), 15, BF
	PSET (157, 102), 5
	PSET (157, 113), 5
	PSET (159, 102), 5
	PSET (159, 113), 5
	PSET (160, 101), 5
	PSET (160, 114), 5
	PSET (160, 103), 5
	PSET (160, 112), 5
	PSET (161, 100), 5
	PSET (161, 115), 5
	PSET (161, 102), 5
	PSET (161, 113), 5
	PSET (162, 103), 5
	PSET (162, 112), 5
	PSET (163, 105), 5
	PSET (163, 110), 5
	PSET (163, 103), 5
	PSET (163, 112), 5
	PUT (200, 100), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	PUT (270, 100), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT (290, 100), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT STEP(0, 16), Onde%, PSET
	PUT (310, 100), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	PUT STEP(16, 0), Onde%, PSET
	FOR x% = 0 TO 63
		FOR y% = 0 TO 15
			IF POINT(100 + x%, 100 + y%) <> 0 THEN
				PSET (200 + x%, 100 + y%), POINT(100 + x%, 100 + y%)
				PSET (270 + y%, 100 + x%), POINT(100 + x%, 100 + y%)
				PSET (290 + y%, 100 + (63 - x%)), POINT(100 + x%, 100 + y%)
				PSET (310 + (63 - x%), 100 + y%), POINT(100 + x%, 100 + y%)
			END IF
		NEXT
	NEXT
	GET (290, 100)-STEP(15, 63), Ammi%(0)
	GET (310, 100)-STEP(63, 15), Ammi%(258)
	GET (270, 100)-STEP(15, 63), Ammi%(516)
	GET (200, 100)-STEP(63, 15), Ammi%(774)

	OPEN "objects2.bob" FOR OUTPUT AS #1
		PRINT #1, "ONDE";
		ScriviOggetto Onde%(), 66, 1
		PRINT #1, "MIRI";
		ScriviOggetto Mirino%(), 66, 1
		PRINT #1, "MINA";
		ScriviOggetto Mina%(), 66, 1
		PRINT #1, "ESPL";
		ScriviOggetto Espl%(), 660, 1
		PRINT #1, "AMMI";
		ScriviOggetto Ammi%(), 1032, 1
		PRINT #1, "INCR";
		ScriviOggetto Incr%(), 1032, 1
		PRINT #1, "CACC";
		ScriviOggetto Cacc%(), 1032, 1
		PRINT #1, "SOMM";
		ScriviOggetto Somm%(), 1032, 1
	CLOSE #1
END SUB

DEFINT A-Z
SUB LeggiOggetti
	OPEN "objects.bob" FOR INPUT AS #1
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
	Lungh$ = INPUT$(2, 1)
	Lungh% = VAL("&h" + HEX$(ASC(LEFT$(Lungh$, 1))) + HEX$(ASC(RIGHT$(Lungh$, 1))))
	FOR i = 0 TO Lungh%
		Dato$ = INPUT$(2, 1)
		msd$ = HEX$(ASC(LEFT$(Dato$, 1)))
		lsd$ = HEX$(ASC(RIGHT$(Dato$, 1)))
		Vett%(i) = VAL("&h" + RIGHT$("0" + msd$, 2) + RIGHT$("0" + lsd$, 2))
	NEXT
END SUB

DEFSNG A-Z
SUB ScriviOggetto (Vett%(), n.Elem%, Num.File%)
	ne$ = RIGHT$("000" + HEX$(n.Elem%), 4)
 ' Dato$ = CHR$(VAL("&H" + LEFT$(ne$, 2))) + CHR$(VAL("&H" + RIGHT$(ne$, 2)))
	PRINT #Num.File%, ne$;
	FOR i = 0 TO n.Elem%
		a$ = RIGHT$("000" + HEX$(Vett%(i)), 4)
	 ' Dato$ = CHR$(VAL("&h" + LEFT$(a$, 2))) + CHR$(VAL("&h" + RIGHT$(a$, 2)))
		PRINT #Num.File%, a$;
	NEXT
END SUB

