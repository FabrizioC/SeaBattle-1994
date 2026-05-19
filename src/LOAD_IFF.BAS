DEFINT A-Z
CLS
OPEN "a:\sb\worldmap.IFF" FOR INPUT AS #1
DO WHILE NOT (EOF(1))
	Letto$ = INPUT$(4, 1)
	SELECT CASE Letto$
	CASE "DPPS"
		PRINT "Trovato -> DPPS"
		LunFile$ = ""
		FOR i = 0 TO 3
			LunFile$ = LunFile$ + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		NEXT
		PRINT TAB(5); " LunghezzaBlocco: "; LunFile$
		FOR i = 1 TO VAL("&H" + LunFile$)
			Byte$ = INPUT$(1, 1)
		NEXT
	CASE "CRNG"
		PRINT "Trovato -> CRNG"
		LunFile$ = ""
		FOR i = 0 TO 3
			LunFile$ = LunFile$ + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		NEXT
		PRINT TAB(5); " LunghezzaBlocco: "; LunFile$
		FOR i = 1 TO VAL("&H" + LunFile$)
			Byte$ = INPUT$(1, 1)
		NEXT
	CASE "CAMG"
		PRINT "Trovato -> CAMG"
		LunFile$ = ""
		FOR i = 0 TO 3
			LunFile$ = LunFile$ + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		NEXT
		PRINT TAB(5); " LunghezzaBlocco: "; LunFile$
		FOR i = 1 TO VAL("&H" + LunFile$)
			Byte$ = INPUT$(1, 1)
		NEXT
	CASE "FORM"
		PRINT "Trovato -> FORM"
		LunFile$ = ""
		FOR i = 0 TO 3
			LunFile$ = LunFile$ + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		NEXT
		PRINT TAB(5); "   LunghezzaFile: "; LunFile$
	CASE "ILBM"
		PRINT "Trovato -> ILBM"
	CASE "BMHD"
		PRINT "Trovato -> BMHD"
		NumEl$ = ""
		FOR i = 0 TO 3
			NumEl$ = NumEl$ + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		NEXT
		w$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2) + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		Wid% = VAL("&H" + w$)
		h$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2) + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		Height% = VAL("&H" + h$)
		x$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2) + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		y$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2) + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		nPlanes$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		nPlanes% = VAL("&H" + nPlanes$)
		Masking$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		Compression$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		Compression% = VAL("&H" + Compression$)
		Pad1$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		transparentColor$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2) + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		xAspect$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		yAspect$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		pageWidth$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2) + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		pageHeight$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2) + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		PRINT TAB(5); "  NumeroElementi: "; NumEl$
		PRINT TAB(5); "               w: "; w$;
		PRINT TAB(30); "               h: "; h$
		PRINT TAB(5); "               x: "; x$;
		PRINT TAB(30); "               y: "; y$
		PRINT TAB(5); "         nPlanes: "; nPlanes$
		PRINT TAB(5); "         Masking: "; Masking$
		PRINT TAB(5); "     Compression: "; Compression$
		PRINT TAB(5); "            Pad1: "; Pad1$
		PRINT TAB(5); "transparentColor: "; transparentColor$
		PRINT TAB(5); "         xAspect: "; xAspect$;
		PRINT TAB(30); "         yAspect: "; yAspect$
		PRINT TAB(5); "       pageWidth: "; pageWidth$;
		PRINT TAB(30); "      pageHeight: "; pageHeight$
		SLEEP
	CASE "CMAP"
		PRINT "Trovato -> CMAP"
		NumEl$ = ""
		FOR i = 0 TO 3
			NumEl$ = NumEl$ + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		NEXT
		NumCol% = VAL("&H" + NumEl$) / 3
		PRINT TAB(5); "  NumeroElementi: "; NumEl$; "  ("; NumCol%; "Colori)"
		DIM PalCol&(NumCol% - 1)
		FOR NC = 0 TO NumCol% - 1
			R$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1)) / 4.1), 2)
			G$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1)) / 4.1), 2)
			B$ = RIGHT$("0" + HEX$(ASC(INPUT$(1, 1)) / 4.1), 2)
			PalCol&(NC) = VAL("&H" + B$ + G$ + R$)
			PRINT TAB(5); "           R G B: "; R$; "."; G$; "."; B$
		NEXT
		SLEEP
	CASE "CAMG"
		PRINT "Trovato -> CAMG"
		ViewMode$ = ""
		FOR i = 0 TO 7
			ViewMode$ = ViewMode$ + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		NEXT
		PRINT TAB(5); "        ViewMode: "; ViewMode$
	CASE "BODY"
		PRINT "Trovato -> BODY"
		NumData$ = ""
		FOR i = 0 TO 3
			NumData$ = NumData$ + RIGHT$("0" + HEX$(ASC(INPUT$(1, 1))), 2)
		NEXT
		nCol% = Wid% / 8
		PRINT TAB(5); "     NumByteData: "; NumData$
		NumData& = VAL("&H" + NumData$)
		DIM DatB%(nPlanes%, nCol% + 1)
		Bl& = 0
		IF Compression% = 1 THEN
			SCREEN 13
'      WINDOW SCREEN (0, 0)-(Wid%, Height%)
			FOR NC = 0 TO NumCol% - 1
				PALETTE NC, PalCol&(NC)
			NEXT
'      PALETTE 1, &H2F2F2F
'      COLOR 1
			FOR Linea = 1 TO Height%
'        PRINT "Linea:"; Linea,
				FOR BitPlane = 0 TO nPlanes% - 1
					Colonna = 0
					DO
						N = ASC(INPUT$(1, 1))
						Bl& = Bl& + 1
						IF N < 128 THEN
							FOR i = 0 TO N
								DatB%(BitPlane, Colonna) = ASC(INPUT$(1, 1))
								Bl& = Bl& + 1
								Colonna = Colonna + 1
							NEXT
						ELSE
							DatB%(BitPlane, Colonna) = ASC(INPUT$(1, 1))
							Bl& = Bl& + 1
							FOR i = 1 TO 256 - N
								DatB%(BitPlane, Colonna + i) = DatB%(BitPlane, Colonna)
							NEXT
							Colonna = Colonna + 256 - N + 1
						END IF
					LOOP UNTIL Colonna = nCol%
				NEXT
				yscr = Linea - 1
				FOR Colonna = 0 TO nCol% - 1
					xscr = Colonna * 8
					IF xscr < 640 AND Linea < 480 THEN
						FOR i = 0 TO 7
							iand = 2 ^ i
							Colore% = 1 * SGN(DatB%(0, Colonna) AND iand)
							Colore% = Colore% + 2 * SGN(DatB%(1, Colonna) AND iand)
							Colore% = Colore% + 4 * SGN(DatB%(2, Colonna) AND iand)
							Colore% = Colore% + 8 * SGN(DatB%(3, Colonna) AND iand)
							IF nPlane% > 4 THEN
								Colore% = Colore% + 16 * SGN(DatB%(4, Colonna) AND iand)
							END IF
							PSET (xscr + 8 - i, yscr), Colore%
						NEXT
					END IF
				NEXT
'        PRINT Bl
			NEXT
		ELSE
		END IF
		EXIT DO
	END SELECT
LOOP

CLOSE #1
							

