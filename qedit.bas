DEFINT A-Z
DECLARE SUB tasten (t$)
DECLARE SUB intro ()
DECLARE SUB help ()
DECLARE SUB altshiftstrg ()
DECLARE SUB LP (x, y, c, t$) ' stellt Zahlen und Buchstaben in 5*5 Pixel dar
DECLARE SUB clg ()
DECLARE SUB show ()
DECLARE SUB ZAHL (x, y, c, Lrz, z)   ' ntzlich fr ver„nderliche Zahlen
DECLARE SUB menu ()
DECLARE SUB palC ()
DECLARE SUB showc ()
DECLARE SUB clkey ()                 ' l”scht den Tastenpuffer
DECLARE SUB grafik ()
DECLARE SUB cursor ()
DECLARE SUB message (m$)
DECLARE SUB initshow ()
DECLARE SUB cursorEx ()
DECLARE SUB savecolour ()
DECLARE SUB regenerate ()
DECLARE SUB zeile2 (z2$)
DECLARE FUNCTION datei$ ()
DECLARE FUNCTION speichern ()
DECLARE FUNCTION zeile$ (x, y, c, zl$)

DIM SHARED B(75, 50)  AS STRING * 1  ' Bilddatenfeld
DIM SHARED g(300, 200) AS STRING * 1 ' Umgerechnetes Datenfeld (mit Zoomfaktor)

DIM SHARED xc, yc, xk, yk, xg, yg, xs, ys, ccc, z
DIM SHARED ink  AS STRING, f AS STRING
DIM SHARED za AS STRING, ze AS STRING, zz AS STRING, ee AS STRING
DIM SHARED convert                   ' Farbumwandlung in 16 Farben

DIM SHARED cc(9) AS STRING * 1       ' benutzerdefinierte/gespeicherte Farben
DIM SHARED ct(23, 10) AS STRING * 1  ' Farbtabelle

DIM SHARED c1 AS STRING              ' Variablen zum Zwischenspeichern der
DIM SHARED c2 AS STRING              ' Farben (unter dem Cursor)
DIM SHARED c3 AS STRING
DIM SHARED c4 AS STRING
 
pfad$ = ".\qed\ " ' Pfad zu den Dateien

ON ERROR GOTO fehler

1
SCREEN 13
CLS

10 '* ini-Datei einlesen
  OPEN "qedit.ini" FOR INPUT AS #1
    LINE INPUT #1, t$: xg = VAL(t$)
    LINE INPUT #1, t$: yg = VAL(t$)
    LINE INPUT #1, t$
      initshow
      FOR x = 0 TO 23
        FOR y = 0 TO 10
          IF ct(x, y) = CHR$(VAL(t$)) THEN xc = x: yc = y: ccc = ASC(ct(xc, yc)): x = 23: y = 10
          NEXT
        NEXT
    FOR i = 0 TO 9
      LINE INPUT #1, t$: cc(i) = CHR$(VAL(t$))
      NEXT
    CLOSE

z = 1
20
regenerate

DO '* Hauptprogramm
ink = INKEY$
 
  SELECT CASE ink ' diese
    CASE CHR$(27)
      EXIT DO
    CASE "”"
      clg
      GOSUB 40
    CASE "o"
      clg
      GOSUB 40
    CASE "s"
      clg
      GOSUB 45
    END SELECT
  tasten (ink)
 
  ccc = ASC(ct(xc, yc))
  palC
  showc
  cursor
  grafik
  cursorEx
LOOP

cursorEx
30 '* ini-Datei aktualisieren
  OPEN "qedit.ini" FOR OUTPUT AS #1
    PRINT #1, xg
    PRINT #1, yg
    PRINT #1, ccc
    FOR i = 0 TO 9
      PRINT #1, ASC(cc(i))
      NEXT
    CLOSE

SYSTEM


40 ' Datei ”ffnen
  message "datei ”ffnen:"
  d$ = datei$
  IF (d$ = "") THEN RETURN
  d$ = d$ + ".qed"
  OPEN pfad$ + d$ FOR INPUT AS #1
  LINE INPUT #1, t$: xg = VAL(t$)
  LINE INPUT #1, t$: yg = VAL(t$)
  FOR y = 0 TO yg
    FOR x = 0 TO xg
      LINE INPUT #1, t$
      g(x, y) = CHR$(VAL(t$))
      IF EOF(1) THEN x = xg: y = yg ' notfalls bei Dateiende abbrechen
      NEXT
    NEXT
  CLOSE
  RETURN

45 ' Datei speichern
  s = speichern
  IF s > 0 THEN
    message "dateiname eing. :"
    d$ = datei$
    IF d$ = "" THEN regenerate: RETURN
    d$ = d$ + ee
   
    IF s = 1 THEN          ' * als qed-Datei
      OPEN pfad$ + d$ FOR OUTPUT AS #1
      PRINT #1, xg
      PRINT #1, yg
      FOR y = 0 TO yg
        FOR x = 0 TO xg
          PRINT #1, ASC(g(x, y))
          NEXT
        NEXT
    ELSE                   ' * zB. als QBasic- oder Pascal-Datei, etc. ...
      OPEN pfad$ + d$ FOR OUTPUT AS #1
      zeile2 (za)
      FOR y = 0 TO yg
        FOR x = 0 TO xg
          PRINT #1, zeile$(x, y, ASC(g(x, y)), ze)
          NEXT
        NEXT
      zeile2 (zz)
      END IF

    CLOSE
    END IF
  CLS
  regenerate
  RETURN


50 ' * neue ini-Datei erstellen
  OPEN "qedit.ini" FOR OUTPUT AS #1
    PRINT #1, 50
    PRINT #1, 20
    PRINT #1, 15
    FOR i = 0 TO 9
      PRINT #1, 0
      NEXT
    CLOSE
    RUN

fehler:
IF ERL = 10 THEN RESUME 50: intro
SELECT CASE ERR
  CASE 52:   message "Dateiname- oder Nummer unzul„ssig": SLEEP: message ""
  CASE 53:   message "Datei nicht gefunden": SLEEP: message ""
             RESUME 1
  CASE 58:   message "Datei existiert bereits"
  CASE 61:   message "Datentr„ger voll"
  CASE 64:   message "Ungltiger Dateiname"
  CASE 70:   message "Zugriff verweigert"
  CASE 71:   message "Datentr„ger nicht bereit"
  CASE 72:   message "Datentr„gerfehler"
  CASE 76:   message "Pfad nicht gefunden"
  CASE ELSE: message "Es trat Fehler Nr." + STR$(ERR) + " auf!"
  END SELECT
clkey: SLEEP: message ""
RESUME NEXT

SUB benutzerdef
END SUB

SUB clg '* l”scht die Grafikausgabe
LINE (100, 28)-(309, 174), 25, B
LINE (101, 29)-(308, 173), 0, BF
END SUB

SUB clkey
DO
LOOP UNTIL INKEY$ = ""
END SUB

SUB cursor
IF (ys - 1 >= 0) THEN c1 = g(xs, ys - 1): IF (INT(TIMER * 5) MOD 2) THEN g(xs, ys - 1) = CHR$(10)
IF (ys + 1 <= yg) THEN c2 = g(xs, ys + 1): IF (INT(TIMER * 5) MOD 2) THEN g(xs, ys + 1) = CHR$(10)
IF (xs - 1 >= 0) THEN c3 = g(xs - 1, ys): IF (INT(TIMER * 5) MOD 2) THEN g(xs - 1, ys) = CHR$(10)
IF (xs + 1 <= xg) THEN c4 = g(xs + 1, ys): IF (INT(TIMER * 5) MOD 2) THEN g(xs + 1, ys) = CHR$(10)
END SUB

SUB cursorEx
IF (ys - 1 >= 0) THEN g(xs, ys - 1) = c1
IF (ys + 1 <= yg) THEN g(xs, ys + 1) = c2
IF (xs - 1 >= 0) THEN g(xs - 1, ys) = c3
IF (xs + 1 <= xg) THEN g(xs + 1, ys) = c4
END SUB

FUNCTION datei$
  DO
    SLEEP
    LINE (95, 190)-(308, 198), 0, BF
    ink = INKEY$
    IF (ink <> CHR$(8)) AND (ink <> CHR$(13)) AND (ink <> CHR$(27)) AND (ink <> "/") AND (ink <> "\") AND (ink <> ":") AND (ink <> "*") AND (ink <> "?") AND (ink <> CHR$(34)) AND (ink <> "<") AND (ink <> ">") AND (ink <> "|") THEN
      t$ = t$ + ink': LOCATE 1: PRINT ink
      END IF
    IF (ink = CHR$(8)) AND (t$ > "") THEN t$ = LEFT$(t$, LEN(t$) - 1)
    IF (ink = CHR$(27)) THEN EXIT DO
    IF (ink = CHR$(13)) THEN EXIT DO
    IF (LEN(t$) = 9) THEN t$ = LEFT$(t$, LEN(t$) - 1)
    LP 100, 196, 30, t$
  LOOP
  message ""
  ink = ""
  datei$ = t$
END FUNCTION

SUB grafik
FOR y = 0 TO yg
  FOR x = 0 TO xg
    LINE (x * z + 101, y * z + 29)-(x * z + 101 + z - 1, y * z + 29 + z - 1), ASC(g(x, y)), BF
    NEXT
  NEXT
END SUB

SUB help

  CLS : clkey
  LINE (0, 6)-(319, 194), 26, B
  LINE (30, 0)-(171, 10), 0, BF
  LINE (30, 0)-(171, 10), 28, B
  LP 35, 7, 30, "stratysoft grafikeditor 2.0"
  LP 5, 17, 26, "( ": LP 0, 0, 29, "Hilfeauswahl": LP 0, 0, 26, " )"
  LP 30, 27, 31, " 1":  LP 50, -1, 28, "- allgemein"
  LP 30, 0, 31, " 2":   LP 50, -1, 28, "- Tasten"
  LP 30, 0, 31, " 3":   LP 50, -1, 28, "- Bugbehebung"
  LP 30, 0, 31, " 4":   LP 50, -1, 28, "- Sonstiges"
  LP 30, 0, 31, "ESC": LP 50, -1, 28, "- Ende"
  LP 30, 0, 31, ""
  LP 25, 0, 31, "email:": LP 0, 0, 29, "  strathausen (a) gmx.de"

DO
  ink = INKEY$
LOOP UNTIL ((ink >= "1") AND (ink <= "4")) OR (ink = CHR$(27))

IF (ink = CHR$(27)) THEN GOTO he

  CLS
  LINE (0, 6)-(319, 194), 26, B
  LINE (30, 0)-(171, 10), 0, BF
  LINE (30, 0)-(171, 10), 28, B
  LP 35, 7, 30, "stratysoft grafikeditor 2.0"

h = VAL(ink)
ON h GOTO h1, h2, h3, h4

h1:
  LP 5, 17, 26, "( ": LP 0, 0, 29, "Hilfe": LP 0, 0, 26, " )"
  LP 30, 27, 31, "wozu ist das programm gut?"
  LP 5, 40, 27, "mit diesem programmm k”nnen sie grafiken erstellen und diese"
  LP 5, 0, 27, "zb. als qbasic- oder pascal-programme abspeichern."
  LP 5, 0, 27, "(sehr ntzlich, um programme abwechslungsreich zu gestalten)"
  LP 5, 0, 27, ""
  LP 5, 0, 27, "ausserdem k”nnen sie diese bilder auch als .qed-datei abspeichern,"
  LP 5, 0, 27, "um sie sp„ter mit q-edit weiter zu bearbeiten."
  LP 5, 0, 27, "in einer solchen datei findet man eine reihe von zahlen."
  LP 5, 0, 27, "die ersten beiden zahlen geben die breite und h”he"
  LP 5, 0, 27, "des bildes die weiteren farbwerte an."
  LP 5, 0, 27, ""
  LP 5, 0, 27, "es gibt auch die m”glichkeit benutzerdefinierte dateien zu er-"
  LP 5, 0, 27, "stellen (zb. fr andere programmiersprachen)."
  LP 5, 0, 27, ""
  LP 5, 0, 29, "viel spaá!"
  SLEEP
  GOTO he

h2:
  LP 5, 17, 26, "( ": LP 0, 0, 30, "tastenbelegung": LP 0, 0, 26, " )"
  LP 30, 0, 28, ""
  LP 30, 0, 28, "shift-left":   LP 95, -1, 31, " ->": LP 0, 0, 28, " Malen"
  LP 30, 0, 28, "space/leer":   LP 95, -1, 31, " ->": LP 0, 0, 28, " Malen"
  LP 30, 0, 28, "strg/crtl":    LP 95, -1, 31, " ->": LP 0, 0, 28, " Farbpipette"
  LP 30, 0, 28, "enter + zahl": LP 95, -1, 31, " ->": LP 0, 0, 28, " Farbe speichern (Zellen 0-9)"
  LP 30, 0, 28, "alt + zahl":   LP 95, -1, 31, " ->": LP 0, 0, 28, " gespeicherte Farbe abrufen (Zellen 0-9)"
  LP 30, 0, 28, "cursortasten": LP 95, -1, 31, " ->": LP 0, 0, 28, " Cursor auf dem Bild bewegen"
  LP 30, 0, 28, "z":            LP 95, -1, 31, " ->": LP 0, 0, 28, " Zoom (1:1, 2:1, 4:1)"
  LP 30, 0, 28, ""
  LP 30, 0, 28, ""
  LP 5, 0, 26, "( ": LP 0, 0, 30, "hotkeys": LP 0, 0, 26, " )"
  LP 30, 0, 28, ""
  LP 30, 0, 28, "f1":           LP 95, -1, 31, " ->": LP 0, 0, 28, " hilfe"
  LP 30, 0, 28, "Z":            LP 95, -1, 31, " ->": LP 0, 0, 28, " Zoom (1:1, 2:1, 4:1)"
  LP 30, 0, 28, "Z":            LP 95, -1, 31, " ->": LP 0, 0, 28, " Zoom (1:1, 2:1, 4:1)"
  LP 30, 0, 28, ""
  SLEEP
  GOTO he

h3:
  LP 5, 17, 26, "( ": LP 0, 0, 30, "m”gliche bugs": LP 0, 0, 26, " )"
  LP 20, 0, 28, ""
  LP 20, 0, 30, "pfad nicht gefunden"
  LP 20, 0, 28, ""
  LP 20, 0, 28, "gegen diesen bug kann man folgendes tun:"
  LP 20, 0, 28, "1. das verzeichnis '.\qed' muss existieren."
  LP 20, 0, 28, ""
  LP 20, 0, 28, ""
  LP 20, 0, 28, ""
  SLEEP
  GOTO he

h4:
  SLEEP

he:
  CLS
  regenerate

END SUB

SUB initshow
FOR i = 0 TO 15
  ct(i, 0) = CHR$(i)
  ct(i, 1) = CHR$(i + 16)
  NEXT
FOR j = 0 TO 8
  FOR i = 0 TO 23
    ct(i, j + 2) = CHR$(i + 32 + j * 24)
    NEXT
  NEXT

END SUB

SUB intro
END SUB

SUB LP (x, y, c, t$) ' v2.1
IF x = 0 AND y = 0 THEN x = xk: y = yk
IF y = 0 THEN y = yk + 7
IF y < 0 THEN y = yk
t$ = UCASE$(t$)

FOR p = 1 TO LEN(t$)
SELECT CASE MID$(t$, p, 1)
  CASE "A" TO "Z": pa = ASC(MID$(t$, p, 1)) - 64
  CASE "Ž", "„":   pa = 27
  CASE "™", "”":   pa = 28
  CASE "š", "":   pa = 29
  CASE "á":        pa = 30
  CASE "(":        pa = 31
  CASE ")":        pa = 32
  CASE "[":        pa = 33
  CASE "]":        pa = 34
  CASE "<":        pa = 35
  CASE ">":        pa = 36
  CASE " ":        pa = 37
  CASE ".":        pa = 38
  CASE ":":        pa = 39
  CASE ",":        pa = 40
  CASE ";":        pa = 41
  CASE "=":        pa = 42
  CASE "-":        pa = 43
  CASE "+":        pa = 44
  CASE "!":        pa = 45
  CASE CHR$(34):   pa = 46
  CASE "?":        pa = 47
  CASE "0" TO "9": pa = ASC(MID$(t$, p, 1))
  CASE "'":        pa = 58
  CASE "\":        pa = 59
  CASE "/":        pa = 60
  END SELECT

ON pa GOTO Ap, Bp, Cp, Dp, Ep, Fp, Gp, Hp, Ip, Jp, Kp, LP, Mp, Np, Op, pp, Qp, Rp, Sp, Tp, Up, Vp, Wp, Xp, Yp, Zp, AEp, OEp, UEp, SZp, Kla1, Klz1, Kla2, Klz2, Kla3, Klz3, lr, Pnkt, DPnkt, Komma, Smk, Glz, Mns, Pls, Ausr, Anf, frz, p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, aps, bsl, slh

GOTO pe

Ap:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
Bp:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  x = x + 5
  GOTO pe
Cp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
Dp:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  x = x + 5
  GOTO pe
Ep:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 2, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
Fp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 2, y), c
  x = x + 4
  GOTO pe
Gp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
Hp:
  PSET (x + 2, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
Ip:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 3, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  x = x + 4
  GOTO pe
Jp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 5, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 5, y - 2), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  x = x + 5
  GOTO pe
Kp:
  PSET (x + 2, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
LP:
  PSET (x + 2, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
Mp:
  PSET (x + 3, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 6, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 6, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 6, y), c
  x = x + 6
  GOTO pe
Np:
  PSET (x + 2, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
Op:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  x = x + 5
  GOTO pe
pp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 2, y), c
  x = x + 5
  GOTO pe
Qp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 4, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
Rp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 4, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
Sp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  x = x + 5
  GOTO pe
Tp:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 3, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 3, y), c
  x = x + 5
  GOTO pe
Up:
  PSET (x + 2, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  x = x + 5
  GOTO pe
Vp:
  PSET (x + 2, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 4, y - 1), c
  PSET (x + 3, y), c
  x = x + 5
  GOTO pe
Wp:
  PSET (x + 2, y - 4), c
  PSET (x + 6, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 6, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 6, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 4, y - 1), c
  PSET (x + 6, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 5, y), c
  x = x + 6
  GOTO pe
Xp:
  PSET (x + 2, y - 4), c
  PSET (x + 6, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 4, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 6, y), c
  x = x + 6
  GOTO pe
Yp:
  PSET (x + 2, y - 4), c
  PSET (x + 6, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 4, y - 2), c
  PSET (x + 4, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  PSET (x + 5, y), c
  x = x + 6
  GOTO pe
Zp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 6, y - 4), c
  PSET (x + 5, y - 3), c
  PSET (x + 4, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  PSET (x + 5, y), c
  PSET (x + 6, y), c
  x = x + 6
  GOTO pe
AEp:
  PSET (x + 2, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 3, y - 1), c
  PSET (x + 4, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 5, y), c
  x = x + 5
  GOTO pe
OEp:
  PSET (x + 2, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  x = x + 5
  GOTO pe
UEp:
  PSET (x + 2, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 2, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 4, y), c
  x = x + 5
  GOTO pe
SZp:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 4, y), c
  x = x + 5
  GOTO pe
Kla1:
  PSET (x + 3, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 3, y), c
  x = x + 3
  GOTO pe
Klz1:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 3, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 2, y), c
  x = x + 3
  GOTO pe
Kla2:
  PSET (x + 3, y - 4), c
  PSET (x + 2, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 2, y), c
  PSET (x + 3, y), c
  x = x + 3
  GOTO pe
Klz2:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 3, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 3, y), c
  PSET (x + 2, y), c
  x = x + 3
  GOTO pe
Kla3:
  PSET (x + 4, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 4, y), c
  x = x + 4
  GOTO pe
Klz3:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 4, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 2, y), c
  x = x + 4
  GOTO pe
lr:
  x = x + 4
  GOTO pe
Pnkt:
  PSET (x + 2, y), c
  x = x + 3
  GOTO pe
DPnkt:
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 1), c
  x = x + 3
  GOTO pe
Komma:
  PSET (x + 3, y), c
  PSET (x + 2, y + 1), c
  x = x + 3
  GOTO pe
Smk:
  PSET (x + 3, y - 2), c
  PSET (x + 3, y), c
  PSET (x + 2, y + 1), c
  x = x + 3
  GOTO pe
Glz:
  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 2, y - 1), c
  PSET (x + 3, y - 1), c
  PSET (x + 4, y - 1), c
  x = x + 4
  GOTO pe
Mns:
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  x = x + 4
  GOTO pe
Pls:
  PSET (x + 3, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 3, y - 1), c
  x = x + 4
  GOTO pe
Ausr:
  PSET (x + 2, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 2, y), c
  x = x + 2
  GOTO pe
Anf:
  PSET (x + 2, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 4, y - 3), c
  x = x + 4
  GOTO pe
frz:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 3), c
  PSET (x + 3, y - 2), c
  PSET (x + 3, y), c
  x = x + 4
  GOTO pe
p0:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  x = x + 5
  GOTO pe
p1:
  PSET (x + 4, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 4, y - 1), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), c
  x = x + 5
  GOTO pe
p2:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 4, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 2, y - 0), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), c
  x = x + 5
  GOTO pe
p3:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  x = x + 5
  GOTO pe
p4:
  PSET (x + 4, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 3, y - 1), c
  PSET (x + 4, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 4, y - 0), c
  x = x + 5
  GOTO pe
p5:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y - 0), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  x = x + 5
  GOTO pe
p6:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  x = x + 5
  GOTO pe
p7:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), c
  PSET (x + 5, y - 3), c
  PSET (x + 4, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 3, y - 0), c
  x = x + 5
  GOTO pe
p8:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 2, y - 1), c
  PSET (x + 5, y - 1), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  x = x + 5
  GOTO pe
p9:
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 2, y - 3), c
  PSET (x + 5, y - 3), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), c
  PSET (x + 5, y - 1), c
  PSET (x + 2, y - 0), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  x = x + 5
  GOTO pe
aps:
  PSET (x + 3, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 2, y - 2), c
  x = x + 3
  GOTO pe
bsl:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 3), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 1), c
  PSET (x + 6, y), c
  x = x + 6
  GOTO pe
slh:
  PSET (x + 6, y - 4), c
  PSET (x + 5, y - 3), c
  PSET (x + 4, y - 2), c
  PSET (x + 3, y - 1), c
  PSET (x + 2, y), c
  x = x + 6
'  GOTO pe
 
pe:
NEXT
xk = x: yk = y
END SUB ' tool by StratySoft, Philipp Strathausen

SUB menu

LINE (0, 6)-(319, 194), 26, B
LINE (30, 0)-(171, 10), 0, BF
LINE (30, 0)-(171, 10), 28, B
LP 35, 7, 30, "stratysoft grafikeditor 2.0"

LP 5, 17, 26, "( "
LP 0, 0, 31, "s": LP 0, 0, 26, "peichern": LP 0, 0, 26, " - "
LP 0, 0, 31, "”": LP 0, 0, 26, "ffnen":    LP 0, 0, 26, " - "
LP 0, 0, 31, "g": LP 0, 0, 26, "”áe":      LP 0, 0, 26, " - "
LP 0, 0, 31, "h": LP 0, 0, 26, "ilfe":     LP 0, 0, 26, " )"

LINE (10, 89)-(42, 160), 25, B
LINE (8, 160)-(69, 174), 25, B
LP 8, 166, 26, "gespeicherte"
LP 8, 172, 26, "farben"

LINE (8, 76)-(83, 84), 25, B
LINE (48, 84)-(93, 101), 25, B
LP 8, 82, 27, "position:"
LP 49, 91, 27, "x ="
LP 49, 0, 27, "y ="
ZAHL 69, 91, 27, 4, 0
ZAHL 69, 0, 27, 4, 0
END SUB

SUB message (m$)

IF m$ > "" THEN
  LP 15, 196, 30, m$
ELSE
  LP 10, 187, 28, "message-line"
  LINE (8, 189)-(72, 180), 28, B
  LINE (10, 189)-(309, 199), 28, B
  LINE (11, 190)-(308, 198), 0, BF
END IF

END SUB

SUB palC '* abgespeicherte Farben
FOR i = 0 TO 9
  ZAHL 10, 95 + i * 7, 30, 1, i
  LINE (18, 92 + i * 7)-(21, 94 + i * 7), ASC(cc(i)), BF
  LINE (17, 91 + i * 7)-(22, 95 + i * 7), 29, B
  ZAHL 25, 95 + i * 7, 30, 4, ASC(cc(i))
  NEXT
END SUB

SUB regenerate ' h„ufig aufgerufene Unterprogramme

  CLS
  menu
  initshow
  show
  message ""
  cursor
  cursorEx
  clg
  clkey

END SUB

SUB savecolour '* Farbwerte speichern
message "unter welcher nr. farbe speichern? ("
ZAHL 0, 0, 31, 1, 0
ZAHL 0, 0, 31, 1, -9
LP 0, 0, 31, ")"
SLEEP
ink = INKEY$
SELECT CASE ink
  CASE "0" TO "9": cc(VAL(ink)) = CHR$(ccc)
  END SELECT
message ""
END SUB

SUB show '* Farbpalette darstellen
LINE (8, 28)-(83, 64), 25, B
LINE (6, 64)-(93, 72), 25, B
LP 6, 70, 26, "farbe nr."
LINE (68, 65)-(68, 71), 25
FOR x = 0 TO 23
  FOR y = 0 TO 10
      LINE (x * 3 + 10, y * 3 + 30)-(x * 3 + 12, y * 3 + 32), ASC(ct(x, y)), BF
    NEXT
  NEXT
END SUB

SUB showc '* Position des Farb-Cursors darstellen
FOR x = 0 TO 23
  FOR y = 0 TO 10
    IF (x = xc) AND (y = yc) THEN
      PSET (x * 3 + 11, y * 3 + 31), -(INT(TIMER * 10) MOD 2 = 0) * 10 - (INT(TIMER * 10) MOD 2 = 1) * 1
    ELSE
      PSET (x * 3 + 11, y * 3 + 31), ASC(ct(x, y))
      END IF
    NEXT
  NEXT
ZAHL 50, 70, 27, 4, ccc
LINE (70, 66)-(91, 70), ccc, BF
END SUB

FUNCTION speichern ' * bestimmt, wie eine Grafik abgespeichert wird
CLS

LINE (0, 6)-(319, 194), 26, B
LINE (30, 0)-(171, 10), 0, BF
LINE (30, 0)-(171, 10), 28, B
LP 35, 7, 30, "stratysoft grafikeditor 2.0"
LP 5, 17, 26, "( ": LP 0, 0, 29, "bild speichern": LP 0, 0, 26, " )"

message ""

  LP 30, 27, 31, " 1":  LP 0, 0, 28, " als qed-datei"
  LP 30, 0, 31, " 2":   LP 0, 0, 28, " als qbasic-programm"
  LP 30, 0, 31, " 3":   LP 0, 0, 28, " als pascal-programm"
  LP 30, 0, 31, " 4":   LP 0, 0, 28, " benutzerdefiniert"
  LP 30, 0, 31, " ESC": LP 0, 0, 28, " abbrechen"

DO
  ink = INKEY$
  LOOP UNTIL (ink = CHR$(27)) OR ((ink >= "1") AND (ink <= "4"))

IF (ink = CHR$(27)) THEN s = -1: GOTO se

s = VAL(ink)
ee = ""      'Dateiendung
convert = 0  'in 16 Farben Konvertieren

ON s GOTO se, s2, s3, s4

s2:
  za = "screen 13"         'Text vor dem Hauptprogramm
  ze = "pset(|x|,|y|),|c|" 'Hauptprogramm
  zz = "system"            'Text nach dem Hauptprogramm
  ee = ".bas"              'Dateiendung
  GOTO se

s3:  
  za = "Uses|  Graph;||Var|  Gd, Gm, x, y : Integer;||Begin|  Gd:=Detect;|  InitGraph (Gd,Gm,'C:\tp\bgi\');| y:=50;| x:=50"
  ze = "  putPixel(x +|x|,y +|y|,|c|);"
  zz = "  ReadLn;|  CloseGraph;|end."
  ee = ".pas"
  convert = 1
  GOTO se

s4:

se:
ze = ze + "  "
IF ee = "" THEN ee = ".qed"
speichern = s

END FUNCTION

SUB tasten (t$)
 
  SELECT CASE t$
    CASE "8"
      yc = yc - 1
      IF (yc <= 1) AND (xc >= 16) THEN yc = 10
      IF (yc <= -1) THEN yc = 10
    CASE "4"
      xc = xc - 1
      IF (yc <= 1) AND (xc <= -1) THEN xc = 15
      IF (xc <= -1) THEN xc = 23
    CASE "6"
      xc = xc + 1
      IF (yc <= 1) AND (xc >= 16) THEN xc = 0
      IF (xc >= 24) THEN xc = 0
    CASE "2"
      yc = yc + 1
      IF (yc >= 11) AND (xc >= 16) AND (xc <= 31) THEN yc = 2
      IF (yc >= 11) THEN yc = 0
 
    CASE CHR$(0) + CHR$(72) ' Cursor-Up
      ys = ys - 1
      IF (ys < 0) THEN ys = yg
      ZAHL 69, 98, 27, 4, ys
    CASE CHR$(0) + CHR$(75) ' Cursor-Left
      xs = xs - 1
      IF (xs < 0) THEN xs = xg
      ZAHL 69, 91, 27, 4, xs
    CASE CHR$(0) + CHR$(77) ' Cursor-Right
      xs = xs + 1
      IF (xs > xg) THEN xs = 0
      ZAHL 69, 91, 27, 4, xs
    CASE CHR$(0) + CHR$(80) ' Cursor-Down
      ys = ys + 1
      IF (ys > yg) THEN ys = 0
      ZAHL 69, 98, 27, 4, ys
 
    CASE CHR$(13)           ' ENTER
      savecolour
    CASE " "
      g(xs, ys) = CHR$(ccc)
    CASE "z"
      z = z * 2
      IF z > 4 THEN z = 1
      clg
    CASE "h"
      help
    CASE CHR$(0) + CHR$(59) ' F1
      help
    CASE CHR$(0) + CHR$(71) ' POS1
      xs = 0
      ZAHL 69, 91, 27, 4, xs
    CASE CHR$(0) + CHR$(79) ' ENDE
      xs = xg
      ZAHL 69, 91, 27, 4, xs
    CASE CHR$(0) + CHR$(73) ' BILD-Hoch
      ys = 0
      ZAHL 69, 98, 27, 4, ys
    CASE CHR$(0) + CHR$(81) ' BILD-Runter
      ys = yg
      ZAHL 69, 98, 27, 4, ys
    END SELECT


'* Alt/Shift/Strg abfragen

  DEF SEG = &H40
    ky = PEEK(&H17)
    DEF SEG

  '* Farbe malen (bei Shift-Left)
  IF (ky AND 2) THEN g(xs, ys) = CHR$(ccc)

  '* Farbpipette (bei Strg/Crtl)
  IF (ky AND 4) THEN
    FOR x = 0 TO 23
      FOR y = 0 TO 10
        IF ct(x, y) = g(xs, ys) THEN xc = x: yc = y: x = 23: y = 10
        NEXT
      NEXT
    END IF

  '* gespeicherte Farbe aufrufen (bei Alt/AltGr)
  IF (ky AND 8) THEN
    message "welche farbe aufrufen? (0-9)"
    SLEEP
    ink = INKEY$
    IF (ink >= "0") AND (ink <= "9") THEN
      FOR x = 0 TO 23
        FOR y = 0 TO 10
          IF ct(x, y) = cc(VAL(ink)) THEN xc = x: yc = y: x = 23: y = 10
          NEXT
        NEXT
      END IF
      message ""
    END IF

END SUB

SUB ZAHL (x, y, c, Lrz, z)
' Lrz=Leerzeichen vor der Zahl (bei Žnderung der Anzahl der Stellen von
' fortlaufenden Zahlen ntzlich)
IF x = 0 AND y = 0 THEN x = xk: y = yk
IF y = 0 THEN y = yk + 7
IF Lrz - LEN(STR$(z)) > 0 THEN t$ = SPACE$(Lrz - LEN(STR$(z)) - 1) + STR$(z) ELSE t$ = LTRIM$(STR$(z))

FOR p = 1 TO LEN(t$)
SELECT CASE MID$(t$, p, 1)
  CASE "0" TO "9": pa = ASC(MID$(t$, p, 1)) - 47
  CASE "-": pa = 11
  CASE " ": pa = 12
  END SELECT
ON pa GOTO Z0, Z1, z2, Z3, Z4, Z5, Z6, Z7, Z8, Z9, zm, zl
GOTO ze

Z0:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), c

  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), 0
  PSET (x + 4, y - 2), 0
  PSET (x + 5, y - 2), c

  PSET (x + 2, y - 1), c
  PSET (x + 3, y - 1), 0
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), c

  PSET (x + 2, y - 0), 0
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), 0
  x = x + 5
  GOTO ze

Z1:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), 0
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), 0
  PSET (x + 3, y - 3), c
  PSET (x + 4, y - 3), c
  PSET (x + 5, y - 3), 0

  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), 0
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), 0

  PSET (x + 2, y - 1), 0
  PSET (x + 3, y - 1), 0
  PSET (x + 4, y - 1), c
  PSET (x + 5, y - 1), 0

  PSET (x + 2, y - 0), 0
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), c
  x = x + 5
  GOTO ze

z2:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), c

  PSET (x + 2, y - 2), 0
  PSET (x + 3, y - 2), 0
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), 0

  PSET (x + 2, y - 1), 0
  PSET (x + 3, y - 1), c
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), 0

  PSET (x + 2, y - 0), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), c
  x = x + 5
  GOTO ze

Z3:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), c

  PSET (x + 2, y - 2), 0
  PSET (x + 3, y - 2), 0
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), 0

  PSET (x + 2, y - 1), c
  PSET (x + 3, y - 1), 0
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), c

  PSET (x + 2, y - 0), 0
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), 0
  x = x + 5
  GOTO ze

Z4:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), 0
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), 0
  PSET (x + 3, y - 3), c
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), 0

  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), 0
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), 0

  PSET (x + 2, y - 1), c
  PSET (x + 3, y - 1), c
  PSET (x + 4, y - 1), c
  PSET (x + 5, y - 1), c

  PSET (x + 2, y - 0), 0
  PSET (x + 3, y - 0), 0
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), 0
  x = x + 5
  GOTO ze

Z5:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), 0

  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), 0

  PSET (x + 2, y - 1), 0
  PSET (x + 3, y - 1), 0
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), c

  PSET (x + 2, y - 0), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), 0
  x = x + 5
  GOTO ze

Z6:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), 0

  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), 0

  PSET (x + 2, y - 1), c
  PSET (x + 3, y - 1), 0
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), c

  PSET (x + 2, y - 0), 0
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), 0
  x = x + 5
  GOTO ze

Z7:
  PSET (x + 2, y - 4), c
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), c

  PSET (x + 2, y - 3), 0
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), c

  PSET (x + 2, y - 2), 0
  PSET (x + 3, y - 2), 0
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), 0

  PSET (x + 2, y - 1), 0
  PSET (x + 3, y - 1), c
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), 0

  PSET (x + 2, y - 0), 0
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), 0
  PSET (x + 5, y - 0), 0
  x = x + 5
  GOTO ze

Z8:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), c

  PSET (x + 2, y - 2), 0
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), 0

  PSET (x + 2, y - 1), c
  PSET (x + 3, y - 1), 0
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), c

  PSET (x + 2, y - 0), 0
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), 0
  x = x + 5
  GOTO ze

Z9:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), c
  PSET (x + 4, y - 4), c
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), c
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), c

  PSET (x + 2, y - 2), 0
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), c

  PSET (x + 2, y - 1), 0
  PSET (x + 3, y - 1), 0
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), c

  PSET (x + 2, y - 0), c
  PSET (x + 3, y - 0), c
  PSET (x + 4, y - 0), c
  PSET (x + 5, y - 0), 0
  x = x + 5
  GOTO ze

zm:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), 0
  PSET (x + 4, y - 4), 0
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), 0
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), 0

  PSET (x + 2, y - 2), c
  PSET (x + 3, y - 2), c
  PSET (x + 4, y - 2), c
  PSET (x + 5, y - 2), c

  PSET (x + 2, y - 1), 0
  PSET (x + 3, y - 1), 0
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), 0

  PSET (x + 2, y - 0), 0
  PSET (x + 3, y - 0), 0
  PSET (x + 4, y - 0), 0
  PSET (x + 5, y - 0), 0
  x = x + 5
  GOTO ze

zl:
  PSET (x + 2, y - 4), 0
  PSET (x + 3, y - 4), 0
  PSET (x + 4, y - 4), 0
  PSET (x + 5, y - 4), 0

  PSET (x + 2, y - 3), 0
  PSET (x + 3, y - 3), 0
  PSET (x + 4, y - 3), 0
  PSET (x + 5, y - 3), 0

  PSET (x + 2, y - 2), 0
  PSET (x + 3, y - 2), 0
  PSET (x + 4, y - 2), 0
  PSET (x + 5, y - 2), 0

  PSET (x + 2, y - 1), 0
  PSET (x + 3, y - 1), 0
  PSET (x + 4, y - 1), 0
  PSET (x + 5, y - 1), 0

  PSET (x + 2, y - 0), 0
  PSET (x + 3, y - 0), 0
  PSET (x + 4, y - 0), 0
  PSET (x + 5, y - 0), 0
  x = x + 5
ze:
  xk = x: yk = y
NEXT
END SUB ' tool by StratySoft, Philipp Strathausen

FUNCTION zeile$ (x, y, c, zl$) '

IF convert THEN IF c >= 16 THEN c = c * 15 / 255

FOR i = 1 TO LEN(zl$) - 2
  IF MID$(zl$, i, 1) = "|" THEN
    IF MID$(zl$, i, 3) = "|x|" THEN t$ = t$ + STR$(x): i = i + 2
    IF MID$(zl$, i, 3) = "|y|" THEN t$ = t$ + STR$(y): i = i + 2
    IF MID$(zl$, i, 3) = "|c|" THEN t$ = t$ + STR$(c): i = i + 2
  ELSE
    t$ = t$ + MID$(zl$, i, 1)
    END IF
  NEXT

zeile$ = t$
END FUNCTION

SUB zeile2 (z2$)

t$ = ""

FOR i = 1 TO LEN(z2$)
 
  IF MID$(z2$, i, 1) = "|" THEN
    PRINT #1, t$: t$ = ""
  ELSE
    t$ = t$ + MID$(z2$, i, 1)
    END IF
   
  NEXT
   
PRINT #1, t$

END SUB

