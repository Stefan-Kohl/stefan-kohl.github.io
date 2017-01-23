PROGRAM Suche_Carmichael_Zahlen_mit_3_Faktoren_und_einem_gegebenen_Faktor;

USES crt;

CONST maxprim     =  30000;
      max_p1      =   3000;
      maxref      =     30;  {(Maximales C3(p)) + 1}

TYPE  carmzahl    = RECORD
                      q1,q2,q3 : longint;
                    END;
      carmzahlptr = ^carmzahl;
      reftable    = ARRAY[1..maxref] OF carmzahlptr;
      reftableptr = ^reftable;

VAR   p1,gesamtanzahl  : longint;
      prim             : PACKED ARRAY[1..maxprim] OF boolean;
      tabelle          : ARRAY[1..max_p1] OF reftableptr;
      ausgabe          : text;

  FUNCTION istprim (n : longint) : boolean;

  VAR  teiler,maxteiler  : longint;
       fuendig           : boolean;

  BEGIN;
    fuendig := FALSE;
    maxteiler := round(sqrt(n)); teiler := 2;
    WHILE (teiler <= maxteiler) AND NOT fuendig DO
    BEGIN;
      IF n mod teiler = 0 THEN fuendig := TRUE;
      IF teiler = 2 THEN teiler := 3 ELSE teiler := teiler + 2;
    END;
    istprim := NOT fuendig;
  END;

  PROCEDURE primzahlen_generieren;

  VAR  teiler,maxteiler,pos : longint;

  BEGIN;
    FOR pos := 1 TO maxprim DO prim[pos] := TRUE; prim[1] := FALSE;
    teiler := 2; maxteiler := round(sqrt(maxprim));
    WHILE teiler <= maxteiler DO
    BEGIN;
      pos := teiler + teiler;
      WHILE pos <= maxprim DO
      BEGIN;
        prim[pos] := FALSE;
        pos := pos + teiler;
      END;
      IF teiler = 2 THEN teiler := 3 ELSE teiler := teiler + 2;
    END;
  END;

  PROCEDURE tabelle_berechnen (p1 : longint);

  VAR  p2,p3,a,b,min_b,max_b,
       p2_zaehler,p3_zaehler,nenner,ab_p1q  : longint;
       p2_prim,p3_prim                      : boolean;
       faktor                               : real;

    FUNCTION teilbarkeit (p1,p2,p3 : longint) : boolean;

    BEGIN;
      teilbarkeit := (p1 mod (p3 - 1))*(p2 mod (p3 - 1)) mod (p3 - 1) - 1 = 0;
    END;

    FUNCTION vorgekommen (p1,p2,p3 : longint) : boolean;

    VAR  gefunden  : boolean;
         index     : integer;

    BEGIN;
      gefunden := FALSE;
      IF tabelle[p1] <> NIL THEN
      BEGIN;
        index := 1;
        WHILE tabelle[p1]^[index] <> NIL DO
          WITH tabelle[p1]^[index]^ DO
        BEGIN;
          IF    ((q1 = p1) OR (q2 = p1) OR (q3 = p1))
            AND ((q1 = p2) OR (q2 = p2) OR (q3 = p2))
            AND ((q1 = p3) OR (q2 = p3) OR (q3 = p3))
          THEN gefunden := TRUE;
          index := index + 1;
        END;
      END;
      vorgekommen := gefunden;
    END;

    PROCEDURE eintragen (p1,p2,p3 : longint);

      PROCEDURE eintragen_unter (p : longint);

      VAR  index  : integer;

      BEGIN;
        IF tabelle[p] = NIL THEN
        BEGIN;
          new(tabelle[p]);
          FOR index := 1 TO maxref DO tabelle[p]^[index] := NIL;
        END;
        index := 1;
        WHILE tabelle[p]^[index] <> NIL DO index := index + 1;
        new(tabelle[p]^[index]);
        WITH tabelle[p]^[index]^ DO
        BEGIN;
          q1 := p1; q2 := p2; q3 := p3;
        END;
      END;

    BEGIN;
      IF p2 <= max_p1 THEN eintragen_unter(p2);
      IF p3 <= max_p1 THEN eintragen_unter(p3);
    END;

    PROCEDURE tabelle_durchsuchen;

    VAR  index         : integer;
         p2,p3,ab_p1q  : longint;

    BEGIN;
      IF tabelle[p1] <> NIL THEN
      BEGIN;
        index := 1;
        WHILE tabelle[p1]^[index] <> NIL DO
          WITH tabelle[p1]^[index]^ DO
        BEGIN;
          IF p1 = q2 THEN
          BEGIN;
            IF q1 < q3 THEN BEGIN; p2 := q1; p3 := q3; END
                       ELSE BEGIN; p2 := q3; p3 := q1; END;
          END ELSE  {p1 = q3}
          BEGIN;
            IF q1 < q2 THEN BEGIN; p2 := q1; p3 := q2; END
                       ELSE BEGIN; p2 := q2; p3 := q1; END;
          END;
          a := (p1*p2 - 1) div (p3 - 1);
          b := (p1*p3 - 1) div (p2 - 1);
          ab_p1q := a*b - p1*p1;
          writeln(ausgabe,' ':6,'| ',p1:8,' | ',p2:8,' | ',p3:10,' | ',
                                      a:5,' | ',b:7,' | ',ab_p1q:11,' |');
          index := index + 1;
        END;
      END;
    END;

  BEGIN;
    writeln(ausgabe,' ':28,'Gegebener Faktor  :  ',p1:3);
    writeln(ausgabe); writeln(ausgabe);
    writeln(ausgabe,' ':6,
    '+----------+----------+------------+-------+---------+-------------+');
    writeln(ausgabe,' ':6,
    '|    p1    |    p2    |     p3     |   a   |    b    |  ab - p1^2  |');
    writeln(ausgabe,' ':6,
    '+----------+----------+------------+-------+---------+-------------+');
    clrscr;
    gotoxy(34,10); write('p1 = ',p1:4);
    gotoxy(34,12); write('a  = ');
    tabelle_durchsuchen;
    writeln(ausgabe,' ':6,
    '+----------+----------+------------+-------+---------+-------------+');
    FOR a := 2 TO p1 - 1 DO
    BEGIN;
      gotoxy(39,12); write(a:4);
      min_b := (p1*p1) div a;
      faktor := p1/(p1 - 2);
      max_b := round(faktor*p1*p1) div a;
      IF min_b < a THEN min_b := a;
      FOR b := min_b TO max_b DO
      BEGIN;
        p2_zaehler := a*b + (p1 - 1)*a - p1;
        p3_zaehler := a*b + (p1 - 1)*b - p1;
        nenner     := a*b - p1*p1;
        IF   p2_zaehler mod nenner = 0
        THEN IF   p3_zaehler mod nenner = 0 THEN
        BEGIN;
          p2 := p2_zaehler div nenner;
          p3 := p3_zaehler div nenner;
          IF (p2 >= 3) AND (p3 >= 3) AND (p2 <> p1) AND (p3 <> p1) THEN
          BEGIN;
            IF p2 <= maxprim THEN p2_prim := prim[p2]
                             ELSE p2_prim := istprim(p2);
            IF p3 <= maxprim THEN p3_prim := prim[p3]
                             ELSE p3_prim := istprim(p3);
            IF   p2_prim AND p3_prim
            THEN IF   teilbarkeit(p1,p2,p3)
                 THEN IF   teilbarkeit(p1,p3,p2)
                      THEN IF  teilbarkeit(p2,p3,p1)
                           THEN IF NOT vorgekommen(p1,p2,p3) THEN
            BEGIN;
              eintragen(p1,p2,p3);
              gesamtanzahl := gesamtanzahl + 1;
              ab_p1q := a*b - p1*p1;
              writeln(ausgabe,' ':6,'| ',p1:8,' | ',p2:8,' | ',p3:10,' | ',
                                          a:5,' | ',b:7,' | ',ab_p1q:11,' |');
            END;
          END;
        END;
      END;
    END;
    writeln(ausgabe,' ':6,
    '+----------+----------+------------+-------+---------+-------------+');
    IF (p1 = 97) OR (p1 = 997) OR (p1 = max_p1) THEN
    BEGIN;
      writeln(ausgabe); writeln(ausgabe); writeln(ausgabe); writeln(ausgabe);
      writeln(ausgabe,' ':6,
      'Gesamtanzahl der bis hierher gefundenen Carmichael - Zahlen : ',
      gesamtanzahl:6);
    END;
    writeln(ausgabe); writeln(ausgabe); writeln(ausgabe); writeln(ausgabe);
  END;

BEGIN;
  clrscr;
  primzahlen_generieren;
  assign(ausgabe,'CM3FATUR.TXT');
  rewrite(ausgabe); writeln(ausgabe);
  writeln(ausgabe,' ':7,
  'Carmichael - Zahlen mit 3 Faktoren, von welchen einer gegeben ist');
  writeln(ausgabe); writeln(ausgabe); writeln(ausgabe);
  FOR p1 := 1 TO max_p1 DO tabelle[p1] := NIL; gesamtanzahl := 0;
  FOR p1 := 3 TO max_p1 DO IF prim[p1] THEN tabelle_berechnen(p1);
  writeln(ausgabe); close(ausgabe);
  REPEAT UNTIL keypressed;
END.
