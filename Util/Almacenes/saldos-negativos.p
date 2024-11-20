OUTPUT TO c:\tmp\saldos-negativos.txt.
FOR EACH almacen NO-LOCK WHERE codcia = 001,
    EACH almmmate OF almacen NO-LOCK:
    FIND LAST almstkal OF almmmate WHERE fecha <= 12/31/2012 NO-LOCK NO-ERROR.
    IF AVAILABLE almstkal AND AlmStkal.StkAct < 0 THEN DO:
        PUT UNFORMATTED
            almacen.codalm '|'
            almmmate.codmat '|'
            almstkal.stkact
            SKIP.
    END.
END.
OUTPUT CLOSE.

