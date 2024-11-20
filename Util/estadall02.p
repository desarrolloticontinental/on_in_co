DEF VAR x-month AS INT.
DEF VAR x-year AS INT.
DEF VAR x-nrofchi AS INT.
DEF VAR k AS INT.
DEF VAR x-MonthEnd.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 001:
    x-Month = 03.     /* Mes de partida */
    DO k = 2007 TO 2009:
        x-Year = k.
        x-MonthEnd = 12.
        IF x-Year = 2009 THEN x-MonthEnd = 07.
        REPEAT WHILE x-Month <= x-MonthEnd:
            x-NroFchI = x-Year * 100 + x-Month.
            FOR EACH evtall02 NO-LOCK WHERE codcia = 001
                AND nrofch = x-nrofchi
                AND coddiv = gn-divi.coddiv
                /*AND codunico = '10035793521'*/:
                DISPLAY nrofch coddiv codunico codmat.
                PAUSE 0.
            END.

            x-Month = x-Month + 1.
        END.    /* REPEAT */
        x-Month = 01.
    END.
END.



