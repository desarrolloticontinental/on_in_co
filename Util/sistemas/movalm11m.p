OUTPUT TO d:\movimientos.txt.
DEF VAR x-almacenes AS CHAR INIT '11m,11,14,21'.
DEF VAR k AS INTE.
DEF VAR x-descrip AS CHAR FORMAT 'x(40)'.

DO k = 1 TO NUM-ENTRIES(x-almacenes):
    FOR EACH almcmov NO-LOCK WHERE codcia = 1
        AND codalm = ENTRY(k, x-almacenes)
        AND fchdoc >= 01/01/2023
        AND flgest <> "A",
        EACH almdmov OF almcmov NO-LOCK,
        FIRST almmmatg OF almdmov NO-LOCK:
        x-descrip = ''.
        FIND Almtmovm WHERE Almtmovm.CodCia = almcmov.codcia AND
            Almtmovm.Tipmov = almcmov.tipmov AND
            Almtmovm.Codmov = almcmov.codmov
            NO-LOCK NO-ERROR.
        IF AVAILABLE almtmovm THEN x-descrip = Almtmovm.Desmov.
        DISPLAY 
            almcmov.codalm
            almcmov.tipmov 
            almcmov.codmov 
            x-descrip LABEL 'Descripcion'
            almcmov.fchdoc
            almcmov.nroser 
            almcmov.nrodoc
            almdmov.codmat
            almmmatg.desmat
            (almdmov.candes * almdmov.factor) LABEL 'Cantidad'
            almmmatg.undstk
            WITH STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
    END.

END.
