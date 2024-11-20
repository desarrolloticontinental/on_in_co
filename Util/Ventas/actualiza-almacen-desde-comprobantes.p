DEF VAR pMensaje AS CHAR NO-UNDO.

DEFINE VAR lCount AS INT INIT 0.

FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'FAI'
    AND fchdoc >= 09/01/2016
    AND flgest <> 'A':
    FIND FIRST almcmov WHERE almcmov.codcia = 001
        AND almcmov.codref = ccbcdocu.coddoc
        AND almcmov.nroref = ccbcdocu.nrodoc
        AND almcmov.flgest <> 'a'
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almcmov THEN DO:
        lCount = lCount + 1.
        RUN vta2\act_almv2 (ROWID(ccbcdocu), OUTPUT pmensaje).
        DISPLAY lCount ccbcdocu.coddoc ccbcdocu.nrodoc pmensaje
            WITH STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
    END.
END.
