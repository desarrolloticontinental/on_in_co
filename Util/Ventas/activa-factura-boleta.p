DEF VAR pMensaje AS CHAR NO-UNDO.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'tck'
    AND fchdoc >= TODAY - 1
    AND flgest <> 'A':
    FIND FIRST almcmov WHERE almcmov.codcia = 001
        AND almcmov.codref = ccbcdocu.coddoc
        AND almcmov.nroref = ccbcdocu.nrodoc
        AND almcmov.flgest <> 'a'
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almcmov THEN DO:
        RUN vta2\act_almv2 (ROWID(ccbcdocu), OUTPUT pmensaje).
        DISPLAY ccbcdocu.coddoc ccbcdocu.nrodoc pmensaje
            WITH STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
    END.
END.

/*
FIND ccbcdocu WHERE codcia = 1
    AND coddoc = 'tck'
    AND nrodoc = '101060375'.

DEF VAR pMensaje AS CHAR NO-UNDO.

RUN vta2\act_almv2 (ROWID(ccbcdocu), OUTPUT pmensaje).

DISPLAY pmensaje.
*/
