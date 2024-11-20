DISABLE TRIGGERS FOR LOAD OF ccbcdocu.

DEF VAR x-Peso AS DECI NO-UNDO.
DEF VAR x-Volumen AS DECI NO-UNDO.

/*
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1,
    EACH ccbcdocu EXCLUSIVE-LOCK WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.coddiv = gn-divi.coddiv
    AND ccbcdocu.fchdoc >= 01/01/2023
    AND LOOKUP(ccbcdocu.coddoc, 'G/R,FAI') > 0:
    x-peso = 0.
    x-volumen = 0.
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK,
        FIRST Almmmatg OF ccbddocu NO-LOCK:
        x-Peso = x-Peso + (ccbddocu.candes * ccbddocu.factor * almmmatg.pesmat).
        x-Volumen = x-Volumen + (ccbddocu.candes * ccbddocu.factor * almmmatg.libre_d02 / 1000000).
    END.
    ccbcdocu.libre_d01 = x-Peso.
    ccbcdocu.libre_d02 = x-Volumen.
END.
*/

DISABLE TRIGGERS FOR LOAD OF almcmov.
DEF BUFFER b-cmov FOR almcmov.
FOR EACH b-cmov NO-LOCK WHERE b-cmov.codcia = 1
    AND b-cmov.tipmov = "S"
    AND b-cmov.codmov = 03
    AND b-cmov.fchdoc >= 01/01/2023:
    FIND almcmov WHERE ROWID(almcmov) = ROWID(b-cmov) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE almcmov THEN NEXT.
    x-peso = 0.
    x-volumen = 0.
    FOR EACH almdmov OF almcmov NO-LOCK,
        FIRST Almmmatg OF almdmov NO-LOCK:
        x-Peso = x-Peso + (almdmov.candes * almdmov.factor * almmmatg.pesmat).
        x-Volumen = x-Volumen + (almdmov.candes * almdmov.factor * almmmatg.libre_d02 / 1000000).
    END.
    almcmov.libre_d01 = x-Peso.
    almcmov.libre_d02 = x-Volumen.
    RELEASE almcmov.
END.

