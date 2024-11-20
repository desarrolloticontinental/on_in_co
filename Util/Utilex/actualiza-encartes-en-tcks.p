/* Actualiza datos faltantes en ventas UTILEX */
/* NOTA: conectarse a la base de datos correspondiente antes de ejecutarlo */


DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00027'.

DEFINE VAR lActualizar AS LOG.
DEFINE VAR lRowId AS ROWID.

DISABLE TRIGGERS FOR LOAD OF integral.ccbcdocu.

DEF BUFFER B-ccbcdocu FOR ccbcdocu.

FOR EACH integral.ccbcdocu WHERE integral.ccbcdocu.codcia = s-codcia
    AND integral.ccbcdocu.coddoc = 'TCK'
    AND integral.ccbcdocu.coddiv = s-coddiv
    AND integral.ccbcdocu.fchdoc >= 01/01/2014
    AND integral.ccbcdocu.flgest <> 'A' NO-LOCK,
    FIRST utilex.faccpedi NO-LOCK WHERE utilex.faccpedi.codcia = s-codcia
    AND utilex.faccpedi.coddoc = 'P/M'
    AND utilex.faccpedi.nroped = integral.ccbcdocu.nroped
    AND utilex.faccpedi.flgsit = 'CD':
    IF lActualizar THEN DO:

        lRowid = ROWID(ccbcdocu).
        FIND FIRST b-ccbcdocu WHERE ROWID(b-ccbcdocu)=lRowid EXCLUSIVE NO-ERROR.
        IF AVAILABLE b-ccbcdocu THEN DO:
            ASSIGN integral.ccbcdocu.libre_c05 = 'CD|' + utilex.faccpedi.libre_c05.
        END.
            
    END.
    
END.
