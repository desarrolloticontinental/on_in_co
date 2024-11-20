DEF TEMP-TABLE t-codigo FIELD codmat AS CHAR FORMAT 'x(6)'
    INDEX Llave00 AS PRIMARY codmat.

INPUT FROM d:\tmp\productos.prn.
REPEAT :
    CREATE t-codigo.
    IMPORT t-codigo.
END.
INPUT CLOSE.

/* por cada cliente por cada producto */
DEF STREAM Reporte.
OUTPUT STREAM Reporte TO d:\reporte.txt.
PUT STREAM Reporte UNFORMATTED 
    'DIVISION|DOC|NUMERO|FECHA|CODCLI|NOMCLI|ARTICULO|UNIDAD|CANTIDAD|IMPORTE|MONEDA' SKIP.
RLOOP:
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1, EACH t-codigo NO-LOCK:
    FOR EACH ccbddocu NO-LOCK WHERE ccbddocu.codcia = 1 AND
        ccbddocu.coddiv = gn-divi.coddiv AND
        ccbddocu.codmat = t-codigo.codmat AND
        LOOKUP(ccbddocu.coddoc, 'FAC,BOL') > 0,
        FIRST ccbcdocu OF ccbddocu NO-LOCK WHERE ccbcdocu.flgest <> 'A'
        BREAK BY ccbcdocu.codcli BY ccbcdocu.fchdoc:
        IF LAST-OF(ccbcdocu.codcli) THEN DO:
            DISPLAY t-codigo.codmat. PAUSE 0.
            PUT STREAM Reporte UNFORMATTED 
            ccbcdocu.coddiv '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.codcli '|'
            ccbcdocu.nomcli '|'
            ccbddocu.codmat '|'
            ccbddocu.undvta '|'
            ccbddocu.candes '|'
            ccbddocu.implin '|'
            ccbcdocu.codmon
            SKIP.
        END.
    END.
END.
OUTPUT CLOSE.

/*
FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = 0:
    DISPLAY gn-clie.codcli. PAUSE 0.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1,
        EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1 AND
        ccbcdocu.coddiv = gn-divi.coddiv AND
        ccbcdocu.codcli = gn-clie.codcli AND
        LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0 AND
        ccbcdocu.flgest <> 'A',
        EACH ccbddocu OF ccbcdocu NO-LOCK,
        FIRST t-codigo NO-LOCK WHERE t-codigo.codmat = ccbddocu.codmat
        BREAK BY ccbddocu.codmat BY ccbcdocu.fchdoc :
        IF LAST-OF(ccbddocu.codmat) THEN
            PUT STREAM Reporte UNFORMATTED 
            ccbcdocu.coddiv '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.codcli '|'
            gn-clie.nomcli '|'
            ccbddocu.codmat '|'
            ccbddocu.undvta '|'
            ccbddocu.candes '|'
            ccbddocu.implin '|'
            ccbcdocu.codmon
            SKIP.
    END.
END.
*/
