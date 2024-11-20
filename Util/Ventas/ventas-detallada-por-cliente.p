DEF VAR x-codcli AS CHAR FORMAT 'x(11)'.

INPUT FROM c:\tmp\junior.prn.
OUTPUT TO c:\tmp\ventas.prn.
PUT UNFORMATTED 
    'COD|NUMERO|FECHA|CLIENTE|NOMBRE|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD|IMPORTE'
    SKIP.
REPEAT :
    IMPORT x-codcli.
    IF x-codcli = '' THEN LEAVE.
    FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
        AND codcli = x-codcli
        AND fchdoc >= 11/01/2014
        AND fchdoc <= TODAY
        AND LOOKUP(coddoc, 'FAC,BOL,TCK,N/C,N/D') > 0
        AND flgest <> 'A',
        EACH ccbddocu OF ccbcdocu NO-LOCK, FIRST almmmatg OF ccbddocu NO-LOCK:
        PUT UNFORMATTED
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.codcli '|'
            ccbcdocu.nomcli '|'
            ccbddocu.codmat '|'
            almmmatg.desmat '|'
            ccbddocu.candes '|'
            ccbddocu.undvta '|'
            ccbddocu.implin
            SKIP.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.

