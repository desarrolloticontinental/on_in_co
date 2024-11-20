DEF VAR x-codcli AS CHAR FORMAT 'x(11)'.

INPUT FROM c:\tmp\clie.prn.
OUTPUT TO c:\tmp\detalle.txt.
PUT UNFORMATTED
    'CLIENTE|NOMBRE|DIVISION|DOC|NUMERO|EMISION|MON|TC|TOTAL DOC|ARTICULO|DESCRIPCION|CANTIDAD|FACTOR|UNIDAD|IMPORTE'
    SKIP.
REPEAT :
    IMPORT x-codcli.
    IF x-codcli = '' THEN LEAVE.
    FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
        AND codcli = x-codcli
        AND LOOKUP(coddoc, 'FAC,N/C') > 0
        AND flgest <> 'A'
        AND fchdoc >= 01/01/2011:
        IF ccbcdocu.coddoc = 'FAC' AND ccbcdocu.tpofac <> "A" THEN NEXT.
        IF ccbcdocu.coddoc = "N/C" THEN DO:
            FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
                PUT UNFORMATTED
                    ccbcdocu.codcli '|'
                    ccbcdocu.nomcli '|'
                    ccbcdocu.coddiv '|'
                    ccbcdocu.coddoc '|'
                    ccbcdocu.nrodoc '|'
                    ccbcdocu.fchdoc '|'
                    ccbcdocu.codmon '|'
                    ccbcdocu.tpocmb '|'
                    ccbcdocu.imptot '|'
                    ccbddocu.codmat '|'.
                IF ccbcdocu.cndcre = "D" THEN DO:
                    FIND almmmatg OF ccbddocu NO-LOCK NO-ERROR.
                    IF AVAILABLE almmmatg THEN
                        PUT UNFORMATTED 
                        almmmatg.desmat '|'.
                    ELSE PUT UNFORMATTED 
                         '|'.
                END.
                ELSE DO:
                    FIND CcbTabla WHERE CcbTabla.CodCia = 001
                        AND CcbTabla.Tabla = "N/C"
                        AND CcbTabla.Codigo = ccbddocu.codmat
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE CcbTabla THEN
                        PUT UNFORMATTED 
                        CcbTabla.nombre '|'.
                    ELSE PUT UNFORMATTED 
                         '|'.
                END.
                PUT UNFORMATTED
                    ccbddocu.candes '|'
                    ccbddocu.factor '|'
                    ccbddocu.undvta '|'
                    ccbddocu.implin 
                    SKIP.
            END.
        END.
        ELSE DO:
            PUT UNFORMATTED
                ccbcdocu.codcli '|'
                ccbcdocu.nomcli '|'
                ccbcdocu.coddiv '|'
                ccbcdocu.coddoc '|'
                ccbcdocu.nrodoc '|'
                ccbcdocu.fchdoc '|'
                ccbcdocu.codmon '|'
                ccbcdocu.tpocmb '|'
                ccbcdocu.imptot '|'
                '|'
                '|'
                '|'
                '|'
                '|'
                SKIP.
        END.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.

