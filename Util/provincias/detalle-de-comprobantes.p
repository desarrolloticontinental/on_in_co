DEF VAR x-linea AS CHAR.

INPUT FROM c:\tmp\ncredito.prn.
OUTPUT TO c:\tmp\luciano-ncredito.txt.
PUT UNFORMATTED
    'DIVISION|TIPO|CODIGO|NUMERO|EMISION|VENCTO|CLIENTE|NOMBRE|IMPDOC|PRODUCTO|CANTIDAD|UNIDAD|IMPPROD|DESCRIPCION'
    SKIP.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND ccbcdocu WHERE codcia = 1
        AND coddoc = SUBSTRING(x-linea,1,3)
        AND nrodoc = SUBSTRING(x-linea,4).
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
        PUT UNFORMATTED
            ccbcdocu.divori '|'
            ccbcdocu.tpofac '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.fchvto '|'
            ccbcdocu.codcli '|'
            ccbcdocu.nomcli '|'
            ccbcdocu.imptot '|'
            ccbddocu.codmat '|'
            ccbddocu.candes '|'
            ccbddocu.undvta '|'
            ccbddocu.implin '|'.
        FIND almmmatg OF ccbddocu NO-LOCK NO-ERROR.
        FIND CcbTabla WHERE CcbTabla.CodCia = 001
            AND CcbTabla.Tabla = ccbcdocu.coddoc
            AND CcbTabla.Codigo = ccbddocu.codmat
            NO-LOCK NO-ERROR.
        IF ccbcdocu.coddoc = 'N/C' 
            AND ccbcdocu.cndcre = "N" 
            AND AVAILABLE ccbtabla THEN
            PUT UNFORMATTED
            CcbTabla.Nombre FORMAT 'x(30)'
            SKIP.
        ELSE IF AVAILABLE almmmatg THEN
            PUT UNFORMATTED
            almmmatg.desmat FORMAT 'x(30)'
            SKIP.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.

