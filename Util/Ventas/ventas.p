DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.
DEF VAR x-codref AS CHAR FORMAT 'x(3)'.
DEF VAR x-nroref AS CHAR FORMAT 'x(9)'.
DEF BUFFER BDOCU FOR ccbcdocu.

INPUT FROM c:\tmp\auditoria.prn.
OUTPUT TO c:\tmp\ventas.txt.
PUT
    "CODIGO|"
    "DESCRIPCION|"
    "DOCUMENTO|"
    "NUMERO|"
    "FECHA|"
    "CLIENTE|"
    "CANTIDAD|"
    "UNIDAD|"
    "IMPORTE|"
    "REFERENCIA|"
    SKIP.
REPEAT:
    IMPORT x-codmat.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 001:
        FOR EACH ccbddocu NO-LOCK USE-INDEX llave03 WHERE codcia = 001
            AND LOOKUP(ccbddocu.coddoc, 'FAC,BOL,N/C,TCK') > 0
            AND ccbddocu.coddiv = gn-divi.coddiv
            AND ccbddocu.codmat = x-codmat
            AND ccbddocu.fchdoc >= 03/01/2009
            AND ccbddocu.fchdoc <= 02/29/2012,
            FIRST ccbcdocu OF ccbddocu NO-LOCK,
            FIRST almmmatg OF ccbddocu NO-LOCK:
            ASSIGN
                x-codref = ccbcdocu.codref
                x-nroref = ccbcdocu.nroref.
            IF x-codref <> 'G/R' OR x-nroref = '' THEN DO:
                FIND BDOCU WHERE BDOCU.codcia = 001
                    AND BDOCU.coddoc = "G/R"
                    AND BDOCU.codref = ccbcdocu.coddoc
                    AND BDOCU.nroref = ccbcdocu.nroref
                    AND BDOCU.flgest <> "A"
                    NO-LOCK NO-ERROR.
                IF AVAILABLE BDOCU THEN
                    ASSIGN
                    x-codref = BDOCU.coddoc
                    x-nroref = BDOCU.nrodoc.
            END.
            PUT
                ccbddocu.codmat '|'
                almmmatg.desmat '|'
                ccbddocu.coddoc '|'
                ccbcdocu.nrodoc '|'
                ccbcdocu.fchdoc '|'
                ccbcdocu.codcli ' ' ccbcdocu.nomcli '|'
                ccbddocu.candes * ccbddocu.factor '|'
                almmmatg.undstk '|'
                (IF ccbcdocu.codmon = 1 THEN ccbddocu.implin ELSE ccbddocu.implin * ccbcdocu.tpocmb) '|'
                x-codref x-nroref '|'
                SKIP.
        END.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.

                                   
