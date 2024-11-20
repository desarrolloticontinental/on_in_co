DEF VAR x-linea AS CHAR FORMAT 'x(200)'.
DEF VAR x-abono AS DEC.
DEF VAR x-cargo AS DEC.
DEF VAR x-dolares AS DEC.
DISABLE TRIGGERS FOR LOAD OF cb-dmov.

INPUT FROM c:\tmp\cancelaciones.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        CREATE cb-dmov.
        ASSIGN
            cb-dmov.codcia = 001
            cb-dmov.periodo = 2012
            cb-dmov.nromes = 06
            cb-dmov.codope = '076'
            cb-dmov.nroast = '060046'.
        ASSIGN
            cb-dmov.codcta = SUBSTRING(x-linea,1,8).
        FIND cb-ctas WHERE cb-ctas.codcia = 000
            AND cb-ctas.codcta = cb-dmov.codcta
            NO-LOCK.
        ASSIGN
            cb-dmov.coddiv = cb-ctas.coddiv
            cb-dmov.clfaux = cb-ctas.clfaux
            cb-dmov.codaux = SUBSTRING(x-linea,41,10)
            cb-dmov.coddoc = SUBSTRING(x-linea,51,2)
            cb-dmov.nrodoc = SUBSTRING(x-linea,58,20).
        ASSIGN
            x-abono = DECIMAL(SUBSTRING(x-linea,11,15))
            x-cargo = DECIMAL(SUBSTRING(x-linea,26,15))
            x-dolares = DECIMAL(SUBSTRING(x-linea,81,12))
            cb-dmov.impmn1 = x-abono + x-cargo
            cb-dmov.impmn2 = x-dolares
            cb-dmov.codmon = (IF SUBSTRING(x-linea,93,1) = "D" THEN 2 ELSE 1)
            cb-dmov.tpomov = (IF x-abono > 0 THEN YES ELSE NO)
            cb-dmov.glodoc = SUBSTRING(x-linea,103).
    END.
END.
INPUT CLOSE.



