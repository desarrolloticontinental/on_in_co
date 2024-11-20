OUTPUT TO c:\tmp\detallado-ventas.txt.
PUT UNFORMATTED
    'DIVISION|EMISION|COD|NUMERO|REF|NROREF|CLIENTE|NOMBRE|MON|PRODUCTO|DESCRIPCION|UNIDAD|CANTIDAD|DCTO1|DCTO2DCTO3|UNITARIO|TOTAL'
    SKIP.
FOR EACH gn-divi NO-LOCK WHERE codcia = 001,
    EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
    AND ccbcdocu.coddiv = gn-divi.coddiv
    AND codven = '151'
    AND fchdoc >= 01/01/2009
    AND flgest <> 'a'
    AND LOOKUP(coddoc, 'fac,bol,n/c,n/d') > 0:
    IF tpofac = 'A' OR tpofac = 'S' THEN NEXT.
    IF (coddoc = 'N/C' AND cndcre = 'N')
        OR coddoc = 'N/D' THEN DO:
        /* OTRAS NOTAS DE CREDITO */
        FOR EACH ccbddocu OF ccbcdocu NO-LOCK,
            FIRST CcbTabla NO-LOCK WHERE CcbTabla.CodCia = CcbDdocu.codcia 
            AND CcbTabla.Tabla  = CcbDDocu.coddoc 
            AND CcbTabla.Codigo = CcbDDocu.codmat:
            PUT UNFORMATTED
                ccbcdocu.coddiv '|'
                ccbcdocu.fchdoc '|'
                ccbcdocu.coddoc '|'
                ccbcdocu.nrodoc '|'
                ccbcdocu.codref '|'
                ccbcdocu.nroref '|'
                ccbcdocu.codcli '|'
                ccbcdocu.nomcli '|'
                ccbcdocu.codmon '|'
                ccbddocu.codmat '|'
                CcbTabla.Nombre '|'
                ccbddocu.undvta '|'
                ccbddocu.candes '|'
                CcbDDocu.Por_Dsctos[1] '|'
                CcbDDocu.Por_Dsctos[2] '|'
                CcbDDocu.Por_Dsctos[3] '|'
                ccbddocu.preuni '|'
                ccbddocu.implin
                SKIP.
        END.
    END.
    ELSE DO:
        /* NORMAL */
        FOR EACH ccbddocu OF ccbcdocu NO-LOCK,
            FIRST almmmatg OF ccbddocu NO-LOCK:
            PUT UNFORMATTED
                ccbcdocu.coddiv '|'
                ccbcdocu.fchdoc '|'
                ccbcdocu.coddoc '|'
                ccbcdocu.nrodoc '|'
                ccbcdocu.codref '|'
                ccbcdocu.nroref '|'
                ccbcdocu.codcli '|'
                ccbcdocu.nomcli '|'
                ccbcdocu.codmon '|'
                ccbddocu.codmat '|'
                almmmatg.desmat '|'
                ccbddocu.undvta '|'
                ccbddocu.candes '|'
                CcbDDocu.Por_Dsctos[1] '|'
                CcbDDocu.Por_Dsctos[2] '|'
                CcbDDocu.Por_Dsctos[3] '|'
                ccbddocu.preuni '|'
                ccbddocu.implin
                SKIP.
        END.
    END.
END.
OUTPUT CLOSE.

