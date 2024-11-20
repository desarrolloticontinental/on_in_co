OUTPUT TO c:\tmp\ventascissac.txt.
PUT UNFORMATTED
    'COD|NUMERO|REF|NROREF|FECHA|CLIENTE|NOMBRE|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD|CATCONTA|IMPORTE SOLES|IMPORTE LINEA|MONEDA'
    SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND LOOKUP(coddoc, 'fac,bol,n/c') > 0
    AND fchdoc >= 01/01/2013
    AND fchdoc <= TODAY,
    LAST gn-tcmb NO-LOCK WHERE gn-tcmb.fecha <= ccbcdocu.fchdoc:
    IF ccbcdocu.coddoc = 'n/c' AND ccbcdocu.cndcre <> 'D'
        THEN NEXT.
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK,
        FIRST almmmatg OF ccbddocu NO-LOCK:
        PUT UNFORMATTED
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.codref '|'
            ccbcdocu.nroref '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.codcli '|'
            ccbcdocu.nomcli '|'
            ccbddocu.codmat '|'
            almmmatg.desmat '|'
            ccbddocu.candes '|'
            ccbddocu.undvta '|'
            almmmatg.catconta[1] '|'.
        IF ccbcdocu.codmon = 2 THEN
            PUT UNFORMATTED ROUND(ccbddocu.implin * gn-tcmb.venta, 2) '|'.
            ELSE PUT UNFORMATTED ccbddocu.implin '|'.
        PUT UNFORMATTED
            ccbddocu.implin '|'
            ccbcdocu.codmon '|'.
        PUT UNFORMATTED SKIP.
    END.
END.
