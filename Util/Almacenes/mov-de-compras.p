OUTPUT TO c:\tmp\mov-de-compras.txt.
PUT UNFORMATTED
    'ALMACEN|MOV|FECHA|DOCUMENTO|ORDEN|GUIA|PROVEEDOR|PRODUCTO|MARCA|LINEA|SUBLINEA|UNDSTOCK|UNDCOMPRA|CANTIDAD|FACTOREQUIV|MONEDA|TPOCMB|UNITARIO'
    SKIP.
FOR EACH almacen NO-LOCK WHERE codcia = 001,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = 001
    AND almcmov.codalm = almacen.codalm
    AND almcmov.fchdoc >= 10/01/2014
    AND (almcmov.tipmov = "I" AND (almcmov.codmov = 02 OR almcmov.codmov = 06))
    AND flgest <> "A",
    FIRST gn-prov NO-LOCK WHERE gn-prov.CodCia = 000 AND 
    gn-prov.CodPro = Almcmov.CodPro,
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT UNFORMATTED
        almcmov.codalm '|'
        almcmov.tipmov  '-'
        almcmov.codmov '|'
        almcmov.fchdoc '|'
        string(almcmov.nroser, '999') '-'
        string(almcmov.nrodoc, '9999999') '|'
        string(almcmov.nrorf1, '999999') '|'
        almcmov.nrorf3 '|'
        almcmov.codpro ' '
        gn-prov.NomPro '|'
        almdmov.codmat  ' '
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        almmmatg.undstk '|'
        almdmov.codund '|'
        almdmov.candes '|'
        almdmov.factor '|'
        almcmov.codmon '|'
        almdmov.tpocmb '|'
        almdmov.preuni
        SKIP.
END.
/*
FOR EACH almacen NO-LOCK WHERE codcia = 001,
    EACH almcmov NO-LOCK WHERE almcmov.codcia = 001
    AND almcmov.codalm = almacen.codalm
    AND almcmov.fchdoc >= 10/01/2011
    AND almcmov.fchdoc <= 03/31/2012
    AND (almcmov.tipmov = "S" AND almcmov.codmov = 09),
    EACH almdmov OF almcmov NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    PUT /*UNFORMATTED*/
        almcmov.codalm '|'
        almcmov.tipmov  '-'
        almcmov.codmov '|'
        almcmov.fchdoc '|'
        almcmov.nroser '-'
        almcmov.nrodoc '|'
        almdmov.codmat  ' '
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        almmmatg.undstk '|'
        almdmov.codund '|'
        almdmov.candes '|'
        almdmov.factor 
        SKIP.
END.
*/
OUTPUT CLOSE.

