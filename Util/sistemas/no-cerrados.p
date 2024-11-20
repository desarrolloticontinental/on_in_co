DEF TEMP-TABLE t-facdpedi LIKE facdpedi.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 001,
    EACH FacCPedi WHERE faccpedi.codcia = 001
    AND faccpedi.coddiv = gn-divi.coddiv
    AND faccpedi.coddoc = 'PED'
    AND faccpedi.fchven < TODAY - 7
    AND LOOKUP(faccpedi.flgest, 'G,P,X,W,WX,WL') > 0:
    FIND FIRST Facdpedi OF Faccpedi WHERE Facdpedi.canate > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Facdpedi THEN NEXT.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE t-facdpedi.
        BUFFER-COPY facdpedi TO t-facdpedi.
    END.
END.

OUTPUT TO d:\retenidos.txt.
PUT UNFORMATTED
    'CANAL DE VENTA|NUMERO|FECHA|CLIENTE|NOMBRE|VENDEDOR|ALMACEN|ARTICULO|DESCRIPCION|FAMILIA|SUBFAMILIA|CANTIDAD|UNIDAD|IMPORTE' SKIP.
FOR EACH t-facdpedi NO-LOCK, FIRST almmmatg OF t-facdpedi NO-LOCK, FIRST faccpedi OF t-facdpedi NO-LOCK:
    PUT UNFORMATTED
        faccpedi.coddiv '|'
        faccpedi.nroped '|'
        faccpedi.fchped '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        faccpedi.codven '|'
        t-facdpedi.almdes '|'
        t-facdpedi.codmat '|'
        almmmatg.desmat '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        (t-facdpedi.canped * t-facdpedi.factor) '|'
        almmmatg.undstk '|'
        t-facdpedi.implin
        SKIP.
END.
OUTPUT CLOSE.

