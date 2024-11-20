DEF VAR x-nompro LIKE gn-prov.nompro.
OUTPUT TO d:\tmp\barras.txt.
PUT UNFORMATTED
    'ARTICULO|DESCRIPCION|UNIDAD|MARCA|PROVEEDOR|'
    'NOMBRE|LINEA|SUBLINEA|EAN 13|EAN 14 (1)|'
    'EQUIVALENCIA (1)|EAN 14 (2)|'
    'EQUIVALENCIA (2)|EAN 14 (3)|'
    'EQUIVALENCIA (3)|EAN 14 (4)|'
    'EQUIVALENCIA (4)|EAN 14 (5)|'
    'EQUIVALENCIA (5)|MONEDA|TC|COSTO PROMEDIO|CLAS MAY|CLAS UTILEX'
    SKIP.
FOR EACH INTEGRAL.almmmatg WHERE
    almmmatg.CodCia = 1
    AND almmmatg.TpoArt <> "D" NO-LOCK,
    EACH Almmmat1 OF Almmmatg NO-LOCK:
    FIND gn-prov WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = Almmmatg.codpr1
        NO-LOCK NO-ERROR.
    x-nompro = ''.
    IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.
    PUT UNFORMATTED
        Almmmatg.codmat '|'
        Almmmatg.desmat '|'
        Almmmatg.undbas '|'
        Almmmatg.desmar '|'
        Almmmatg.codpr1 '|'
        x-nompro        '|'
        Almmmatg.CODFAM '|'
        Almmmatg.subfam '|'
        Almmmatg.codbrr '|'
        Almmmat1.barras[1] '|'
        Almmmat1.equival[1] '|'
        Almmmat1.barras[2] '|'
        Almmmat1.equival[2] '|'
        Almmmat1.barras[3] '|'
        Almmmat1.equival[3] '|'
        Almmmat1.barras[4] '|'
        Almmmat1.equival[4] '|'
        Almmmat1.barras[5] '|'
        Almmmat1.equival[5] '|'
        Almmmatg.monvta '|'
        Almmmatg.tpocmb '|'
        Almmmatg.ctotot '|'.
    FIND FIRST factabla WHERE factabla.codcia = Almmmatg.codcia AND 
                                factabla.tabla = 'RANKVTA' AND 
                                factabla.codigo = almmmatg.codmat
                                NO-LOCK NO-ERROR.
    IF AVAILABLE FacTabla THEN
        PUT UNFORMATTED
        factabla.campo-c[6] '|'
        factabla.campo-c[5]
        SKIP.
    ELSE PUT UNFORMATTED '|' SKIP.
END.
OUTPUT CLOSE.
