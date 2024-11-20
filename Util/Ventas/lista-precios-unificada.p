DEF VAR x-ctotot LIKE almmmatg.ctotot NO-UNDO.
DEF VAR x-precio-a AS DEC DECIMALS 4 NO-UNDO.
DEF VAR x-precio-b AS DEC DECIMALS 4 NO-UNDO.
DEF VAR x-precio-c AS DEC DECIMALS 4 NO-UNDO.
DEF VAR x-precio-oficina AS DEC DECIMALS 4 NO-UNDO.
DEF VAR x-precio-utilex  AS DEC DECIMALS 4 NO-UNDO.
DEF VAR x-unidad-utilex AS CHAR NO-UNDO.

OUTPUT TO c:\tmp\listageneral.txt.
PUT UNFORMATTE
    'CODIGO|DESCRIPCION|MARCA|RANKING|CATEGORIA|LINEA|SUBLINEA|UNIDAD STOCK|COSTO UNITARIO|PRECIO A|UNIDAD A|PRECIO B|UNIDAD B|PRECIO C|UNIDAD C|PRECIO OFICINA|UNIDAD OFICINA LIMA|PRECIO OFICINA UTILEX|UNIDAD UTILEX'
    SKIP.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1
    AND tpoart <> 'd'.
    ASSIGN
        x-precio-a = almmmatg.prevta[2]
        x-precio-b = almmmatg.prevta[3]
        x-precio-c = almmmatg.prevta[4]
        x-precio-oficina = almmmatg.preofi
        x-ctotot = almmmatg.ctotot
        x-precio-utilex = 0
        x-unidad-utilex = ''.
    IF almmmatg.monvta = 2 THEN
        ASSIGN
        x-ctotot = x-ctotot * almmmatg.tpocmb
        x-precio-a = ROUND(x-precio-a * almmmatg.tpocmb, 4)
        x-precio-b = ROUND(x-precio-b * almmmatg.tpocmb, 4)
        x-precio-c = ROUND(x-precio-c * almmmatg.tpocmb, 4)
        x-precio-oficina = ROUND(x-precio-oficina * almmmatg.tpocmb, 4).
    FIND VtaListaMinGn WHERE VtaListaMinGn.codcia = 1
        AND VtaListaMinGn.codmat = almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE VtaListaMinGn THEN DO:
        ASSIGN
            x-precio-utilex = VtaListaMinGn.preofi
            x-unidad-utilex = VtaListaMinGn.Chr__01.
        IF VtaListaMinGn.monvta = 2 THEN
            x-precio-utilex = x-precio-utilex * VtaListaMinGn.tpocmb.
    END.
    PUT UNFORMATTED
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        almmmatg.ordtmp '|'
        Almmmatg.tiprot[1]  '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        almmmatg.undstk '|'
        x-ctotot '|'
        x-precio-a '|'
        Almmmatg.UndA '|'
        x-precio-b '|'
        Almmmatg.UndB '|'
        x-precio-c '|'
        Almmmatg.UndC '|'
        x-precio-oficina '|'
        Almmmatg.CHR__01 '|'
        x-precio-utilex '|'
        x-unidad-utilex
        SKIP.
END.
