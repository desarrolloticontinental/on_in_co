OUTPUT TO d:\tmp\ordenes.txt.
DEF VAR x-cndvta AS CHAR.
PUT UNFORMATTED
    'FECHA|ENTREGA|ORDEN|SITUACION|PROVEEDOR|NOMPRO|CONDICION|DESCRIPCION|MONEDA|ALMACEN|PRODUCTO|DESPRO|MARCA|LINA|SUBLINEA|UNIDAD|CANTIDAD|UNITARIO'
    SKIP.
FOR EACH lg-cocmp NO-LOCK WHERE codcia = 1
    AND fchdoc >= 01/01/2015
    AND fchdoc <= 12/31/2015
    AND coddiv = '00000'
    AND FlgSit = "A"
    /*AND userid-com = 'admin'*/,
    EACH lg-docmp NO-LOCK WHERE lg-docmp.codcia = lg-cocmp.codcia
    AND lg-docmp.coddiv = lg-cocmp.coddiv
    AND lg-docmp.tpodoc = lg-cocmp.tpodoc
    AND lg-docmp.nrodoc = lg-cocmp.nrodoc,
    FIRST almmmatg OF lg-docmp NO-LOCK:
    x-cndvta = ''.
    FIND FIRST gn-concp WHERE gn-concp.Codig = LG-COCmp.CndCmp NO-LOCK NO-ERROR.
    IF AVAILABLE gn-concp THEN x-cndvta = Gn-ConCp.Nombr.


    PUT UNFORMATTED
        lg-cocmp.fchdoc '|'
        lg-cocmp.fchent '|'
        lg-cocmp.nrodoc FORMAT '999999' '|'
        lg-cocmp.flgsit '|'
        lg-cocmp.codpro '|'
        lg-cocmp.nompro '|'
        lg-cocmp.cndcmp '|'
        x-cndvta        '|'
        lg-cocmp.codmon '|'
        lg-cocmp.codalm '|'
        lg-docmp.codmat '|'
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        lg-docmp.undcmp '|'
        lg-docmp.canpedi '|'
        (LG-DOCmp.ImpTot / lg-docmp.canpedi) / (1 + lg-docmp.igv / 100) '|'
        SKIP.
END.
OUTPUT CLOSE.
