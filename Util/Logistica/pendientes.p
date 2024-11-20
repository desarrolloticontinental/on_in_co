DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

OUTPUT TO c:\tmp\compras.txt.
FOR EACH lg-cocmp NO-LOCK WHERE codcia = s-codcia
    AND flgsit <> 'A'
    AND fchdoc >= 12/01/09,
    EACH lg-docmp OF lg-cocmp NO-LOCK /*WHERE lg-docmp.canaten < lg-docmp.canpedi*/,
    FIRST almmmatg OF lg-docmp NO-LOCK,
    FIRST gn-prov NO-LOCK WHERE gn-prov.codcia = 000
    AND gn-prov.codpro = almmmatg.codpr1,
    FIRST Almacen OF lg-cocmp NO-LOCK:
    DISPLAY
        (IF lg-cocmp.tpodoc = 'I' THEN 'IMPORTACIONES' ELSE 'NACIONALES') FORMAT 'x(15)'
        '|'
        lg-cocmp.nrodoc
        '|'
        lg-cocmp.fchdoc
        '|'
        lg-cocmp.fchvto
        '|'
        LG-COCmp.CodAlm
        '-'
        Almacen.Descripcion
        '|'
        gn-prov.nompro
        '-'
        lg-cocmp.codpro
        '|'       
        lg-docmp.codmat
        '|'
        almmmatg.desmat
        '|'
        LG-DOCmp.UndCmp
        '|'
        almmmatg.ctolis
        '|'
        LG-DOCmp.CanPedi 
        '|'
        LG-DOCmp.CanAten
        '|'
        Almmmatg.tpoart
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.

