DEF TEMP-TABLE detalle
    FIELD codpro LIKE gn-prov.codpro
    FIELD nompro LIKE gn-prov.nompro
    FIELD ruc LIKE gn-prov.Ruc FORMAT 'x(11)'
    FIELD impmn1 AS DEC FORMAT '(>>>,>>>,>>9.99)' COLUMN-LABEL 'COMPRAS EN S/.'
    FIELD impmn2 AS DEC FORMAT '(>>>,>>>,>>9.99)' COLUMN-LABEL 'COMPRAS EN US$'.

FOR EACH almcmov NO-LOCK WHERE codcia = 1
    AND tipmov = 'i'
    AND codmov = 02
    AND fchdoc >= 01/01/2011
    AND flgest <> 'a',
    FIRST gn-prov NO-LOCK WHERE gn-prov.codcia = 000
    AND gn-prov.codpro = almcmov.codpro,
    EACH almdmov OF almcmov NO-LOCK.
    FIND detalle WHERE detalle.codpro = almcmov.codpro NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.codpro = gn-prov.codpro
        detalle.ruc = gn-prov.ruc
        detalle.nompro = gn-prov.nompro.
    IF almcmov.codmon = 1 THEN
        detalle.impmn1 = detalle.impmn1 + almdmov.impcto.
    IF almcmov.codmon = 2 THEN
        detalle.impmn2 = detalle.impmn2 + almdmov.impcto.
END.

OUTPUT TO c:\tmp\conti-proveedor.txt.
FOR EACH detalle.
    DISPLAY detalle WITH STREAM-IO NO-BOX WIDTH 320.
END.
