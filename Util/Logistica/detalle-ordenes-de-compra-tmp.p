DEF TEMP-TABLE detalle
    FIELD codcia AS INT
    FIELD nrodoc LIKE lg-cocmp.nrodoc
    FIELD tpodoc LIKE lg-cocmp.tpodoc
    FIELD coddiv LIKE lg-cocmp.coddiv
    FIELD fchdoc AS DATE
    FIELD fchent AS DATE
    FIELD cndcmp LIKE lg-cocmp.cndcmp
    FIELD codalm AS CHAR FORMAT 'x(3)'
    FIELD codmon AS INT
    INDEX llave01 AS UNIQUE PRIMARY codcia coddiv tpodoc nrodoc.

DEF TEMP-TABLE detalle2 LIKE lg-docmp
    INDEX llave01 AS PRIMARY codcia coddiv tpodoc nrodoc.

FOR EACH lg-cocmp NO-LOCK WHERE codcia = 1
    AND fchdoc >= 11/01/2010
    AND codpro = '10005035'
    AND flgsit <> 'a':
    CREATE detalle.
    BUFFER-COPY lg-cocmp TO detalle.
END.

INPUT FROM c:\tmp\lg-docmp.d.
REPEAT:
    CREATE detalle2.
    IMPORT detalle2.
END.
INPUT CLOSE.

OUTPUT TO c:\tmp\compras-faber.txt.
FOR EACH detalle, EACH detalle2 OF detalle, FIRST almmmatg OF detalle2 NO-LOCK:
    DISPLAY
        detalle.nrodoc '|'
        detalle.fchdoc '|'
        detalle.fchent '|'
        detalle.codalm '|'
        detalle.codmon '|'
        detalle2.codmat '|'
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        detalle2.canpedi '|'
        detalle2.undcmp FORMAT 'x(6)' '|'
        detalle2.imptot
        WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE WIDTH 320.
END.
OUTPUT CLOSE.

