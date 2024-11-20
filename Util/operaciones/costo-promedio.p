DEF VAR x-ctouni AS DEC.
DEF VAR x-fchdoc AS DATE.

DEF TEMP-TABLE detalle
    FIELD codmat AS CHAR FORMAT 'x(6)'
    FIELD stkact AS DEC.

INPUT FROM m:\tmp\xyz.prn.
REPEAT :
    CREATE detalle.
    IMPORT detalle.
END.
INPUT CLOSE.

OUTPUT TO m:\tmp\armando.txt.
FOR EACH detalle,FIRST almmmatg NO-LOCK WHERE almmmatg.codcia = 1
    AND almmmatg.codmat = detalle.codmat:
    x-ctouni = 0.
    x-fchdoc = ?.
    FIND LAST almstkge WHERE almstkge.codcia = 1
        AND almstkge.codmat = detalle.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE almstkge THEN x-ctouni = almstkge.ctouni.
    FIND LAST almdmov USE-INDEX almd07 WHERE almdmov.codcia = 1
        AND almdmov.codmat = detalle.codmat
        AND tipmov = 's'
        AND codmov = 02
        NO-LOCK NO-ERROR.
    IF AVAILABLE almdmov THEN x-fchdoc = almdmov.fchdoc.
    DISPLAY
        detalle.codmat  COLUMN-LABEL 'Codigo'
        desmat          COLUMN-LABEL 'Descripcion'
        desmar          COLUMN-LABEL 'Marca'
        undbas          COLUMN-LABEL 'Unidad'
        detalle.stkact  COLUMN-LABEL 'Stock'
        x-ctouni        COLUMN-LABEL 'Costo Promedio'
        x-fchdoc        COLUMN-LABEL 'Ultima Venta'
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
OUTPUT CLOSE.

