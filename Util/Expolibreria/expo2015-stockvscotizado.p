DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-almacenes AS CHAR INIT '11e,21e,34e,35e,60e' NO-UNDO.
DEF VAR x-codalm AS CHAR NO-UNDO.
DEF VAR x-codmat AS CHAR NO-UNDO.
DEF VAR x-vendido AS DEC NO-UNDO.
DEF VAR x-stkact AS DEC NO-UNDO.
DEF VAR k AS INT NO-UNDO.
DEF VAR pComprometido AS DEC NO-UNDO.
DEF STREAM REPORTE.

DEF TEMP-TABLE detalle
    FIELD codmat LIKE almmmatg.codmat
    FIELD vendido AS DEC
    FIELD codalm AS CHAR EXTENT 5
    FIELD stock AS DEC EXTENT 5.

INPUT FROM c:\tmp\cotizados.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6)
        x-vendido = DECIMAL(SUBSTRING(x-linea,11)).
    CREATE detalle.
    ASSIGN
        detalle.codmat = x-codmat
        detalle.vendido = x-vendido.
END.
INPUT CLOSE.

FOR EACH detalle:
    DISPLAY detalle.codmat detalle.vendido WITH STREAM-IO NO-BOX.
    PAUSE 0.
    DO k = 1 TO 5:
        x-codalm = ENTRY(k,x-almacenes).
        x-stkact = 0.
        FIND almmmate WHERE almmmate.codcia = s-codcia
            AND almmmate.codalm = x-codalm
            AND almmmate.codmat = detalle.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE almmmate THEN x-stkact = almmmate.stkact.
        RUN vta2/stock-comprometido (detalle.codmat, x-codalm, OUTPUT pComprometido).
        ASSIGN 
            detalle.codalm[k] = x-codalm
            detalle.stock[k] = x-stkact - pcomprometido.
    END.
END.
OUTPUT STREAM REPORTE TO c:\tmp\expo-stock-vs-cotizado.txt.
PUT STREAM REPORTE UNFORMATTED
    'CODIGO|DESCRIPCION|UNIDAD|LINEA|SUBLINEA|MARCA|PROVEEDOR|COTIZADO|EMPAQUE INNER|ALM 11E|ALM 21E|ALM 34E|ALM 35E|ALM 60E'
    SKIP.
FOR EACH detalle NO-LOCK,
    FIRST almmmatg NO-LOCK WHERE almmmatg.codcia = s-codcia
    AND almmmatg.codmat = detalle.codmat:
    PUT STREAM REPORTE UNFORMATTED
        detalle.codmat '|'
        almmmatg.desmat '|'
        almmmatg.undstk '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        almmmatg.desmar '|'
        almmmatg.codpr1 '|'
        detalle.vendido '|'
        almmmatg.stkrep '|'.
    DO k = 1 TO 5:
        PUT STREAM REPORTE UNFORMATTED
            detalle.stock[k] '|'.
    END.
    PUT STREAM REPORTE UNFORMATTED SKIP.
END.
OUTPUT STREAM REPORTE CLOSE.

