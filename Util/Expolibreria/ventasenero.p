DEF TEMP-TABLE detalle
    FIELD codmat AS CHAR FORMAT 'x(6)' LABEL 'Producto'
    FIELD desmat AS CHAR FORMAT 'x(60)' LABEL 'Descripcion'
    FIELD undstk AS CHAR FORMAT 'x(6)' label 'Unidad'
    FIELD ventas AS DEC EXTENT 3
    FIELD stkact AS DEC EXTENT 3.
DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-StkComprometido AS DEC NO-UNDO.

INPUT FROM c:\tmp\expoenero.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE detalle.
    ASSIGN
        codmat = SUBSTRING(x-linea,1,6)
        ventas[1] = DECIMAL(SUBSTRING(x-linea,61,15))
        ventas[2] = DECIMAL(SUBSTRING(x-linea,76,15))
        ventas[3] = DECIMAL(SUBSTRING(x-linea,91,15)).
END.
INPUT CLOSE.

DEF VAR x-alm AS CHAR INIT '11,35,38' NO-UNDO.
DEF VAR x-codalm AS CHAR NO-UNDO.
DEF VAR k AS INT NO-UNDO.

FOR EACH detalle, FIRST almmmatg WHERE almmmatg.codcia = 1
    AND almmmatg.codmat = detalle.codmat NO-LOCK:
/*     DISPLAY detalle.codmat SKIP. */
/*     PAUSE 0.                     */
    ASSIGN
        detalle.desmat = almmmatg.desmat
        detalle.undstk = almmmatg.undstk.
    DO k = 1 TO 3:
        x-codalm = ENTRY(k, x-alm).
        FIND almmmate WHERE almmmate.codcia = 1
            AND almmmate.codalm = x-codalm
            AND almmmate.codmat = detalle.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE almmmate THEN DO:
            detalle.stkact[k] = almmmate.stkact.
            /* stock comprometido */
            RUN vta2/Stock-Comprometido (detalle.CodMat,
                                              x-codalm,
                                              OUTPUT x-StkComprometido).
            detalle.stkact[k] = detalle.stkact[k] - x-StkComprometido.
            IF detalle.stkact[k] < 0 THEN detalle.stkact[k] = 0.
        END.
    END.
END.
/*MESSAGE 'fin primera parte'.*/
OUTPUT TO c:\tmp\ventasvsstocks.txt.
FOR EACH detalle:
    DISPLAY
        detalle.codmat
        detalle.desmat
        detalle.undstk
        detalle.ventas[1] COLUMN-LABEL 'Ventas 11'
        detalle.stkact[1] COLUMN-LABEL 'StkDisp 11'
        detalle.ventas[2] COLUMN-LABEL 'Ventas 35'
        detalle.stkact[2] COLUMN-LABEL 'StkDisp 35'
        detalle.ventas[3] COLUMN-LABEL 'Ventas 38'
        detalle.stkact[3] COLUMN-LABEL 'StkDisp 38'
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

