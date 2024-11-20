/* INGRESOS POR COMPRAS VS FACTURADO */

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.
DEF VAR fecha AS DATE NO-UNDO.
DEF VAR periodo AS INT NO-UNDO.
DEF VAR mes AS INT NO-UNDO.

DEF TEMP-TABLE detalle LIKE almdmov
    FIELD OrdCmp AS CHAR
    FIELD codpro AS CHAR.

ASSIGN
    fecha = 01/01/2014
    periodo = YEAR(fecha)
    mes = MONTH(fecha).

/* INGRESOS POR COMPRAS */
FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.tipmov = "I"
    AND (almcmov.codmov = 02 OR almcmov.codmov = 06)
    AND almcmov.fchdoc >= fecha
    AND almcmov.flgest <> "A":
    FOR EACH almdmov OF almcmov NO-LOCK:
        CREATE detalle.
        BUFFER-COPY almdmov 
            TO detalle
            ASSIGN
            detalle.ordcmp = almcmov.nrorf1
            detalle.codpro = almcmov.codpro
            detalle.codmon = almcmov.codmon
            detalle.tpocmb = almcmov.tpocmb.
    END.
END.
/* DEVOLUCIONES DE COMPRAS */
FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia
    AND almcmov.tipmov = "S"
    AND almcmov.codmov = 09
    AND almcmov.fchdoc >= fecha
    AND almcmov.flgest <> "A":
    FOR EACH almdmov OF almcmov NO-LOCK:
        CREATE detalle.
        BUFFER-COPY almdmov 
            TO detalle
            ASSIGN
            detalle.ordcmp = almcmov.nrorf1
            detalle.codpro = almcmov.codpro
            detalle.codmon = almcmov.codmon
            detalle.tpocmb = almcmov.tpocmb.
    END.
END.

OUTPUT TO c:\tmp\compras.txt.
PUT UNFORMATTED 
    "ORDEN|MOVIMIENTO|PROVEEDOR|RUC|NOMBRE|ALMACEN|MOVIMIENTO|FECHA|MONEDA|TPOCAMBIO|ARTICULO|DESCRIPCION|CANTIDAD|UNIDAD|UNITARIO(S/IGV)"
    SKIP.
FOR EACH detalle NO-LOCK, FIRST gn-prov WHERE gn-prov.codcia = pv-codcia
    AND gn-prov.codpro = detalle.codpro,
    FIRST almmmatg OF detalle NO-LOCK:
    PUT UNFORMATTED
        detalle.ordcmp '|'
        detalle.tipmov detalle.codmov '|'
        detalle.codpro '|'
        gn-prov.Ruc '|'
        gn-prov.nompro '|'
        detalle.codalm '|'
        detalle.nrodoc '|'
        detalle.fchdoc '|'
        detalle.codmon '|'
        detalle.tpocmb '|'
        detalle.codmat '|'
        almmmatg.desmat '|'
        detalle.candes * detalle.factor '|'
        almmmatg.undstk '|'
        detalle.preuni
        SKIP.
END.
OUTPUT CLOSE.

