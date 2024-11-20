DEF VAR x-linea AS CHAR.
DEF VAR x-codmat AS CHAR.
DEF VAR x-cantidad AS DEC.
DEF VAR x-acumulado AS DEC.
DEF VAR x-compra AS DEC.
DEF TEMP-TABLE Detalle LIKE Almdmov.
DEF BUFFER b-dmov FOR almdmov.
INPUT FROM d:\tmp\xxx.prn.
OUTPUT TO d:\tmp\detalle.txt.
PUT UNFORMATTED
    'FECHA|PROVEEDOR|OC|G/R|ALMACEN|TIPMOV|CODMOV|SERIE|CORRELATIVO|ARTICULO|A DEVOLVER|COMPRADO|UNIDAD|FACTOR|MONEDA|TPOCMB|UNITARIO'
    SKIP.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6)
        x-cantidad = DECIMAL(SUBSTRING(x-linea,11))
        x-acumulado = 0.
    FOR EACH almacen NO-LOCK WHERE almacen.codcia = 001,
        EACH almdmov NO-LOCK WHERE almdmov.codcia = 001
        AND almdmov.codalm = almacen.codalm
        AND almdmov.codmat = x-codmat
        AND (almdmov.tipmov = "I" OR almdmov.tipmov = "S")
        AND (almdmov.codmov = 02 OR almdmov.codmov = 90 OR almdmov.codmov = 91)
        AND almdmov.fchdoc < DATE(01,05,2016),
        FIRST almmmatg OF almdmov NO-LOCK,
        FIRST almcmov OF almdmov NO-LOCK
        BY almdmov.fchdoc DESC:
        /* Buscamos su anulacion */
        FIND FIRST b-dmov WHERE b-dmov.codcia = almdmov.codcia
            AND b-dmov.codalm = almdmov.codalm
            AND b-dmov.tipmov = "S"
            AND b-dmov.codmov = almdmov.codmov
            AND b-dmov.nroser = almdmov.nroser
            AND b-dmov.nrodoc = almdmov.nrodoc
            AND b-dmov.codmat = almdmov.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE b-dmov THEN NEXT.
        x-compra = almdmov.candes.
        x-acumulado = x-acumulado + x-compra.
        IF x-acumulado > x-cantidad THEN DO:
            /* Quitamos el exceso */
            x-compra = x-compra - (x-acumulado - x-cantidad).
        END.
        CREATE Detalle.
        BUFFER-COPY almdmov TO Detalle.
        PUT UNFORMATTED
            detalle.fchdoc '|'
            almcmov.codpro '|'
            almcmov.nrorf1 '|'
            almcmov.nrorf3 '|'
            detalle.codalm '|'
            detalle.tipmov '|'
            detalle.codmov '|'
            detalle.nroser '|'
            detalle.nrodoc '|'
            detalle.codmat '|'
            x-cantidad '|'
            x-compra   '|'
            detalle.codund '|'
            detalle.factor '|'
            detalle.codmon '|'
            almcmov.tpocmb '|'
            detalle.preuni 
            SKIP.
        IF x-acumulado >= x-cantidad THEN LEAVE.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.

