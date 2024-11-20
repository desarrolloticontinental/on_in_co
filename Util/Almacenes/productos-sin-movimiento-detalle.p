DEF VAR x-stkact AS DEC NO-UNDO.
DEF VAR x-stkcol AS DEC NO-UNDO.
DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-almacenes AS CHAR NO-UNDO.
DEF VAR x-Titulo AS CHAR NO-UNDO.
DEF VAR x-Titulo2 AS CHAR NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR sw AS LOG NO-UNDO.

DEF TEMP-TABLE detalle
    FIELD linea AS CHAR
    FIELD stkact AS DEC EXTENT 400
    FIELD ultmov AS CHAR EXTENT 400
    FIELD fchmov AS DATE EXTENT 400
    FIELD ctopro AS DEC EXTENT 400.

x-Titulo = 'Codigo|Descripcion|Marca|Familia|Subfamilia|Unidad|'.
x-Almacenes = ''.

FOR EACH almmmatg NO-LOCK WHERE codcia = 1:
    x-stkact = 0.
    sw = NO.
    FOR EACH almmmate OF almmmatg NO-LOCK:
        x-stkact = x-stkact + almmmate.stkact.
    END.
    IF x-stkact <= 0 THEN NEXT.
    FIND LAST almdmov USE-INDEX almd07
        WHERE almdmov.codcia = 1
        AND almdmov.codmat = almmmatg.codmat
        AND almdmov.codmov <> 03
        NO-LOCK NO-ERROR.
    IF almdmov.fchdoc > ( TODAY  - (365 * 2) ) THEN NEXT.
    x-linea = almmmatg.codmat + '|' +
        almmmatg.desmat + '|' +
        almmmatg.desmar + '|' +
        almmmatg.codfam + '|' +
        almmmatg.subfam + '|' +
        almmmatg.undstk + '|'.
    FOR EACH almacen NO-LOCK WHERE almacen.codcia = 1:
        x-stkact = 0.
        FIND almmmate WHERE almmmate.codcia = 1
            AND almmmate.codalm = almacen.codalm
            AND almmmate.codmat = almmmatg.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE almmmate THEN x-stkact = almmmate.stkact.
        IF x-stkact > 0 THEN DO:
            IF sw = NO THEN DO:
                CREATE detalle.
                detalle.linea = x-linea.
                sw = YES.
            END.
            IF LOOKUP(almacen.codalm, x-Almacenes, '|') = 0 THEN DO:
                x-Almacenes = x-Almacenes + almacen.codalm + '|'.
            END.
            j = LOOKUP(almacen.codalm, x-Almacenes, '|').
            detalle.stkact[j] = x-stkact.
            /* ultimo movimiento por el almacen */
            FIND LAST almdmov USE-INDEX almd03
                WHERE almdmov.codcia = 1
                AND almdmov.codalm = almacen.codalm
                AND almdmov.codmat = almmmatg.codmat
                AND almdmov.codmov <> 03
                NO-LOCK NO-ERROR.
            IF AVAILABLE almdmov THEN DO:
                detalle.ultmov[j] = STRING(almdmov.tipmov, 'XX') + STRING(almdmov.codmov, '99').
                detalle.fchmov[j] = almdmov.fchdoc.
                FIND LAST almstkge WHERE almstkge.codcia = 1
                    AND almstkge.codmat = almmmatg.codmat
                    AND almstkge.fecha <= almdmov.fchdoc
                    NO-LOCK NO-ERROR.
                IF AVAILABLE almstkge THEN detalle.ctopro[j] = AlmStkge.CtoUni.
            END.
        END.
    END.
END.

x-Titulo2 = ''.
DO j = 1 TO NUM-ENTRIES(x-Almacenes, '|'):
    x-Titulo2 = x-Titulo2 + 'Alm.' + ENTRY(j, x-Almacenes, '|') + '|' + 
                'Mov.|Fch.Mov|Costo|'.
END.
x-Titulo = x-Titulo + x-Titulo2.

OUTPUT TO c:\tmp\detalle.txt.
PUT x-Titulo FORMAT 'x(600)' SKIP.
FOR EACH detalle:
    x-linea = TRIM(detalle.linea).
    DO j = 1 TO NUM-ENTRIES(x-Almacenes, '|'):
        x-linea = x-linea + STRING(detalle.stkact[j], '>>>9.99') + '|' +
            STRING(detalle.ultmov[j], 'x(5)') + '|'.
        IF detalle.fchmov[j] = ? 
        THEN x-linea = x-linea + " " + '|'.
        ELSE x-linea = x-linea + STRING(detalle.fchmov[j], '99/99/9999') + '|'.
        x-linea = x-linea + STRING(detalle.ctopro[j], '->>>>>9.99') + '|'.
    END.
    PUT x-linea FORMAT 'x(600)' SKIP.
END.
OUTPUT CLOSE.

