DEF VAR x-stkact AS DEC FORMAT '->>>9.99' NO-UNDO.
DEF VAR x-linea AS CHAR FORMAT 'x(320)'.
DEF STREAM resumen.
DEF STREAM detalle.

OUTPUT STREAM resumen TO c:\tmp\resumen.txt.
OUTPUT STREAM detalle TO c:\tmp\detalle.txt.
PUT STREAM resumen
    'Codigo|Descripcion|Marca|Familia|Subfamilia|Unidad|Stock'
    SKIP.
PUT STREAM detalle
    'Codigo|Descripcion|Marca|Familia|Subfamilia|Unidad|'.
FOR EACH almacen NO-LOCK WHERE almacen.codcia = 1:
    PUT STREAM detalle almacen.codalm '|'.
END.
PUT STREAM detalle SKIP.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1:
    x-stkact = 0.
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
    PUT STREAM resumen 
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        almmmatg.undstk '|'
        x-stkact
        SKIP.
    PUT STREAM detalle
        almmmatg.codmat '|'
        almmmatg.desmat '|'
        almmmatg.desmar '|'
        almmmatg.codfam '|'
        almmmatg.subfam '|'
        almmmatg.undstk '|'.
    x-linea = ''.
    FOR EACH almacen NO-LOCK WHERE almacen.codcia = 1:
        x-stkact = 0.
        FIND almmmate WHERE almmmate.codcia = 1
            AND almmmate.codalm = almacen.codalm
            AND almmmate.codmat = almmmatg.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE almmmate THEN x-stkact = almmmate.stkact.
        x-linea = x-linea + STRING(x-stkact,'->>>9.99') + '|'.
    END.
    PUT STREAM detalle x-linea SKIP.
END.
OUTPUT STREAM detalle CLOSE.
OUTPUT STREAM resumen CLOSE.

