DEF VAR x-tipmov AS CHAR INIT 'I,S' NO-UNDO.
DEF VAR k AS INT.
DEF TEMP-TABLE detalle
    FIELD codalm AS CHAR
    FIELD tipmov AS CHAR
    FIELD codmov AS INT
    FIELD periodo AS INT
    FIELD nromes AS INT
    FIELD contador AS INT
    INDEX llave01 AS PRIMARY codalm tipmov codmov periodo nromes.
DO k = 1 TO 2:
    FOR EACH almacen NO-LOCK WHERE codcia = 1,
        EACH almcmov NO-LOCK WHERE almcmov.codcia = 1
        AND almcmov.codalm = almacen.codalm
        AND almcmov.tipmov = ENTRY(k,x-tipmov)
        AND almcmov.fchdoc >= 01/01/17:
        FIND detalle WHERE detalle.codalm = almcmov.codalm
            AND detalle.tipmov = almcmov.tipmov
            AND detalle.codmov = almcmov.codmov
            AND detalle.periodo = YEAR(almcmov.fchdoc)
            AND detalle.nromes = MONTH(almcmov.fchdoc)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            ASSIGN
                detalle.codalm = almcmov.codalm
                detalle.tipmov = almcmov.tipmov
                detalle.codmov = almcmov.codmov
                detalle.periodo = YEAR(almcmov.fchdoc)
                detalle.nromes = MONTH(almcmov.fchdoc).
        END.
        detalle.contador = detalle.contador + 1.
    END.
END.

OUTPUT TO d:\tmp\doc-alm.txt.
PUT UNFORMATTED 'ALM|TIPO|MOV|PERIODO|MES|CONTADOR' SKIP.
FOR EACH detalle NO-LOCK.
    PUT UNFORMATTED
        detalle.codalm '|'
        detalle.tipmov '|'
        detalle.codmov '|'
        detalle.periodo '|'
        detalle.nromes '|'
        detalle.contador
        SKIP.
END.
OUTPUT CLOSE.


