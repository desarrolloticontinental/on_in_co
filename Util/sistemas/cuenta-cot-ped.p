DEF VAR x-coddoc AS CHAR INIT 'COT,PED,P/M' NO-UNDO.
DEF VAR k AS INT NO-UNDO.

DEF TEMP-TABLE detalle
    FIELD coddiv AS CHAR
    FIELD coddoc AS CHAR
    FIELD nromes AS INT
    FIELD periodo AS INT
    FIELD contador AS INT
    INDEX llave00 AS PRIMARY coddiv  coddoc periodo nromes.

DO k = 1 TO NUM-ENTRIES(x-coddoc):
    FOR EACH faccpedi NO-LOCK WHERE codcia = 1
        AND coddoc = ENTRY(k, x-coddoc)
        AND fchped >= 01/01/17:
        FIND detalle WHERE detalle.coddiv = faccpedi.coddiv
            AND detalle.coddoc = faccpedi.coddoc
            AND detalle.nromes = MONTH(faccpedi.fchped)
            AND detalle.periodo = YEAR(faccpedi.fchped)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            ASSIGN
                detalle.coddiv = faccpedi.coddiv
                detalle.coddoc = faccpedi.coddoc
                detalle.nromes = MONTH(faccpedi.fchped)
                detalle.periodo = YEAR(faccpedi.fchped).
        END.
        ASSIGN
            detalle.contador = detalle.contador + 1.
    END.
END.
OUTPUT TO d:\tmp\doc-ventas.txt.
PUT UNFORMATTED 'DIVISION|DOC|MES|PERIODO|CONTADOR|' SKIP.
FOR EACH detalle NO-LOCK:
    PUT UNFORMATTED
        detalle.coddiv '|'
        detalle.coddoc '|'
        detalle.nromes '|'
        detalle.periodo '|'
        detalle.contador
        SKIP.
END.
OUTPUT CLOSE.

