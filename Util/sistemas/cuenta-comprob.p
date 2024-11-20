DEF VAR x-coddoc AS CHAR INIT 'FAC,BOL,N/C,N/D,G/R'.
DEF VAR k AS INT.

DEF TEMP-TABLE Detalle
    FIELD coddoc AS CHAR
    FIELD coddiv AS CHAR
    FIELD nromes AS INT
    FIELD periodo AS INT
    FIELD contador AS INT
    INDEX llave01 AS PRIMARY coddoc coddiv periodo nromes .

DO k = 1 TO NUM-ENTRIES(x-coddoc):
    FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
        AND coddoc = ENTRY(k,x-coddoc)
        AND fchdoc >= 01/01/17:
        FIND detalle WHERE detalle.coddoc = ccbcdocu.coddoc
            AND detalle.coddiv = ccbcdocu.coddiv
            AND detalle.nromes = MONTH(ccbcdocu.fchdoc)
            AND detalle.periodo = YEAR(ccbcdocu.fchdoc)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            ASSIGN
                detalle.coddoc = ccbcdocu.coddoc
                detalle.coddiv = ccbcdocu.coddiv
                detalle.nromes = MONTH(ccbcdocu.fchdoc)
                detalle.periodo = YEAR(ccbcdocu.fchdoc).
        END.
        detalle.contador = detalle.contador + 1.
    END.
END.

OUTPUT TO d:\tmp\comprb-ventas.txt.
PUT UNFORMATTED 'DOC|DIVISION|PERIODO|MES|CONTADOR' SKIP.
FOR EACH detalle NO-LOCK:
    PUT UNFORMATTED
        detalle.coddoc '|'
        detalle.coddiv '|'
        detalle.periodo '|'
        detalle.nromes '|'
        detalle.contador
        SKIP.

END.
OUTPUT CLOSE
