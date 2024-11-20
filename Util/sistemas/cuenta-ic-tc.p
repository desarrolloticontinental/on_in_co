DEF TEMP-TABLE detalle
    FIELD coddiv AS CHAR
    FIELD tipo AS CHAR
    FIELD tc AS CHAR
    FIELD nromes AS INT
    FIELD periodo AS INT
    FIELD contador AS INT
    INDEX llave00 AS PRIMARY coddiv tipo tc periodo nromes.

FOR EACH ccbccaja NO-LOCK WHERE ccbccaja.codcia = 001
    AND ccbccaja.flgcie = 'C'
    AND ccbccaja.fchcie >= 01/01/17:
    FIND detalle WHERE detalle.coddiv = ccbccaja.coddiv
        AND detalle.tipo = ccbccaja.tipo
        AND detalle.tc = ccbccaja.Voucher[4]
        AND detalle.nromes = MONTH(ccbccaja.fchcie)
        AND detalle.periodo = YEAR(ccbccaja.fchcie)
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.coddiv = ccbccaja.coddiv
            detalle.tipo = ccbccaja.tipo
            detalle.tc = ccbccaja.Voucher[4]
            detalle.nromes = MONTH(ccbccaja.fchcie)
            detalle.periodo = YEAR(ccbccaja.fchcie).
    END.
    ASSIGN
        detalle.contador = detalle.contador + 1.
END.
OUTPUT TO d:\tmp\doc-ic-tc.txt.
PUT UNFORMATTED 'DIVISION|TIPO|T.C.|MES|PERIODO|CONTADOR|' SKIP.
FOR EACH detalle NO-LOCK:
    PUT UNFORMATTED
        detalle.coddiv '|'
        detalle.tipo '|'
        detalle.tc '|'
        detalle.nromes '|'
        detalle.periodo '|'
        detalle.contador
        SKIP.
END.
OUTPUT CLOSE.

