DEF TEMP-TABLE Detalle
    FIELD coddiv AS CHAR
    FIELD periodo AS INT
    FIELD nromes AS INT
    FIELD contador AS INT
    INDEX llave01 AS PRIMARY coddiv periodo nromes.

FOR EACH ccbccaja NO-LOCK WHERE codcia = 1
    AND coddoc = 'i/c'
    AND fchdoc >= 01/01/17
    AND flgest <> 'A'
    AND CcbCCaja.Voucher[4] > '':
    FIND Detalle WHERE Detalle.coddiv = Ccbccaja.coddiv
        AND Detalle.periodo = YEAR(Ccbccaja.fchdoc)
        AND Detalle.nromes = MONTH(Ccbccaja.fchdoc)
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE Detalle THEN DO:
        CREATE Detalle.
        ASSIGN
            Detalle.coddiv = Ccbccaja.coddiv
            Detalle.periodo = YEAR(Ccbccaja.fchdoc)
            Detalle.nromes = MONTH(Ccbccaja.fchdoc).
    END.
    ASSIGN
        Detalle.contador = Detalle.contador + 1.
END.

OUTPUT TO d:\tmp\cuenta-tc.txt.
PUT UNFORMATTED 'DIVISION|PERIODO|MES|CONTADOR' SKIP.
FOR EACH Detalle NO-LOCK:
    PUT UNFORMATTED 
        Detalle.coddiv '|'
        Detalle.periodo '|'
        Detalle.nromes '|'
        Detalle.contador SKIP.
END.
