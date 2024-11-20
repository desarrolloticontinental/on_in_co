DEF TEMP-TABLE detalle
    FIELD coddiv AS CHAR
    FIELD periodo AS INT
    FIELD nromes AS INT
    FIELD contador AS INT
    INDEX Llave01 AS PRIMARY coddiv periodo nromes.

FOR EACH gn-clie NO-LOCK WHERE codcia = 0
    AND fching >= 01/01/17
    AND fching <= TODAY:
    FIND detalle WHERE detalle.coddiv = gn-clie.coddiv AND
        detalle.periodo = YEAR(gn-clie.fching) AND
        detalle.nromes = MONTH(gn-clie.fching) 
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.coddiv = gn-clie.coddiv 
            detalle.periodo = YEAR(gn-clie.fching)
            detalle.nromes = MONTH(gn-clie.fching) .
    END.
    detalle.contador = detalle.contador + 1.
END.


OUTPUT TO d:\tmp\cuenta-cli.txt.
PUT UNFORMATTED 'DIVISION|PERIODO|MES|CONTADOR' SKIP.
FOR EACH detalle NO-LOCK:
    PUT UNFORMATTED
        detalle.coddiv '|'
        periodo '|'
        nromes '|'
        contador SKIP.
END.
OUTPUT CLOSE.

