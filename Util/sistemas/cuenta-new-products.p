DEF TEMP-TABLE detalle
    FIELD codfam AS CHAR
    FIELD periodo AS INT
    FIELD nromes AS INT
    FIELD contador AS INT
    INDEX llave01 AS PRIMARY codfam periodo nromes.

FOR EACH almmmatg NO-LOCK WHERE codcia = 1
    AND fching >= 01/01/17:
    FIND detalle WHERE detalle.codfam = almmmatg.codfam AND
        periodo = YEAR(fching) AND 
        nromes = MONTH(fching)
        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codfam = almmmatg.codfam
            periodo = YEAR(fching)
            nromes = MONTH(fching).
    END.
    contador = contador + 1.
END.

OUTPUT TO d:\tmp\cuenta-new-product.txt.
PUT UNFORMATTE
    'LINEA|PERIODO|NROMES|CONTADOR' SKIP.
FOR EACH detalle NO-LOCK:
    PUT UNFORMATTED
        detalle.codfam '|'
        periodo '|'
        nromes '|'
        contador
        SKIP.
END.
OUTPUT CLOSE.

