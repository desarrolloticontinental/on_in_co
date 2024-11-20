DEF VAR x-md AS DECI.
OUTPUT TO d:\porcentaje.txt.
FOR EACH vtalistamay NO-LOCK WHERE codcia = 1
    AND coddiv = '20015',
    FIRST almmmatg OF vtalistamay NO-LOCK WHERE almmmatg.codfam = '014':
    IF almmmatg.monvta = almmmatg.dsctoprom[1] THEN DO:
        x-md = (vtalistamay.preofi - almmmatg.dsctoprom[2]) / almmmatg.dsctoprom[2].
    END.
    ELSE IF almmmatg.monvta = 1 THEN DO:
        x-md = ((vtalistamay.preofi / almmmatg.tpocmb) - almmmatg.dsctoprom[2]) / almmmatg.dsctoprom[2].
    END.
    ELSE DO:
        x-md = ((vtalistamay.preofi / almmmatg.tpocmb) - almmmatg.dsctoprom[2]) / almmmatg.dsctoprom[2].
    END.
    PUT UNFORMATTED
        vtalistamay.codmat '|'
        '11' '|'
        x-md '|'
        '0.00' 
        SKIP.
END.
