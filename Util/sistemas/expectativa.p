DEF VAR x-md AS DECI.
OUTPUT TO d:\espectativa.txt.
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
        (IF almmmatg.dsctoprom[1] = 2 THEN '$' ELSE 'S/') '|'
        almmmatg.dsctoprom[2] '|'
        x-md '|'
        'SI' '|'
        (IF almmmatg.monvta = 2 THEN '$' ELSE 'S/') '|'
        vtalistamay.preofi
        SKIP.
END.
