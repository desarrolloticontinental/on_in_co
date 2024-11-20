DEF TEMP-TABLE t-ctas LIKE cb-ctas.

detalle:
FOR EACH cb-dmov WHERE codcia = 1
    AND periodo = 2012
    AND nromes = 00
    AND LENGTH(codcta) < 8:
    FIND cb-ctas WHERE cb-ctas.codcia = 000
        AND cb-ctas.ctaant = cb-dmov.codcta
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cb-ctas THEN DO:
        FIND t-ctas WHERE t-ctas.codcia = 000
            AND t-ctas.codcta = cb-dmov.codcta
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE t-ctas THEN DO:
            CREATE t-ctas.
            t-ctas.codcta = cb-dmov.codcta.
        END.
        NEXT detalle.
    END.
    cb-dmov.codcta = cb-ctas.codcta.
END.
OUTPUT TO c:\tmp\errores.txt.
FOR EACH t-ctas:
    DISPLAY t-ctas.codcta.
END.
OUTPUT CLOSE.

