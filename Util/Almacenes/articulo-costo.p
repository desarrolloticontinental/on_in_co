/* Códigos y costos */
OUTPUT TO c:\tmp\articulo-costo.txt.
PUT UNFORMATTED
    'ARTICULO|COSTO C/IGV S/.'
    SKIP.
FOR EACH almmmatg NO-LOCK WHERE codcia = 1:
    PUT UNFORMATTED
        almmmatg.codmat '|'
        (IF almmmatg.monvta = 1 THEN almmmatg.ctotot ELSE almmmatg.ctotot * almmmatg.tpocmb)
        SKIP.
END.
OUTPUT CLOSE.
