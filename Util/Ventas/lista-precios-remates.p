OUTPUT TO c:\tmp\remates.txt.
FOR EACH vtatabla NO-LOCK WHERE codcia = 1
    AND tabla = 'REMATES',
    FIRST almmmatg no-lock WHERE almmmatg.codcia = 001
    AND almmmatg.codmat = llave_c1:
    DISPLAY
        almmmatg.codmat
        almmmatg.desmat
        almmmatg.desmar
        (IF almmmatg.monvta = 1 THEN 'S/.' ELSE 'US$')
        almmmatg.tpocmb
        almmmatg.undbas
        almmmatg.CHR__01
        almmmatg.prevta[1]
        vtatabla.valor[1]
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
