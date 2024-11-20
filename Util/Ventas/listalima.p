OUTPUT TO c:\tmp\listalima.txt.
FOR EACH almmmatg NO-LOCK WHERE codcia = 001
    AND tpoart <> 'D':
    DISPLAY
        codmat 
        desmat FORMAT 'x(60)'
        desmar 
        monvta 
        ctotot
        tpocmb 
        preofi 
        CHR__01 FORMAT 'x(6)'
        WITH STREAM-IO NO-BOX WIDTH 320.


END.
