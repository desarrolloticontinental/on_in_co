OUTPUT TO c:\tmp\listautilex.txt.
FOR EACH vtalistamingn NO-LOCK WHERE vtalistamingn.codcia = 001,
    FIRST almmmatg OF vtalistamingn NO-LOCK WHERE tpoart <> 'D':
    DISPLAY
        vtalistamingn.codmat 
        almmmatg.desmat FORMAT 'x(60)'
        almmmatg.desmar 
        vtalistamingn.monvta 
        vtalistamingn.tpocmb 
        vtalistamingn.preofi 
        vtalistamingn.CHR__01 FORMAT 'x(6)'
        WITH STREAM-IO NO-BOX WIDTH 320.


END.
