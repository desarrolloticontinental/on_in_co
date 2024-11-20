OUTPUT TO c:\tmp\logprecios.txt.
FOR EACH almmmatg NO-LOCK WHERE codcia = 001,
    EACH logmmatg OF almmmatg WHERE logdate >= 01/01/2012:
    PUT UNFORMATTED
        logmmatg.codmat '|'
        logmmatg.desmat '|'
        logmmatg.undstk '|'
        logmmatg.monvta '|'
        logmmatg.tpocmb '|'
        logmmatg.preofi '|'
        logmmatg.CHR__01 '|'
        logmmatg.ctolis '|'
        logmmatg.ctotot '|'
        logmmatg.prevta[1] '|'
        logmmatg.prevta[2] '|'
        logmmatg.unda '|'
        logmmatg.prevta[3] '|'
        logmmatg.undb '|'
        logmmatg.prevta[4] '|'
        logmmatg.undc '|'
        logmmatg.logdate '|'
        logmmatg.logtime '|'
        logmmatg.loguser
        SKIP.
END.
OUTPUT CLOSE.
