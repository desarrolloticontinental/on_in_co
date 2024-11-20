DEF VAR pre-lis AS DEC.
DEF VAR mrglis AS DEC.

FOR EACH almmmatg WHERE codcia = 1
    AND tpoart <> 'd'
    AND prevta[1] = 0
    AND CHR__02 = 'T'
    AND preofi > 0:
    pre-lis = preofi / 1.0739.
    MrgLis = ((pre-lis / Almmmatg.Ctotot) - 1 ) * 100. 
    ASSIGN
        almmmatg.prevta[1] = pre-lis
        almmmatg.mrguti = mrglis.
    /*
    DISPLAY ctotot preofi prevta[1] pre-lis mrguti mrglis
        WITH STREAM-IO NO-LABELS.
    */
END.
