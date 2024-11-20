DISABLE TRIGGERS FOR LOAD OF cissac.almmmatg.
FOR EACH integral.almmmatg NO-LOCK WHERE
    integral.almmmatg.codcia = 001
    AND integral.almmmatg.usuario BEGINS 'jpl'
    AND integral.almmmatg.codmat = '044819'.
    /*AND (integral.almmmatg.fching >= TODAY - 2 OR integral.almmmatg.fchact >= TODAY - 2 ):*/
    DISPLAY 
        integral.almmmatg.usuario
        integral.almmmatg.codmat
        integral.almmmatg.fching
        integral.almmmatg.fchact.
    
    FIND cissac.almmmatg OF integral.almmmatg NO-ERROR.
    IF NOT AVAILABLE cissac.almmmatg THEN CREATE cissac.almmmatg.
    BUFFER-COPY integral.almmmatg TO cissac.almmmatg.
    
END.
