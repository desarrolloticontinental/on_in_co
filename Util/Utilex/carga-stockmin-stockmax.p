DISABLE TRIGGERS FOR LOAD OF almmmate.

FOR EACH almmmate WHERE codcia = 1 AND codalm = '10a':
    FIND LAST almstkal WHERE almstkal.codcia = 1
        AND almstkal.codalm = almmmate.codalm
        AND almstkal.codmat = almmmate.codmat
        AND almstkal.fecha <= 02/14/2014
        NO-LOCK NO-ERROR.
    ASSIGN
        almmmate.stkmin = 0
        almmmate.stkmax = 0.
    IF AVAILABLE almstkal AND almstkal.stkact > 0
        THEN ASSIGN
                almmmate.stkmin = almstkal.stkact
                almmmate.stkmax = 1.
END.
