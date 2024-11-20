DEF VAR X AS CHAR.
DISABLE TRIGGERS FOR LOAD OF almmmate.

INPUT FROM c:\tmp\repos2.prn.
REPEAT :
    IMPORT UNFORMATTED X.
    IF X = '' THEN LEAVE.
    FIND almmmate WHERE codcia = 1
        AND codalm = SUBSTRING(X,1,5)
        AND codmat = SUBSTRING(X,6,10)
        NO-ERROR.
    stkmin = DECIMAL(SUBSTRING(X,16,10)) .
    stkmax = DECIMAL(SUBSTRING(X,26,10)) .
END.
INPUT CLOSE.

