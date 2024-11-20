DEF VAR x-linea AS CHAR.
DISABLE TRIGGERS FOR LOAD OF almmmate.
FOR EACH almmmate WHERE codcia = 001
    AND codalm = '65'
    AND ABS(vinmn1) + ABS(vinmn2) + ABS(stkmin)  > 0:
    ASSIGN
        almmmate.vinmn1 = 0
        almmmate.stkmin = 0
        almmmate.vctmn1 = 0
        almmmate.vctmn2 = 0.
        /*almmmate.stkmax = 0.*/
END.

INPUT FROM d:\tmp\alm65.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND almmmate WHERE almmmate.codcia = 001
        AND almmmate.codalm = '65'
        AND almmmate.codmat = SUBSTRING(x-linea,1,6)
        NO-ERROR.
    IF AVAILABLE almmmate THEN DO:
        ASSIGN
            almmmate.vinmn1 = DECIMAL(SUBSTRING(x-linea,11,10))
            almmmate.stkmin = almmmate.vinmn1
            almmmate.vctmn1 = almmmate.vinmn1
            almmmate.vctmn2 = almmmate.vinmn1.
            /*almmmate.stkmax = DECIMAL(SUBSTRING(x-linea,21,10)).*/
    END.

END.
INPUT CLOSE.

