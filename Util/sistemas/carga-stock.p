
DEF VAR x-almacenes AS CHAR INIT '39,11,05,63,65,69,517'.
DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR k AS INTE NO-UNDO.

INPUT FROM d:\eliam.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    DO k = 1 TO NUM-ENTRIES(x-almacenes):
        FIND almmmate WHERE codcia = 1
            AND codalm = ENTRY(k,x-almacenes)
            AND codmat = SUBSTRING(x-linea,1,6)
            EXCLUSIVE-LOCK.
        IF NOT AVAILABLE almmmate THEN NEXT.
        DISPLAY almmmate.codalm almmmate.codmat.
        PAUSE 0.
        almmmate.stkact = 100000.
    END.
END.
