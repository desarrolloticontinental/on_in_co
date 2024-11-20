FIND ccbcdocu WHERE codcia = 1
    AND coddoc = 'bol'
    AND nrodoc = '221000002'.
DISPLAY codalm nrosal.
FOR EACH ccbddocu OF ccbcdocu:
    DISPLAY codmat candes factor undvta preuni implin
        WITH 1 COL.
    UPDATE factor undvta.
END.

FIND almcmov WHERE almcmov.codcia = 001
    AND almcmov.codref = ccbcdocu.coddoc
    AND almcmov.nroref = ccbcdocu.nrodoc
    AND almcmov.flgest <> 'a'
    NO-LOCK NO-ERROR.
IF AVAILABLE almcmov THEN FOR EACH almdmov OF almcmov:
    DISPLAY almdmov.codmat almdmov.candes almdmov.factor almdmov.codund.
    UPDATE almdmov.factor almdmov.codund.
END.


