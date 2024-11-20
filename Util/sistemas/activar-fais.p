DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbdcaja.

DEF VAR x-linea AS CHAR.
INPUT FROM d:\tmp\fais.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND ccbcdocu WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.coddoc = 'FAI'
        AND ccbcdocu.nrodoc = x-linea
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbcdocu THEN NEXT.
    FOR EACH ccbdcaja WHERE ccbdcaja.codcia = 1
        AND ccbdcaja.codref = ccbcdocu.coddoc 
        AND ccbdcaja.nroref = ccbcdocu.nrodoc.
        DELETE ccbdcaja.
    END.
    ASSIGN
        ccbcdocu.flgest = 'P'
        ccbcdocu.sdoact = ccbcdocu.imptot.
END.
