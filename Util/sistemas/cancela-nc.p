FIND ccbcdocu WHERE codcia = 1
    AND coddoc = 'n/c'
    AND nrodoc = '25200019202'.
CREATE ccbdmov.
BUFFER-COPY ccbcdocu TO ccbdmov
    ASSIGN
    ccbdmov.codref = ccbcdocu.coddoc
    ccbdmov.nroref = ccbcdocu.nrodoc
    ccbdmov.imptot = ccbcdocu.sdoact.
ASSIGN
    ccbcdocu.sdoact = 0
    ccbcdocu.flgest = "C"
    ccbcdocu.fchcan = TODAY.
