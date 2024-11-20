/* extorno de canje por letra */
FIND ccbcmvto WHERE codcia = 1
    AND coddoc = 'cje'
    AND nrodoc = '160000708'
    AND flgest = 'A'.
DISPLAY glosa.
glosa = ''.
FOR EACH ccbdcaja WHERE ccbdcaja.codcia = 1
    AND ccbdcaja.coddoc = ccbcmvto.coddoc
    AND ccbdcaja.nrodoc = ccbcmvto.nrodoc.
    DISPLAY ccbdcaja.codref ccbdcaja.nroref codmon imptot.
    FIND ccbcdocu WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.coddoc = ccbdcaja.codref
        AND ccbcdocu.nrodoc = ccbdcaja.nroref.
    ASSIGN
        ccbcdocu.flgest = 'P'
        Ccbcdocu.FlgSit = 'X'
        ccbcdocu.fchcan = ?
        ccbcdocu.sdoact = ccbcdocu.sdoact + ccbdcaja.imptot.
    DELETE ccbdcaja.
END.

FOR EACH ccbcdocu WHERE ccbcdocu.codcia = 001
    AND ccbcdocu.coddoc = 'let'
    AND ccbcdocu.codref = ccbcmvto.coddoc
    AND ccbcdocu.nroref = ccbcmvto.nrodoc.
    DISPLAY ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.flgest.
    ccbcdocu.flgest = "X".
END.
ASSIGN
    ccbcmvto.flgest = "P".
