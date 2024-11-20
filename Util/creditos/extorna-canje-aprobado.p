/* extorno de canje por letra */
FIND ccbcmvto WHERE codcia = 1
    AND coddoc = 'cje'
    AND nrodoc = '018000004'
    AND flgest = 'E'.
DISPLAY fchdoc codcli.
IF AVAILABLE ccbcmvto THEN DO:

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
    FOR EACH ccbdmov WHERE ccbdmov.codcia = 1
        AND ccbdmov.codref = ccbcmvto.coddoc
        AND ccbdmov.nroref = ccbcmvto.nrodoc:
        FIND ccbcdocu WHERE ccbcdocu.codcia = 1
            AND ccbcdocu.coddoc = ccbdmov.coddoc
            AND ccbcdocu.nrodoc = ccbdmov.nrodoc.
        ASSIGN
            ccbcdocu.flgest = 'P'
            ccbcdocu.sdoact = ccbcdocu.sdoact + ccbdmov.imptot.
        DISPLAY ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.flgest.
    END.
    ASSIGN
        ccbcmvto.flgest = "P".
END.
ELSE DO:
    MESSAGE "No existe Canje de Letra" VIEW-AS ALERT-BOX WARNING.
END.
