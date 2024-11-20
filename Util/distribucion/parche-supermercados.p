FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'fac'
    AND nrodoc >= '26300000032'
    AND nrodoc <= '26300000145'
    AND nroord = '7101112994',
    EACH controlod WHERE controlod.codcia = 1
    AND controlod.coddoc = ccbcdocu.libre_c01
    AND controlod.nrodoc = ccbcdocu.libre_c02:
    DISPLAY ccbcdocu.nrodoc ccbcdocu.codcli ccbcdocu.nroord
        controlod.nrofac controlod.usrfac controlod.fchfac
        WITH STREAM-IO WIDTH 320.
    PAUSE 0.
    controlod.nrofac = ccbcdocu.nrodoc.
    controlod.usrfac = ccbcdocu.usuario.
    controlod.fchfac = ccbcdocu.fchdoc.
END.
