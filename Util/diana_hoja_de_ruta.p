output to d:\sie\facVShr.txt.
for each di-rutaC where
        di-rutac.codcia = 1 and
        di-rutac.coddiv >= "" /* ccbcdocu.coddiv */ and
        di-rutac.fchdoc >= 07/01/07 and
        di-rutac.codveh = "xg-6773"
        no-lock, each di-rutad of di-rutac no-lock,
        each ccbcdocu where
        ccbcdocu.codcia = di-rutad.codcia and
        ccbcdocu.coddoc = di-rutad.codref and
        ccbcdocu.nrodoc = di-rutad.nroref no-lock:
        display
            di-rutac.nrodoc
            di-rutac.codveh format "x(10)"
            di-rutac.fchdoc
            di-rutaC.fchsal
            di-rutac.horsal
            di-rutac.horret
            ccbcdocu.coddoc
            ccbcdocu.nrodoc
            di-rutad.horlle format "xx:xx"
            di-rutad.horpar format "xx:xx"
            nomcli
            imptot
            Di-RutaD.flgest
            with stream-io width 252.
end.
output close.
