
output to d:\sie\facVShr2.txt.

for each di-rutaC where
        di-rutac.codcia = 1 and
        di-rutac.coddiv >= "" /* ccbcdocu.coddiv */ and
        di-rutac.fchdoc >= 07/01/07 and
        di-rutac.codveh = "xg-6773"
        no-lock, each di-rutag of di-rutac no-lock,
        EACH Almcmov WHERE Almcmov.CodCia = Di-RutaG.CodCia
        AND Almcmov.CodAlm = Di-RutaG.CodAlm
        AND Almcmov.TipMov = Di-RutaG.Tipmov
        AND Almcmov.CodMov = Di-RutaG.Codmov
        AND Almcmov.NroSer = Di-RutaG.serref
        AND Almcmov.NroDoc = Di-RutaG.nroref NO-LOCK:

        display
            di-rutac.nrodoc
            di-rutac.codveh format "x(10)"
            di-rutac.fchdoc
            di-rutaC.fchsal
            di-rutac.horsal format "xx:xx"
            di-rutac.horret format "xx:xx"
            almcmov.almdes
            almcmov.fchdoc
            di-rutag.horlle format "xx:xx"
            di-rutag.horpar format "xx:xx"
            Di-Rutag.flgest
            with stream-io width 252.
end.
output close.
