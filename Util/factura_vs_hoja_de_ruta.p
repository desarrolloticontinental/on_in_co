
DEF VAR L AS LOGICAL.
def var mon as character.
output to d:\sie\facVShr.txt.
FOR EACH ccbcdocu WHERE
    ccbcdocu.codcia = 1 and
    ccbcdocu.coddiv = "00015" and
    ccbcdocu.coddoc = "FAC" and
    ccbcdocu.fchdoc >= 01/01/08 AND
    ccbcdocu.flgest <> "A" no-lock
    break by ccbcdocu.coddoc by ccbcdocu.nrodoc by ccbcdocu.fchdoc:
    mon = if ccbcdocu.codmon = 1 then "S/." else "US$".
    PUT UNFORMATTED
        ccbcdocu.coddoc "|"
        ccbcdocu.nrodoc "|"
        ccbcdocu.fchdoc "|"
        ccbcdocu.ruc "|"
        ccbcdocu.nomcli "|"
        mon "|"
        ccbcdocu.imptot "|".
    L = FALSE.
    FOR each di-rutad where
        di-rutad.codcia = ccbcdocu.codcia and
        di-rutad.coddiv >= "" /* ccbcdocu.coddiv */ and
        di-rutad.codref = ccbcdocu.coddoc and
        di-rutad.nroref = ccbcdocu.nrodoc no-lock,
        FIRST di-rutaC OF di-rutad NO-LOCK:
        IF NOT L THEN do:
            PUT UNFORMATTED   
                di-rutad.coddoc "|"
                di-rutad.nrodoc "|"
                di-rutaC.fchsal "|" skip.
            l = true.
        end.
        else
            PUT UNFORMATTED
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                "|"
                di-rutad.coddoc "|"
                di-rutad.nrodoc "|"
                di-rutaC.fchsal "|" skip.
    END.
    IF NOT L THEN PUT "" SKIP.
end.
output close.
