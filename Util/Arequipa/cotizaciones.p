DEF VAR xImpAnt AS DEC.
DEF VAR xImpDep AS DEC.
DEF VAR xImpNC  AS DEC.
DEF VAR xdistrito AS CHAR.
DEF STREAM reporte.

OUTPUT STREAM reporte TO d:\tmp\arequipa.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1 AND coddoc = 'cot'
    AND coddiv = '10060'
    AND fchped >= 01/07/17 
    AND flgest <> 'a',
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 000
    AND gn-clie.codcli = faccpedi.codcli:
    DISPLAY faccpedi.coddoc faccpedi.nroped faccpedi.fchped.
    PAUSE 0.
    ximpant = 0.
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = faccpedi.codcia
        AND ccbcdocu.coddoc = 'A/R'
        AND ccbcdocu.flgest <> 'A'
        AND ccbcdocu.codcli = faccpedi.codcli
        AND ccbcdocu.fchdoc >= 01/07/17:
        ximpant = ximpant + ccbcdocu.imptot.
    END.
    ximpdep = 0.
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = faccpedi.codcia
        AND ccbcdocu.coddoc = 'BD'
        AND ccbcdocu.flgest <> 'A'
        AND ccbcdocu.codcli = faccpedi.codcli
        AND ccbcdocu.fchdoc >= 01/07/17:
        ximpdep = ximpdep + ccbcdocu.imptot.
    END.
    ximpnc = 0.
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = faccpedi.codcia
        AND ccbcdocu.coddoc = 'N/C'
        AND ccbcdocu.flgest <> 'A'
        AND ccbcdocu.codcli = faccpedi.codcli
        AND ccbcdocu.fchdoc >= 01/07/17:
        ximpnc = ximpnc + ccbcdocu.imptot.
    END.
    FIND TabDistr WHERE TabDistr.CodProvi = gn-clie.CodProv 
        AND TabDistr.CodDepto = gn-clie.CodDept 
        AND TabDistr.CodDistr = gn-clie.CodDist
        NO-LOCK NO-ERROR.
        
    PUT STREAM reporte UNFORMATTED
        faccpedi.coddoc '|'
        faccpedi.nroped '|'
        faccpedi.fchped '|'
        faccpedi.codcli '|'
        faccpedi.nomcli '|'
        faccpedi.imptot '|'
        faccpedi.fmapgo '|'
        ximpant '|'
        ximpdep '|'
        ximpnc '|'
        (IF AVAILABLE TabDistr THEN TabDistr.NomDistr ELSE '')
        SKIP.
END.
