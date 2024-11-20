DEF VAR s-codcia AS INTE INIT 001.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia AND
    faccpedi.coddoc = 'PED' AND
    faccpedi.coddiv = gn-divi.coddiv AND
    faccpedi.fchped >= 01/01/2023 AND
    faccpedi.flgest <> 'A',
    EACH facdpedi OF faccpedi NO-LOCK:
    IF facdpedi.canped * facdpedi.preuni <> facdpedi.implin THEN
        DISPLAY facdpedi.coddoc facdpedi.nroped
        facdpedi.codmat.
END.
