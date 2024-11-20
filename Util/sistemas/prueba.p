FOR EACH gn-divi NO-LOCK WHERE codcia = 1,
    EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 1
    AND faccpedi.coddiv = gn-divi.coddiv
    AND faccpedi.flgest = "C"
    AND faccpedi.coddoc = "OTR"
    AND faccpedi.fchped >= 01/01/2023:
    FIND FIRST almcmov WHERE almcmov.codcia = Faccpedi.codcia AND
        almcmov.CodRef = Faccpedi.coddoc AND
        almcmov.NroRef = Faccpedi.nroped AND
        almcmov.codalm = Faccpedi.codalm AND
        almcmov.flgest <> 'A'
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almcmov OR 
        NOT (almcmov.tipmov = "S" AND
             almcmov.codmov = 03 AND
             almcmov.flgest <> "A" AND
             almcmov.FlgSit = "T")
        THEN DO:
        NEXT.
    END.
    DISPLAY faccpedi.coddoc faccpedi.nroped faccpedi.codcli
        almcmov.tipmov almcmov.codmov
        WITH STREAM-IO NO-BOX WIDTH 100.

END.
