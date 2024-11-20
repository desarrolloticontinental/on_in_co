FOR EACH almcincidencia NO-LOCK WHERE codcia = 1 AND flgest = "C":
    DISPLAY AlmCIncidencia.NroDoc AlmCIncidencia.FechaAprobacion.
    PAUSE 0.
    FOR EACH Almcrepo NO-LOCK WHERE almcrepo.CodCia = AlmCIncidencia.CodCia AND
        almcrepo.TipMov = "INC" AND
        almcrepo.CodRef = "INC" AND
        almcrepo.NroRef = AlmCIncidencia.NroControl:
        FOR EACH Faccpedi WHERE Faccpedi.codcia = 001
            AND Faccpedi.coddoc = 'OTR'
            AND Faccpedi.codref = 'R/A'
            AND INTEGER(SUBSTRING(Faccpedi.nroref,1,3)) = Almcrepo.nroser
            AND INTEGER(SUBSTRING(Faccpedi.nroref,4)) = Almcrepo.nrodoc
            AND faccpedi.fchchq = ?:
            faccpedi.fchchq = faccpedi.fchped.
            faccpedi.horchq = faccpedi.hora.
        END.
    END.

END.
