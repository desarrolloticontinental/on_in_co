FOR EACH vtalistamay WHERE codcia = 1
    AND coddiv = '00015':
    FIND almcatvtad WHERE AlmCatVtaD.CodCia = 1
        AND AlmCatVtaD.CodDiv = '00015'
        AND AlmCatVtaD.codmat = vtalistamay.codmat
        AND AlmCatVtaD.CodPro = '51135890'
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almcatvtad THEN DELETE vtalistamay.
END.
