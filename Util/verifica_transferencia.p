for each almcmov where
    Almcmov.CodCia = 1
    AND Almcmov.CodAlm = "12" AND
    Almcmov.TipMov = "S" AND
    Almcmov.CodMov >= 0 AND
    Almcmov.FlgEst <> "A" AND
    Almcmov.FlgSit = "T" AND
    Almcmov.AlmDes = "11" no-lock,
    each almdmov of almcmov no-lock:
    display
        Almcmov.CodMov
        almdmov.codmat
        almdmov.candes.
    FIND Almmmate WHERE
        Almmmate.CodCia = Almdmov.CodCia AND
         Almmmate.CodAlm = Almcmov.almdes AND 
         Almmmate.CodMat = Almdmov.CODMAT no-lock no-error.
         display
     Almmmate.CodMat
