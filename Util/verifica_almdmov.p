
for each almdmov where
    codcia = 1 and
    tipmov = "i" and
    /*
    codmov = 17 and
    */
    codalm = "11" and
    nrodoc >= 41174 AND
    nrodoc <= 41202
    /*
    fchdoc >= 04/01/08 and
    fchdoc <= 04/01/08 and
    */
    /*
    codmat = "033229"
    (nrodoc = 37498 or
    nrodoc = 37499 or
    nrodoc = 37506 or
    nrodoc = 37507 or
    nrodoc = 37508 or
    nrodoc = 37509 or
    nrodoc = 37510
    )
    */
    ,
    FIRST Almtmovm OF Almdmov NO-LOCK:

    ASSIGN fchdoc = 10/31/09.

    FIND Almcmov WHERE
        Almcmov.CodCia = Almdmov.codcia AND
        Almcmov.CodAlm = Almdmov.codalm AND
        Almcmov.TipMov = Almdmov.tipmov AND
        Almcmov.CodMov = Almdmov.codmov AND
        Almcmov.NroDoc = Almdmov.nrodoc NO-ERROR.
    IF NOT AVAILABLE Almcmov THEN NEXT.

    ASSIGN Almcmov.fchdoc = 10/31/09.

    DISPLAY
        almdmov.codmov
        Almtmovm.Desmov
        Almdmov.nrodoc
        Almdmov.codmat
        Almdmov.candes
        Almdmov.codalm
        Almdmov.fchdoc
        with stream-io width 132.
end.
