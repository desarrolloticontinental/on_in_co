DEF VAR f-factor AS DEC NO-UNDO.
FOR EACH almdmov WHERE codcia = 1
    AND fchdoc >= 01/01/09 NO-LOCK,
    FIRST almmmatg OF almdmov NO-LOCK:
    FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
        AND Almtconv.Codalter = Almdmov.codund
        NO-LOCK NO-ERROR.
    f-factor = 1.
    IF AVAILABLE Almtconv THEN f-Factor = Almtconv.Equival / Almmmatg.FacEqu.
    IF almdmov.factor <> f-factor THEN DO:
        DISPLAY
            almdmov.codmat
            almdmov.codalm
            almdmov.tipmov
            almdmov.codmov
            almdmov.fchdoc
            almdmov.nroser
            almdmov.nrodoc
            almdmov.candes
            almdmov.factor
            f-factor
            almdmov.codund
            WITH STREAM-IO NO-BOX WIDTH 200.
    END.
END.

