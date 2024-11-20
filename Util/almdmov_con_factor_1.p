OUTPUT TO d:\Tmp\factor.txt.
FOR EACH almcmov WHERE
    Almcmov.CodCia = 1 AND
    Almcmov.CodAlm >= "" AND
    Almcmov.TipMov = "S" AND
    Almcmov.CodMov = 02 AND
    Almcmov.NroSer >= 0 AND
    Almcmov.NroDoc >= 0 AND
    Almcmov.FchDoc >= 10/01/09 NO-LOCK,
    EACH Almdmov OF almcmov NO-LOCK:
    IF Almdmov.Factor <> 1 THEN NEXT.
    FOR almmmatg
        FIELDS (almmmatg.CodCia almmmatg.CodMat Almmmatg.UndBas Almmmatg.FacEqu)
        WHERE almmmatg.CodCia = Almdmov.CodCia
        AND almmmatg.CodMat = Almdmov.CodMat
        NO-LOCK:
    END.
    IF AVAILABLE almmmatg AND
        almmmatg.UndBas <> Almdmov.CodUnd THEN DO:
        FIND Almtconv WHERE
            Almtconv.CodUnid = Almmmatg.UndBas AND  
            Almtconv.Codalter = Almdmov.CodUnd NO-LOCK NO-ERROR.
        /*
        IF AVAILABLE Almtconv THEN
            F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
            */
        /*
        FIND almcmov OF almdmov NO-LOCK NO-ERROR.
        */
        DISPLAY
            Almdmov.FchDoc
            almdmov.codalm
            Almdmov.NroSer
            Almcmov.NroDoc
            Almdmov.CodMat
            Almmmatg.UndBas COLUMN-LABEL "Unidad!Catálogo"
            Almdmov.CodUnd  COLUMN-LABEL "Unidad!Almdmov"
            Almdmov.CanDes
            Almdmov.Factor  COLUMN-LABEL "Factor!Almdmov"
            Almtconv.Equival WHEN AVAILABLE Almtconv
            Almcmov.CodDoc
            Almcmov.CodMov
            Almcmov.CodRef
            Almcmov.NroFac
            Almcmov.NroRef
            Almcmov.NroRf1
            Almcmov.NroRf2
            Almcmov.NroRf3
            WITH STREAM-IO WIDTH 230.
    END.
END.
OUTPUT CLOSE.

MESSAGE "Ya Terminó"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
