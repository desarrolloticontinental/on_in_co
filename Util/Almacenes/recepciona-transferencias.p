DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-codalm AS CHAR INIT '11'.
DEF VAR s-codmov AS INT INIT 03.
DEF VAR r-Rowid AS ROWID.
DEF VAR x-NroDoc AS INT NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'ADMIN'.

DEF BUFFER cmov FOR almcmov.
DEF BUFFER ITEM FOR almdmov.

FOR EACH cmov WHERE cmov.codcia = 1
    AND cmov.codalm = '11e'
    AND cmov.tipmov = 's'
    AND cmov.codmov = 03
    AND cmov.almdes = s-codalm
    AND cmov.flgest <> 'a'
    AND cmov.fchdoc = TODAY
    AND cmov.flgsit = 't'
    AND usuario = 'admin':
    DISPLAY usuario nroser nrodoc flgest flgsit.
    PAUSE 0.
    FIND Almtdocm WHERE
        Almtdocm.CodCia = S-CODCIA AND
        Almtdocm.CodAlm = S-CODALM AND
        Almtdocm.TipMov = "I" AND
        Almtdocm.CodMov = S-CODMOV
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.

    CREATE Almcmov.
    ASSIGN 
        x-Nrodoc  = Almtdocm.NroDoc.
    REPEAT:
        IF NOT CAN-FIND(FIRST Almcmov WHERE Almcmov.codcia = Almtdocm.CodCia
                        AND Almcmov.CodAlm = Almtdocm.CodAlm 
                        AND Almcmov.TipMov = Almtdocm.TipMov
                        AND Almcmov.CodMov = Almtdocm.CodMov
                        AND Almcmov.NroSer = 000
                        AND Almcmov.NroDoc = x-NroDoc
                        NO-LOCK)
            THEN LEAVE.
        ASSIGN
            x-NroDoc = x-NroDoc + 1.
    END.
    ASSIGN
        Almcmov.NroDoc  = x-NroDoc
        Almcmov.CodCia  = Almtdocm.CodCia 
        Almcmov.CodAlm  = Almtdocm.CodAlm 
        Almcmov.TipMov  = Almtdocm.TipMov 
        Almcmov.CodMov  = Almtdocm.CodMov 
        Almcmov.NroSer  = 000
        Almcmov.FlgSit  = ""
        Almcmov.FchDoc  = TODAY
        Almcmov.HorRcp  = STRING(TIME,"HH:MM:SS")
        /*Almcmov.NomRef  = F-NomDes:SCREEN-VALUE IN FRAME {&FRAME-NAME}            */
        Almcmov.Nrorf1  = STRING(CMOV.NroSer,"999") + STRING(CMOV.NroDoc)
        /*Almcmov.Nrorf2  = Almcmov.Nrorf2:SCREEN-VALUE IN FRAME {&FRAME-NAME}*/
        Almcmov.Nrorf3  = CMOV.nrorf3
        Almcmov.AlmDes  = CMOV.CodAlm
        Almcmov.usuario = S-USER-ID. 
    ASSIGN
        Almtdocm.NroDoc = x-NroDoc + 1.

    RUN Genera-Detalle.
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    ASSIGN 
        CMOV.FlgSit  = "R" 
        CMOV.HorRcp  = STRING(TIME,"HH:MM:SS")
        CMOV.NroRf2  = STRING(Almcmov.NroDoc).
END.


PROCEDURE Genera-Detalle:

    FOR EACH ITEM OF CMOV NO-LOCK:
        CREATE almdmov.
        ASSIGN Almdmov.CodCia = Almcmov.CodCia 
               Almdmov.CodAlm = Almcmov.CodAlm 
               Almdmov.TipMov = Almcmov.TipMov 
               Almdmov.CodMov = Almcmov.CodMov 
               Almdmov.NroSer = Almcmov.NroSer 
               Almdmov.NroDoc = Almcmov.NroDoc 
               Almdmov.CodMon = Almcmov.CodMon 
               Almdmov.FchDoc = Almcmov.FchDoc 
               Almdmov.TpoCmb = Almcmov.TpoCmb 
               Almdmov.codmat = ITEM.codmat 
               Almdmov.CanDes = ITEM.CanDes 
               Almdmov.CodUnd = ITEM.CodUnd 
               Almdmov.Factor = ITEM.Factor 
               Almdmov.ImpCto = ITEM.ImpCto 
               Almdmov.PreUni = ITEM.PreUni 
               Almdmov.AlmOri = Almcmov.AlmDes 
               Almdmov.CodAjt = '' 
               Almdmov.HraDoc = Almcmov.HorRcp
                      R-ROWID = ROWID(Almdmov).

        RUN ALM\ALMACSTK (R-ROWID).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

        RUN alm/almacpr1 (R-ROWID, 'U').
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    END.

END PROCEDURE.
