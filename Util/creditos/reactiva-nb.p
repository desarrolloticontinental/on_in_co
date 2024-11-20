FIND ccbcmvto WHERE codcia = 1
    AND coddoc = 'n/b'
    AND nrodoc = '000005340'.
ASSIGN 
    ccbcmvto.flgest = 'P'
    ccbcmvto.glosa  = ''.

FOR EACH ccbdmvto WHERE ccbdmvto.codcia = ccbcmvto.codcia
    AND ccbdmvto.coddoc = ccbcmvto.coddoc
    AND ccbdmvto.nrodoc = ccbcmvto.nrodoc:
    FIND Ccbcdocu WHERE Ccbcdocu.codcia = 001
        AND Ccbcdocu.coddoc = ccbdmvto.codref
        AND Ccbcdocu.nrodoc = ccbdmvto.nroref
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Ccbcdocu THEN DO:
        MESSAGE 'NO pudo localizar el documento' ccbdmvto.codref ccbdmvto.nroref
            VIEW-AS ALERT-BOX WARNING.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        Ccbcdocu.sdoact = Ccbcdocu.sdoact - ccbdmvto.imptot
        Ccbcdocu.flgest = (IF Ccbcdocu.sdoact <= 0 THEN 'C' ELSE 'P')
        Ccbcdocu.fchcan = (IF Ccbcdocu.sdoact <= 0 THEN TODAY ELSE ?).

    INSERT INTO CcbDCaja ( CodCia, CodDiv, CodDoc, NroDoc, 
                           CodRef, NroRef, ImpTot, FchDoc, 
                           CodCli, CodMon, TpoCmb )
        VALUES ( Ccbcmvto.CodCia, Ccbcmvto.CodDiv, Ccbcmvto.CodDoc, Ccbcmvto.NroDoc, 
                 Ccbcdocu.coddoc , Ccbcdocu.nrodoc, ccbdmvto.ImpTot, Ccbcmvto.FchCbd, 
                 Ccbcdocu.CodCli, Ccbcdocu.CodMon, CcbCMvto.TpoCmb ).
    
END.
