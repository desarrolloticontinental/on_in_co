FOR EACH CCBCMVTO WHERE
    CcbCMvto.CodCia = 1 AND
    CcbCMvto.FlgEst = "P" AND
    CcbCMvto.CodDoc = 'CJE' AND
    CcbCMvto.NroDoc >= "" AND
    CcbCMvto.usuario >= "" AND
    CcbCMvto.CodDiv  = "00000" AND
    FCHDOC = TODAY:
    DISPLAY
        CCBCMVTO
        WITH 2 COL.
    FOR EACH CcbDMvto WHERE
        CcbDMvto.CodCia = CcbCMvto.CodCia AND
        CcbDMvto.CodDoc = CcbCMvto.CodDoc AND
        CcbDMvto.NroDoc = CcbCMvto.NroDoc AND
        CcbDMvto.CodCli = CcbCMvto.CodCli:
        DISPLAY
              CcbDMvto.CodDoc
              CcbDMvto.NroDoc
              CcbDMvto.CodCli
            CcbDMvto.TpoRef
              CcbDMvto.CodRef
              CcbDMvto.NroRef
              CcbDMvto.FchEmi
              CcbDMvto.FchVto
              CcbDMvto.ImpDoc
              CcbDMvto.ImpTot
              CcbDMvto.CodDpto
              CcbDMvto.CodProv
              CcbDMvto.CodDist.
              
        DELETE CcbDMvto.
        
    END.
    
    DELETE CCBCMVTO.
    
END.    
