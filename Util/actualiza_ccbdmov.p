
for each ccbdcaja no-lock where
    ccbdcaja.codcia = 1 AND
    ccbdcaja.coddoc = "N/C" and
    ccbdcaja.codref <> "N/C":
    /* Busca Documento */
    FIND FIRST ccbCDocu WHERE
        ccbCDocu.CodCia = ccbdcaja.codcia AND
        ccbCDocu.CodDoc = ccbdcaja.coddoc AND
        ccbCDocu.NroDoc = ccbdcaja.NroDoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbCDocu THEN NEXT.

    /* Crea Detalle de la Aplicación */
    CREATE CCBDMOV.
    ASSIGN
        CCBDMOV.CodCia = ccbdcaja.codcia
        CCBDMOV.CodDiv = ccbdcaja.CodDiv
        CCBDMOV.NroDoc = ccbCDocu.NroDoc
        CCBDMOV.CodDoc = ccbCDocu.CodDoc
        CCBDMOV.CodMon = ccbCDocu.CodMon
        CCBDMOV.CodRef = ""
        CCBDMOV.NroRef = ""
        CCBDMOV.CodCli = ccbCDocu.CodCli
        CCBDMOV.FchDoc = ccbCDocu.FchDoc
        CCBDMOV.ImpTot = CcbDCaja.ImpTot
        CCBDMOV.FchMov = CcbDCaja.FchDoc
        CCBDMOV.HraMov = ""
        CCBDMOV.TpoCmb = ccbdcaja.tpocmb
        CCBDMOV.usuario = CcbCDocu.usuario.

end.
