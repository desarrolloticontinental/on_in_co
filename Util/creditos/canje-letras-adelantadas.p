DEF BUFFER B-CDOCU FOR ccbcdocu.
DEF BUFFER COMPROBANTE FOR ccbcdocu.

OUTPUT TO c:\tmp\canjeletrasadelantadas.txt.
PUT UNFORMATTED
    'DIVISION|CANJE|EMITIDO|APROBADO|CLIENTE|MONEDA|TC|GLOSA|' +
    'ANTICIPO|EMISION|IMPORTE|APLICACION|FECHA|IMPORTE|COMPROBANTE|IMPORTE|' +
    'LETRA|EMISION|VENCIMIENTO|IMPORTE'
    SKIP.
FOR EACH ccbcmvto NO-LOCK WHERE codcia = 1
    AND coddoc = 'CLA'
    AND fchdoc >= 01/01/2013
    AND flgest = "E",
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 000
    AND gn-clie.codcli = ccbcmvto.codcli:
    FOR EACH CcbCDocu NO-LOCK WHERE CcbCDocu.CodCia = CcbCMvto.CodCia
        AND CcbCDocu.CodCli = CcbCMvto.CodCli
        AND CcbCDocu.CodRef = CcbCMvto.CodDoc
        AND CcbCDocu.NroRef = CcbCMvto.NroDoc
        AND CcbCDocu.CodDoc = "A/R",
        EACH CCBDMOV NO-LOCK WHERE CCBDMOV.CodCia = CcbCDocu.CodCia
        AND CCBDMOV.CodDoc = CcbCDocu.CodDoc
        AND CCBDMOV.NroDoc = CcbCDocu.NroDoc,
        EACH ccbdcaja NO-LOCK WHERE ccbdcaja.codcia = ccbdmov.codcia
        AND ccbdcaja.coddoc = ccbdmov.codref
        AND ccbdcaja.nrodoc = ccbdmov.nroref,
        FIRST COMPROBANTE NO-LOCK WHERE COMPROBANTE.codcia = ccbdcaja.codcia
        AND COMPROBANTE.coddoc = ccbdcaja.codref
        AND COMPROBANTE.nrodoc = ccbdcaja.nroref,
        EACH B-CDocu NO-LOCK WHERE B-CDocu.CodCia = CcbCMvto.CodCia
        AND B-CDocu.CodDiv = CcbCMvto.CodDiv
        AND B-CDocu.CodDoc = "LET"
        AND B-CDocu.CodRef = CcbCMvto.CodDoc
        AND B-CDocu.NroRef = CcbCMvto.NroDoc:
        PUT UNFORMATTED
            ccbcmvto.coddiv '|'
            ccbcmvto.coddoc ' '
            ccbcmvto.nrodoc '|'
            ccbcmvto.fchdoc '|'
            ccbcmvto.fchapr '|'
            ccbcmvto.codcli ' '
            gn-clie.nomcli '|'
            ccbcmvto.codmon '|'
            ccbcmvto.tpocmb '|'
            ccbcmvto.glosa '|'
            ccbcdocu.coddoc ' '
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.imptot '|'
            ccbdmov.codref ' '
            ccbdmov.nroref '|'
            ccbdmov.fchmov '|'
            ccbdmov.imptot '|'
            ccbdcaja.codref ' '
            ccbdcaja.nroref '|'
            COMPROBANTE.imptot '|'
            b-cdocu.coddoc ' '
            b-cdocu.nrodoc '|'
            b-cdocu.fchdoc '|'
            b-cdocu.fchvto '|'
            b-cdocu.imptot
            SKIP.
    END.
END.
OUTPUT CLOSE.

