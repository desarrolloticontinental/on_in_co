DEF BUFFER B-CDOCU FOR Ccbcdocu.
DEF BUFFER B-DDOCU FOR Ccbddocu.
DEF BUFFER FACTURA FOR Ccbcdocu.
DEF VAR pMensaje AS CHAR NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.
ASSIGN
    s-CodDiv = "00000".

RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FIND B-CDOCU WHERE B-CDOCU.codcia = 1
        AND B-CDOCU.coddoc = 'FAC'
        AND B-CDOCU.nrodoc = '27600000037'.
    CREATE FACTURA.
    BUFFER-COPY B-CDOCU
        TO FACTURA
        ASSIGN
        FACTURA.CodDiv = s-CodDiv        /* OJO */
        FACTURA.NroDoc = "28200000023"
        FACTURA.FchDoc = DATE(07,31,2016)
        FACTURA.FlgEst = "P"
        FACTURA.FlgCie = "S"        /* OJO >>> SUNAT */
        FACTURA.FchCie = TODAY
        FACTURA.HorCie = STRING(TIME,'HH:MM:SS')
        FACTURA.NroSal = B-CDOCU.CodDoc + B-CDOCU.NroDoc    /* OJO: EL ORIGEN */
        NO-ERROR.
    IF FACTURA.SdoAct <= 0 THEN FACTURA.FlgEst = "C".
    IF ERROR-STATUS:ERROR THEN DO:
        pMensaje = "Correlativo del comprobante mal registrado o duplicado".
        UNDO RLOOP, LEAVE.
    END.
    FIND gn-clie WHERE gn-clie.codcia = 000
        AND gn-clie.codcli = FACTURA.CodCli
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN
        ASSIGN
        FACTURA.DirCli = gn-clie.dircli
        FACTURA.NomCli = gn-clie.nomcli
        FACTURA.RucCli = gn-clie.ruc.
    FOR EACH B-DDOCU OF B-CDOCU NO-LOCK:
        CREATE Ccbddocu.
        BUFFER-COPY B-DDOCU
            TO Ccbddocu
            ASSIGN
            CcbDDocu.CodDiv = FACTURA.CodDiv        /* OJO */
            CcbDDocu.CodCli = FACTURA.CodCli
            CcbDDocu.FchDoc = FACTURA.FchDoc
            CcbDDocu.NroDoc = FACTURA.NroDoc
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            pMensaje = "Correlativo del comprobante mal registrado o duplicado".
            UNDO RLOOP, LEAVE RLOOP.
        END.
    END.
END.
IF pMensaje <> '' THEN MESSAGE pMensaje.
