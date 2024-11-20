DEFINE VARIABLE S-CODCIA  AS INTEGER INIT 001.
DEFINE VARIABLE CL-CODCIA  AS INTEGER INIT 000.

DEFINE VARIABLE chAppCom AS COM-HANDLE.

DEF VAR f-LineaCredito AS DEC NO-UNDO.
DEF VAR f-Saldo AS DEC NO-UNDO.
DEF VAR f-Saldo1 AS DEC NO-UNDO.
DEF VAR t-Resultado AS CHAR NO-UNDO.

CREATE "sp_db2.Speed400db2" chAppCom.

FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia:
    RUN linea-de-credito ( gn-clie.CodCli,
                           OUTPUT f-LineaCredito,
                           OUTPUT f-Saldo,
                           1,
                           '130',
                           TRUE,
                           OUTPUT t-Resultado).
    RUN Linea-Speed.
    DISPLAY
        gn-clie.codcli
        gn-clie.nomcli
        f-LineaCredito
        f-Saldo
        f-Saldo1
        WITH STREAM-IO NO-BOX.
END.

/* release com-handles */
RELEASE OBJECT chAppCom NO-ERROR.

PROCEDURE Linea-Speed:

    DEFINE VAR lValor        AS DECIMAL.    /* SALDO DEL CLIENTE = LINEA CRED - POR PAGAR */
    DEFINE VAR lCodCli       AS CHAR.

    lCodCli = SUBSTRING(gn-clie.codcli,1,10).
    IF gn-clie.codant <> '' AND LENGTH(gn-clie.codant) = 10
        THEN lCodCli = gn-clie.codant.

    lValor = chAppCom:GetLineaCredito(1,lCodCli).
    ASSIGN
        f-Saldo1 = lValor * FacCfgGn.TpoCmb[1].    /* Como está en US$ lo pasamos a S/. */

END PROCEDURE.


PROCEDURE Linea-de-Credito:

    DEFINE INPUT  PARAMETER pCodCli AS CHAR.
    DEFINE OUTPUT PARAMETER dImpLCred AS DECIMAL.     /* Importe del pedido */
    DEFINE OUTPUT PARAMETER pSaldo AS DECIMAL.     /* Importe del pedido */
    DEFINE INPUT  PARAMETER pCodMon AS INT.
    DEFINE INPUT  PARAMETER pFmaPgo AS CHAR.
    DEFINE INPUT  PARAMETER pMensaje AS LOG.        /* Muestra o no el mensaje de error */
    DEFINE OUTPUT PARAMETER pResultado AS CHAR.     /* OK o ADM-ERROR */


    DEFINE VARIABLE F-TOTAL AS DECIMAL.
    DEFINE VARIABLE cMonLCred AS INTEGER NO-UNDO.

    ASSIGN
        pSaldo = 0
        dImpLCred = 0
        pResultado = 'ADM-ERROR'.

    FIND gn-convt WHERE gn-convt.codig = pFmaPgo NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-convt THEN RETURN.
    IF gn-convt.tipvta = "1" THEN DO:       /* CONTADO */
        pResultado = 'OK'.
        RETURN.
    END.

    /* LINEA DE CREDITO */
    FIND GN-CLIE WHERE GN-CLIE.CODCIA = CL-CODCIA AND
        GN-CLIE.CODCLI = pCodCli NO-LOCK NO-ERROR.
    IF NOT AVAILABLE GN-CLIE THEN RETURN.
    FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.
    /* MLR 02/Jun/2008 Verifica Linea de Crédito Campaña */
    cMonLCred = Gn-Clie.MonLC.
    FOR EACH Gn-ClieL WHERE Gn-ClieL.CodCia = gn-clie.codcia AND
        Gn-ClieL.CodCli = gn-clie.codcli AND
        Gn-ClieL.FchIni <> ? AND
        Gn-ClieL.FchFin <> ? AND
        TODAY >= Gn-ClieL.FchIni AND
        TODAY <= Gn-ClieL.FchFin NO-LOCK
        BY gn-cliel.fchini BY gn-cliel.fchfin:
        cMonLCred = Gn-ClieL.MonLC.
        dImpLCred = Gn-ClieL.ImpLC.
    END.

    /* Suma Todos los Documentos Pendientes */
    FOR EACH CcbCDocu NO-LOCK WHERE CcbCDocu.CodCia = S-CODCIA 
        AND CcbCDocu.CodCli = pCodCli 
        AND LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL,LET,CHQ,N/D,N/C,A/R,BD') > 0 
        AND CcbCDocu.FlgEst = "P":
        /* DOCUMENTOS QUE NO DEBEN APARECER EN LA LINEA DE CREDITO */
        /* IF LOOKUP(Ccbcdocu.coddoc, 'A/R,BD') > 0 THEN NEXT. */
        /* ******************************************************* */
        /* RHC 23.11.08 LETRAS ADELANTADAS */
        IF Ccbcdocu.coddoc = 'LET' AND Ccbcdocu.codref = 'CLA' THEN NEXT.

        F-TOTAL = CcbCDocu.SdoAct.
        IF cMonLCred = 1 THEN DO:
            IF CcbCDocu.CodMon = 2 THEN F-TOTAL = F-TOTAL * FacCfgGn.Tpocmb[1].
        END.
        ELSE DO:
            IF CcbCDocu.CodMon = 1 THEN F-TOTAL = F-TOTAL / FacCfgGn.Tpocmb[1].
        END.

        FIND FacDocum WHERE FacDocum.CodCia = S-CODCIA 
            AND FacDocum.CodDoc = CcbCDocu.CodDoc 
            NO-LOCK NO-ERROR.
        IF AVAILABLE FacDocum AND FacDocum.TpoDoc
        THEN pSaldo = pSaldo + F-TOTAL.
        ELSE pSaldo = pSaldo - F-TOTAL.
    END.
    pSaldo = dImpLCred - pSaldo.


END PROCEDURE.
