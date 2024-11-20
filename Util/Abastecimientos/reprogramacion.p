DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.
DEF NEW SHARED VAR s-nroser AS INT INIT 252.
DEF NEW SHARED VAR s-coddoc AS CHAR INIT 'N/C'.
DEF NEW SHARED VAR s-codalm AS CHAR.
DEF NEW SHARED VAR S-USER-ID AS CHAR INIT 'ADMIN'.

DEF VAR pMensaje AS CHAR NO-UNDO.

DEFINE TEMP-TABLE T-FELogErrores NO-UNDO LIKE FELogErrores.
DEFINE TEMP-TABLE DETA LIKE CcbDDocu.

DEF BUFFER B-CDOCU FOR ccbcdocu.

DEF VAR x-Formato AS CHAR INIT '999-999999' NO-UNDO.

FIND B-CDOCU WHERE B-CDOCU.codcia = s-codcia
    AND B-CDOCU.coddoc = 'FAC'
    AND B-CDOCU.nrodoc = '25100055230'.

FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
    AND FacCorre.CodDiv = S-CODDIV
    AND FacCorre.CodDoc = S-CODDOC 
    AND FacCorre.NroSer = s-NroSer
    AND FacCorre.FlgEst = YES   /* Debe estar activa */
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
   RETURN 'ADM-ERROR'.
END.

RUN sunat/p-formato-doc (INPUT s-CodDoc, OUTPUT x-Formato).
ASSIGN
    s-CodAlm = B-CDOCU.CodAlm.
FIND FIRST FacCfgGn WHERE FacCfgGn.CodCia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCfgGn THEN DO:
    RETURN 'ADM-ERROR'.
END.

RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    {lib/lock-genericov3.i ~
        &Tabla="FacCorre" ~
        &Condicion="FacCorre.CodCia = S-CODCIA ~
                    AND FacCorre.CodDiv = S-CODDIV ~
                    AND FacCorre.CodDoc = S-CODDOC ~
                    AND FacCorre.NroSer = s-NroSer" ~
        &Bloqueo="EXCLUSIVE-LOCK" ~
        &Accion="RETRY" ~
        &Mensaje="YES" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'" }
    
    CREATE Ccbcdocu.
    BUFFER-COPY B-CDOCU     /* La FAC o BOL */
        EXCEPT B-CDOCU.Glosa B-CDOCU.NroOrd
        TO CcbCDocu
        ASSIGN 
        CcbCDocu.CodCia = S-CODCIA
        CcbCDocu.CodDiv = S-CODDIV
        CcbCDocu.CodDoc = S-CODDOC          /* N/C */
        CcbCDocu.NroDoc = STRING(FacCorre.NroSer, ENTRY(1, x-Formato, '-')) +
                            STRING(FacCorre.Correlativo, ENTRY(2, x-Formato, '-'))
        CcbCDocu.CodRef = B-CDOCU.CodDoc    /* FAC o BOL */
        CcbCDocu.NroRef = B-CDOCU.NroDoc
        CcbCDocu.FchDoc = TODAY
        CcbCDocu.FchVto = ADD-INTERVAL (TODAY, 1, 'years')
        CcbCDocu.FlgEst = "P"
        CcbCDocu.TpoCmb = FacCfgGn.TpoCmb[1]
        CcbCDocu.TpoFac = ""
        CcbCDocu.CndCre = 'D'
        CcbCDocu.Tipo   = "CREDITO"
        CcbCDocu.CodCaja= ''    /*s-CodTer*/
        CcbCDocu.usuario = S-USER-ID
        CcbCDocu.SdoAct = B-CDOCU.ImpTot
        CcbCDocu.ImpTot2 = 0
        CcbCDocu.ImpDto2 = 0
        CcbCDocu.CodMov = 09     /* INGRESO POR DEVOLUCION DEL CLIENTE */
        CcbCDocu.CodAlm = s-CodAlm
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        UNDO RLOOP, RETURN 'ADM-ERROR'.
    END.
    /* ************************ */
    /* Control de N/C generadas */
    /* ************************ */
    /* ************************ */
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.

    /* ACTUALIZAR EL CENTRO DE COSTO 22.07.04 CY */
    FIND GN-VEN WHERE gn-ven.codcia = s-codcia AND gn-ven.codven = B-CDOCU.codven
        NO-LOCK NO-ERROR.
    IF AVAILABLE GN-VEN THEN Ccbcdocu.cco = gn-ven.cco.

    RUN Genera-Detalle.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        UNDO RLOOP, RETURN 'ADM-ERROR'.
    END.
    
    /* Generamos el movimiento de almacén por devolución de mercadería */
    RUN vta2/ing-devo-utilex (ROWID(Ccbcdocu)).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        UNDO RLOOP, RETURN 'ADM-ERROR'.
    END.
    
    RUN Graba-Totales.

/*     RUN sunat/progress-to-ppll-v3  ( INPUT Ccbcdocu.coddiv,             */
/*                                      INPUT Ccbcdocu.coddoc,             */
/*                                      INPUT Ccbcdocu.nrodoc,             */
/*                                      INPUT-OUTPUT TABLE T-FELogErrores, */
/*                                      OUTPUT pMensaje ).                 */
/*     /* RHC 16/04/2018 En TODOS los casos: ANULAMOS los movimientos */   */
/*     IF RETURN-VALUE <> "OK" THEN DO:                                    */
/*         MESSAGE pmensaje VIEW-AS ALERT-BOX WARNING.                     */
/*         LEAVE RLOOP.                                                    */
/*     END.                                                                */

    MESSAGE 'cuatro' SKIP ccbcdocu.coddoc ccbcdocu.nrodoc.

END.
RETURN 'OK'.


PROCEDURE Graba-Totales:

{vta/graba-totales-abono.i}


END PROCEDURE.


PROCEDURE Genera-Detalle:

DEF VAR i AS INT NO-UNDO.

/* POR CADA ITEM DE LA FAC CREA UNO SIMILAR EN LA N/C */
EMPTY TEMP-TABLE DETA.
FOR EACH Ccbddocu OF B-CDOCU NO-LOCK, FIRST Almmmatg OF Ccbddocu NO-LOCK:
    /* Recalculamos Precios */
    CREATE DETA.
    BUFFER-COPY Ccbddocu
        TO DETA
        ASSIGN
        DETA.CanDes = Ccbddocu.candes
        DETA.CanDev = Ccbddocu.candes
        DETA.PreUni = ( Ccbddocu.ImpLin - Ccbddocu.ImpDto2 ) / Ccbddocu.CanDes
        DETA.ImpLin = ROUND (DETA.CanDes * DETA.PreUni, 2).
    IF DETA.AftIgv = YES THEN DETA.ImpIgv = ROUND(DETA.ImpLin / ( 1 + B-CDOCU.PorIgv / 100) * B-CDOCU.PorIgv / 100, 2).
END.
IF NOT CAN-FIND(FIRST DETA NO-LOCK) THEN DO:
    pMensaje = "El Comprobante: " + B-CDOCU.CodDoc + " " + B-CDOCU.NroDoc + CHR(10) +
        "YA tiene devoluciones en el almacén. NO se puede generar la N/C".
    RETURN 'ADM-ERROR'.
END.

i = 1.
FOR EACH DETA ON STOP UNDO, RETURN 'ADM-ERROR' ON ERROR UNDO, RETURN 'ADM-ERROR' BY DETA.NroItm:
    CREATE CcbDDocu.
    ASSIGN 
        CcbDDocu.NroItm = i
        CcbDDocu.CodCia = CcbCDocu.CodCia 
        CcbDDocu.Coddiv = CcbCDocu.Coddiv 
        CcbDDocu.CodDoc = CcbCDocu.CodDoc 
        CcbDDocu.NroDoc = CcbCDocu.NroDoc
        CcbDDocu.CodMat = DETA.codmat 
        CcbDDocu.PreUni = DETA.PreUni 
        CcbDDocu.CanDes = DETA.CanDes 
        CcbDDocu.Factor = DETA.Factor 
        CcbDDocu.ImpIsc = DETA.ImpIsc
        CcbDDocu.ImpIgv = DETA.ImpIgv 
        CcbDDocu.ImpLin = DETA.ImpLin
        CcbDDocu.AftIgv = DETA.AftIgv
        CcbDDocu.AftIsc = DETA.AftIsc
        CcbDDocu.UndVta = DETA.UndVta
        CcbDDocu.ImpCto = DETA.ImpCto
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
    /* Actualizamos control de devoluciones en el detalle de la FAC o BOL */
    {lib/lock-genericov3.i ~
        &Tabla="Ccbddocu" ~
        &Alcance="FIRST" ~
        &Condicion="CcbDDocu.CodCia = B-CDOCU.CodCia ~
        AND CcbDDocu.CodDiv = B-CDOCU.CodDiv ~
        AND CcbDDocu.CodDoc = B-CDOCU.CodDoc ~
        AND CcbDDocu.NroDoc = B-CDOCU.NroDoc ~
        AND Ccbddocu.CodMat = DETA.CodMat" ~
        &Bloqueo="EXCLUSIVE-LOCK" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &txtMensaje="pMensaje" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'"}
    ASSIGN
        Ccbddocu.candev = Ccbddocu.candev + DETA.candes.
    i = i + 1.
END.

DEF VAR f-Des AS DEC NO-UNDO.
DEF VAR f-Dev AS DEC NO-UNDO.
DEF VAR c-Sit AS CHAR NO-UNDO.

FOR EACH CcbDDocu OF CcbCDocu NO-LOCK:
    F-Des = F-Des + CcbDDocu.CanDes.
    F-Dev = F-Dev + CcbDDocu.CanDev. 
END.
IF F-Dev > 0 THEN C-SIT = "P".
IF F-Des = F-Dev THEN C-SIT = "D".
ASSIGN 
    CcbCDocu.FlgCon = C-SIT.

END PROCEDURE.
