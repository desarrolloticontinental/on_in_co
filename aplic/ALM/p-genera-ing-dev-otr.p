&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-CMOV FOR Almcmov.
DEFINE BUFFER B-DMOV FOR Almdmov.
DEFINE BUFFER ORDEN FOR FacCPedi.
DEFINE TEMP-TABLE t-Detalle NO-UNDO LIKE logisdchequeo.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT PARAMETER pCodDoc  AS CHAR.
DEF INPUT PARAMETER pNroDoc  AS CHAR.
DEF INPUT PARAMETER x-UsrChq AS CHAR.
DEF INPUT PARAMETER pMotivo  AS CHAR.
DEF INPUT PARAMETER pObserv  AS CHAR.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-codalm AS CHAR.
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR s-user-id AS CHAR.

DEF VAR s-CodMov AS INTE INIT 30 NO-UNDO.

DEFINE VARIABLE S-CODDOC   AS CHAR INITIAL "D/F".

FIND FacDocum WHERE FacDocum.CodCia = S-CODCIA AND
     FacDocum.CodDoc = S-CODDOC 
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacDocum OR FacDocum.CodMov = 0 THEN DO:
   MESSAGE "Codigo de Documento no configurado" VIEW-AS ALERT-BOX ERROR.
   RETURN 'ADM-ERROR'.
END.
FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
  AND  FacCorre.CodDoc = S-CODDOC 
  AND  FacCorre.CodDiv = S-CODDIV 
  AND  FacCorre.FlgEst = YES
  EXCLUSIVE-LOCK NO-ERROR.
/* ASSIGN                                                   */
/*     S-CODMOV = FacDocum.CodMov.                          */
/* FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA AND */
/*      FacCorre.CodDoc = S-CODDOC AND                      */
/*      FacCorre.CodDiv = S-CODDIV AND                      */
/*      FacCorre.TipMov = 'I' AND                           */
/*      FacCorre.CodMov = S-CODMOV AND                      */
/*      FacCorre.FlgEst = YES                               */
/*      NO-LOCK NO-ERROR.                                   */
IF NOT AVAILABLE FacCorre THEN DO:
   MESSAGE "Correlativo NO configurado: " s-CodDoc VIEW-AS ALERT-BOX ERROR.
   RETURN 'ADM-ERROR'.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: B-CMOV B "?" ? INTEGRAL Almcmov
      TABLE: B-DMOV B "?" ? INTEGRAL Almdmov
      TABLE: ORDEN B "?" ? INTEGRAL FacCPedi
      TABLE: t-Detalle T "?" NO-UNDO INTEGRAL logisdchequeo
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 7.19
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
    AND Faccpedi.coddoc = pCodDoc
    AND Faccpedi.NroPed = pNroDoc
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccpedi THEN RETURN ERROR.

DEFINE VARIABLE I-NRODOC       AS INTEGER NO-UNDO.
DEFINE VARIABLE I-NROSER       AS INTEGER NO-UNDO.
DEFINE VARIABLE pCuenta AS INTE NO-UNDO.

FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA 
    AND FacCorre.CodDoc = S-CODDOC 
    AND FacCorre.CodDiv = S-CODDIV 
    /*AND FacCorre.CodAlm = S-CODALM*/
    AND FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
     
IF AVAILABLE FacCorre THEN ASSIGN I-NroSer = FacCorre.NroSer.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

  pMensaje = ''.
  CICLO:
  DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      {lib/lock-genericov3.i ~
          &Tabla="Almacen" ~
          &Condicion="Almacen.CodCia = S-CODCIA AND Almacen.CodAlm = s-CodAlm" ~
          &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
          &Accion="RETRY" ~
          &Mensaje="NO" ~
          &txtMensaje="pMensaje" ~
          &TipoError="UNDO, LEAVE" ~
          }
      ASSIGN 
          I-NroDoc = Almacen.CorrIng
          Almacen.CorrIng = Almacen.CorrIng + 1.
      CREATE Almcmov.
      ASSIGN 
          Almcmov.CodCia = S-CodCia 
          Almcmov.CodAlm = s-CodAlm
          Almcmov.TipMov = "I"
          Almcmov.CodMov = S-CodMov 
          Almcmov.NroSer = 000
          Almcmov.NroDoc = I-NRODOC
          Almcmov.FchDoc = TODAY
          Almcmov.NroRef = Faccpedi.NroPed
          Almcmov.TpoCmb = FacCfgGn.Tpocmb[1]
          Almcmov.FlgEst = "P"
          Almcmov.HorRcp = STRING(TIME,"HH:MM:SS")
          Almcmov.usuario = S-USER-ID
          Almcmov.NomRef  = Faccpedi.NomCli
          Almcmov.CodRef = Faccpedi.CodDoc
          Almcmov.NroRef = Faccpedi.NroPed
          Almcmov.NroRf1 = Faccpedi.NroPed
          Almcmov.Observ = pObserv
          NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
          {lib/mensaje-de-error.i ~
              &CuentaError="pCuenta" ~    /* OPCIONAL */
              &MensajeError="pMensaje" ~
              }
          UNDO CICLO, LEAVE.
      END.

      ASSIGN
          Almcmov.NroRf3 = pMotivo.
      
      RUN Numero-de-Documento(YES).
      IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
          pMensaje = 'Error en el correlativo del No. de Devolución'.
          UNDO, LEAVE.
      END.
      ASSIGN 
          Almcmov.NroRf2 = STRING(I-NroSer,"999") + STRING(I-NroDoc,"999999").
      ASSIGN 
          Almcmov.CodCli = Faccpedi.CodCli
          Almcmov.CodMon = Faccpedi.CodMon
          Almcmov.CodVen = Faccpedi.CodVen.

      RUN Genera-Detalle.  /* Detalle de la Guia */ 
      IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
          IF TRUE <> (pMensaje > '')  THEN pMensaje = 'NO se pudo generar el detalle de la devolución'.
          UNDO, RETURN 'ADM-ERROR'.
      END.

      RUN Actualiza-Factura(1).
      IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
          IF TRUE <> (pMensaje > '')  THEN pMensaje = 'NO se pudo actualizar el comprobante'.
          UNDO, RETURN 'ADM-ERROR'.
      END.
  END.
  IF pMensaje > '' THEN RETURN 'ADM-ERROR'.
  RETURN 'OK'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Actualiza-Factura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Factura Procedure 
PROCEDURE Actualiza-Factura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER FACTOR AS INTEGER.

  DEFINE VARIABLE F-Des AS DECIMAL INIT 0 NO-UNDO.
  DEFINE VARIABLE F-Dev AS DECIMAL INIT 0 NO-UNDO.
  DEFINE VARIABLE C-SIT AS CHARACTER INIT "" NO-UNDO.
  DEFINE VARIABLE pCuenta AS INTE NO-UNDO.
  
  RLOOP:
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
      FIND CURRENT Faccpedi EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF ERROR-STATUS:ERROR THEN DO:
          {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
          UNDO RLOOP, RETURN 'ADM-ERROR'.                  
      END.
      FOR EACH Almdmov OF Almcmov NO-LOCK:
          FIND FIRST Facdpedi OF Faccpedi WHERE Facdpedi.CodMat = Almdmov.CodMat 
              EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF ERROR-STATUS:ERROR THEN DO:
              {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
              UNDO RLOOP, RETURN 'ADM-ERROR'.                  
          END.
          ASSIGN 
              Facdpedi.Libre_d05 = Facdpedi.Libre_d05 + (FACTOR * Almdmov.CanDes).
      END.
/*       IF Faccpedi.FlgEst <> 'A' THEN DO:          */
/*           FOR EACH Facdpedi OF Faccpedi NO-LOCK:  */
/*               F-Des = F-Des + Facdpedi.Libre_d05. */
/*               F-Dev = F-Dev + Facdpedi.Libre_d05. */
/*           END.                                    */
/*           IF F-Dev > 0 THEN C-SIT = "P".          */
/*           IF F-Des = F-Dev THEN C-SIT = "D".      */
/*           ASSIGN                                  */
/*               Faccpedi.FlgCon = C-SIT.            */
/*       END.                                        */
      /* Actualizamos todas los movimientos de salida por transferencias relacionadas */
      FOR EACH Almcmov EXCLUSIVE-LOCK WHERE Almcmov.codcia = s-codcia 
          AND Almcmov.codref = Faccpedi.coddoc
          AND Almcmov.nroref = Faccpedi.nroped
          AND Almcmov.tipmov = "S"
          AND Almcmov.codmov = 03
          AND Almcmov.flgest <> "A"
          AND Almcmov.flgsit = "T":
          ASSIGN
              Almcmov.FlgSit = "R"
              Almcmov.DateUpdate = TODAY
              Almcmov.HourUpdate = STRING(TIME,'HH:MM:SS')
              Almcmov.UserUpdate = s-user-id.
      END.
      RELEASE Almcmov.
      FIND CURRENT Faccpedi NO-LOCK NO-ERROR.
  END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Genera-Detalle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Detalle Procedure 
PROCEDURE Genera-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Barremos lo chequeado
------------------------------------------------------------------------------*/

  /* Acumulamos */
  EMPTY TEMP-TABLE t-Detalle.
  FOR EACH LogisDChequeo NO-LOCK WHERE LogisDChequeo.CodCia = s-CodCia
      AND LogisDChequeo.CodDiv = s-CodDiv
      AND LogisDChequeo.CodPed = Faccpedi.CodDoc
      AND LogisDChequeo.NroPed = Faccpedi.NroPed,
      FIRST Almmmatg OF LogisDChequeo NO-LOCK:
      FIND t-Detalle WHERE t-Detalle.codmat = LogisDChequeo.codmat
          EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE t-Detalle THEN CREATE t-Detalle.
      ASSIGN
          t-Detalle.CodCia = s-CodCia
          t-Detalle.CodMat = LogisDChequeo.CodMat
          t-Detalle.UndVta = Almmmatg.UndStk
          t-Detalle.Factor = 1
          t-Detalle.CanChk = t-Detalle.CanChk + LogisDChequeo.CanPed.
  END.

  DEF VAR r-Rowid AS ROWID NO-UNDO.

  RLOOP:
  DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      FOR EACH t-Detalle NO-LOCK, 
          FIRST Facdpedi OF Faccpedi NO-LOCK WHERE Facdpedi.codmat = t-Detalle.codmat:
          CREATE Almdmov.
          ASSIGN 
              Almdmov.CodCia = Almcmov.CodCia 
              Almdmov.CodAlm = Almcmov.CodAlm 
              Almdmov.TipMov = Almcmov.TipMov 
              Almdmov.CodMov = Almcmov.CodMov 
              Almdmov.NroSer = Almcmov.NroSer
              Almdmov.NroDoc = Almcmov.NroDoc 
              Almdmov.CodMon = Almcmov.CodMon 
              Almdmov.FchDoc = Almcmov.FchDoc 
              Almdmov.TpoCmb = Almcmov.TpoCmb 
              Almdmov.codmat = t-Detalle.codmat
              Almdmov.CanDes = t-Detalle.CanChk
              Almdmov.CodUnd = t-Detalle.UndVta
              Almdmov.Factor = t-Detalle.Factor
              Almdmov.PreUni = ((Facdpedi.ImpLin - Facdpedi.ImpDto2) / Facdpedi.CanPed )
              Almdmov.CodAjt = '' 
              Almdmov.ImpLin = ROUND (Almdmov.CanDes * Almdmov.PreUni, 2)
              Almdmov.AftIsc = Facdpedi.AftIsc 
              Almdmov.AftIgv = Facdpedi.AftIgv 
              Almdmov.Flg_factor = Facdpedi.Flg_factor
              Almdmov.HraDoc     = Almcmov.HorRcp
              R-ROWID = ROWID(Almdmov).
          IF Almdmov.AftIgv = YES THEN Almdmov.ImpIgv = ROUND(Almdmov.ImpLin / ( 1 + Faccpedi.PorIgv / 100) * Faccpedi.PorIgv / 100, 2).

          RUN ALM\ALMACSTK (R-ROWID).
          IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
              pMensaje = 'Producto: ' + t-Detalle.codmat + ' NO asignado al almacén'.
              UNDO RLOOP, RETURN 'ADM-ERROR'.
          END.
          /* RHC 31.03.04 REACTIVAMOS KARDEX POR ALMACEN */
          RUN alm/almacpr1 (R-ROWID, 'U').
          IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
      END.

      /* ***************************************************************************** */
      /* Control de series */
      /* ***************************************************************************** */
      DEFINE VAR hProc AS HANDLE NO-UNDO.

      RUN alm/almacen-library PERSISTENT SET hProc.
      FOR EACH Almdmov OF Almcmov NO-LOCK,
          EACH LogisDChequeo NO-LOCK WHERE LogisDChequeo.CodCia = s-CodCia
          AND LogisDChequeo.CodDiv = s-CodDiv
          AND LogisDChequeo.CodPed = Faccpedi.CodDoc
          AND LogisDChequeo.NroPed = Faccpedi.NroPed
          AND logisdchequeo.CodMat = Almdmov.CodMat
          AND logisdchequeo.SerialNumber > '':
          RUN FIFO_Control-de-Series IN hProc (INPUT "WRITE",
                                               INPUT Almdmov.CodAlm,
                                               INPUT Almdmov.AlmOri,
                                               INPUT Almdmov.TipMov,
                                               INPUT Almdmov.CodMov,
                                               INPUT Almdmov.NroSer,
                                               INPUT Almdmov.NroDoc,
                                               INPUT Almdmov.CodMat,
                                               INPUT logisdchequeo.UndVta,
                                               INPUT logisdchequeo.SerialNumber,
                                               INPUT logisdchequeo.CanPed,
                                               INPUT 1,
                                               OUTPUT pMensaje).
          IF RETURN-VALUE = 'ADM-ERROR' THEN  UNDO RLOOP, RETURN 'ADM-ERROR'.
      END.
      DELETE PROCEDURE hProc.
      /* ***************************************************************************** */
  END.
  RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Numero-de-Documento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Numero-de-Documento Procedure 
PROCEDURE Numero-de-Documento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER L-INCREMENTA AS LOGICAL.

  IF L-INCREMENTA THEN 
      FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
        AND  FacCorre.CodDoc = S-CODDOC 
        AND  FacCorre.CodDiv = S-CODDIV 
        AND  FacCorre.FlgEst = YES
        EXCLUSIVE-LOCK NO-ERROR.
  ELSE
      FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
         AND  FacCorre.CodDoc = S-CODDOC 
         AND  FacCorre.CodDiv = S-CODDIV
         /*AND  FacCorre.CodAlm = S-CODALM */
         AND  FacCorre.FlgEst = YES
         NO-LOCK NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN 'ADM-ERROR'.
  ASSIGN I-NroDoc = FacCorre.Correlativo.
  IF L-INCREMENTA THEN ASSIGN FacCorre.Correlativo = FacCorre.Correlativo + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

