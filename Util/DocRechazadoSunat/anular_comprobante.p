&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-DDOCU FOR CcbDDocu.



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
DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR.
DEF NEW SHARED VAR s-user-id AS CHAR INIT 'SYSTEM'.

/* "clona" una FAC o BOL y actualiza el kardex */

DEF INPUT PARAMETER pRowid AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEF BUFFER b-ccbcdocu FOR ccbcdocu.

FIND b-Ccbcdocu WHERE ROWID(b-Ccbcdocu) = pRowid NO-LOCK NO-ERROR.

IF NOT AVAILABLE b-Ccbcdocu OR
    LOOKUP(TRIM(b-Ccbcdocu.coddoc), 'FAC,BOL') = 0 OR
    b-Ccbcdocu.flgest = "A"
    THEN DO:
    pMensaje = "Documento (FAC o BOL) NO encontrado o ya ha sido anulado".
    RETURN.
END.

DEF VAR s-CodDoc AS CHAR NO-UNDO.
DEF VAR s-NroDoc AS CHAR NO-UNDO.

ASSIGN
    s-CodDoc = b-Ccbcdocu.coddoc
    s-NroDoc = b-Ccbcdocu.nrodoc
    s-CodDiv = b-Ccbcdocu.coddiv.

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
      TABLE: B-DDOCU B "?" ? INTEGRAL CcbDDocu
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 5.85
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN Principal (OUTPUT pMensaje).
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    IF TRUE <> (pMensaje > "") THEN pMensaje = "NO se pudo anular el comprobante".
    RETURN 'ADM-ERROR'.
END.
pMensaje = "".
RETURN 'OK'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Principal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Principal Procedure 
PROCEDURE Principal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    RUN _Anula_Comprobante (INPUT ROWID(b-ccbcdocu),
                            OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
END.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-_Anula_Comprobante) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _Anula_Comprobante Procedure 
PROCEDURE _Anula_Comprobante :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER pRowid AS ROWID NO-UNDO.
  DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

  ELIMINACION:          
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
      /* Bloqueamos el Comprobante */
      {lib/lock-genericov3.i ~
          &Tabla="CcbCDocu" ~
          &Condicion="ROWID(Ccbcdocu) = pRowid" ~
          &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
          &Accion="RETRY" ~
          &Mensaje="NO" ~
          &txtMensaje="pMensaje" ~
          &TipoError= "UNDO, RETURN 'ADM-ERROR'" ~
          }
      
      /* ANULAMOS LA FACTURA EN PROGRESS */
      ASSIGN 
          CcbCDocu.FlgEst = "A"
          CcbCDocu.SdoAct = 0
          CcbCDocu.UsuAnu = S-USER-ID
          CcbCDocu.FchAnu = TODAY
          /*CcbCDocu.Glosa  = "A N U L A D O"*/
          .

     /* *********************************************************************** */
     /* *********************************************************************** */
     /* HAY RUTINAS QUE NO SE DEBEN HACER SI ES UNA FACTURA POR FAI */
     /* *********************************************************************** */
      IF CcbCDocu.CodRef = "FAI" THEN LEAVE.
     /* *********************************************************************** */
     /* *********************************************************************** */
      
      /* DESCARGA ALMACENES */
      RUN vta2/des_alm (ROWID(CcbCDocu)).
      IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
          pMensaje = "ERROR: Comprobante " + s-CodDoc + " " + s-NroDoc + CHR(10) +
              "No se pudo actualizar los saldos del almacén".
          UNDO, RETURN 'ADM-ERROR'.
      END.
      /* ANULAMOS LA GUIA DE REMISION */
/*       FOR EACH GUIAS EXCLUSIVE-LOCK WHERE GUIAS.CodCia = pCodCia */
/*           AND GUIAS.CodDoc = "G/R"                               */
/*           AND GUIAS.CodRef = CcbCDocu.CodDoc                     */
/*           AND GUIAS.NroRef = CcbCDocu.NroDoc                     */
/*           AND GUIAS.FlgEst = "F" ON ERROR UNDO, THROW:           */
/*           ASSIGN                                                 */
/*               GUIAS.FlgEst = "A"                                 */
/*               GUIAS.SdoAct = 0                                   */
/*               GUIAS.UsuAnu = S-USER-ID                           */
/*               GUIAS.FchAnu = TODAY                               */
/*               GUIAS.Glosa  = "A N U L A D O".                    */
/*       END.                                                       */
/*       IF AVAILABLE GUIAS THEN RELEASE GUIAS.                     */
  END.
  RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

