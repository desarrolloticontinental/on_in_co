&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : qregistroventas.p
    Purpose     : Genera un registro en la tabla wmigrv

    Syntax      :

    Description :  Migración de ventas al contado en base a los cierres del día anterior

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 5.04
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */


DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-user-id AS CHAR INIT "AUTOMATICO".
DEF VAR cl-codcia AS INT INIT 000.
DEF VAR cb-codcia AS INT INIT 000.
DEF VAR x-CodDoc AS CHAR NO-UNDO.
DEF VAR pTipo AS CHAR INIT "I" NO-UNDO.

DEFINE TEMP-TABLE t-wmigrv NO-UNDO LIKE wmigrv.

FIND cb-cfgg WHERE cb-cfgg.CodCia = cb-codcia 
    AND cb-cfgg.Codcfg = 'R02' NO-LOCK NO-ERROR.
IF NOT AVAILABLE cb-cfgg THEN DO:
    DISPLAY
        "ERROR: NO está configurado las cuentas R02 en el cb-cfgg"
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
    RETURN.
END.

/* N/C y N/D DE UTILEX */
EMPTY TEMP-TABLE t-wmigrv.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia
    AND Ccbcdocu.coddiv = gn-divi.coddiv
    AND LOOKUP(Ccbcdocu.coddoc, 'N/C,N/D') > 0
    AND Ccbcdocu.fchdoc = TODAY - 1,
    FIRST Facdocum OF Ccbcdocu NO-LOCK,
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
    AND gn-clie.codcli = ccbcdocu.codcli:
    /* PASAMOS INCLUSO LOS ANULADOS */
    IF Ccbcdocu.imptot <= 0 THEN NEXT.
    /* NO REPETIMOS EL PASE: ESTO SE HACE YA QUE UTILEX NO ACTUALIZA EL REGISTRO EN LINEA */
    x-coddoc = Ccbcdocu.coddoc.
    CASE ccbcdocu.coddoc:
        WHEN 'N/C' THEN x-coddoc = "NC".
        WHEN 'N/D' THEN x-coddoc = "ND".
    END CASE.
    /* VERIFICAMOS QUE NO TENGA UNA MIGRACION ANTERIOR */
    FIND FIRST WMigRv WHERE wmigrv.wvtdoc = x-coddoc
        AND wmigrv.wvndoc = Ccbcdocu.nrodoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE WMigRv THEN NEXT.
    /* ************************************************************** */
    /* FIN DE FILTRO */
    DISPLAY
        ccbcdocu.coddiv ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.fchdoc ccbcdocu.flgest
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
    RUN Carga-FAC-BOL.
END.
/* FIN DEL PROCESO ************************** */

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Carga-FAC-BOL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-FAC-BOL Procedure 
PROCEDURE Carga-FAC-BOL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{aplic/sypsa/carga-fac-bol-credito.i}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Transferir-Asiento) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transferir-Asiento Procedure 
PROCEDURE Transferir-Asiento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


{aplic/sypsa/transferir-asiento-contado.i}

/*
DEFINE VAR lwCorre AS CHARACTER.

/* BLOQUEAMOS Sí o Sí el correlativo */
REPEAT:
  FIND wmigcorr WHERE wmigcorr.Proceso = "RV"
      AND wmigcorr.Periodo = YEAR(TODAY - 1)
      AND wmigcorr.Mes = MONTH(TODAY - 1)
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  IF NOT AVAILABLE wmigcorr THEN DO:
      IF NOT LOCKED wmigcorr THEN DO:
          /* CREAMOS EL CONTROL */
          CREATE wmigcorr.
          ASSIGN
              wmigcorr.Correlativo = 1
              wmigcorr.Mes = MONTH(TODAY - 1)
              wmigcorr.Periodo = YEAR(TODAY - 1)
              wmigcorr.Proceso = "RV".
          LEAVE.
      END.
      ELSE UNDO, RETRY.
  END.
  LEAVE.
END.
FOR EACH T-WMIGRV NO-LOCK:
  CREATE WMIGRV.
  BUFFER-COPY T-WMIGRV
      TO WMIGRV.

      lwcorre = STRING(wmigcorr.Periodo, '9999') + STRING(wmigcorr.Mes, '99') + 
                  STRING(wmigcorr.Correlativo, '999999').
      lwCorre = SUBSTRING(lwCorre,3).

      ASSIGN
      wmigrv.FlagFecha = DATETIME(TODAY - 1, MTIME)
      wmigrv.FlagTipo = "I"
      wmigrv.FlagUsuario = s-user-id
        /*
      wmigrv.wcorre = STRING(wmigcorr.Periodo, '9999') + STRING(wmigcorr.Mes, '99') + 
                      STRING(wmigcorr.Correlativo, '9999')
                      */
      wmigrv.wcorre = lwCorre
      wmigrv.wsecue = 0001
      wmigrv.wvejer = wmigcorr.Periodo
      wmigrv.wvperi = wmigcorr.Mes.
  ASSIGN
      wmigcorr.Correlativo = wmigcorr.Correlativo + 1.
END.
IF AVAILABLE(wmigcorr) THEN RELEASE wmigcorr.
IF AVAILABLE(wmigrv)   THEN RELEASE wmigrv.
EMPTY TEMP-TABLE T-WMIGRV.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

