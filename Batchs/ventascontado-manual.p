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

/* COMPROBANTES AL CONTADO */
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia
    AND Ccbcdocu.coddiv = gn-divi.coddiv
    AND LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL,TCK') > 0
    AND Ccbcdocu.fchdoc >= 07/01/2012
    AND Ccbcdocu.fchdoc <= 07/31/2012,
    FIRST Facdocum OF Ccbcdocu NO-LOCK,
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
    AND gn-clie.codcli = ccbcdocu.codcli:
    /* FILTRO DEL DOCUMENTO */
    IF Ccbcdocu.Tipo <> "MOSTRADOR" THEN NEXT.
    /* PASAMOS INCLUSO LOS ANULADOS */
    x-coddoc = Ccbcdocu.coddoc.
    CASE ccbcdocu.coddoc:
        WHEN 'FAC' THEN x-coddoc = "FC".
        WHEN 'BOL' THEN x-coddoc = "BV".
        WHEN 'TCK' THEN x-coddoc = "TK".
        WHEN 'LET' THEN x-coddoc = "LT".
        WHEN 'N/C' THEN x-coddoc = "NC".
        WHEN 'N/D' THEN x-coddoc = "ND".
        WHEN 'CHQ' THEN x-coddoc = "CD".
    END CASE.
    EMPTY TEMP-TABLE t-wmigrv.

    DISPLAY
        ccbcdocu.coddiv ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.fchdoc ccbcdocu.flgest
        WITH STREAM-IO NO-BOX WIDTH 200.
    PAUSE 0.
    RUN Carga-FAC-BOL.
    RUN Transferir-Asiento.
END.
/* FIN DEL PROCESO ************************** */

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

{aplic/sypsa/carga-fac-bol-contado.i}


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


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

