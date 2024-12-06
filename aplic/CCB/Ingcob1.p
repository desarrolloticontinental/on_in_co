&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : Ingresos de caja por cancelacion de cuentas

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF NEW SHARED VAR s-coddoc AS CHAR INITIAL "L/C".
DEF NEW SHARED VAR s-ptovta AS INT.
DEF NEW SHARED VAR s-tipo   AS CHAR INITIAL "CANCELACION".
DEF NEW SHARED VAR s-tpocmb AS DEC.
DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv LIKE gn-divi.coddiv.
DEF SHARED VAR s-codter LIKE ccbcterm.codter.


RUN Verifica.
IF RETURN-VALUE = "ADM-ERROR" THEN RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure



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
         HEIGHT             = 2
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN ccb/w-liqcob.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verifica Procedure 
PROCEDURE Verifica :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Control de documentos por terminal */

/*
  FIND ccbdterm WHERE CcbDTerm.CodCia = s-codcia AND
      CcbDTerm.CodDiv = s-coddiv AND
      CcbDTerm.CodDoc = s-coddoc AND
      CcbDTerm.CodTer = s-codter NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ccbdterm
  THEN DO:
      MESSAGE "El ingreso a caja no esta configurado en este terminal" VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  s-ptovta = ccbdterm.nroser.
 */
  /* Control de correlativos */
  FIND FacCorre WHERE faccorre.codcia = s-codcia AND 
                      faccorre.coddiv = S-CODDIV AND
                      faccorre.coddoc = s-coddoc 
                      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE FacCorre
  THEN DO:
      MESSAGE "No Esta definida la Liquidacion de Cobradores" SKIP
              "Para la Division "S-CODDIV VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.

  /* Tipo de cambio */

  FIND FacCfgGn WHERE FacCfgGn.CodCia = s-codcia NO-LOCK NO-ERROR.
  IF AVAILABLE FacCfgGn
  THEN s-TpoCmb = FacCfgGn.Tpocmb[2].
  ELSE s-TpoCmb = 1.
  
  RETURN "OK".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


