&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
         HEIGHT             = 5
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
  DEF INPUT-OUTPUT PARAMETER pCodMat AS CHAR.
  
  DEF SHARED VAR s-codcia AS INT.

  /* CODIGO NORMAL */
  FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = pCodMat
    NO-LOCK NO-ERROR.
  IF AVAILABLE Almmmatg THEN DO:
      RETURN.
  END.

  /* EAN 13 */
  FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codbrr = pCodMat
    AND Almmmatg.tpoart <> 'D'      /* NO Desactivados */
    NO-LOCK NO-ERROR.
  IF AMBIGUOUS Almmmatg THEN DO:
    MESSAGE 'Existe m�s de un producto registrado con el c�digo EAN 13' pCodMat
        VIEW-AS ALERT-BOX ERROR.
    pCodMat = ''.
    RETURN.
  END.

  IF NOT AVAILABLE Almmmatg THEN DO:
    ASSIGN
        pCodMat = STRING(INTEGER(SELF:SCREEN-VALUE), '999999')
        NO-ERROR.
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = pCodMat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE 'Art�culo NO registrado en el Cat�logo' VIEW-AS ALERT-BOX ERROR.
        pCodMat = ''.
        RETURN.
    END.
  END.
  pCodMat = Almmmatg.codmat.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

