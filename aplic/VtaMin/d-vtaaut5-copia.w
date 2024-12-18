&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE SHARED VARIABLE S-CODCIA AS INTEGER.

DEFINE OUTPUT PARAMETER X-CODMAT AS CHAR.
DEFINE OUTPUT PARAMETER X-CANPED AS DECI.

DEFINE VARIABLE lFlag AS LOGICAL     NO-UNDO.

ASSIGN
    x-CodMat = ?
    x-CanPed = 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS F-CODMAT F-CANPED Btn_OK Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS F-CODMAT F-CANPED 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "Cancelar" 
     SIZE 12 BY 1.08
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Aceptar" 
     SIZE 12 BY 1.08
     BGCOLOR 8 .

DEFINE VARIABLE F-CANPED AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cantidad" 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE F-CODMAT AS CHARACTER FORMAT "X(14)":U 
     LABEL "Articulo" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     F-CODMAT AT ROW 1.42 COL 3.71
     F-CANPED AT ROW 2.62 COL 2.71
     Btn_OK AT ROW 1.35 COL 42.57
     Btn_Done AT ROW 2.54 COL 42.57 WIDGET-ID 12
     SPACE(1.71) SKIP(0.60)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Registro".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{src/adm-vm/method/vmviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN F-CANPED IN FRAME D-Dialog
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN F-CODMAT IN FRAME D-Dialog
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Registro */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done D-Dialog
ON CHOOSE OF Btn_Done IN FRAME D-Dialog /* Cancelar */
DO:
  
  IF NOT lflag THEN 
      ASSIGN
        x-codmat = ''
        x-canped = 0.
        
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Aceptar */
DO:
  IF DECIMAL (F-Canped:SCREEN-VALUE) = 0 THEN DO:
     APPLY "ENTRY" TO F-CANPED.
     RETURN "ADM-ERROR".   
  END.    
  ASSIGN 
      x-codmat = F-CodMat:SCREEN-VALUE 
      x-canped = deci(F-Canped:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-CANPED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CANPED D-Dialog
ON LEAVE OF F-CANPED IN FRAME D-Dialog /* Cantidad */
DO:

    DEFINE VARIABLE ivar    AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE cLis01  AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cLis02  AS CHARACTER    NO-UNDO.

    ASSIGN 
        cLis01 = '018697,025983,025984,005165,025982,041093,019112,019100,032342,' +
                    '042863,018445,005162,005186,032742,041092,005208,005206,005209,' +
                    '005153,018698'. 
        cLis02 = '019349,022255,022252,004687,022250,022251,022257,019350,017387,' + 
                 '017388,015775,017386,017385,015580,015584,015581,015583,015582,' + 
                 '018709'.

/*     lFlag = YES.                                                                             */
/*     FIND FIRST almmmatg WHERE almmmatg.codcia = 1                                            */
/*         AND almmmatg.codmat = f-codmat:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR. */
/*     IF AVAIL almmmatg THEN DO:                                                               */
/*         ivar = DECI(SELF:SCREEN-VALUE).                                                      */
/*         IF almmmatg.codfam <> '020' AND (ivar - TRUNCATE(ivar,0)) > 0 THEN DO:               */
/*             MESSAGE 'Este producto no acepta decimales'                                      */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                           */
/*             lFlag = NO.                                                                      */
/*             APPLY 'Entry' TO f-canped.                                                       */
/*             RETURN NO-APPLY.                                                                 */
/*         END.                                                                                 */
/*     END.                                                                                     */
/*     IF DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN NO-APPLY.                                     */


    lFlag = YES.
    FIND FIRST almmmatg WHERE almmmatg.codcia = 1
        AND almmmatg.codmat = f-codmat:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
    IF AVAIL almmmatg THEN DO:
        ivar = DECI(SELF:SCREEN-VALUE).
        IF (ivar - TRUNCATE(ivar,0)) > 0 THEN DO:
            /*Primera Lista*/
            IF LOOKUP(Almmmatg.codmat,cLis01) > 0 THEN DO:
                IF (ivar - TRUNCATE(ivar,0)) <> 0.5 THEN DO:
                    MESSAGE '   Este producto no acepta    ' SKIP
                            'decimales no multiplos de 0.50'
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.            
                    lFlag = NO.                    
                END.
            END. 
            ELSE IF LOOKUP(Almmmatg.codmat,cLis02) > 0 THEN DO: /*Segunda Lista*/
                CASE (ivar - TRUNCATE(ivar,0)):
                    WHEN 0.25 THEN lFlag = YES.
                    WHEN 0.50 THEN lFlag = YES.
                    WHEN 0.75 THEN lFlag = YES.
                    OTHERWISE DO:
                        MESSAGE 'Este producto no acepta decimales' SKIP 
                                '    no m�ltiplos de 0.25         '
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.            
                        lFlag = NO.
                    END.
                END CASE.
            END.
            ELSE IF almmmatg.codfam <> '020' THEN DO:
                MESSAGE 'Este producto no acepta decimales'
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.            
                lFlag = NO.
                APPLY 'Entry' TO f-canped.
                RETURN NO-APPLY.            
            END.
                
            IF  NOT lFlag THEN DO:
                APPLY 'Entry' TO f-canped.
                RETURN NO-APPLY.            
            END.
        END.
    END. 
    IF DECI(SELF:SCREEN-VALUE) = 0 THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-CODMAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CODMAT D-Dialog
ON LEAVE OF F-CODMAT IN FRAME D-Dialog /* Articulo */
DO:
    IF SELF:SCREEN-VALUE = "" THEN RETURN.

/*     IF LENGTH(SELF:SCREEN-VALUE) <= 6 THEN DO:                              */
/*         ASSIGN                                                              */
/*             SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999") */
/*             NO-ERROR.                                                       */
/*     END.                                                                    */
/*     IF LENGTH(SELF:SCREEN-VALUE) > 6 THEN DO:                               */
/*         FIND FIRST Almmmatg WHERE Almmmatg.CodCia = S-CODCIA                */
/*             AND  Almmmatg.CodBrr = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.      */
/*         IF NOT AVAILABLE Almmmatg THEN DO:                                  */
/*             MESSAGE "Codigo de Articulo no Existe" VIEW-AS ALERT-BOX ERROR. */
/*             RETURN NO-APPLY.                                                */
/*         END.                                                                */
/*         SELF:SCREEN-VALUE = Almmmatg.CodMat.                                */
/*    END.                                                                     */
/*                                                                             */
/*    FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA                           */
/*                   AND  Almmmatg.codmat = SELF:SCREEN-VALUE                  */
/*                  NO-LOCK NO-ERROR.                                          */
/*    IF NOT AVAILABLE Almmmatg THEN DO:                                       */
/*       MESSAGE "Codigo de Articulo no Existe" VIEW-AS ALERT-BOX ERROR.       */
/*       RETURN NO-APPLY.                                                      */
/*    END.                                                                     */
/*    IF Almmmatg.TpoArt <> "A" THEN DO:                                       */
/*       MESSAGE "Articulo no Activo" VIEW-AS ALERT-BOX ERROR.                 */
/*       RETURN NO-APPLY.                                                      */
/*    END.                                                                     */

    DEF VAR pCodMat AS CHAR NO-UNDO.
    DEF VAR pCanPed AS DEC NO-UNDO.

    ASSIGN 
        pCodMat = SELF:SCREEN-VALUE.

    RUN alm/p-codbrr (INPUT-OUTPUT pCodMat, INPUT-OUTPUT pCanPed, s-codcia).

    IF pCodMat = '' THEN RETURN NO-APPLY.
    DISPLAY
        pCodMat @ f-CodMat
        pCanPed @ F-CANPED
        WITH FRAME {&FRAME-NAME}.
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = SELF:SCREEN-VALUE
        NO-LOCK.
    IF Almmmatg.Chr__01 = "" THEN DO:
       MESSAGE "Articulo no tiene unidad de Oficina" VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY F-CODMAT F-CANPED 
      WITH FRAME D-Dialog.
  ENABLE F-CODMAT F-CANPED Btn_OK Btn_Done 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit D-Dialog 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
MESSAGE "entra here!".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Parametros D-Dialog 
PROCEDURE Procesa-Parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    output-var-1 como ROWID
    output-var-2 como CHARACTER
    output-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recoge-Parametros D-Dialog 
PROCEDURE Recoge-Parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    input-var-1 como CHARACTER
    input-var-2 como CHARACTER
    input-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN ASSIGN input-var-1 = "".
        /*
            ASSIGN
                input-para-1 = ""
                input-para-2 = ""
                input-para-3 = "".
         */      
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

