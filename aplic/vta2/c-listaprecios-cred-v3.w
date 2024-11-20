&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME C-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Dialog 
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
DEFINE SHARED VARIABLE s-acceso-semaforos AS LOG.

/* Local Variable Definitions ---                                       */

DEFINE &IF "{&NEW}" = "" &THEN INPUT PARAMETER &ELSE VARIABLE &ENDIF titulo AS CHARACTER.

/* Poner aqui el nombre del l-lookup en vez de h_l-tabla */
&SCOPED-DEFINE l-lookup h_l-listaprecios-cred

DEFINE NEW SHARED VAR s-codmat AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartConsulta
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME C-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_l-listaprecios-cred AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-detart AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "img\b-cancel":U
     LABEL "&Cancelar" 
     SIZE 10.86 BY 1.65
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "img\b-ok":U
     LABEL "&Aceptar" 
     SIZE 10.86 BY 1.65
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME C-Dialog
     Btn_OK AT ROW 18.5 COL 92
     Btn_Cancel AT ROW 18.5 COL 103
     SPACE(6.27) SKIP(0.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "<T�tulo de Consulta>".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartConsulta
   Allow: Basic,Browse,DB-Fields,Smart,Query
   Container Links: 
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX C-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME C-Dialog:SCROLLABLE       = FALSE
       FRAME C-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX C-Dialog
/* Query rebuild information for DIALOG-BOX C-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX C-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Dialog C-Dialog
ON WINDOW-CLOSE OF FRAME C-Dialog /* <T�tulo de Consulta> */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Dialog
ON CHOOSE OF Btn_OK IN FRAME C-Dialog /* Aceptar */
DO:
    RUN captura-datos IN {&l-lookup}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Dialog 


/* ***************************  Main Block  *************************** */

IF titulo <> "" THEN ASSIGN FRAME {&FRAME-NAME}:TITLE = titulo.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vta2/l-listaprecios-cred-v3.w':U ,
             INPUT  FRAME C-Dialog:HANDLE ,
             INPUT  'Layout = ,
                     SortBy-Case = Codigo':U ,
             OUTPUT h_l-listaprecios-cred ).
       RUN set-position IN h_l-listaprecios-cred ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_l-listaprecios-cred ( 15.50 , 117.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'lgc/v-detart.r':U ,
             INPUT  FRAME C-Dialog:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-detart ).
       RUN set-position IN h_v-detart ( 16.58 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.00 , 36.57 ) */

       /* Links to SmartViewer h_v-detart. */
       RUN add-link IN adm-broker-hdl ( h_l-listaprecios-cred , 'Record':U , h_v-detart ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_l-listaprecios-cred ,
             Btn_OK:HANDLE , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-detart ,
             h_l-listaprecios-cred , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available C-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME C-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Dialog  _DEFAULT-ENABLE
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
  ENABLE Btn_OK Btn_Cancel 
      WITH FRAME C-Dialog.
  VIEW FRAME C-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-C-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize C-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    RUN toma-handle IN {&l-lookup} ( THIS-PROCEDURE ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-aceptar C-Dialog 
PROCEDURE p-aceptar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    APPLY "CHOOSE" TO Btn_OK IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records C-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartConsulta, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed C-Dialog 
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

