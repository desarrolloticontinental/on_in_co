&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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
define variable s-aplic-id as character no-undo initial "@@".
define variable texto      as character no-undo format "x(80)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PF-G002

/* Definitions for FRAME F-Main                                         */
&Scoped-define FIELDS-IN-QUERY-F-Main PF-G002.CodMnu PF-G002.Tipo ~
PF-G002.Etiqueta PF-G002.Programa PF-G002.Persistente ~
PF-G002.Acceso-directo PF-G002.Tecla-Aceleradora PF-G002.Seguridad-Grupos ~
PF-G002.Icon 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F-Main PF-G002.CodMnu PF-G002.Tipo ~
PF-G002.Etiqueta PF-G002.Programa PF-G002.Persistente ~
PF-G002.Acceso-directo PF-G002.Seguridad-Grupos PF-G002.Icon 
&Scoped-define ENABLED-TABLES-IN-QUERY-F-Main PF-G002
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F-Main PF-G002

&Scoped-define FIELD-PAIRS-IN-QUERY-F-Main~
 ~{&FP1}CodMnu ~{&FP2}CodMnu ~{&FP3}~
 ~{&FP1}Etiqueta ~{&FP2}Etiqueta ~{&FP3}~
 ~{&FP1}Programa ~{&FP2}Programa ~{&FP3}~
 ~{&FP1}Icon ~{&FP2}Icon ~{&FP3}
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH PF-G002 SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F-Main PF-G002
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main PF-G002


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS PF-G002.CodMnu PF-G002.Tipo PF-G002.Etiqueta ~
PF-G002.Programa PF-G002.Persistente PF-G002.Acceso-directo ~
PF-G002.Seguridad-Grupos PF-G002.Icon 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}CodMnu ~{&FP2}CodMnu ~{&FP3}~
 ~{&FP1}Etiqueta ~{&FP2}Etiqueta ~{&FP3}~
 ~{&FP1}Programa ~{&FP2}Programa ~{&FP3}~
 ~{&FP1}Icon ~{&FP2}Icon ~{&FP3}
&Scoped-define ENABLED-TABLES PF-G002
&Scoped-define FIRST-ENABLED-TABLE PF-G002
&Scoped-Define ENABLED-OBJECTS BUTTON-4 BUTTON-15 BUTTON-5 BUTTON-16 ~
BUTTON-6 BUTTON-3 BUTTON-7 BUTTON-1 RECT-1 BUTTON-14 BUTTON-8 BUTTON-2 ~
BUTTON-9 BUTTON-10 BUTTON-13 BUTTON-12 BUTTON-11 BUTTON-17 
&Scoped-Define DISPLAYED-FIELDS PF-G002.CodMnu PF-G002.Tipo ~
PF-G002.Etiqueta PF-G002.Programa PF-G002.Persistente ~
PF-G002.Acceso-directo PF-G002.Tecla-Aceleradora PF-G002.Seguridad-Grupos ~
PF-G002.Icon 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "&Tecla" 
     SIZE 7.14 BY .81.

DEFINE BUTTON BUTTON-10 
     LABEL "<<" 
     SIZE 8 BY .85.

DEFINE BUTTON BUTTON-11 
     LABEL ">>" 
     SIZE 8 BY .85.

DEFINE BUTTON BUTTON-12 
     LABEL "Borrar" 
     SIZE 6 BY .85.

DEFINE BUTTON BUTTON-13 
     LABEL "Insertar" 
     SIZE 6 BY .85.

DEFINE BUTTON BUTTON-14 
     LABEL "..." 
     SIZE 2.86 BY .81.

DEFINE BUTTON BUTTON-15 
     LABEL "Aceptar" 
     SIZE 8 BY .85.

DEFINE BUTTON BUTTON-16 
     LABEL "Cancelar" 
     SIZE 8 BY .85.

DEFINE BUTTON BUTTON-17 
     LABEL "Ayuda" 
     SIZE 8 BY .85.

DEFINE BUTTON BUTTON-2 
     LABEL "&Limpiar" 
     SIZE 7.14 BY .81.

DEFINE BUTTON BUTTON-3 
     LABEL "Button 3" 
     SIZE 7.14 BY 1.42.

DEFINE BUTTON BUTTON-4 
     LABEL "&Insertar" 
     SIZE 7.14 BY .85.

DEFINE BUTTON BUTTON-5 
     LABEL "&Borrar" 
     SIZE 7.14 BY .85.

DEFINE BUTTON BUTTON-6 
     LABEL "&Linea" 
     SIZE 8 BY .85.

DEFINE BUTTON BUTTON-7 
     LABEL "&Separador" 
     SIZE 8 BY .85.

DEFINE BUTTON BUTTON-8 
     LABEL "&Arriba" 
     SIZE 7.14 BY .85.

DEFINE BUTTON BUTTON-9 
     LABEL "Abajo" 
     SIZE 7.14 BY .85.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 31.43 BY 4.46.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F-Main FOR 
      PF-G002 SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-4 AT ROW 7.88 COL 1.57
     BUTTON-15 AT ROW 17.12 COL 2.14
     BUTTON-5 AT ROW 7.88 COL 8.86
     PF-G002.CodMnu AT ROW 1.19 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .81
     PF-G002.Tipo AT ROW 2.04 COL 9 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 6
          LIST-ITEMS "SUB-MENU","SEPARADOR","LINEA","PROCESO" 
          SIZE 18.43 BY 1
     PF-G002.Etiqueta AT ROW 3 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25.86 BY .81
     PF-G002.Programa AT ROW 3.92 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 25.72 BY .81
     PF-G002.Persistente AT ROW 5 COL 11.14
          VIEW-AS TOGGLE-BOX
          SIZE 11.14 BY .65
     PF-G002.Acceso-directo AT ROW 5.81 COL 11.14
          VIEW-AS TOGGLE-BOX
          SIZE 14.14 BY .65
     PF-G002.Tecla-Aceleradora AT ROW 6.85 COL 12.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.43 BY .81
     BUTTON-16 AT ROW 17.12 COL 11.14
     BUTTON-6 AT ROW 7.88 COL 19.57
     BUTTON-3 AT ROW 4.96 COL 25.57
     BUTTON-7 AT ROW 7.88 COL 28
     BUTTON-1 AT ROW 6.85 COL 34.43
     PF-G002.Seguridad-Grupos AT ROW 1.62 COL 40.72 NO-LABEL
          VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
          SIZE 23.43 BY 3.81
     BUTTON-14 AT ROW 3.92 COL 36.72
     PF-G002.Icon AT ROW 5.81 COL 38.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31.14 BY .81
     BUTTON-8 AT ROW 7.88 COL 38.57
     BUTTON-2 AT ROW 6.85 COL 42.86
     BUTTON-9 AT ROW 7.88 COL 46
     BUTTON-10 AT ROW 7.88 COL 55.43
     BUTTON-13 AT ROW 2.23 COL 64.72
     BUTTON-12 AT ROW 3.23 COL 64.72
     BUTTON-11 AT ROW 7.88 COL 63.72
     BUTTON-17 AT ROW 17.12 COL 63.14
     RECT-1 AT ROW 1.15 COL 40.14
     " Grupos de Acceso" VIEW-AS TEXT
          SIZE 13.72 BY .5 AT ROW 1.08 COL 41.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71.86 BY 17.23
         FONT 4.

DEFINE FRAME F-OPCIONES
    WITH DOWN KEEP-TAB-ORDER OVERLAY 
         NO-LABELS SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.57 ROW 8.88
         SCROLLABLE SIZE 71 BY 8.73
         BGCOLOR 15 FGCOLOR 0 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Configurador de Menús"
         HEIGHT             = 17.27
         WIDTH              = 72
         MAX-HEIGHT         = 21.27
         MAX-WIDTH          = 109.57
         VIRTUAL-HEIGHT     = 21.27
         VIRTUAL-WIDTH      = 109.57
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F-OPCIONES:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F-OPCIONES:MOVE-AFTER-TAB-ITEM (BUTTON-4:HANDLE IN FRAME F-Main)
       XXTABVALXX = FRAME F-OPCIONES:MOVE-BEFORE-TAB-ITEM (BUTTON-15:HANDLE IN FRAME F-Main)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN PF-G002.Tecla-Aceleradora IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F-OPCIONES
                                                                        */
ASSIGN 
       FRAME F-OPCIONES:HEIGHT           = 8.08
       FRAME F-OPCIONES:WIDTH            = 70.72.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "integral.PF-G002"
     _Query            is OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-OPCIONES
/* Query rebuild information for FRAME F-OPCIONES
     _Query            is NOT OPENED
*/  /* FRAME F-OPCIONES */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{scr/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Configurador de Menús */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Configurador de Menús */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

define frame f-opciones
    texto VIEW-AS TEXT
    WITH 1000 DOWN.

/* Include custom  Main Block code for SmartWindows. */
{scr/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {scr/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {scr/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
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

  {&OPEN-QUERY-F-Main}
  GET FIRST F-Main.
  IF AVAILABLE PF-G002 THEN 
    DISPLAY PF-G002.CodMnu PF-G002.Tipo PF-G002.Etiqueta PF-G002.Programa 
          PF-G002.Persistente PF-G002.Acceso-directo PF-G002.Tecla-Aceleradora 
          PF-G002.Seguridad-Grupos PF-G002.Icon 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-4 BUTTON-15 BUTTON-5 PF-G002.CodMnu PF-G002.Tipo 
         PF-G002.Etiqueta PF-G002.Programa PF-G002.Persistente 
         PF-G002.Acceso-directo BUTTON-16 BUTTON-6 BUTTON-3 BUTTON-7 BUTTON-1 
         RECT-1 PF-G002.Seguridad-Grupos BUTTON-14 PF-G002.Icon BUTTON-8 
         BUTTON-2 BUTTON-9 BUTTON-10 BUTTON-13 BUTTON-12 BUTTON-11 BUTTON-17 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME F-OPCIONES IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-OPCIONES}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Pinta-Opciones.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pinta-Opciones W-Win 
PROCEDURE Pinta-Opciones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH PF-G002 
  WHERE PF-G002.Aplic-Id = s-aplic-id
  BY PF-G002.CodMnu
  WITH FRAME F-OPCIONES:
    CASE PF-G002.Tipo:
        WHEN "SUB-MENU" THEN TEXTO = PF-G002.Etiqueta.
        WHEN "PROCESO" THEN TEXTO = PF-G002.Etiqueta.
        WHEN "LINEA"     THEN TEXTO = FILL("-", 100).
        WHEN "SEPARADOR" THEN TEXTO = " ".
        OTHERWISE        TEXTO = PF-G002.Etiqueta.
     END CASE.

    CASE LENGTH( PF-G002.CodMnu ):
        WHEN 2 THEN TEXTO = TEXTO.
        WHEN 4 THEN TEXTO = "   " + TEXTO.
        WHEN 6 THEN TEXTO = "      " + TEXTO.
        OTHERWISE   TEXTO = "        " + TEXTO.
    END CASE.
    DISPLAY TEXTO.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {scr/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {scr/adm/template/snd-list.i "PF-G002"}

  /* Deal with any unexpected table requests before closing.           */
  {scr/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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


