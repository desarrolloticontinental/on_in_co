&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME W-Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Consulta 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 06/26/95 - 12:00 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE VARIABLE pto AS LOGICAL.
pto = SESSION:SET-WAIT-STATE("").

&IF "{&NEW}" = "" &THEN
    DEFINE INPUT  PARAMETER x-tabla  AS CHARACTER.
    DEFINE OUTPUT PARAMETER RegAct   AS RECID.    
&ELSE
    DEFINE VARIABLE x-tabla  AS CHARACTER INITIAL "01".
    DEFINE VARIABLE RegAct   AS RECID.
&ENDIF
RegAct = 0.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME W-Consulta
&Scoped-define BROWSE-NAME brw-consulta

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cb-tabl

/* Definitions for BROWSE brw-consulta                                  */
&Scoped-define FIELDS-IN-QUERY-brw-consulta cb-tabl.Codigo cb-tabl.Nombre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw-consulta 
&Scoped-define QUERY-STRING-brw-consulta FOR EACH cb-tabl ~
      WHERE cb-tabl.Tabla = x-Tabla ~
 AND (INDEX(cb-tabl.Nombre,Busca:SCREEN-VALUE) > 0 OR Busca:SCREEN-VALUE = "") NO-LOCK
&Scoped-define OPEN-QUERY-brw-consulta OPEN QUERY brw-consulta FOR EACH cb-tabl ~
      WHERE cb-tabl.Tabla = x-Tabla ~
 AND (INDEX(cb-tabl.Nombre,Busca:SCREEN-VALUE) > 0 OR Busca:SCREEN-VALUE = "") NO-LOCK.
&Scoped-define TABLES-IN-QUERY-brw-consulta cb-tabl
&Scoped-define FIRST-TABLE-IN-QUERY-brw-consulta cb-tabl


/* Definitions for DIALOG-BOX W-Consulta                                */
&Scoped-define OPEN-BROWSERS-IN-QUERY-W-Consulta ~
    ~{&OPEN-QUERY-brw-consulta}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-2 brw-consulta Busca b-aceptar ~
b-cancelar 
&Scoped-Define DISPLAYED-OBJECTS Busca 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-aceptar AUTO-GO 
     IMAGE-UP FILE "img\b-ok":U
     LABEL "&Acepta" 
     SIZE 11.57 BY 1.42.

DEFINE BUTTON b-cancelar AUTO-END-KEY 
     IMAGE-UP FILE "img\b-cancel":U
     LABEL "&Cancela" 
     SIZE 11.57 BY 1.42.

DEFINE VARIABLE Busca AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .69
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 54 BY 1.88
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 54 BY 9.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brw-consulta FOR 
      cb-tabl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brw-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw-consulta W-Consulta _STRUCTURED
  QUERY brw-consulta NO-LOCK DISPLAY
      cb-tabl.Codigo COLUMN-LABEL "C�digo" FORMAT "x(5)":U
      cb-tabl.Nombre COLUMN-LABEL "Descripci�n" FORMAT "x(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 52 BY 7.98
         BGCOLOR 15 FGCOLOR 0 FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME W-Consulta
     brw-consulta AT ROW 1.54 COL 3 HELP
          "`"
     Busca AT ROW 9.62 COL 3 NO-LABEL
     b-aceptar AT ROW 10.96 COL 25
     b-cancelar AT ROW 10.96 COL 38.72
     RECT-3 AT ROW 1.27 COL 2
     RECT-2 AT ROW 10.69 COL 2
     SPACE(1.42) SKIP(0.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4
         TITLE "Clasificaci�n de Auxiliares/Tablas SUNAT".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX W-Consulta
   FRAME-NAME                                                           */
/* BROWSE-TAB brw-consulta RECT-2 W-Consulta */
ASSIGN 
       FRAME W-Consulta:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN Busca IN FRAME W-Consulta
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw-consulta
/* Query rebuild information for BROWSE brw-consulta
     _TblList          = "INTEGRAL.cb-tabl"
     _Options          = "NO-LOCK"
     _Where[1]         = "cb-tabl.Tabla = x-Tabla
 AND (INDEX(cb-tabl.Nombre,Busca:SCREEN-VALUE) > 0 OR Busca:SCREEN-VALUE = """")"
     _FldNameList[1]   > integral.cb-tabl.Codigo
"Codigo" "C�digo" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > integral.cb-tabl.Nombre
"Nombre" "Descripci�n" "x(60)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brw-consulta */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME b-aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-aceptar W-Consulta
ON CHOOSE OF b-aceptar IN FRAME W-Consulta /* Acepta */
DO:
      RegAct = RECID( {&FIRST-TABLE-IN-QUERY-brw-consulta} ).      
      APPLY "GO" TO brw-consulta.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brw-consulta
&Scoped-define SELF-NAME brw-consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw-consulta W-Consulta
ON ANY-PRINTABLE OF brw-consulta IN FRAME W-Consulta
DO:
    APPLY "ENTRY" TO Busca.
    APPLY LASTKEY TO Busca. 
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw-consulta W-Consulta
ON GO OF brw-consulta IN FRAME W-Consulta
DO:
    APPLY "CHOOSE" TO b-aceptar.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw-consulta W-Consulta
ON MOUSE-SELECT-DBLCLICK OF brw-consulta IN FRAME W-Consulta
DO:
    APPLY "CHOOSE" TO b-aceptar.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brw-consulta W-Consulta
ON RETURN OF brw-consulta IN FRAME W-Consulta
DO:
    APPLY "CHOOSE" TO b-aceptar.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Busca W-Consulta
ON LEAVE OF Busca IN FRAME W-Consulta
DO:
    {&OPEN-QUERY-brw-consulta}
    APPLY "ENTRY" TO brw-consulta.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Busca W-Consulta
ON RETURN OF Busca IN FRAME W-Consulta
DO:
    APPLY "ENTRY" TO brw-consulta.
    RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Consulta 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
/*
FRAME W-Consulta:X    = CURRENT-WINDOW:WIDTH-PIXELS -
                          FRAME W-Consulta:WIDTH-PIXELS.
FRAME W-Consulta:Y    = CURRENT-WINDOW:HEIGHT-PIXELS -
                         FRAME W-Consulta:HEIGHT-PIXELS + 20.
*/
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Consulta  _DEFAULT-DISABLE
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
  HIDE FRAME W-Consulta.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Consulta  _DEFAULT-ENABLE
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
  DISPLAY Busca 
      WITH FRAME W-Consulta.
  ENABLE RECT-3 RECT-2 brw-consulta Busca b-aceptar b-cancelar 
      WITH FRAME W-Consulta.
  {&OPEN-BROWSERS-IN-QUERY-W-Consulta}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

