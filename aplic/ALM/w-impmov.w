&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-CMOV NO-UNDO LIKE INTEGRAL.Almcmov.
DEFINE TEMP-TABLE T-DMOV NO-UNDO LIKE INTEGRAL.Almdmov.


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
DEF FRAME F-Mensaje
    " Procesando informacion "
    " Un momento por favor "
    WITH NO-LABELS CENTERED OVERLAY VIEW-AS DIALOG-BOX WIDTH 30 TITLE "Mensaje".

DEF VAR s-Button-1 AS LOGICAL INIT TRUE.
DEF VAR s-Button-2 AS LOGICAL INIT FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES T-CMOV

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 T-CMOV.CodAlm T-CMOV.FchDoc ~
T-CMOV.TipMov T-CMOV.CodMov T-CMOV.NroSer T-CMOV.NroDoc T-CMOV.Observ 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH T-CMOV NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 T-CMOV
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 T-CMOV


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 BROWSE-1 BUTTON-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "img\proces":U
     LABEL "Button 1" 
     SIZE 11 BY 1.73 TOOLTIP "Carga Temporal".

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "adeicon\rbuild%":U
     IMAGE-INSENSITIVE FILE "adeicon\stop-u":U
     LABEL "Button 2" 
     SIZE 11 BY 1.73 TOOLTIP "Actualiza Catalogo".

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "adeicon\exit-au":U
     LABEL "Button 3" 
     SIZE 11 BY 1.73 TOOLTIP "Salir".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      T-CMOV SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _STRUCTURED
  QUERY BROWSE-1 DISPLAY
      T-CMOV.CodAlm
      T-CMOV.FchDoc COLUMN-LABEL "Fecha Emisi�n"
      T-CMOV.TipMov COLUMN-LABEL "Tipo"
      T-CMOV.CodMov COLUMN-LABEL "C�digo"
      T-CMOV.NroSer COLUMN-LABEL "Seri N�"
      T-CMOV.NroDoc COLUMN-LABEL "Documento N�"
      T-CMOV.Observ
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 83 BY 13.27
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-1 AT ROW 1.19 COL 3
     BROWSE-1 AT ROW 3.12 COL 2
     BUTTON-2 AT ROW 1.19 COL 15
     BUTTON-3 AT ROW 1.19 COL 27
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.14 BY 15.85
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: T-CMOV T "?" NO-UNDO INTEGRAL Almcmov
      TABLE: T-DMOV T "?" NO-UNDO INTEGRAL Almdmov
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Importar Movimientos de Almac�n"
         HEIGHT             = 15.88
         WIDTH              = 86.29
         MAX-HEIGHT         = 18.12
         MAX-WIDTH          = 96.57
         VIRTUAL-HEIGHT     = 18.12
         VIRTUAL-WIDTH      = 96.57
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
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* BROWSE-TAB BROWSE-1 BUTTON-1 F-Main */
/* SETTINGS FOR BUTTON BUTTON-2 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "Temp-Tables.T-CMOV"
     _FldNameList[1]   = Temp-Tables.T-CMOV.CodAlm
     _FldNameList[2]   > Temp-Tables.T-CMOV.FchDoc
"T-CMOV.FchDoc" "Fecha Emisi�n" ? "date" ? ? ? ? ? ? no ?
     _FldNameList[3]   > Temp-Tables.T-CMOV.TipMov
"T-CMOV.TipMov" "Tipo" ? "character" ? ? ? ? ? ? no ?
     _FldNameList[4]   > Temp-Tables.T-CMOV.CodMov
"T-CMOV.CodMov" "C�digo" ? "integer" ? ? ? ? ? ? no ?
     _FldNameList[5]   > Temp-Tables.T-CMOV.NroSer
"T-CMOV.NroSer" "Seri N�" ? "integer" ? ? ? ? ? ? no ?
     _FldNameList[6]   > Temp-Tables.T-CMOV.NroDoc
"T-CMOV.NroDoc" "Documento N�" ? "integer" ? ? ? ? ? ? no ?
     _FldNameList[7]   = Temp-Tables.T-CMOV.Observ
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Importar Movimientos de Almac�n */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Importar Movimientos de Almac�n */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  IF s-Button-1 = YES
  THEN DO:
    MESSAGE "El archivo a importar debe estar en C:\TMP\ALMCMOV.TXT y C:\TMP\ALMDMOV.TXT" SKIP
      "Continuamos con el proceso?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE rpta-1 AS LOGICAL.
    IF rpta-1 = NO THEN RETURN NO-APPLY.
    VIEW FRAME F-Mensaje.
    FOR EACH T-CMOV:
      DELETE T-CMOV.
    END.
    FOR EACH T-DMOV:
        DELETE T-DMOV.
    END.
    INPUT FROM c:\tmp\almcmov.txt.
    REPEAT:
      CREATE T-CMOV.
      IMPORT T-CMOV.
    END.
    INPUT CLOSE.
    INPUT FROM c:\tmp\almdmov.txt.
    REPEAT:
      CREATE T-DMOV.
      IMPORT T-DMOV.
    END.
    INPUT CLOSE.
    FOR EACH T-CMOV WHERE T-CMOV.CodAlm = '':
        DELETE T-CMOV.
    END.
    FOR EACH T-DMOV WHERE T-DMOV.CodAlm = '':
        DELETE T-DMOV.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    HIDE FRAME F-Mensaje.
    ASSIGN
        BUTTON-2:SENSITIVE = YES
        s-Button-1 = NO
        s-Button-2 = YES.
    BUTTON-1:LOAD-IMAGE-UP('adeicon/stop-u').
  END.
  ELSE DO:
    ASSIGN
        BUTTON-2:SENSITIVE = NO
        s-Button-1 = YES
        s-Button-2 = NO.
    BUTTON-1:LOAD-IMAGE-UP('img/proces').
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO:
  RUN Importa.
  IF RETURN-VALUE = 'ADM-ERROR':U
  THEN RETURN NO-APPLY.
  ASSIGN
    BUTTON-2:SENSITIVE = NO
    s-Button-1 = YES
    s-Button-2 = NO.
  BUTTON-1:LOAD-IMAGE-UP('img/proces').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Button 3 */
DO:
  RUN dispatch IN THIS-PROCEDURE ('exit':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
/* VM - INCLUDE PARA LA CREACION DEL MENU BAR */
{src/adm/template/cntnrwin.i}

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

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
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  ENABLE BUTTON-1 BROWSE-1 BUTTON-3 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importa W-Win 
PROCEDURE Importa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST T-CMOV NO-ERROR.
  IF NOT AVAILABLE T-CMOV THEN RETURN 'ADM-ERROR':U.
  MESSAGE 'Confirme Inicio de la Importaci�n'
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE rpta-1 AS LOGICAL.
  IF rpta-1 = NO THEN RETURN 'ADM-ERROR':U.

  /* BLOQUEAMOS LOS TRIGGERS DE REPLICA */
  DISABLE TRIGGERS FOR LOAD OF almcmov.
  DISABLE TRIGGERS FOR LOAD OF almdmov.

  VIEW FRAME F-Mensaje.
  FOR EACH T-CMOV:
    FIND almcmov OF T-CMOV NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almcmov
    THEN DO:
        CREATE almcmov.
        BUFFER-COPY T-CMOV TO almcmov.
        FOR EACH T-DMOV OF T-CMOV:
            CREATE almdmov.
            BUFFER-COPY T-DMOV TO almdmov.
        END.
    END.
  END.
  FOR EACH T-CMOV:
    DELETE T-CMOV.
  END.
  FOR EACH T-DMOV:
    DELETE T-DMOV.
  END.
  HIDE FRAME F-Mensaje.

  MESSAGE "Importaci�n de Movimiento de Almac�n Completa" VIEW-AS ALERT-BOX WARNING.
  {&OPEN-QUERY-{&BROWSE-NAME}}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "T-CMOV"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

