&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
{bin/s-global.i}
{pln/s-global.i}

DEF VAR RB-REPORT-LIBRARY AS CHAR INIT ''.          /* Archivo PRL a usar */
DEF VAR RB-REPORT-NAME AS CHAR INIT ''.             /* Nombre del reporte */
DEF VAR RB-INCLUDE-RECORDS AS CHAR INIT ''.         /* "O" si necesita filtro */
DEF VAR RB-FILTER AS CHAR INIT ''.                  /* Filtro de impresion */
DEF VAR RB-OTHER-PARAMETERS AS CHAR INIT ''.        /* Otros parametros */
DEF VAR MyPath AS CHAR.

GET-KEY-VALUE SECTION 'Startup' KEY 'BASE' VALUE MyPath.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-NroSem RADIO-SET-Orden BUTTON-1 ~
BUTTON-2 BUTTON-3 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-NroSem RADIO-SET-Orden 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "img\b-ok":U
     LABEL "Button 1" 
     SIZE 12 BY 1.54.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "img\b-cancel":U
     LABEL "Button 2" 
     SIZE 12 BY 1.54.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "img\excel":U
     LABEL "Button 3" 
     SIZE 12 BY 1.54.

DEFINE VARIABLE COMBO-BOX-NroSem AS INTEGER FORMAT "99":U INITIAL 1 
     LABEL "Semana" 
     VIEW-AS COMBO-BOX INNER-LINES 13
     LIST-ITEMS "01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52" 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE RADIO-SET-Orden AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Codigo", 1,
"Nombre", 2,
"Seccion", 3
     SIZE 12 BY 3 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-BOX-NroSem AT ROW 1.58 COL 19 COLON-ALIGNED
     RADIO-SET-Orden AT ROW 2.92 COL 21 NO-LABEL
     BUTTON-1 AT ROW 1.38 COL 46
     BUTTON-2 AT ROW 3.12 COL 46
     BUTTON-3 AT ROW 4.85 COL 46
     "Ordenado por:" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 2.92 COL 11
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60.14 BY 5.77
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Reporte de Obreros"
         HEIGHT             = 5.73
         WIDTH              = 60.43
         MAX-HEIGHT         = 20
         MAX-WIDTH          = 90.72
         VIRTUAL-HEIGHT     = 20
         VIRTUAL-WIDTH      = 90.72
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Reporte de Obreros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Reporte de Obreros */
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
  ASSIGN
    COMBO-BOX-NroSem RADIO-SET-Orden.
  RUN Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO:
  RUN dispatch IN THIS-PROCEDURE ('exit':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Button 3 */
DO:
  ASSIGN
    COMBO-BOX-NroSem RADIO-SET-Orden.
  RUN Excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY COMBO-BOX-NroSem RADIO-SET-Orden 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE COMBO-BOX-NroSem RADIO-SET-Orden BUTTON-1 BUTTON-2 BUTTON-3 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel W-Win 
PROCEDURE Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER init 1.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 2.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
chWorkSheet:Columns("A"):ColumnWidth = 10.
chWorkSheet:Columns("B"):ColumnWidth = 20.
chWorkSheet:Columns("C"):ColumnWidth = 20.
chWorkSheet:Columns("D"):ColumnWidth = 20.
chWorkSheet:Columns("E"):ColumnWidth = 20.
chWorkSheet:Columns("F"):ColumnWidth = 20.

chWorkSheet:Range("A2"):Value = "Codigo".
chWorkSheet:Range("B2"):Value = "Ap. Paterno".
chWorkSheet:Range("C2"):Value = "Ap. Materno".
chWorkSheet:Range("D2"):Value = "Nombres".
chWorkSheet:Range("E2"):Value = "Cargo".
chWorkSheet:Range("F2"):Value = "Seccion".
chWorkSheet:Range("G2"):Value = "Sexo".
chWorkSheet:Range("H2"):Value = "Distrito".
chWorkSheet:Range("I2"):Value = "Direccion".
chWorkSheet:Range("J2"):Value = "Grado Instruccion".
chWorkSheet:Range("K2"):Value = "Fecha de Ingreso".

CASE RADIO-SET-Orden:
WHEN 1 THEN DO:
    FOR EACH pl-flg-sem WHERE pl-flg-sem.codcia = s-codcia
            AND pl-flg-sem.codpln = 02
            AND pl-flg-sem.periodo = s-periodo
            AND pl-flg-sem.nrosem = COMBO-BOX-NroSem
            AND pl-flg-sem.sitact <> 'Inactivo' NO-LOCK,
            FIRST pl-pers WHERE pl-pers.codper = pl-flg-sem.codper NO-LOCK:
        t-column = t-column + 1.
        cColumn = STRING(t-Column).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + pl-flg-sem.codper.
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.patper.
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.matper.
        cRange = "D" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.nomper.
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-flg-sem.cargo.
        cRange = "F" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-flg-sem.seccion.
        cRange = "G" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.sexper.
        cRange = "H" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.distri.
        cRange = "I" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.dirper.
        cRange = "J" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.titulo.
        cRange = "K" + cColumn.
        chWorkSheet:Range(cRange):Value = PL-FLG-SEM.fecing.
    END.            
END.
WHEN 2 THEN DO:
    FOR EACH pl-flg-sem WHERE pl-flg-sem.codcia = s-codcia
            AND pl-flg-sem.codpln = 02
            AND pl-flg-sem.periodo = s-periodo
            AND pl-flg-sem.nrosem = COMBO-BOX-NroSem
            AND pl-flg-sem.sitact <> 'Inactivo' NO-LOCK,
            FIRST pl-pers WHERE pl-pers.codper = pl-flg-sem.codper NO-LOCK
            BY pl-pers.patper BY pl-pers.matper BY pl-pers.nomper:
        t-column = t-column + 1.
        cColumn = STRING(t-Column).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + pl-flg-sem.codper.
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.patper.
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.matper.
        cRange = "D" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.nomper.
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-flg-sem.cargo.
        cRange = "F" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-flg-sem.seccion.
        cRange = "G" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.sexper.
        cRange = "H" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.distri.
        cRange = "I" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.dirper.
        cRange = "J" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.titulo.
        cRange = "K" + cColumn.
        chWorkSheet:Range(cRange):Value = PL-FLG-SEM.fecing.
    END.            
END.
WHEN 3 THEN DO:
    FOR EACH pl-flg-sem WHERE pl-flg-sem.codcia = s-codcia
            AND pl-flg-sem.codpln = 02
            AND pl-flg-sem.periodo = s-periodo
            AND pl-flg-sem.nrosem = COMBO-BOX-NroSem
            AND pl-flg-sem.sitact <> 'Inactivo' NO-LOCK,
            FIRST pl-pers WHERE pl-pers.codper = pl-flg-sem.codper NO-LOCK
            BY pl-flg-sem.seccion:
        t-column = t-column + 1.
        cColumn = STRING(t-Column).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + pl-flg-sem.codper.
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.patper.
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.matper.
        cRange = "D" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.nomper.
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-flg-sem.cargo.
        cRange = "F" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-flg-sem.seccion.
        cRange = "G" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.sexper.
        cRange = "H" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.distri.
        cRange = "I" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.dirper.
        cRange = "J" + cColumn.
        chWorkSheet:Range(cRange):Value = pl-pers.titulo.
        cRange = "K" + cColumn.
        chWorkSheet:Range(cRange):Value = PL-FLG-SEM.fecing.
    END.            
END.
END CASE.
/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir W-Win 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Parametros de impresion */
  RB-REPORT-LIBRARY = mypath + 'pln/reporte.prl'.
  RB-REPORT-NAME = 'Maestro de Personal Obrero'.
  CASE RADIO-SET-Orden:
    WHEN 2 THEN RB-REPORT-NAME = 'Maestro de Obreros x Nombre'.
    WHEN 3 THEN RB-REPORT-NAME = 'Maestro de Obreros x Seccion'.
  END CASE.
  RB-INCLUDE-RECORDS = 'O'.
  RB-FILTER = 'pl-flg-sem.codcia = ' + STRING(s-codcia) +
                ' and pl-flg-sem.codpln = 02' +
                ' and pl-flg-sem.periodo = ' + STRING(s-periodo) +
                ' and pl-flg-sem.nrosem = ' + STRING(COMBO-BOX-NroSem) +
                ' and pl-flg-sem.SitAct <> "Inactivo"'.
  RB-OTHER-PARAMETERS = 's-nomcia = ' + s-nomcia.
  RUN lib/_imprime (RB-REPORT-LIBRARY,
                RB-REPORT-NAME,
                RB-INCLUDE-RECORDS,
                RB-FILTER,
                RB-OTHER-PARAMETERS).
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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


