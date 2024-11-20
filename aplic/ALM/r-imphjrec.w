&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
/*{src/bin/_prns.i} */  /* Para la impresion */
{src/adm2/widgetprto.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE SHARED VAR S-CODCIA AS INTEGER.
DEFINE SHARED VAR CL-CODCIA AS INTEGER.
DEFINE SHARED VAR S-NOMCIA AS CHARACTER.
DEFINE SHARED VAR S-CODALM AS CHARACTER.
DEFINE SHARED VAR S-DESALM AS CHARACTER.
DEFINE SHARED VAR S-USER-ID AS CHAR.


DEFINE VAR s-task-no AS INTEGER NO-UNDO.
DEFINE VAR cAlmc     AS CHARACTER NO-UNDO.

DEFINE VAR S-CODMOV LIKE ALMTMOVM.CODMOV.
FIND FacDocum WHERE codcia = s-codcia 
    AND coddoc = 'D/F'
    NO-LOCK NO-ERROR.
IF AVAILABLE facdocum THEN S-CODMOV = facdocum.codmov.

DEFINE VAR RUTA AS CHAR NO-UNDO.
GET-KEY-VALUE SECTION "STARTUP" KEY "BASE" VALUE RUTA.

DEFINE BUFFER DMOV FOR Almdmov.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS r-pages x-nropages x-codAlm BUTTON-1 r-imp ~
btn-Excel Btn_Ok Btn_Done x-zona RECT-71 RECT-72 RECT-73 RECT-74 
&Scoped-Define DISPLAYED-OBJECTS r-pages x-nropages x-mensaje x-codAlm ~
r-imp x-zona 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-Excel 
     IMAGE-UP FILE "IMG/excel.bmp":U
     LABEL "Button 1" 
     SIZE 12 BY 1.54.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "img\b-cancel":U
     LABEL "Cancelar" 
     SIZE 12 BY 1.54
     BGCOLOR 8 .

DEFINE BUTTON Btn_Ok 
     IMAGE-UP FILE "img\b-ok":U
     LABEL "Aceptar" 
     SIZE 12 BY 1.54.

DEFINE BUTTON BUTTON-1 
     LABEL "..." 
     SIZE 3 BY .88.

DEFINE VARIABLE x-codAlm AS CHARACTER FORMAT "X(256)":U 
     LABEL "Almacen" 
     VIEW-AS FILL-IN 
     SIZE 55 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY .81 NO-UNDO.

DEFINE VARIABLE x-nropages AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .81 NO-UNDO.

DEFINE VARIABLE x-zona AS CHARACTER FORMAT "X(6)":U 
     LABEL "Zona" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE r-imp AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Impresi�n Laser", 1,
"Impresion Matricial", 2
     SIZE 52 BY .81 NO-UNDO.

DEFINE VARIABLE r-pages AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todas", 1,
"P�ginas: ", 2
     SIZE 9 BY 1.88 NO-UNDO.

DEFINE RECTANGLE RECT-71
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 70.14 BY 1.88
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-72
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72.29 BY 4.04.

DEFINE RECTANGLE RECT-73
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72.29 BY 2.81.

DEFINE RECTANGLE RECT-74
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72.29 BY 4.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     r-pages AT ROW 4.92 COL 3.72 NO-LABEL WIDGET-ID 52
     x-nropages AT ROW 6 COL 10.72 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     x-mensaje AT ROW 9.96 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     x-codAlm AT ROW 1.88 COL 9 COLON-ALIGNED WIDGET-ID 28
     BUTTON-1 AT ROW 1.88 COL 66 WIDGET-ID 32
     r-imp AT ROW 8.81 COL 4 NO-LABEL WIDGET-ID 16
     btn-Excel AT ROW 11.08 COL 35.43 WIDGET-ID 26
     Btn_Ok AT ROW 11.08 COL 47.43
     Btn_Done AT ROW 11.08 COL 59.43
     x-zona AT ROW 2.85 COL 9 COLON-ALIGNED WIDGET-ID 42
     "Ingrese n�mero de p�ginas o rango de p�ginas separadas" VIEW-AS TEXT
          SIZE 41 BY .5 AT ROW 6.81 COL 12.72 WIDGET-ID 46
     "por coma. Por ejemplo:1,2,5-10." VIEW-AS TEXT
          SIZE 41 BY .5 AT ROW 7.42 COL 12.72 WIDGET-ID 48
     "Rango P�ginas" VIEW-AS TEXT
          SIZE 11.72 BY .5 AT ROW 4.23 COL 3.29 WIDGET-ID 58
          FGCOLOR 1 
     "Criterios Impresi�n" VIEW-AS TEXT
          SIZE 12.72 BY .5 AT ROW 1.27 COL 3.29 WIDGET-ID 60
          FGCOLOR 1 
     RECT-71 AT ROW 10.92 COL 2.43 WIDGET-ID 24
     RECT-72 AT ROW 4.42 COL 1.72 WIDGET-ID 50
     RECT-73 AT ROW 1.42 COL 1.72 WIDGET-ID 56
     RECT-74 AT ROW 8.54 COL 1.72 WIDGET-ID 62
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 74.14 BY 12.31
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
         TITLE              = "IMPRESION HOJA DE RECONTEO (Diferencias)"
         HEIGHT             = 12.31
         WIDTH              = 74.14
         MAX-HEIGHT         = 12.31
         MAX-WIDTH          = 74.14
         VIRTUAL-HEIGHT     = 12.31
         VIRTUAL-WIDTH      = 74.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN x-mensaje IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* IMPRESION HOJA DE RECONTEO (Diferencias) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* IMPRESION HOJA DE RECONTEO (Diferencias) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-Excel W-Win
ON CHOOSE OF btn-Excel IN FRAME F-Main /* Button 1 */
DO:
    ASSIGN 
        x-CodAlm 
        r-imp 
        r-pages
        x-nropages
        x-zona.
    RUN Carga-Temporal.
    RUN Excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Win
ON CHOOSE OF Btn_Done IN FRAME F-Main /* Cancelar */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ok W-Win
ON CHOOSE OF Btn_Ok IN FRAME F-Main /* Aceptar */
DO:
  ASSIGN 
      x-CodAlm 
      r-imp 
      r-pages
      x-nropages
      x-zona.
  RUN Carga-Temporal.
  RUN Imprime.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* ... */
DO:

    DEF VAR x-Almacenes AS CHAR.
    x-Almacenes = x-CodAlm:SCREEN-VALUE.
    RUN alm/d-repalm (INPUT-OUTPUT x-Almacenes).
    x-CodAlm:SCREEN-VALUE = x-Almacenes.
    cAlmc = x-Almacenes.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal W-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR L-Ubica AS LOGICAL INIT YES.    
    DEFINE VAR cCodAlm AS CHARACTER NO-UNDO.
    DEFINE VAR i       AS INTEGER   NO-UNDO INIT 0.
    DEFINE VAR cNomCia AS CHARACTER   NO-UNDO.
    DEFINE VAR iVarA   AS INTEGER     NO-UNDO.
    DEFINE VAR iVarB   AS INTEGER     NO-UNDO.
    DEFINE VAR cVar    AS CHARACTER   NO-UNDO.
    DEFINE VAR iint    AS INTEGER     NO-UNDO.
    DEFINE VAR iint2   AS INTEGER     NO-UNDO.
    DEFINE VAR x-nropages-2  AS CHARACTER   NO-UNDO.

    DEFINE VAR lDiferencias AS LOGICAL.

    REPEAT WHILE L-Ubica:
        s-task-no = RANDOM(900000,999999).
        FIND FIRST w-report WHERE w-report.task-no = s-task-no NO-LOCK NO-ERROR.
        IF NOT AVAILABLE w-report THEN L-Ubica = NO.
    END.

    cNomCia = "CONTI".
    cAlmc = x-codalm:SCREEN-VALUE IN FRAME {&FRAME-NAME}. 

    /*Determina Paginas*/
   /* MESSAGE 'PAGES ' R-PAGES SKIP NUM-ENTRIES(x-nropages).*/

    IF r-pages = 2 THEN DO:
        DO iint = 1 TO NUM-ENTRIES(x-nropages):
            cvar = ''.
            IF INDEX(ENTRY(iint,x-nropages),'-') > 0 THEN DO:
                iVarA = INT(SUBSTRING(ENTRY(iint,x-nropages),1,(INDEX(ENTRY(iint,x-nropages),'-') - 1))).
                iVarB = INT(SUBSTRING(ENTRY(iint,x-nropages),(INDEX(ENTRY(iint,x-nropages),'-') + 1))).
               /* MESSAGE 'a ' ivara  SKIP 'b ' ivarb.  */
                DO iint2 = iVarA TO iVarB:
                    cvar = cvar + STRING(iint2) + ','.
                END.
                cvar = SUBSTRING(cvar,1,INT(LENGTH(cvar) - 1)).
            END.
            ELSE cvar = STRING(INT(ENTRY(iint,x-nropages))).
            x-nropages-2 = x-nropages-2 + cvar + ','.                    
        END.
    END.
/*    MESSAGE x-nropages-2.*/

    lDiferencias = NO.

    FOR EACH Almacen WHERE Almacen.CodCia = s-CodCia
        AND LOOKUP (Almacen.CodAlm,cAlmc) > 0 NO-LOCK:
        FOR EACH AlmDInv USE-INDEX Llave01
            WHERE AlmDInv.CodCia  =  Almacen.CodCia
            AND AlmDInv.CodAlm    =  Almacen.CodAlm
            AND (r-pages = 1 OR
                LOOKUP(STRING(AlmDInv.NroPagina),x-nropages-2) > 0)
            /*AND AlmDInv.NroPagina <= INTEGER(txt-page-2)*/
            AND AlmDInv.NomCia    =  cNomCia 
            /*AND AlmDInv.NroPagina < 9000 NO-LOCK:*/
            AND AlmDInv.CodUbi    BEGINS x-zona NO-LOCK:
            /*MESSAGE 'Pasa a la paginas'.*/
            IF lDiferencias = NO OR (AlmDInv.QtyFisico - AlmDInv.QtyConteo) <> 0 THEN DO:

                CREATE w-report.
                ASSIGN
                    w-report.Task-No    = s-task-no
                    w-report.Llave-I    = AlmDInv.CodCia
                    w-report.Campo-I[1] = AlmDInv.NroPagina
                    w-report.Campo-I[2] = AlmDInv.NroSecuencia
/*Campo referencia*/ w-report.Campo-I[3] = i + 1
                    w-report.Campo-C[1] = AlmDInv.CodAlm
                    w-report.Campo-C[2] = AlmDInv.CodUbi
                    w-report.Campo-C[3] = AlmDInv.CodMat
                    w-report.Campo-C[4] = "Almacen: " + AlmDInv.CodAlm + "- " + almacen.descripcion
                    w-report.Campo-F[1] = AlmDInv.QtyFisico
                    w-report.Campo-F[2] = 0 /*AlmDInv.QtyConteo*/
                    w-report.Campo-F[3] = 0. /*(AlmDInv.QtyFisico - AlmDInv.QtyConteo).*/
                IF i MOD 25 = 0 THEN i = 1.
            END.
            DISPLAY "Cargando para el almacen  " + Almacen.CodAlm @ x-mensaje
                WITH FRAME {&FRAME-NAME}.
        END.
    END.
    DISPLAY "" @ x-mensaje WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY r-pages x-nropages x-mensaje x-codAlm r-imp x-zona 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE r-pages x-nropages x-codAlm BUTTON-1 r-imp btn-Excel Btn_Ok Btn_Done 
         x-zona RECT-71 RECT-72 RECT-73 RECT-74 
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
    DEFINE VARIABLE t-Column                AS INTEGER INIT 5.
    DEFINE VARIABLE i-Column                AS INTEGER NO-UNDO.
    DEFINE VARIABLE j-Column                AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE x-DesMat AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE x-DesMar AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE x-Und    AS CHARACTER   NO-UNDO.
    
    /* create a new Excel Application object */
    CREATE "Excel.Application" chExcelApplication.
    
    /* create a new Workbook */
    chWorkbook = chExcelApplication:Workbooks:Add().
    
    /* get the active Worksheet */
    chWorkSheet = chExcelApplication:Sheets:Item(1).
    
    /*Header del Excel */
    cRange = "E" + '2'.
    chWorkSheet:Range(cRange):Value = "STOCK DEL SISTEMA - ANTES DE INVENTARIO ".
    cRange = "K" + '2'.
    chWorkSheet:Range(cRange):Value = TODAY.
    cRange = "C" + '3'.
    chWorkSheet:Range(cRange):Value = "Almacen(es): ".
    cRange = "D" + '3'.
    chWorkSheet:Range(cRange):Value = x-CodAlm.
/*
    cRange = "E" + '3'.
    chWorkSheet:Range(cRange):Value = "-" + txt-desalm.*/
    
    
    /*Formato*/
    chWorkSheet:Columns("A"):NumberFormat = "@".
    chWorkSheet:Columns("B"):NumberFormat = "@".
    chWorkSheet:Columns("C"):NumberFormat = "@".
    chWorkSheet:Columns("D"):NumberFormat = "@".
    chWorkSheet:Columns("E"):NumberFormat = "@".
    
    /* set the column names for the Worksheet */
    t-Column = t-Column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "Almc.".
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro.Pag".
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "Nro.Sec".
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "Ubicaci�n".
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "C�digo".
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "Descripci�n".
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "Marca".
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "Unidad".
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "Sistema".
    
    FOR EACH w-report WHERE w-report.task-no = s-task-no
        BREAK BY w-report.Campo-C[1]
            BY w-report.Campo-I[1]
            BY w-report.Campo-I[2]:
        FIND FIRST Almmmatg WHERE Almmmatg.CodCia = s-CodCia
            AND Almmmatg.CodMat = w-report.Campo-C[3] NO-LOCK NO-ERROR.
        IF AVAIL Almmmatg THEN 
            ASSIGN 
                x-DesMat = Almmmatg.DesMat
                x-DesMar = Almmmatg.DesMar
                x-Und    = Almmmatg.UndBas.
        t-Column = t-Column + 1.
        cColumn = STRING(t-Column).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):Value = w-report.Campo-C[1].
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = w-report.Campo-I[1].
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = w-report.Campo-I[2].
        cRange = "D" + cColumn.
        chWorkSheet:Range(cRange):Value = w-report.Campo-C[2].
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = w-report.Campo-C[3].
        cRange = "F" + cColumn.
        chWorkSheet:Range(cRange):Value = x-Desmat.
        cRange = "G" + cColumn.
        chWorkSheet:Range(cRange):Value = x-DesMar.
        cRange = "H" + cColumn.
        chWorkSheet:Range(cRange):Value = x-Und.
        cRange = "I" + cColumn.
        chWorkSheet:Range(cRange):Value = w-report.Campo-F[1].
        cRange = "J" + cColumn.
        chWorkSheet:Range(cRange):Value = w-report.Campo-F[2].

        DISPLAY "Cargando Informacion para almacen " + w-report.Campo-C[1] @ x-mensaje
            WITH FRAME {&FRAME-NAME}.

    END.
    
    DISPLAY "" @ x-mensaje WITH FRAME {&FRAME-NAME}.
    
    /* launch Excel so it is visible to the user */
     chExcelApplication:Visible = TRUE.
    
    /* release com-handles */
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet.

    /*Borrando Temporal*/
    FOR EACH w-report WHERE task-no = s-task-no:
        DELETE w-report.
    END.
    s-task-no = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime W-Win 
PROCEDURE Imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  DEF VAR RB-REPORT-LIBRARY AS CHAR.              /* Archivo PRL a usar */
  DEF VAR RB-REPORT-NAME AS CHAR.                 /* Nombre del reporte */
  DEF VAR RB-INCLUDE-RECORDS AS CHAR.             /* "O" si necesita filtro */
  DEF VAR RB-FILTER AS CHAR.                      /* Filtro de impresion */
  DEF VAR RB-OTHER-PARAMETERS AS CHAR INITIAL "". /* Otros parametros */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'imprime':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  GET-KEY-VALUE SECTION 'Startup' KEY 'BASE' VALUE RB-REPORT-LIBRARY.
  RB-REPORT-LIBRARY = RB-REPORT-LIBRARY + 'alm/rbalm.prl'.

  IF r-pages = 1 THEN DO:
      IF r-imp = 1 THEN RB-REPORT-NAME = 'Listado de Reconteo a'.
      ELSE RB-REPORT-NAME = 'Listado de Reconteo a Draft'.
  END.
  ELSE DO:
      IF r-imp = 1 THEN RB-REPORT-NAME = 'Listado de Reconteo a1'.
      ELSE RB-REPORT-NAME = 'Listado de Reconteo a1 Draft'.
  END.

  /* RB-REPORT-NAME = 'Listado de Reconteo_2'. */
  RB-INCLUDE-RECORDS = 'O'.
  RB-FILTER = "w-report.task-no = " + STRING(S-TASK-NO).

  RB-OTHER-PARAMETERS = "s-nomcia=" + s-nomcia +
                        "~ncNropage = " + STRING('99999').

  RUN lib/_imprime2 (RB-REPORT-LIBRARY,
                     RB-REPORT-NAME,
                     RB-INCLUDE-RECORDS,
                     RB-FILTER,
                     RB-OTHER-PARAMETERS).


  /*Borrando Temporal*/
  FOR EACH w-report WHERE task-no = s-task-no:
      DELETE w-report.
  END.
s-task-no = 0.

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

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Parametros W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recoge-Parametros W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
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

