&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

DEFINE VARIABLE CMB-lista AS CHARACTER NO-UNDO.
DEFINE VARIABLE x-linea   AS CHARACTER FORMAT "x(80)" NO-UNDO.
DEFINE VARIABLE x-fecha   AS CHARACTER NO-UNDO.
DEFINE VARIABLE stat-reg  AS LOGICAL NO-UNDO.
DEFINE VARIABLE x-valcalI AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-valcalE AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-impIng  AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-impEgr  AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-valcalA AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-con-reg AS INTEGER NO-UNDO.
DEFINE VARIABLE i         AS INTEGER NO-UNDO.
DEFINE VARIABLE x-dis-mon AS INTEGER EXTENT 5 NO-UNDO.
DEFINE VARIABLE x-acu-mon AS INTEGER EXTENT 5 NO-UNDO.

DEFINE BUTTON Btn_OK IMAGE-UP FILE "img/plemrbol"
    LABEL "OK" SIZE 6.43 BY 1.58 BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-Codigo AS CHARACTER FORMAT "X(256)":U 
    LABEL "Personal" VIEW-AS FILL-IN SIZE 6.72 BY .81 BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FILL-IN-Seccion AS CHARACTER FORMAT "X(256)":U 
    LABEL "Proyecto" VIEW-AS FILL-IN SIZE 28 BY .81 BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE RECTANGLE RECT-20 EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL SIZE 37.14 BY 3.08.

DEFINE FRAME F-msg
    FILL-IN-Codigo AT ROW 2.08 COL 6.72 COLON-ALIGNED
    FILL-IN-Seccion AT ROW 2.96 COL 6.72 COLON-ALIGNED
    Btn_OK AT ROW 1.23 COL 30.29
    RECT-20 AT ROW 1 COL 1
    "Espere un momento por favor ..." VIEW-AS TEXT
    SIZE 22.57 BY .62 AT ROW 1.31 COL 4.43
    SPACE(11.13) SKIP(2.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
    FONT 4 TITLE "Procesando..." CENTERED.

DEFINE STREAM strm-boleta. /* STREAM para el reporte */

DEFINE TEMP-TABLE tmp-bole
    FIELD t-codcia AS INTEGER
    FIELD t-CodPer AS CHARACTER
    FIELD t-NomPer AS CHARACTER
    FIELD t-TpoPer AS CHARACTER
    FIELD t-ImpIng AS DECIMAL
    FIELD t-ImpEgr AS DECIMAL
    INDEX i-codper IS PRIMARY t-CodPer ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_pl-flg-m

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES integral.PL-PLAN integral.PL-CALC
&Scoped-define FIRST-EXTERNAL-TABLE integral.PL-PLAN


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR integral.PL-PLAN, integral.PL-CALC.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES integral.PL-FLG-MES integral.PL-PERS

/* Definitions for BROWSE br_pl-flg-m                                   */
&Scoped-define FIELDS-IN-QUERY-br_pl-flg-m integral.PL-FLG-MES.codper ~
integral.PL-PERS.patper integral.PL-PERS.matper integral.PL-PERS.nomper 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_pl-flg-m 
&Scoped-define QUERY-STRING-br_pl-flg-m FOR EACH integral.PL-FLG-MES ~
      WHERE PL-FLG-MES.CodCia = s-CodCia ~
 AND PL-FLG-MES.Periodo = s-Periodo ~
 AND PL-FLG-MES.codpln = PL-PLAN.CodPln ~
 AND PL-FLG-MES.NroMes = FILL-IN-NRO-MES ~
 AND PL-FLG-MES.SitAct <> "Inactivo" NO-LOCK, ~
      EACH integral.PL-PERS OF integral.PL-FLG-MES NO-LOCK ~
    BY integral.PL-FLG-MES.Proyecto ~
       BY integral.PL-FLG-MES.seccion ~
        BY integral.PL-PERS.patper ~
         BY integral.PL-PERS.matper ~
          BY integral.PL-PERS.nomper
&Scoped-define OPEN-QUERY-br_pl-flg-m OPEN QUERY br_pl-flg-m FOR EACH integral.PL-FLG-MES ~
      WHERE PL-FLG-MES.CodCia = s-CodCia ~
 AND PL-FLG-MES.Periodo = s-Periodo ~
 AND PL-FLG-MES.codpln = PL-PLAN.CodPln ~
 AND PL-FLG-MES.NroMes = FILL-IN-NRO-MES ~
 AND PL-FLG-MES.SitAct <> "Inactivo" NO-LOCK, ~
      EACH integral.PL-PERS OF integral.PL-FLG-MES NO-LOCK ~
    BY integral.PL-FLG-MES.Proyecto ~
       BY integral.PL-FLG-MES.seccion ~
        BY integral.PL-PERS.patper ~
         BY integral.PL-PERS.matper ~
          BY integral.PL-PERS.nomper.
&Scoped-define TABLES-IN-QUERY-br_pl-flg-m integral.PL-FLG-MES ~
integral.PL-PERS
&Scoped-define FIRST-TABLE-IN-QUERY-br_pl-flg-m integral.PL-FLG-MES
&Scoped-define SECOND-TABLE-IN-QUERY-br_pl-flg-m integral.PL-PERS


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br_pl-flg-m}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-24 br_pl-flg-m R-seleccion COMBO-S ~
TGL-pantalla B-aceptar 
&Scoped-Define DISPLAYED-OBJECTS R-seleccion COMBO-S TGL-pantalla 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-aceptar 
     IMAGE-UP FILE "img/b-ok":U
     LABEL "&Aceptar" 
     SIZE 10.72 BY 1.54.

DEFINE VARIABLE COMBO-S AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 20.43 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE R-seleccion AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todo el personal", 1,
"Selectivo", 2,
"Por secci�n", 3,
"Por proyecto", 4
     SIZE 14.14 BY 2.08 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.72 BY 8.54.

DEFINE VARIABLE TGL-pantalla AS LOGICAL INITIAL no 
     LABEL "Salida a Pantalla" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.57 BY .5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_pl-flg-m FOR 
      integral.PL-FLG-MES, 
      integral.PL-PERS SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_pl-flg-m
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_pl-flg-m B-table-Win _STRUCTURED
  QUERY br_pl-flg-m NO-LOCK DISPLAY
      integral.PL-FLG-MES.codper FORMAT "X(6)":U
      integral.PL-PERS.patper FORMAT "X(40)":U
      integral.PL-PERS.matper FORMAT "X(40)":U
      integral.PL-PERS.nomper FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 47.14 BY 7.92
         BGCOLOR 15 FGCOLOR 0 FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_pl-flg-m AT ROW 1.31 COL 22.43
     R-seleccion AT ROW 2.38 COL 5.29 NO-LABEL
     COMBO-S AT ROW 4.5 COL 1.72 NO-LABEL
     TGL-pantalla AT ROW 5.54 COL 4.86
     B-aceptar AT ROW 6.15 COL 6.43
     RECT-24 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 69.72 BY 8.54
         BGCOLOR 8 FGCOLOR 0 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: integral.PL-PLAN,integral.PL-CALC
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 8.54
         WIDTH              = 69.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB br_pl-flg-m RECT-24 F-Main */
ASSIGN 
       br_pl-flg-m:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

/* SETTINGS FOR COMBO-BOX COMBO-S IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_pl-flg-m
/* Query rebuild information for BROWSE br_pl-flg-m
     _TblList          = "integral.PL-FLG-MES,integral.PL-PERS OF integral.PL-FLG-MES"
     _Options          = "NO-LOCK"
     _OrdList          = "integral.PL-FLG-MES.Proyecto|yes,integral.PL-FLG-MES.seccion|yes,integral.PL-PERS.patper|yes,integral.PL-PERS.matper|yes,integral.PL-PERS.nomper|yes"
     _Where[1]         = "PL-FLG-MES.CodCia = s-CodCia
 AND PL-FLG-MES.Periodo = s-Periodo
 AND PL-FLG-MES.codpln = PL-PLAN.CodPln
 AND PL-FLG-MES.NroMes = FILL-IN-NRO-MES
 AND PL-FLG-MES.SitAct <> ""Inactivo"""
     _FldNameList[1]   = integral.PL-FLG-MES.codper
     _FldNameList[2]   = integral.PL-PERS.patper
     _FldNameList[3]   = integral.PL-PERS.matper
     _FldNameList[4]   = integral.PL-PERS.nomper
     _Query            is OPENED
*/  /* BROWSE br_pl-flg-m */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-aceptar B-table-Win
ON CHOOSE OF B-aceptar IN FRAME F-Main /* Aceptar */
DO:
    RUN imp_boleta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_pl-flg-m
&Scoped-define SELF-NAME br_pl-flg-m
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_pl-flg-m B-table-Win
ON ROW-ENTRY OF br_pl-flg-m IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_pl-flg-m B-table-Win
ON ROW-LEAVE OF br_pl-flg-m IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_pl-flg-m B-table-Win
ON VALUE-CHANGED OF br_pl-flg-m IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-S
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-S B-table-Win
ON ENTRY OF COMBO-S IN FRAME F-Main
DO:
    ASSIGN CMB-Lista = "".
    CASE INPUT R-seleccion:
    WHEN 3 THEN DO:
        FOR EACH integral.PL-SECC NO-LOCK:
            ASSIGN CMB-Lista = CMB-Lista + "," + integral.PL-SECC.seccion.
        END.
    END.
    WHEN 4 THEN DO:
        FOR EACH integral.PL-PROY NO-LOCK:
            ASSIGN CMB-Lista = CMB-Lista + "," + integral.PL-PROY.proyecto.
        END.
    END.
    END CASE.
    COMBO-S:LIST-ITEMS IN FRAME F-Main = CMB-Lista.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-seleccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-seleccion B-table-Win
ON VALUE-CHANGED OF R-seleccion IN FRAME F-Main
DO:
    CASE INPUT R-seleccion:
    WHEN 1 THEN
        ASSIGN
            Br_pl-flg-m:SENSITIVE = FALSE
            COMBO-S:SENSITIVE     = FALSE.
    WHEN 2 THEN
        ASSIGN
            Br_pl-flg-m:SENSITIVE = TRUE
            COMBO-S:SENSITIVE     = FALSE.
    WHEN 3 OR WHEN 4 THEN DO:
        ASSIGN
            COMBO-S:LIST-ITEMS    = ""
            Br_pl-flg-m:SENSITIVE = FALSE
            COMBO-S:SENSITIVE     = TRUE.
        DISPLAY COMBO-S WITH FRAME F-Main.
    END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "integral.PL-PLAN"}
  {src/adm/template/row-list.i "integral.PL-CALC"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "integral.PL-PLAN"}
  {src/adm/template/row-find.i "integral.PL-CALC"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE imp_boleta B-table-Win 
PROCEDURE imp_boleta :
FIND PF-CIAS WHERE PF-CIAS.CodCia = s-CodCia NO-LOCK.

DEFINE VARIABLE x-pagina  AS INTEGER FORMAT "ZZZ9" NO-UNDO.
DEFINE VARIABLE x-orden   AS INTEGER NO-UNDO.
DEFINE VARIABLE x-Neto    AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-Imptot  AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-titulo  AS CHARACTER FORMAT "x(28)" NO-UNDO.
DEFINE VARIABLE p-archivo AS CHARACTER NO-UNDO.
DEFINE VARIABLE x-mes     AS INTEGER.
DEFINE VARIABLE x-ano     AS INTEGER.
x-titulo = "   ADELANTO DE QUINCENA".


DEFINE FRAME f-cab
    x-orden  COLUMN-LABEL "Orden" FORMAT "ZZZ9"
    t-CodPer COLUMN-LABEL "C�digo" FORMAT "x(6)"
    t-NomPer COLUMN-LABEL "Apellidos y nombre" FORMAT "x(50)"
    t-ImpIng COLUMN-LABEL "Haberes" FORMAT "ZZ,ZZZ,ZZ9.99"
    t-ImpEgr COLUMN-LABEL "Descuentos" FORMAT "ZZ,ZZZ,ZZ9.99"
    x-Neto   COLUMN-LABEL "Importe Neto" FORMAT "ZZ,ZZZ,ZZ9.99"
    HEADER
    PF-CIAS.NomCia FORMAT "x(40)" x-titulo AT 45
    "FECHA :" TO 100 TODAY TO 110 FORMAT '99/99/9999'SKIP
    'CALCULO : ' PL-CALC.descal FORMAT '!(20)'
    /*"PLANILLA EMPLEADOS - MES #" TO 64 FILL-IN-NRO-MES FORMAT "99"*/
    "-" S-Periodo FORMAT "9,999"
    "Pagina :" TO 100 x-pagina TO 110
    SKIP(1)
    WITH DOWN NO-BOX STREAM-IO WIDTH 110.

ASSIGN
    x-acu-mon = 0
    x-impIng  = 0
    x-impEgr  = 0.
    
/*
CASE R-seleccion:
WHEN 1 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL IN FRAME F-Msg = "Secci�n".
    FOR EACH PL-FLG-MES WHERE
        PL-FLG-MES.CodCia  = s-CodCia AND
        PL-FLG-MES.Periodo = s-Periodo AND
        PL-FLG-MES.NroMes  = FILL-IN-NRO-MES AND
        PL-FLG-MES.CodPln  = PL-PLAN.CodPln NO-LOCK:
        IF PL-FLG-MES.SitAct = "Inactivo" THEN NEXT.
        DISPLAY PL-FLG-MES.seccion @ FILL-IN-Seccion WITH FRAME F-Msg.
        RUN busca_datos.
    END.
END.
WHEN 2 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL IN FRAME F-Msg = "Secci�n".
    DO i = 1 TO br_pl-flg-m:NUM-SELECTED-ROWS IN FRAME F-Main:
        ASSIGN stat-reg  = br_pl-flg-m:FETCH-SELECTED-ROW(i).
        IF stat-reg THEN DO:
            DISPLAY PL-FLG-MES.seccion @ FILL-IN-Seccion WITH FRAME F-Msg.
            RUN busca_datos.
        END.
    END.
    ASSIGN stat-reg = br_pl-flg-m:DESELECT-ROWS().
END.
WHEN 3 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL IN FRAME F-Msg = "Secci�n".
    DISPLAY COMBO-S @ FILL-IN-Seccion WITH FRAME F-Msg.
    FOR EACH PL-FLG-MES NO-LOCK WHERE
        PL-FLG-MES.CodCia  = s-CodCia AND
        PL-FLG-MES.Periodo = s-Periodo AND
        PL-FLG-MES.NroMes  = FILL-IN-NRO-MES AND
        PL-FLG-MES.CodPln  = PL-PLAN.CodPln AND
        PL-FLG-MES.Seccion = COMBO-S:
        IF PL-FLG-MES.SitAct = "Inactivo" THEN NEXT.
        RUN busca_datos.
    END.
END.
WHEN 4 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL IN FRAME F-Msg = "Proyecto".
    DISPLAY COMBO-S @ FILL-IN-Seccion WITH FRAME F-Msg.
    FOR EACH PL-FLG-MES NO-LOCK WHERE
        PL-FLG-MES.CodCia   = s-CodCia AND
        PL-FLG-MES.Periodo  = s-Periodo AND
        PL-FLG-MES.NroMes   = FILL-IN-NRO-MES AND
        PL-FLG-MES.CodPln   = PL-PLAN.CodPln AND
        PL-FLG-MES.Proyecto = COMBO-S:
        IF PL-FLG-MES.SitAct = "Inactivo" THEN NEXT.
        RUN busca_datos.
    END.
END.

END CASE.
*/
HIDE FRAME F-Msg.

IF NOT CAN-FIND(FIRST tmp-bole) THEN RETURN.

IF INPUT TGL-pantalla = TRUE THEN DO:
    P-archivo = SESSION:TEMP-DIRECTORY +
          STRING(NEXT-VALUE(sec-arc,integral),"99999999") + ".scr".
    OUTPUT STREAM strm-boleta TO VALUE ( P-archivo ) PAGED PAGE-SIZE 66.
END.
ELSE DO:
    OUTPUT STREAM strm-boleta TO PRINTER PAGED PAGE-SIZE 66.
    /* Seteo de impresi�n (Impresora EPSON) */
    PUT STREAM strm-boleta CONTROL "~033@~0335~033F~033P~033x~001~033C" CHR(66).
    PUT STREAM strm-boleta CONTROL "~033x" NULL "~017~033P".
END.
x-pagina = 1.

DO WITH FRAME f-cab:
    FOR EACH tmp-bole NO-LOCK BREAK BY t-codcia BY t-tpoper BY t-nomper:
        IF FIRST-OF( t-tpoper ) THEN DO:
            x-orden   = 0.
            x-impEgr  = 0.
            x-impIng  = 0.
            x-Imptot  = 0.
        END.
        ASSIGN
            x-Neto   = tmp-bole.t-ImpIng - tmp-bole.t-ImpEgr
            x-Imptot = x-Imptot + x-Neto
            x-impEgr = x-impEgr + tmp-bole.t-ImpEgr
            x-impIng = x-impIng + tmp-bole.t-ImpIng
            x-orden  = x-orden + 1.
        DISPLAY STREAM strm-boleta
            x-orden
            tmp-bole.t-CodPer
            tmp-bole.t-NomPer
            tmp-bole.t-ImpIng
            tmp-bole.t-ImpEgr
            x-Neto
            WITH FRAME f-cab.
        DOWN STREAM strm-boleta 1 WITH FRAME f-cab.
        IF LINE-COUNTER( strm-boleta ) > 62 THEN DO:
            ASSIGN x-pagina = x-pagina + 1.
            PAGE STREAM strm-boleta.
        END.
        ACCUMULATE tmp-bole.t-imping (TOTAL).
        ACCUMULATE tmp-bole.t-impegr (TOTAL).
        ACCUMULATE x-neto            (TOTAL).
        
        IF LAST-OF( t-tpoper ) THEN DO:
            PUT STREAM strm-boleta
                "-----"
                FILL(" ",59) FORMAT "x(59)"
                "------------- "
                "------------- "
                "-------------" SKIP.
            IF LINE-COUNTER( strm-boleta ) > 62 THEN DO:
                ASSIGN x-pagina = x-pagina + 1.
                PAGE STREAM strm-boleta.
            END.
    
            PUT STREAM strm-boleta
                x-orden FORMAT "ZZZZ9"
                " <--- TOTAL PERSONAL  "
                t-tpoper FORMAT "x(37)"
                x-impIng FORMAT "ZZ,ZZZ,ZZ9.99" " "
                x-impEgr FORMAT "ZZ,ZZZ,ZZ9.99" " "
                x-ImpTot FORMAT "ZZ,ZZZ,ZZ9.99" SKIP.

            PUT STREAM strm-boleta
                "-----" FILL(" ",59) FORMAT "x(59)"
                "------------- "
                "------------- "
                "-------------" SKIP.
            IF LINE-COUNTER( strm-boleta ) > 62 THEN DO:
                ASSIGN x-pagina = x-pagina + 1.
                PAGE STREAM strm-boleta.
            END.
        END.
        IF LAST-OF (t-codcia) THEN DO:
           PUT STREAM strm-boleta
               "-----"
               FILL(" ",59) FORMAT "x(59)"
               "------------- "
               "------------- "
               "-------------" SKIP.
            IF LINE-COUNTER( strm-boleta ) > 62 THEN DO:
                ASSIGN x-pagina = x-pagina + 1.
                PAGE STREAM strm-boleta.
            END.
            PUT STREAM strm-boleta
               '            TOTAL GENERAL'   
                FILL(" ",39) FORMAT "x(39)"
                ACCUM TOTAL tmp-bole.t-imping FORMAT "ZZ,ZZZ,ZZ9.99" " "
                ACCUM TOTAL tmp-bole.t-impegr FORMAT "ZZ,ZZZ,ZZ9.99" " "
                ACCUM TOTAL x-neto FORMAT "ZZ,ZZZ,ZZ9.99" SKIP.
                
             PUT STREAM strm-boleta
                 "-----"
                 FILL(" ",59) FORMAT "x(59)"
                 "------------- "
                 "------------- "
                 "-------------" SKIP.
           
           PAGE STREAM strm-boleta.
        END.
    END.
END.


OUTPUT STREAM strm-boleta CLOSE.

FOR EACH tmp-bole:
    DELETE tmp-bole.
END.

IF INPUT TGL-pantalla = TRUE THEN DO:
    RUN bin/_vcat.p ( P-archivo ). 
    OS-DELETE VALUE ( P-archivo ). 
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

    DISPLAY FILL-IN-NRO-MES WITH FRAME F-Main.

    APPLY "VALUE-CHANGED" TO R-seleccion IN FRAME F-Main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "integral.PL-PLAN"}
  {src/adm/template/snd-list.i "integral.PL-CALC"}
  {src/adm/template/snd-list.i "integral.PL-FLG-MES"}
  {src/adm/template/snd-list.i "integral.PL-PERS"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

