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

{pln/s-global.i}

DEFINE VARIABLE s-CodCia AS INTEGER.
DEFINE VARIABLE s-NomCia AS CHARACTER.

DEFINE NEW SHARED VARIABLE VAL-VAR AS DECIMAL EXTENT 20.
DEFINE VARIABLE x-valcalDI AS DECIMAL NO-UNDO FORMAT ">,>>>,>>9.99".
DEFINE VARIABLE x-valcalDE AS DECIMAL NO-UNDO FORMAT ">,>>>,>>9.99".
DEFINE VARIABLE x-valcalDA AS DECIMAL NO-UNDO FORMAT ">,>>>,>>9.99".
DEFINE VARIABLE CMB-lista AS CHARACTER NO-UNDO.
DEFINE VARIABLE x-linea   AS CHARACTER FORMAT "x(64)" NO-UNDO.
DEFINE VARIABLE x-SEM     AS CHARACTER NO-UNDO.
DEFINE VARIABLE stat-reg  AS LOGICAL NO-UNDO.
DEFINE VARIABLE x-con-reg AS INTEGER NO-UNDO.

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

DEFINE BUFFER b-PL-MOV-SEM FOR PL-MOV-SEM. /* BUFFER para busquedas de referencias */

DEFINE STREAM strm-boleta. /* STREAM para el reporte */

DEFINE WORK-TABLE tmp-bole
    FIELD t-nro    AS INTEGER
    FIELD t-codrem AS INTEGER
    FIELD t-refrem AS CHARACTER
    FIELD t-desrem AS CHARACTER
    FIELD t-imprem AS DECIMAL
    FIELD t-coddes AS INTEGER
    FIELD t-desdes AS CHARACTER
    FIELD t-impdes AS DECIMAL
    FIELD t-codApo AS INTEGER
    FIELD t-desApo AS CHARACTER
    FIELD t-impApo AS DECIMAL.

DEFINE TEMP-TABLE Tempo 
       FIELD CodPer AS CHAR FORMAT "x(6)"
       FIELD TpoBol AS CHAR FORMAT "x(15)"
       FIELD CodMov AS INTEGER FORMAT "999"
       FIELD valcalD AS DECI EXTENT 13.

DEFINE  VAr I AS INTEGER.
DEFINE  VAr J AS INTEGER.
DEFINE  VAr valcaltotD AS deci.
DEFINE  VAr valcaltotR AS deci.
DEFINE  VAr totG1 AS deci.
DEFINE  VAr totG2 AS deci.
DEFINE  VAr totG3 AS deci.
DEFINE  VAr totG4 AS deci.
DEFINE  VAr totG5 AS deci.
DEFINE  VAr totG6 AS deci.
DEFINE  VAr totG7 AS deci.
DEFINE  VAr totG8 AS deci.
DEFINE  VAr totG9 AS deci.
DEFINE  VAr totG10 AS deci.
DEFINE  VAr totG11 AS deci.
DEFINE  VAr totG12 AS deci.
DEFINE  VAr totG13 AS deci.

DEFINE VARIABLE valcalD1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD4 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD6 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD7 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD8 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD9 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD10 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD11 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalD12 AS DECIMAL NO-UNDO.

DEFINE VARIABLE valcalR1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR4 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR6 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR7 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR8 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR9 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR10 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR11 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valcalR12 AS DECIMAL NO-UNDO.

DEF TEMP-TABLE T-FLG-SEM LIKE PL-FLG-SEM.

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
&Scoped-define INTERNAL-TABLES integral.PL-FLG-SEM integral.PL-PERS

/* Definitions for BROWSE br_pl-flg-m                                   */
&Scoped-define FIELDS-IN-QUERY-br_pl-flg-m integral.PL-FLG-SEM.codper ~
integral.PL-PERS.patper integral.PL-PERS.matper integral.PL-PERS.nomper 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_pl-flg-m 
&Scoped-define QUERY-STRING-br_pl-flg-m FOR EACH integral.PL-FLG-SEM ~
      WHERE PL-FLG-SEM.CodCia = s-CodCia ~
 AND PL-FLG-SEM.Periodo = s-Periodo ~
 AND PL-FLG-SEM.codpln = PL-PLAN.CodPln ~
 AND PL-FLG-SEM.NroSem = FILL-IN-NRO-SEM ~
 NO-LOCK, ~
      EACH integral.PL-PERS OF integral.PL-FLG-SEM NO-LOCK ~
    BY integral.PL-FLG-SEM.Proyecto ~
       BY integral.PL-FLG-SEM.seccion ~
        BY integral.PL-PERS.patper ~
         BY integral.PL-PERS.matper ~
          BY integral.PL-PERS.nomper
&Scoped-define OPEN-QUERY-br_pl-flg-m OPEN QUERY br_pl-flg-m FOR EACH integral.PL-FLG-SEM ~
      WHERE PL-FLG-SEM.CodCia = s-CodCia ~
 AND PL-FLG-SEM.Periodo = s-Periodo ~
 AND PL-FLG-SEM.codpln = PL-PLAN.CodPln ~
 AND PL-FLG-SEM.NroSem = FILL-IN-NRO-SEM ~
 NO-LOCK, ~
      EACH integral.PL-PERS OF integral.PL-FLG-SEM NO-LOCK ~
    BY integral.PL-FLG-SEM.Proyecto ~
       BY integral.PL-FLG-SEM.seccion ~
        BY integral.PL-PERS.patper ~
         BY integral.PL-PERS.matper ~
          BY integral.PL-PERS.nomper.
&Scoped-define TABLES-IN-QUERY-br_pl-flg-m integral.PL-FLG-SEM ~
integral.PL-PERS
&Scoped-define FIRST-TABLE-IN-QUERY-br_pl-flg-m integral.PL-FLG-SEM
&Scoped-define SECOND-TABLE-IN-QUERY-br_pl-flg-m integral.PL-PERS


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br_pl-flg-m}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 Btn-UP FILL-IN-NRO-SEM br_pl-flg-m ~
Btn-DOWN R-seleccion COMBO-S TGL-pantalla B-aceptar FILL-IN-msg 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NRO-SEM FILL-IN-Copias R-seleccion ~
COMBO-S TGL-pantalla FILL-IN-msg 

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

DEFINE BUTTON Btn-DOWN 
     IMAGE-UP FILE "img/btn-down":U
     LABEL "" 
     SIZE 3 BY .69.

DEFINE BUTTON Btn-UP 
     IMAGE-UP FILE "img/btn-up":U
     LABEL "" 
     SIZE 3 BY .69.

DEFINE VARIABLE COMBO-S AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 20.43 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE FILL-IN-Copias AS INTEGER FORMAT "99":U INITIAL 1 
     LABEL "Copias" 
     VIEW-AS FILL-IN 
     SIZE 3.29 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-msg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mensaje" 
     VIEW-AS FILL-IN 
     SIZE 40.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-NRO-SEM AS INTEGER FORMAT "Z9":U INITIAL 0 
     LABEL "Sem" 
     VIEW-AS FILL-IN 
     SIZE 3.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE R-seleccion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todo el personal", 1,
"Selectivo", 2,
"Por secci�n", 3,
"Por proyecto", 4
     SIZE 14.14 BY 2.08 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.72 BY 9.5.

DEFINE VARIABLE TGL-pantalla AS LOGICAL INITIAL no 
     LABEL "Salida a Pantalla" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.57 BY .5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_pl-flg-m FOR 
      integral.PL-FLG-SEM, 
      integral.PL-PERS SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_pl-flg-m
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_pl-flg-m B-table-Win _STRUCTURED
  QUERY br_pl-flg-m NO-LOCK DISPLAY
      integral.PL-FLG-SEM.codper FORMAT "X(6)":U COLUMN-FONT 4 LABEL-FONT 4
      integral.PL-PERS.patper FORMAT "X(40)":U COLUMN-FONT 4 LABEL-FONT 4
      integral.PL-PERS.matper FORMAT "X(40)":U COLUMN-FONT 4 LABEL-FONT 4
      integral.PL-PERS.nomper FORMAT "X(40)":U COLUMN-FONT 4 LABEL-FONT 4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 47.14 BY 7.92
         BGCOLOR 15 FGCOLOR 0 FONT 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn-UP AT ROW 1.08 COL 9.29
     FILL-IN-NRO-SEM AT ROW 1.19 COL 3 COLON-ALIGNED
     FILL-IN-Copias AT ROW 1.23 COL 16.57 COLON-ALIGNED
     br_pl-flg-m AT ROW 1.31 COL 22.43
     Btn-DOWN AT ROW 1.69 COL 9.29
     R-seleccion AT ROW 2.38 COL 5.57 NO-LABEL
     COMBO-S AT ROW 4.54 COL 1.57 NO-LABEL
     TGL-pantalla AT ROW 5.54 COL 4.86
     B-aceptar AT ROW 6.15 COL 6.57
     FILL-IN-msg AT ROW 9.46 COL 27 COLON-ALIGNED
     RECT-2 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 69.72 BY 9.5
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
         HEIGHT             = 9.5
         WIDTH              = 69.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{bin/_prns.i}
{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB br_pl-flg-m FILL-IN-Copias F-Main */
ASSIGN 
       br_pl-flg-m:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

/* SETTINGS FOR COMBO-BOX COMBO-S IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-Copias IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-Copias:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_pl-flg-m
/* Query rebuild information for BROWSE br_pl-flg-m
     _TblList          = "integral.PL-FLG-SEM,integral.PL-PERS OF integral.PL-FLG-SEM"
     _Options          = "NO-LOCK"
     _OrdList          = "integral.PL-FLG-SEM.Proyecto|yes,integral.PL-FLG-SEM.seccion|yes,integral.PL-PERS.patper|yes,integral.PL-PERS.matper|yes,integral.PL-PERS.nomper|yes"
     _Where[1]         = "PL-FLG-SEM.CodCia = s-CodCia
 AND PL-FLG-SEM.Periodo = s-Periodo
 AND PL-FLG-SEM.codpln = PL-PLAN.CodPln
 AND PL-FLG-SEM.NroSem = FILL-IN-NRO-SEM
"
     _FldNameList[1]   > integral.PL-FLG-SEM.codper
"PL-FLG-SEM.codper" ? ? "character" ? ? 4 ? ? 4 no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > integral.PL-PERS.patper
"PL-PERS.patper" ? ? "character" ? ? 4 ? ? 4 no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > integral.PL-PERS.matper
"PL-PERS.matper" ? ? "character" ? ? 4 ? ? 4 no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > integral.PL-PERS.nomper
"PL-PERS.nomper" ? ? "character" ? ? 4 ? ? 4 no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br_pl-flg-m */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-aceptar B-table-Win
ON CHOOSE OF B-aceptar IN FRAME F-Main /* Aceptar */
DO:
    ASSIGN FILL-IN-NRO-SEM R-seleccion COMBO-S FILL-IN-Copias FILL-IN-msg.
    IF FILL-IN-Copias = 0 THEN FILL-IN-Copias = 1.
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


&Scoped-define SELF-NAME Btn-DOWN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-DOWN B-table-Win
ON CHOOSE OF Btn-DOWN IN FRAME F-Main
DO:
  IF INPUT FRAME F-Main FILL-IN-NRO-SEM - 1 >= 1 THEN DO:
    DISPLAY INPUT FILL-IN-NRO-SEM - 1 @ FILL-IN-NRO-SEM WITH FRAME F-Main.
    ASSIGN FILL-IN-NRO-SEM.
    {&OPEN-QUERY-{&BROWSE-NAME}}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-UP B-table-Win
ON CHOOSE OF Btn-UP IN FRAME F-Main
DO:
  IF INPUT FRAME F-Main FILL-IN-NRO-SEM + 1 <= 53 THEN DO:
    DISPLAY INPUT FILL-IN-NRO-SEM + 1 @ FILL-IN-NRO-SEM WITH FRAME F-Main.
    ASSIGN FILL-IN-NRO-SEM.
    {&OPEN-QUERY-{&BROWSE-NAME}}
  END.
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


&Scoped-define SELF-NAME FILL-IN-NRO-SEM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NRO-SEM B-table-Win
ON LEAVE OF FILL-IN-NRO-SEM IN FRAME F-Main /* Sem */
DO:
    IF INPUT FILL-IN-NRO-SEM > 52 OR INPUT FILL-IN-NRO-SEM = 0 THEN DO:
        BELL.
        MESSAGE "Rango de semanas es de 1 a 52"
            VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FILL-IN-NRO-SEM.
        RETURN NO-APPLY.
    END.
    IF INPUT FILL-IN-NRO-SEM = FILL-IN-NRO-SEM THEN RETURN.
    ASSIGN FILL-IN-NRO-SEM.
    {&OPEN-QUERY-{&BROWSE-NAME}}
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

ASSIGN FILL-IN-NRO-SEM = s-NroSem.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE busca_datos B-table-Win 
PROCEDURE busca_datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
valcalD1 = 0.
valcalD2 = 0.
valcalD3 = 0.
valcalD4 = 0.
valcalD5 = 0.
valcalD6 = 0.
valcalD7 = 0.
valcalD8 = 0.
valcalD9 = 0.
valcalD10 = 0.
valcalD11 = 0.
valcalD12 = 0.
valcaltotD = 0.

valcalR1 = 0.
valcalR2 = 0.
valcalR3 = 0.
valcalR4 = 0.
valcalR5 = 0.
valcalR6 = 0.
valcalR7 = 0.
valcalR8 = 0.
valcalR9 = 0.
valcalR10 = 0.
valcalR11 = 0.
valcalR12 = 0.
valcaltotR = 0.

totG1 = 0.
totG2 = 0.
totG3 = 0.
totG4 = 0.
totG5 = 0.
totG6 = 0.
totG7 = 0.
totG8 = 0.
totG9 = 0.
totG10 = 0.
totG11 = 0.
totG12 = 0.
totG13 = 0.

DEFINE VARIABLE saldo     AS DECIMAL.
DEFINE VARIABLE F-valcalD  AS DECIMAL.
DEFINE VARIABLE linea-msg AS CHARACTER EXTENT 3.
DEFINE VARIABLE j         AS INTEGER.
DEFINE VARIABLE ii        AS INTEGER.
DEFINE VARIABLE x-dirdiv  AS CHAR.
DEFINE VARIABLE x-desdiv  AS CHAR.
DEFINE VARIABLE x-dirper  AS CHAR.
DEFINE VARIABLE x-LEper   AS CHAR.
DEFINE VARIABLE X-RUC     AS CHAR INIT "20100038146" format "X(11)".
FOR EACH tempo:
    DELETE Tempo.
END.

IF PL-FLG-SEM.SitAct = "Inactivo" THEN RETURN. /* Si no esta Activo */

IF NOT CAN-FIND(FIRST PL-MOV-SEM WHERE
    PL-MOV-SEM.CodCia  = s-CodCia        AND
    PL-MOV-SEM.Periodo = s-Periodo       AND
    PL-MOV-SEM.NroSem  = FILL-IN-NRO-SEM AND
    PL-MOV-SEM.CodPln  = PL-PLAN.CodPln  AND
    PL-MOV-SEM.CodCal  = PL-CALC.CodCal  AND
    PL-MOV-SEM.CodPer  = PL-FLG-SEM.CodPer) THEN RETURN. /* Si no tiene c�lculo */


FIND GN-DIVI WHERE GN-DIVI.CodCia = s-codcia AND
     GN-DIVI.CodDiv =  PL-FLG-SEM.CodDiv NO-LOCK NO-ERROR.
IF AVAILABLE GN-DIVI THEN
   ASSIGN x-desdiv = GN-DIVI.DesDiv
          x-dirdiv = GN-DIVI.DirDiv.
x-linea = TRIM(PL-FLG-SEM.CodDiv) + " " + TRIM(x-desdiv).
FIND gn-cias WHERE gn-cias.codcia  = s-codcia NO-LOCK NO-ERROR.

DO ii = 1 TO 1 /*FILL-IN-Copias */:
    FIND GN-DIVI WHERE GN-DIVI.CodCia = s-codcia AND
         GN-DIVI.CodDiv =  PL-FLG-SEM.CodDiv NO-LOCK NO-ERROR.
    IF AVAILABLE GN-DIVI THEN
       ASSIGN x-desdiv = GN-DIVI.DesDiv
              x-dirdiv = TRIM(GN-DIVI.DirDiv).
    x-linea = TRIM(PL-FLG-SEM.CodDiv) + " " + TRIM(x-desdiv).
    PUT STREAM strm-boleta SKIP(1).
    PUT STREAM strm-boleta CONTROL CHR(18) CHR(27) CHR(69).
    PUT STREAM strm-boleta S-NomCia SKIP.
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(70) CHR(27) CHR(77).

   /* PUT STREAM strm-boleta "R.U.C. : " X-Ruc   "DIVISION :" AT 70 
                           " " x-linea FORMAT "X(30)" SKIP.
    PUT STREAM strm-boleta "R.PATRONAL : " ( X-RUC + "0000000") FORMAT "X(18)"
                           x-dirdiv AT 70 FORMAT "X(30)" SKIP.
    PUT STREAM strm-boleta S-DirCia  ' Telefono : ' GN-CIAS.TlflCia  SKIP.*/
    
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(80) CHR(27) CHR(69).
   
    x-linea = "R E P O R T E   D E    C O N C E P T O S  " + STRING(S-PERIODO,"9999").
    
    PUT STREAM strm-boleta x-linea AT 20 SKIP.
    
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(70) CHR(15).
    IF AVAILABLE PL-PERS THEN DO:
        x-linea = PL-PERS.PatPer + " " + PL-PERS.MatPer + ", " + PL-PERS.NomPer.
        x-dirper = TRIM(PL-PERS.dirper) + " " + TRIM(PL-PERS.distri).
        x-LEper  = PL-PERS.NroDocId.
        END.
    ELSE x-linea = "".

    PUT STREAM strm-boleta "Codigo    : " PL-FLG-SEM.codper " " x-linea FORMAT "X(40)" SKIP.
    
    x-linea = FILL("-",150).
 
    PUT STREAM strm-boleta x-linea FORMAT "X(150)" SKIP.
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(69).
    PUT STREAM strm-boleta "   Conceptos           ENE       FEB      MAR       ABR       MAY       JUN       JUL       AGO       SEP       OCT       NOV       DIC     TOTAL " FORMAT "X(245)" SKIP.
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(70).
    x-linea = FILL("-",150).
    PUT STREAM strm-boleta x-linea FORMAT "X(150)" SKIP.
    
end.

DISPLAY PL-FLG-SEM.CodPer @ FILL-IN-Codigo WITH FRAME F-Msg.

/*Cargamos el temporal con los ingresos */
FOR EACH PL-BOLE NO-LOCK WHERE
    PL-BOLE.CodPln = PL-PLAN.CodPln AND
    PL-BOLE.CodCal = PL-CALC.CodCal AND
    PL-BOLE.TpoBol = "Remuneraciones" BY PL-BOLE.nroitm:
    DO J = 1 TO FILL-IN-NRO-SEM:
        FIND PL-SEM WHERE PL-SEM.codcia = s-codcia
            AND PL-SEM.periodo = s-periodo
            AND PL-SEM.nrosem = J
            NO-LOCK NO-ERROR.
        I = PL-SEM.NroMes.
        FIND PL-MOV-SEM WHERE
            PL-MOV-SEM.CodCia  = s-CodCia AND
            PL-MOV-SEM.Periodo = s-Periodo AND
            PL-MOV-SEM.NroSem  = J AND
            PL-MOV-SEM.CodPln  = PL-PLAN.CodPln AND
            PL-MOV-SEM.CodCal  = PL-BOLE.CodCal AND
            PL-MOV-SEM.CodPer  = PL-FLG-SEM.CodPer AND
            PL-MOV-SEM.CodMov  = PL-BOLE.CodMov NO-LOCK NO-ERROR.
        IF AVAILABLE PL-MOV-SEM AND PL-MOV-SEM.valcal-Sem <> 0 THEN DO:
            FIND Tempo WHERE Tempo.CodPer = PL-MOV-SEM.CodPer AND
                             Tempo.CodMov = PL-MOV-SEM.CodMov
                             NO-ERROR.
            IF NOT AVAILABLE Tempo THEN DO:                 
            CREATE tempo.
            ASSIGN
                Tempo.CodPer = PL-MOV-SEM.CodPer 
                Tempo.TpoBol = Pl-BOLE.TpoBol
                Tempo.Codmov = PL-MOV-SEM.CodMov .
            END.
            Tempo.valcal[I] = Tempo.valcal[I] + PL-MOV-SEM.valcal-Sem .    
            Tempo.valcal[13] = Tempo.valcal[13] + PL-MOV-SEM.valcal-Sem .
            valcaltotR = valcaltotR + PL-MOV-SEM.valcal-Sem .
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 01 THEN DO:
             valcalR1 = valcalR1 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 02 THEN DO:
             valcalR2 = valcalR2 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 03 THEN DO:
             valcalR3 = valcalR3 + PL-MOV-SEM.valcal-Sem.
            END.                        
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 04 THEN DO:
             valcalR4 = valcalR4 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 05 THEN DO:
             valcalR5 = valcalR5 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 06 THEN DO:
             valcalR6 = valcalR6 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 07 THEN DO:
             valcalR7 = valcalR7 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 08 THEN DO:
             valcalR8 = valcalR8 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 09 THEN DO:
             valcalR9 = valcalR9 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 10 THEN DO:
             valcalR10 = valcalR10 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 11 THEN DO:
             valcalR11 = valcalR11 + PL-MOV-SEM.valcal-Sem.
            END.                                                                
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 12 THEN DO:
             valcalR12 = valcalR12 + PL-MOV-SEM.valcal-Sem.
            END.        
        END.
    END.
    
END.

 
/*************************************** SEPARACION DE MOVIMIENTO************************************************/

x-con-reg = 0.
/* Cargamos el temporal con los Descuentos */
FOR EACH PL-BOLE NO-LOCK WHERE
    PL-BOLE.CodPln = PL-PLAN.CodPln AND
    PL-BOLE.CodCal = PL-CALC.CodCal AND
    PL-BOLE.TpoBol = "Descuentos" BY PL-BOLE.nroitm:
    DO J = 1 TO FILL-IN-NRO-SEM :
        FIND PL-SEM WHERE PL-SEM.codcia = s-codcia
            AND PL-SEM.periodo = s-periodo
            AND PL-SEM.nrosem = J
            NO-LOCK NO-ERROR.
        I = PL-SEM.NroMes.
        FIND PL-MOV-SEM WHERE
            PL-MOV-SEM.CodCia  = s-CodCia AND
            PL-MOV-SEM.Periodo = s-Periodo AND
            PL-MOV-SEM.NroSem  = J  AND
            PL-MOV-SEM.CodPln  = PL-PLAN.CodPln AND
            PL-MOV-SEM.CodCal  = PL-BOLE.CodCal AND
            PL-MOV-SEM.CodPer  = PL-FLG-SEM.CodPer AND
            PL-MOV-SEM.CodMov  = PL-BOLE.CodMov NO-LOCK NO-ERROR.
        IF AVAILABLE PL-MOV-SEM AND PL-MOV-SEM.valcal-Sem <> 0 THEN DO:
            FIND Tempo WHERE Tempo.CodPer = PL-MOV-SEM.CodPer AND
                             Tempo.CodMov = PL-MOV-SEM.CodMov
                             NO-ERROR.
            IF NOT AVAILABLE Tempo THEN DO:                 
            CREATE tempo.
            ASSIGN
                Tempo.CodPer = PL-MOV-SEM.CodPer 
                Tempo.TpoBol = Pl-BOLE.TpoBol
                Tempo.Codmov = PL-MOV-SEM.CodMov .
            END.
            Tempo.valcal[I] = Tempo.valcal[I] + PL-MOV-SEM.valcal-Sem .    
            Tempo.valcal[13] = Tempo.valcal[13] + PL-MOV-SEM.valcal-Sem .  
            valcaltotD = valcaltotD + PL-MOV-SEM.valcal-Sem .
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 01 THEN DO:
             valcalD1 = valcalD1 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 02 THEN DO:
             valcalD2 = valcalD2 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 03 THEN DO:
             valcalD3 = valcalD3 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 04 THEN DO:
             valcalD4 = valcalD4 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 05 THEN DO:
             valcalD5 = valcalD5 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 06 THEN DO:
             valcalD6 = valcalD6 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 07 THEN DO:
             valcalD7 = valcalD7 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 08 THEN DO:
             valcalD8 = valcalD8 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 09 THEN DO:
             valcalD9 = valcalD9 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 10 THEN DO:
             valcalD10 = valcalD10 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 11 THEN DO:
             valcalD11= valcalD11 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 12 THEN DO:
             valcalD12 = valcalD12 + PL-MOV-SEM.valcal-Sem.
            END.
        END.
    END.

END.
    
    FOR EACH tempo NO-LOCK BREAK BY tempo.tpobol DESCENDING 
                                 BY tempo.CodMov :
          IF FIRST-OF(Tempo.TpoBol) THEN DO:
               PUT STREAM strm-boleta  tempo.Tpobol  SKIP.
          END.
        FIND PL-CONC WHERE PL-CONC.CodMov = tempo.CodMov NO-LOCK NO-ERROR.
        IF AVAILABLE PL-CONC THEN x-linea = PL-CONC.DesMov.

        PUT STREAM strm-boleta tempo.codmov AT 1.
        PUT STREAM strm-boleta x-linea format "x(13)" AT 5 .
        PUT STREAM strm-boleta tempo.valcal[1] FORMAT "->>>>9.99" AT 20.
        PUT STREAM strm-boleta tempo.valcal[2] FORMAT "->>>>9.99" AT 30.
        PUT STREAM strm-boleta tempo.valcal[3] FORMAT "->>>>9.99" AT 40.
        PUT STREAM strm-boleta tempo.valcal[4] FORMAT "->>>>9.99" AT 50.
        PUT STREAM strm-boleta tempo.valcal[5] FORMAT "->>>>9.99" AT 60.
        PUT STREAM strm-boleta tempo.valcal[6] FORMAT "->>>>9.99" AT 70.
        PUT STREAM strm-boleta tempo.valcal[7] FORMAT "->>>>9.99" AT 80.
        PUT STREAM strm-boleta tempo.valcal[8] FORMAT "->>>>9.99" AT 90.
        PUT STREAM strm-boleta tempo.valcal[9] FORMAT "->>>>9.99" AT 100.
        PUT STREAM strm-boleta tempo.valcal[10] FORMAT "->>>>9.99" AT 110.
        PUT STREAM strm-boleta tempo.valcal[11] FORMAT "->>>>9.99" AT 120.
        PUT STREAM strm-boleta tempo.valcal[12] FORMAT "->>>>9.99" AT 130.
        PUT STREAM strm-boleta tempo.valcal[13] FORMAT "->>>>9.99" AT 140 SKIP.
        
    END.
    
    IF valcalD1 >= valcalR1 THEN 
    totG1 = valcalD1 - valcalR1.
    ELSE
    totG1 = valcalR1 - valcalD1.
    IF valcalD2 >= valcalR2 THEN 
    totG2 = valcalD2 - valcalR2.
    ELSE
    totG2 = valcalR2 - valcalD2.
    IF valcalD3 >= valcalR3 THEN 
    totG3 = valcalD3 - valcalR3.
    ELSE
    totG3 = valcalR3 - valcalD3.
    IF valcalD4 >= valcalR4 THEN 
    totG4 = valcalD4 - valcalR4.
    ELSE
    totG4 = valcalR4 - valcalD4.
    IF valcalD5 >= valcalR5 THEN 
    totG5 = valcalD5 - valcalR5.
    ELSE
    totG5 = valcalR5 - valcalD5.
    IF valcalD6 >= valcalR6 THEN 
    totG6 = valcalD6 - valcalR6.
    ELSE
    totG6 = valcalR6 - valcalD6.
    IF valcalD7 >= valcalR7 THEN 
    totG7 = valcalD7 - valcalR7.
    ELSE
    totG7 = valcalR7 - valcalD7.
    IF valcalD8 >= valcalR8 THEN 
    totG8 = valcalD8 - valcalR8.
    ELSE
    totG8 = valcalR8 - valcalD8.
    IF valcalD9 >= valcalR9 THEN 
    totG9 = valcalD9 - valcalR9.
    ELSE
    totG9 = valcalR9 - valcalD9.
    IF valcalD10 >= valcalR10 THEN 
    totG10 = valcalD10 - valcalR10.
    ELSE
    totG10 = valcalR10 - valcalD10.
    IF valcalD11 >= valcalR11 THEN 
    totG11 = valcalD11 - valcalR11.
    ELSE
    totG11 = valcalR11 - valcalD11.
    IF valcalD12 >= valcalR12 THEN 
    totG12 = valcalD12 - valcalR12.
    ELSE
    totG12 = valcalR12 - valcalD12.
    
    IF valcaltotD >= valcaltotR THEN 
    totG13 = valcaltotD - valcaltotR.
    ELSE
    totG13 = valcaltotR - valcaltotD.

    PUT STREAM strm-boleta SKIP(2).
    PUT STREAM strm-boleta "TOT. DESCUENTOS" .
    PUT STREAM strm-boleta valcalD1  AT 19 .
    PUT STREAM strm-boleta valcalD2  AT 29 .    
    PUT STREAM strm-boleta valcalD3  AT 39 .
    PUT STREAM strm-boleta valcalD4  AT 49 .
    PUT STREAM strm-boleta valcalD5  AT 59 .
    PUT STREAM strm-boleta valcalD6  AT 69 .
    PUT STREAM strm-boleta valcalD7  AT 79 .
    PUT STREAM strm-boleta valcalD8  AT 89 .
    PUT STREAM strm-boleta valcalD9  AT 99 .
    PUT STREAM strm-boleta valcalD10  AT 109 .
    PUT STREAM strm-boleta valcalD11  AT 119 .
    PUT STREAM strm-boleta valcalD12  AT 129 .
    PUT STREAM strm-boleta valcaltotD  AT 139 SKIP. 
    PUT STREAM strm-boleta "TOT. REMUNERACION" . 
    PUT STREAM strm-boleta valcalR1  AT 19 .                                          
    PUT STREAM strm-boleta valcalR2  AT 29 .
    PUT STREAM strm-boleta valcalR3  AT 39 .
    PUT STREAM strm-boleta valcalR4  AT 49 .
    PUT STREAM strm-boleta valcalR5  AT 59 .
    PUT STREAM strm-boleta valcalR6  AT 69 .
    PUT STREAM strm-boleta valcalR7  AT 79 .
    PUT STREAM strm-boleta valcalR8  AT 89 .
    PUT STREAM strm-boleta valcalR9  AT 99 .
    PUT STREAM strm-boleta valcalR10  AT 109 .
    PUT STREAM strm-boleta valcalR11  AT 119 .
    PUT STREAM strm-boleta valcalR12  AT 129 .                                        
    PUT STREAM strm-boleta valcaltotR  AT 139 SKIP.    
    PUT STREAM strm-boleta "TOT. GENERAL  " .
    PUT STREAM strm-boleta totg1  AT 19 .    
    PUT STREAM strm-boleta totg2  AT 29 .
    PUT STREAM strm-boleta totg3  AT 39 .
    PUT STREAM strm-boleta totg4  AT 49 .
    PUT STREAM strm-boleta totg5  AT 59 .
    PUT STREAM strm-boleta totg6  AT 69 .
    PUT STREAM strm-boleta totg7  AT 79 .
    PUT STREAM strm-boleta totg8  AT 89 .
    PUT STREAM strm-boleta totg9  AT 99 .
    PUT STREAM strm-boleta totg10  AT 109 .
    PUT STREAM strm-boleta totg11  AT 119 .
    PUT STREAM strm-boleta totg12  AT 129 .
    PUT STREAM strm-boleta TOTG13 AT 139 SKIP.    
    PAGE STREAM strm-boleta.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE busca_datos2 B-table-Win 
PROCEDURE busca_datos2 :
/*------------------------------------------------------------------------------
    Busca datos
------------------------------------------------------------------------------*/

IF PL-FLG-SEM.SitAct = "Inactivo" THEN RETURN. /* Si no esta Activo */
/*
IF NOT CAN-FIND(FIRST PL-MOV-SEM WHERE
    PL-MOV-SEM.CodCia  = s-CodCia        AND
    PL-MOV-SEM.Periodo = s-Periodo       AND
    PL-MOV-SEM.NroSem  = FILL-IN-NRO-SEM AND
    PL-MOV-SEM.CodPln  = PL-PLAN.CodPln  AND
    PL-MOV-SEM.CodCal  = PL-CALC.CodCal  AND
    PL-MOV-SEM.CodPer  = PL-FLG-SEM.CodPer) THEN RETURN. /* Si no tiene c�lculo */
*/

DISPLAY PL-FLG-SEM.CodPer @ FILL-IN-Codigo WITH FRAME F-Msg.


/* Cargamos el temporal con los ingresos */
DO I = 1 TO 12:
FOR EACH PL-MOV-SEM WHERE
        PL-MOV-SEM.CodCia  = s-CodCia AND
        PL-MOV-SEM.Periodo = s-Periodo AND
        PL-MOV-SEM.NroSem  = I /*FILL-IN-NRO-SEM*/ AND
        PL-MOV-SEM.CodPln  = PL-PLAN.CodPln AND
        PL-MOV-SEM.CodCal  = PL-CALC.CodCal AND
        PL-MOV-SEM.CodPer  = PL-FLG-SEM.CodPer NO-LOCK :
        CREATE Tempo.
        RAW-TRANSFER PL-MOV-SEM TO Tempo.
END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE busca_datos_2 B-table-Win 
PROCEDURE busca_datos_2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
valcalD1 = 0.
valcalD2 = 0.
valcalD3 = 0.
valcalD4 = 0.
valcalD5 = 0.
valcalD6 = 0.
valcalD7 = 0.
valcalD8 = 0.
valcalD9 = 0.
valcalD10 = 0.
valcalD11 = 0.
valcalD12 = 0.
valcaltotD = 0.

valcalR1 = 0.
valcalR2 = 0.
valcalR3 = 0.
valcalR4 = 0.
valcalR5 = 0.
valcalR6 = 0.
valcalR7 = 0.
valcalR8 = 0.
valcalR9 = 0.
valcalR10 = 0.
valcalR11 = 0.
valcalR12 = 0.
valcaltotR = 0.

totG1 = 0.
totG2 = 0.
totG3 = 0.
totG4 = 0.
totG5 = 0.
totG6 = 0.
totG7 = 0.
totG8 = 0.
totG9 = 0.
totG10 = 0.
totG11 = 0.
totG12 = 0.
totG13 = 0.

DEFINE VARIABLE saldo     AS DECIMAL.
DEFINE VARIABLE F-valcalD  AS DECIMAL.
DEFINE VARIABLE linea-msg AS CHARACTER EXTENT 3.
DEFINE VARIABLE j         AS INTEGER.
DEFINE VARIABLE ii        AS INTEGER.
DEFINE VARIABLE x-dirdiv  AS CHAR.
DEFINE VARIABLE x-desdiv  AS CHAR.
DEFINE VARIABLE x-dirper  AS CHAR.
DEFINE VARIABLE x-LEper   AS CHAR.
DEFINE VARIABLE X-RUC     AS CHAR INIT "20100038146" format "X(11)".
FOR EACH tempo:
    DELETE Tempo.
END.

/*IF T-FLG-SEM.SitAct = "Inactivo" THEN RETURN. /* Si no esta Activo */
 * 
 * IF NOT CAN-FIND(FIRST PL-MOV-SEM WHERE
 *     PL-MOV-SEM.CodCia  = s-CodCia        AND
 *     PL-MOV-SEM.Periodo = s-Periodo       AND
 *     PL-MOV-SEM.NroSem  = FILL-IN-NRO-SEM AND
 *     PL-MOV-SEM.CodPln  = PL-PLAN.CodPln  AND
 *     PL-MOV-SEM.CodCal  = PL-CALC.CodCal  AND
 *     PL-MOV-SEM.CodPer  = T-FLG-SEM.CodPer) THEN RETURN. /* Si no tiene c�lculo */*/

FIND GN-DIVI WHERE GN-DIVI.CodCia = s-codcia AND
     GN-DIVI.CodDiv =  T-FLG-SEM.CodDiv NO-LOCK NO-ERROR.
IF AVAILABLE GN-DIVI THEN
   ASSIGN x-desdiv = GN-DIVI.DesDiv
          x-dirdiv = GN-DIVI.DirDiv.
x-linea = TRIM(T-FLG-SEM.CodDiv) + " " + TRIM(x-desdiv).
FIND gn-cias WHERE gn-cias.codcia  = s-codcia NO-LOCK NO-ERROR.

DO ii = 1 TO 1 /*FILL-IN-Copias */:
    FIND GN-DIVI WHERE GN-DIVI.CodCia = s-codcia AND
         GN-DIVI.CodDiv =  T-FLG-SEM.CodDiv NO-LOCK NO-ERROR.
    IF AVAILABLE GN-DIVI THEN
       ASSIGN x-desdiv = GN-DIVI.DesDiv
              x-dirdiv = TRIM(GN-DIVI.DirDiv).
    x-linea = TRIM(T-FLG-SEM.CodDiv) + " " + TRIM(x-desdiv).
    PUT STREAM strm-boleta SKIP(1).
    PUT STREAM strm-boleta CONTROL CHR(18) CHR(27) CHR(69).
    PUT STREAM strm-boleta S-NomCia SKIP.
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(70) CHR(27) CHR(77).

   /* PUT STREAM strm-boleta "R.U.C. : " X-Ruc   "DIVISION :" AT 70 
                           " " x-linea FORMAT "X(30)" SKIP.
    PUT STREAM strm-boleta "R.PATRONAL : " ( X-RUC + "0000000") FORMAT "X(18)"
                           x-dirdiv AT 70 FORMAT "X(30)" SKIP.
    PUT STREAM strm-boleta S-DirCia  ' Telefono : ' GN-CIAS.TlflCia  SKIP.*/
    
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(80) CHR(27) CHR(69).
   
    x-linea = "R E P O R T E   D E    C O N C E P T O S  " + STRING(S-PERIODO,"9999").
    
    PUT STREAM strm-boleta x-linea AT 20 SKIP.
    
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(70) CHR(15).
    IF AVAILABLE PL-PERS THEN DO:
        x-linea = PL-PERS.PatPer + " " + PL-PERS.MatPer + ", " + PL-PERS.NomPer.
        x-dirper = TRIM(PL-PERS.dirper) + " " + TRIM(PL-PERS.distri).
        x-LEper  = PL-PERS.NroDocId.
        END.
    ELSE x-linea = "".

    PUT STREAM strm-boleta "Codigo    : " T-FLG-SEM.codper " " x-linea FORMAT "X(40)" SKIP.
    
    x-linea = FILL("-",150).
 
    PUT STREAM strm-boleta x-linea FORMAT "X(150)" SKIP.
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(69).
    PUT STREAM strm-boleta "   Conceptos           ENE       FEB      MAR       ABR       MAY       JUN       JUL       AGO       SEP       OCT       NOV       DIC     TOTAL " FORMAT "X(245)" SKIP.
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(70).
    x-linea = FILL("-",150).
    PUT STREAM strm-boleta x-linea FORMAT "X(150)" SKIP.
    
end.

DISPLAY T-FLG-SEM.CodPer @ FILL-IN-Codigo WITH FRAME F-Msg.

/*Cargamos el temporal con los ingresos */
FOR EACH PL-BOLE NO-LOCK WHERE
    PL-BOLE.CodPln = PL-PLAN.CodPln AND
    PL-BOLE.CodCal = PL-CALC.CodCal AND
    PL-BOLE.TpoBol = "Remuneraciones" BY PL-BOLE.nroitm:
    DO J = 1 TO FILL-IN-NRO-SEM:
        FIND PL-SEM WHERE PL-SEM.codcia = s-codcia
            AND PL-SEM.periodo = s-periodo
            AND PL-SEM.nrosem = J
            NO-LOCK NO-ERROR.
        I = PL-SEM.NroMes.
        FIND PL-MOV-SEM WHERE
            PL-MOV-SEM.CodCia  = s-CodCia AND
            PL-MOV-SEM.Periodo = s-Periodo AND
            PL-MOV-SEM.NroSem  = J AND
            PL-MOV-SEM.CodPln  = PL-PLAN.CodPln AND
            PL-MOV-SEM.CodCal  = PL-BOLE.CodCal AND
            PL-MOV-SEM.CodPer  = T-FLG-SEM.CodPer AND
            PL-MOV-SEM.CodMov  = PL-BOLE.CodMov NO-LOCK NO-ERROR.
        IF AVAILABLE PL-MOV-SEM AND PL-MOV-SEM.valcal-Sem <> 0 THEN DO:
            FIND Tempo WHERE Tempo.CodPer = PL-MOV-SEM.CodPer AND
                             Tempo.CodMov = PL-MOV-SEM.CodMov
                             NO-ERROR.
            IF NOT AVAILABLE Tempo THEN DO:                 
            CREATE tempo.
            ASSIGN
                Tempo.CodPer = PL-MOV-SEM.CodPer 
                Tempo.TpoBol = Pl-BOLE.TpoBol
                Tempo.Codmov = PL-MOV-SEM.CodMov .
            END.
            Tempo.valcal[I] = Tempo.valcal[I] + PL-MOV-SEM.valcal-Sem .    
            Tempo.valcal[13] = Tempo.valcal[13] + PL-MOV-SEM.valcal-Sem .
            valcaltotR = valcaltotR + PL-MOV-SEM.valcal-Sem .
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 01 THEN DO:
             valcalR1 = valcalR1 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 02 THEN DO:
             valcalR2 = valcalR2 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 03 THEN DO:
             valcalR3 = valcalR3 + PL-MOV-SEM.valcal-Sem.
            END.                        
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 04 THEN DO:
             valcalR4 = valcalR4 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 05 THEN DO:
             valcalR5 = valcalR5 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 06 THEN DO:
             valcalR6 = valcalR6 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 07 THEN DO:
             valcalR7 = valcalR7 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 08 THEN DO:
             valcalR8 = valcalR8 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 09 THEN DO:
             valcalR9 = valcalR9 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 10 THEN DO:
             valcalR10 = valcalR10 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 11 THEN DO:
             valcalR11 = valcalR11 + PL-MOV-SEM.valcal-Sem.
            END.                                                                
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 12 THEN DO:
             valcalR12 = valcalR12 + PL-MOV-SEM.valcal-Sem.
            END.        
        END.
    END.
    
END.

 
/*************************************** SEPARACION DE MOVIMIENTO************************************************/

x-con-reg = 0.
/* Cargamos el temporal con los Descuentos */
FOR EACH PL-BOLE NO-LOCK WHERE
    PL-BOLE.CodPln = PL-PLAN.CodPln AND
    PL-BOLE.CodCal = PL-CALC.CodCal AND
    PL-BOLE.TpoBol = "Descuentos" BY PL-BOLE.nroitm:
    DO J = 1 TO FILL-IN-NRO-SEM :
        FIND PL-SEM WHERE PL-SEM.codcia = s-codcia
            AND PL-SEM.periodo = s-periodo
            AND PL-SEM.nrosem = J
            NO-LOCK NO-ERROR.
        I = PL-SEM.NroMes.
        FIND PL-MOV-SEM WHERE
            PL-MOV-SEM.CodCia  = s-CodCia AND
            PL-MOV-SEM.Periodo = s-Periodo AND
            PL-MOV-SEM.NroSem  = J  AND
            PL-MOV-SEM.CodPln  = PL-PLAN.CodPln AND
            PL-MOV-SEM.CodCal  = PL-BOLE.CodCal AND
            PL-MOV-SEM.CodPer  = T-FLG-SEM.CodPer AND
            PL-MOV-SEM.CodMov  = PL-BOLE.CodMov NO-LOCK NO-ERROR.
        IF AVAILABLE PL-MOV-SEM AND PL-MOV-SEM.valcal-Sem <> 0 THEN DO:
            FIND Tempo WHERE Tempo.CodPer = PL-MOV-SEM.CodPer AND
                             Tempo.CodMov = PL-MOV-SEM.CodMov
                             NO-ERROR.
            IF NOT AVAILABLE Tempo THEN DO:                 
            CREATE tempo.
            ASSIGN
                Tempo.CodPer = PL-MOV-SEM.CodPer 
                Tempo.TpoBol = Pl-BOLE.TpoBol
                Tempo.Codmov = PL-MOV-SEM.CodMov .
            END.
            Tempo.valcal[I] = Tempo.valcal[I] + PL-MOV-SEM.valcal-Sem .    
            Tempo.valcal[13] = Tempo.valcal[13] + PL-MOV-SEM.valcal-Sem .  
            valcaltotD = valcaltotD + PL-MOV-SEM.valcal-Sem .
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 01 THEN DO:
             valcalD1 = valcalD1 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 02 THEN DO:
             valcalD2 = valcalD2 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 03 THEN DO:
             valcalD3 = valcalD3 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 04 THEN DO:
             valcalD4 = valcalD4 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 05 THEN DO:
             valcalD5 = valcalD5 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 06 THEN DO:
             valcalD6 = valcalD6 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 07 THEN DO:
             valcalD7 = valcalD7 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 08 THEN DO:
             valcalD8 = valcalD8 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 09 THEN DO:
             valcalD9 = valcalD9 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 10 THEN DO:
             valcalD10 = valcalD10 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 11 THEN DO:
             valcalD11= valcalD11 + PL-MOV-SEM.valcal-Sem.
            END.
            IF AVAILABLE PL-MOV-SEM AND PL-SEM.NroMes = 12 THEN DO:
             valcalD12 = valcalD12 + PL-MOV-SEM.valcal-Sem.
            END.
        END.
    END.

END.
    
    FOR EACH tempo NO-LOCK BREAK BY tempo.tpobol DESCENDING 
                                 BY tempo.CodMov :
          IF FIRST-OF(Tempo.TpoBol) THEN DO:
               PUT STREAM strm-boleta  tempo.Tpobol  SKIP.
          END.
        FIND PL-CONC WHERE PL-CONC.CodMov = tempo.CodMov NO-LOCK NO-ERROR.
        IF AVAILABLE PL-CONC THEN x-linea = PL-CONC.DesMov.

        PUT STREAM strm-boleta tempo.codmov AT 1.
        PUT STREAM strm-boleta x-linea format "x(13)" AT 5 .
        PUT STREAM strm-boleta tempo.valcal[1] FORMAT "->>>>9.99" AT 20.
        PUT STREAM strm-boleta tempo.valcal[2] FORMAT "->>>>9.99" AT 30.
        PUT STREAM strm-boleta tempo.valcal[3] FORMAT "->>>>9.99" AT 40.
        PUT STREAM strm-boleta tempo.valcal[4] FORMAT "->>>>9.99" AT 50.
        PUT STREAM strm-boleta tempo.valcal[5] FORMAT "->>>>9.99" AT 60.
        PUT STREAM strm-boleta tempo.valcal[6] FORMAT "->>>>9.99" AT 70.
        PUT STREAM strm-boleta tempo.valcal[7] FORMAT "->>>>9.99" AT 80.
        PUT STREAM strm-boleta tempo.valcal[8] FORMAT "->>>>9.99" AT 90.
        PUT STREAM strm-boleta tempo.valcal[9] FORMAT "->>>>9.99" AT 100.
        PUT STREAM strm-boleta tempo.valcal[10] FORMAT "->>>>9.99" AT 110.
        PUT STREAM strm-boleta tempo.valcal[11] FORMAT "->>>>9.99" AT 120.
        PUT STREAM strm-boleta tempo.valcal[12] FORMAT "->>>>9.99" AT 130.
        PUT STREAM strm-boleta tempo.valcal[13] FORMAT "->>>>9.99" AT 140 SKIP.
        
    END.
    
    IF valcalD1 >= valcalR1 THEN 
    totG1 = valcalD1 - valcalR1.
    ELSE
    totG1 = valcalR1 - valcalD1.
    IF valcalD2 >= valcalR2 THEN 
    totG2 = valcalD2 - valcalR2.
    ELSE
    totG2 = valcalR2 - valcalD2.
    IF valcalD3 >= valcalR3 THEN 
    totG3 = valcalD3 - valcalR3.
    ELSE
    totG3 = valcalR3 - valcalD3.
    IF valcalD4 >= valcalR4 THEN 
    totG4 = valcalD4 - valcalR4.
    ELSE
    totG4 = valcalR4 - valcalD4.
    IF valcalD5 >= valcalR5 THEN 
    totG5 = valcalD5 - valcalR5.
    ELSE
    totG5 = valcalR5 - valcalD5.
    IF valcalD6 >= valcalR6 THEN 
    totG6 = valcalD6 - valcalR6.
    ELSE
    totG6 = valcalR6 - valcalD6.
    IF valcalD7 >= valcalR7 THEN 
    totG7 = valcalD7 - valcalR7.
    ELSE
    totG7 = valcalR7 - valcalD7.
    IF valcalD8 >= valcalR8 THEN 
    totG8 = valcalD8 - valcalR8.
    ELSE
    totG8 = valcalR8 - valcalD8.
    IF valcalD9 >= valcalR9 THEN 
    totG9 = valcalD9 - valcalR9.
    ELSE
    totG9 = valcalR9 - valcalD9.
    IF valcalD10 >= valcalR10 THEN 
    totG10 = valcalD10 - valcalR10.
    ELSE
    totG10 = valcalR10 - valcalD10.
    IF valcalD11 >= valcalR11 THEN 
    totG11 = valcalD11 - valcalR11.
    ELSE
    totG11 = valcalR11 - valcalD11.
    IF valcalD12 >= valcalR12 THEN 
    totG12 = valcalD12 - valcalR12.
    ELSE
    totG12 = valcalR12 - valcalD12.
    
    IF valcaltotD >= valcaltotR THEN 
    totG13 = valcaltotD - valcaltotR.
    ELSE
    totG13 = valcaltotR - valcaltotD.

    PUT STREAM strm-boleta SKIP(2).
    PUT STREAM strm-boleta "TOT. DESCUENTOS" .
    PUT STREAM strm-boleta valcalD1  AT 19 .
    PUT STREAM strm-boleta valcalD2  AT 29 .    
    PUT STREAM strm-boleta valcalD3  AT 39 .
    PUT STREAM strm-boleta valcalD4  AT 49 .
    PUT STREAM strm-boleta valcalD5  AT 59 .
    PUT STREAM strm-boleta valcalD6  AT 69 .
    PUT STREAM strm-boleta valcalD7  AT 79 .
    PUT STREAM strm-boleta valcalD8  AT 89 .
    PUT STREAM strm-boleta valcalD9  AT 99 .
    PUT STREAM strm-boleta valcalD10  AT 109 .
    PUT STREAM strm-boleta valcalD11  AT 119 .
    PUT STREAM strm-boleta valcalD12  AT 129 .
    PUT STREAM strm-boleta valcaltotD  AT 139 SKIP. 
    PUT STREAM strm-boleta "TOT. REMUNERACION" . 
    PUT STREAM strm-boleta valcalR1  AT 19 .                                          
    PUT STREAM strm-boleta valcalR2  AT 29 .
    PUT STREAM strm-boleta valcalR3  AT 39 .
    PUT STREAM strm-boleta valcalR4  AT 49 .
    PUT STREAM strm-boleta valcalR5  AT 59 .
    PUT STREAM strm-boleta valcalR6  AT 69 .
    PUT STREAM strm-boleta valcalR7  AT 79 .
    PUT STREAM strm-boleta valcalR8  AT 89 .
    PUT STREAM strm-boleta valcalR9  AT 99 .
    PUT STREAM strm-boleta valcalR10  AT 109 .
    PUT STREAM strm-boleta valcalR11  AT 119 .
    PUT STREAM strm-boleta valcalR12  AT 129 .                                        
    PUT STREAM strm-boleta valcaltotR  AT 139 SKIP.    
    PUT STREAM strm-boleta "TOT. GENERAL  " .
    PUT STREAM strm-boleta totg1  AT 19 .    
    PUT STREAM strm-boleta totg2  AT 29 .
    PUT STREAM strm-boleta totg3  AT 39 .
    PUT STREAM strm-boleta totg4  AT 49 .
    PUT STREAM strm-boleta totg5  AT 59 .
    PUT STREAM strm-boleta totg6  AT 69 .
    PUT STREAM strm-boleta totg7  AT 79 .
    PUT STREAM strm-boleta totg8  AT 89 .
    PUT STREAM strm-boleta totg9  AT 99 .
    PUT STREAM strm-boleta totg10  AT 109 .
    PUT STREAM strm-boleta totg11  AT 119 .
    PUT STREAM strm-boleta totg12  AT 129 .
    PUT STREAM strm-boleta TOTG13 AT 139 SKIP.    
    PAGE STREAM strm-boleta.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal B-table-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH T-FLG-SEM:
    DELETE T-FLG-SEM.
  END.
  
  FOR EACH PL-FLG-SEM WHERE PL-FLG-SEM.CodCia = s-CodCia
        AND PL-FLG-SEM.Periodo = s-Periodo
        AND PL-FLG-SEM.codpln = PL-PLAN.CodPln
        AND PL-FLG-SEM.NroSem <= FILL-IN-NRO-SEM
        NO-LOCK,
        EACH PL-PERS OF PL-FLG-SEM NO-LOCK:
    IF PL-FLG-SEM.SitAct = "Inactivo" THEN NEXT.
    IF NOT CAN-FIND(FIRST PL-MOV-SEM WHERE
        PL-MOV-SEM.CodCia  = s-CodCia           AND
        PL-MOV-SEM.Periodo = s-Periodo          AND
        PL-MOV-SEM.NroSem  = PL-FLG-SEM.NroSem  AND
        PL-MOV-SEM.CodPln  = PL-PLAN.CodPln     AND
        PL-MOV-SEM.CodCal  = PL-CALC.CodCal     AND
        PL-MOV-SEM.CodPer  = PL-FLG-SEM.CodPer) THEN NEXT. /* Si no tiene calculo */
    FIND T-FLG-SEM WHERE T-FLG-SEM.CodPer = PL-FLG-SEM.CodPer
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE T-FLG-SEM THEN CREATE T-FLG-SEM.
    BUFFER-COPY PL-FLG-SEM TO T-FLG-SEM.
  END.
  
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
/*------------------------------------------------------------------------------
    Impresi�n de boleta de pago.
------------------------------------------------------------------------------*/
  DEF VAR c-Copias AS INT NO-UNDO.
  
  RUN bin/_prnctr.p.
  IF s-salida-impresion = 0 THEN RETURN.
   
  /* Captura parametros de impresion */
  /* s-pagina-inicial,  s-pagina-final,  s-printer-name,  s-print-file,  s-nro-copias */
  
  RUN aderb/_prlist.p(
      OUTPUT s-printer-list,
      OUTPUT s-port-list,
      OUTPUT s-printer-count).
  s-port-name = ENTRY(LOOKUP(s-printer-name, s-printer-list), s-port-list).
  s-port-name = REPLACE(S-PORT-NAME, ":", "").

  IF s-salida-impresion = 1 
  THEN ASSIGN
        s-nro-copias = 1
        s-print-file = SESSION:TEMP-DIRECTORY + "report.prn".
  IF s-salida-impresion = 3
  THEN ASSIGN
        s-nro-copias = 1.
  
  DEFINE VAR I AS INTEGER.
  DO c-Copias = 1 TO s-nro-copias:
    FOR EACH Tempo:
        DELETE Tempo.
    END.
    CASE s-salida-impresion:
        WHEN 1 THEN OUTPUT STREAM strm-boleta TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Pantalla */
        WHEN 2 THEN OUTPUT STREAM strm-boleta TO VALUE(s-port-name)  PAGED PAGE-SIZE 62. /* Impresora */
        WHEN 3 THEN OUTPUT STREAM strm-boleta TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Archivo */
    END CASE.
    
    PUT STREAM strm-boleta CONTROL {&Prn0} + {&Prn5A} + CHR(66) + {&Prn4} .

    CASE R-seleccion:
        WHEN 1 THEN DO:
            ASSIGN FILL-IN-Seccion:LABEL IN FRAME F-Msg = "Secci�n".
            RUN Carga-Temporal.
            FOR EACH T-FLG-SEM, FIRST PL-PERS OF T-FLG-SEM NO-LOCK:
                DISPLAY T-FLG-SEM.seccion @ FILL-IN-Seccion WITH FRAME F-Msg.
                RUN busca_datos_2.
            END.
/*            GET FIRST br_pl-flg-m.
 *             DO WHILE AVAILABLE(PL-FLG-SEM):
 *                 DISPLAY PL-FLG-SEM.seccion @ FILL-IN-Seccion WITH FRAME F-Msg.
 *                 RUN busca_datos.
 *                 GET NEXT br_pl-flg-m.
 *             END.*/
        END.
        WHEN 2 THEN DO:
            ASSIGN FILL-IN-Seccion:LABEL = "Secci�n".
            DO i = 1 TO br_pl-flg-m:NUM-SELECTED-ROWS IN FRAME F-Main:
                ASSIGN stat-reg  = br_pl-flg-m:FETCH-SELECTED-ROW(i).
                IF stat-reg THEN DO:
                    DISPLAY PL-FLG-SEM.seccion @ FILL-IN-Seccion WITH FRAME F-Msg.
                    RUN busca_datos.
                END.
            END.
            ASSIGN stat-reg = br_pl-flg-m:DESELECT-ROWS().
        END.
        WHEN 3 THEN DO:
            ASSIGN FILL-IN-Seccion:LABEL = "Secci�n".
            DISPLAY COMBO-S @ FILL-IN-Seccion WITH FRAME F-Msg.
            GET FIRST br_pl-flg-m.
            DO WHILE AVAILABLE(PL-FLG-SEM):
                IF PL-FLG-SEM.Seccion = COMBO-S THEN RUN busca_datos.
                GET NEXT br_pl-flg-m.
            END.
        END.
        WHEN 4 THEN DO:
            ASSIGN FILL-IN-Seccion:LABEL = "Proyecto".
            DISPLAY COMBO-S @ FILL-IN-Seccion WITH FRAME F-Msg.
            GET FIRST br_pl-flg-m.
            DO WHILE AVAILABLE(PL-FLG-SEM):
                IF PL-FLG-SEM.Proyecto = COMBO-S THEN RUN busca_datos.
                GET NEXT br_pl-flg-m.
            END.
        END.
    END CASE.      
    PAGE STREAM strm-boleta .
    OUTPUT STREAM strm-boleta CLOSE.
  END.
  HIDE FRAME F-Msg.
  CASE s-salida-impresion:
       WHEN 1 OR WHEN 3 THEN RUN LIB/W-README (s-print-file).
  END CASE. 



/*
DEFINE VARIABLE p-archivo AS CHARACTER.

IF INPUT FRAME {&FRAME-NAME} TGL-pantalla = TRUE THEN DO:
    P-archivo = SESSION:TEMP-DIRECTORY +
          STRING(NEXT-VALUE(sec-arc,integral),"99999999") + ".scr".
    OUTPUT STREAM strm-boleta TO VALUE ( P-archivo ) PAGED PAGE-SIZE 66.
END.
ELSE DO:
    OUTPUT STREAM strm-boleta TO PRINTER PAGED PAGE-SIZE 66.
    PUT STREAM strm-boleta CONTROL CHR(27) CHR(67) CHR(66) CHR(27) CHR(80). /* LETRA NORMAL */
END.

FOR EACH Tempo:
    DELETE Tempo.
END.


DEFINE VAR I AS INTEGER.

CASE R-seleccion:
WHEN 1 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL IN FRAME F-Msg = "Secci�n".
    GET FIRST br_pl-flg-m.
    DO WHILE AVAILABLE(PL-FLG-SEM):
        DISPLAY PL-FLG-SEM.seccion @ FILL-IN-Seccion WITH FRAME F-Msg.
        RUN busca_datos.
        GET NEXT br_pl-flg-m.
    END.
END.
WHEN 2 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL = "Secci�n".
    DO i = 1 TO br_pl-flg-m:NUM-SELECTED-ROWS IN FRAME F-Main:
        ASSIGN stat-reg  = br_pl-flg-m:FETCH-SELECTED-ROW(i).
        IF stat-reg THEN DO:
            DISPLAY PL-FLG-SEM.seccion @ FILL-IN-Seccion WITH FRAME F-Msg.
            RUN busca_datos.
        END.
    END.
    ASSIGN stat-reg = br_pl-flg-m:DESELECT-ROWS().
END.
WHEN 3 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL = "Secci�n".
    DISPLAY COMBO-S @ FILL-IN-Seccion WITH FRAME F-Msg.
    GET FIRST br_pl-flg-m.
    DO WHILE AVAILABLE(PL-FLG-SEM):
        IF PL-FLG-SEM.Seccion = COMBO-S THEN RUN busca_datos.
        GET NEXT br_pl-flg-m.
    END.
END.
WHEN 4 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL = "Proyecto".
    DISPLAY COMBO-S @ FILL-IN-Seccion WITH FRAME F-Msg.
    GET FIRST br_pl-flg-m.
    DO WHILE AVAILABLE(PL-FLG-SEM):
        IF PL-FLG-SEM.Proyecto = COMBO-S THEN RUN busca_datos.
        GET NEXT br_pl-flg-m.
    END.
END.

END CASE.

HIDE FRAME F-Msg.

OUTPUT STREAM strm-boleta CLOSE.

IF INPUT TGL-pantalla = TRUE THEN DO:
    RUN bin/_vcat.p ( P-archivo ). 
    OS-DELETE VALUE ( P-archivo ). 
END.
*/


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

    DISPLAY FILL-IN-NRO-SEM WITH FRAME F-Main.

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
  {src/adm/template/snd-list.i "integral.PL-FLG-SEM"}
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

