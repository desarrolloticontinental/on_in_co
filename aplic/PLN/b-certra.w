&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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

DEFINE VARIABLE stat-reg  AS LOGICAL NO-UNDO.

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

DEFINE  TEMP-TABLE X-Tempo 
    FIELD Codper   LIKE Pl-pers.codper
    FIELD PatPer   LIKE pl-pers.Patper
    FIELD MatPer   LIKE pl-pers.matper
    FIELD Nomper   LIKE pl-pers.Nomper
    FIELD Seccion  LIKE pl-flg-mes.seccion
    FIELD Proyecto LIKE pl-flg-mes.proyecto
    FIELD Cargos   LIKE pl-flg-mes.cargos
    FIELD ccosto   LIKE pl-flg-mes.ccosto
    FIELD Vcontr LIKE pl-flg-mes.vcontr
    FIELD fecing LIKE pl-flg-mes.fecing
    INDEX llave01 Vcontr DESCENDING.
    


DEFINE  VAr I AS INTEGER.
DEF VAR Word AS COM-HANDLE.

RUN Carga-Datos.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES x-tempo

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_pl-flg-m                                   */
&Scoped-define FIELDS-IN-QUERY-br_pl-flg-m x-tempo.codper x-tempo.patper x-tempo.matper x-tempo.nomper x-tempo.vcontr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_pl-flg-m   
&Scoped-define SELF-NAME br_pl-flg-m
&Scoped-define QUERY-STRING-br_pl-flg-m FOR EACH x-tempo BY x-tempo.codper
&Scoped-define OPEN-QUERY-br_pl-flg-m OPEN QUERY {&SELF-NAME} FOR EACH x-tempo BY x-tempo.codper.
&Scoped-define TABLES-IN-QUERY-br_pl-flg-m x-tempo
&Scoped-define FIRST-TABLE-IN-QUERY-br_pl-flg-m x-tempo


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-28 RECT-2 FILL-IN-Copias br_pl-flg-m ~
R-seleccion F-Desde F-Hasta B-aceptar FILL-IN-msg 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Copias R-seleccion F-Desde F-Hasta ~
FILL-IN-msg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS>
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-aceptar 
     IMAGE-UP FILE "img/b-ok":U
     LABEL "&Aceptar" 
     SIZE 10.72 BY 1.54.

DEFINE VARIABLE F-Desde AS DATE FORMAT "99/99/9999":U 
     LABEL "De" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69 NO-UNDO.

DEFINE VARIABLE F-Hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-Copias AS INTEGER FORMAT "99":U INITIAL 1 
     LABEL "Copias" 
     VIEW-AS FILL-IN 
     SIZE 3.29 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-msg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mensaje" 
     VIEW-AS FILL-IN 
     SIZE 43.29 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE R-seleccion AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todo el personal", 1,
"Selectivo", 2,
"Por Cese ", 3
     SIZE 14.14 BY 1.69 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79.72 BY 9.5.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.86 BY 2.31.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_pl-flg-m FOR 
      x-tempo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_pl-flg-m
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_pl-flg-m B-table-Win _FREEFORM
  QUERY br_pl-flg-m NO-LOCK DISPLAY
      x-tempo.codper COLUMN-LABEL "Codigo" FORMAT "X(8)"
  x-tempo.patper COLUMN-LABEL "Apellido Paterno" FORMAT "X(15)"
  x-tempo.matper COLUMN-LABEL "Apellido Materno" FORMAT "X(15)"
  x-tempo.nomper COLUMN-LABEL "Nombres " FORMAT "X(20)"
  x-tempo.vcontr COLUMN-LABEL "Fecha Cese  " FORMAT "99/99/9999"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 57.57 BY 7.92
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-Copias AT ROW 1.23 COL 16.57 COLON-ALIGNED
     br_pl-flg-m AT ROW 1.27 COL 23.14
     R-seleccion AT ROW 2.38 COL 5.57 NO-LABEL
     F-Desde AT ROW 4.38 COL 5.72 COLON-ALIGNED
     F-Hasta AT ROW 5.38 COL 5.72 COLON-ALIGNED
     B-aceptar AT ROW 6.54 COL 7.14
     FILL-IN-msg AT ROW 9.42 COL 33.29 COLON-ALIGNED
     RECT-28 AT ROW 4.15 COL 3.86
     RECT-2 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
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
         HEIGHT             = 9.65
         WIDTH              = 79.72.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_pl-flg-m FILL-IN-Copias F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_pl-flg-m
/* Query rebuild information for BROWSE br_pl-flg-m
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH x-tempo BY x-tempo.codper.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
*/  /* BROWSE br_pl-flg-m */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-aceptar B-table-Win
ON CHOOSE OF B-aceptar IN FRAME F-Main /* Aceptar */
DO:
    ASSIGN R-seleccion f-desde f-hasta FILL-IN-Copias FILL-IN-msg.
    IF FILL-IN-Copias = 0 THEN FILL-IN-Copias = 1.
    RUN genera-certificado.
    MESSAGE "Proceso Terminado"
            VIEW-AS ALERT-BOX INFORMATION.
            
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


&Scoped-define SELF-NAME R-seleccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-seleccion B-table-Win
ON VALUE-CHANGED OF R-seleccion IN FRAME F-Main
DO:
    CASE INPUT R-seleccion:
    WHEN 1 THEN
        ASSIGN
            Br_pl-flg-m:SENSITIVE = FALSE
            F-DESDE:SENSITIVE     = FALSE
            F-HASTA:SENSITIVE     = FALSE.
    WHEN 2 THEN
        ASSIGN
            Br_pl-flg-m:SENSITIVE = TRUE
            F-DESDE:SENSITIVE     = FALSE
            F-HASTA:SENSITIVE     = FALSE.
    WHEN 3 THEN DO:
        ASSIGN
            Br_pl-flg-m:SENSITIVE = FALSE
            F-DESDE:SENSITIVE     = TRUE
            F-HASTA:SENSITIVE     = TRUE.
            F-DESDE = TODAY.
            F-HASTA = TODAY.
            DISPLAY F-DESDE F-HASTA  WITH FRAME F-Main.
 
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Datos B-table-Win 
PROCEDURE Carga-Datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*****************Empleados**************/
for each pl-flg-mes where pl-flg-mes.codcia = S-CODCIA and
                    YEAR(pl-flg-mes.vcontr) = S-PERIODO no-lock 
                       break by pl-flg-mes.codper
                             by pl-flg-mes.vcontr :
 if last-of(pl-flg-mes.codper) then do:
    FIND pl-pers WHERE pl-pers.codcia = S-CODCIA AND
                       pl-pers.codper = pl-flg-mes.codper
                       NO-LOCK NO-ERROR.
    IF NOT AVAILABLE pl-pers THEN NEXT.
    FIND x-tempo WHERE x-tempo.codper = pl-flg-mes.codper
         NO-ERROR.
    IF NOT AVAILABLE x-tempo THEN DO:        
        create x-tempo.
        ASSIGN
        x-tempo.Codper = pl-flg-mes.codper
        x-tempo.PatPer   = pl-pers.Patper
        x-tempo.MatPer   = pl-pers.matper
        x-tempo.Nomper   = pl-pers.Nomper
        x-tempo.Seccion  = pl-flg-mes.seccion
        x-tempo.Cargos   = pl-flg-mes.cargos
        x-tempo.Ccosto    = pl-flg-mes.ccosto
        x-tempo.Proyecto = pl-flg-mes.proyecto
        x-tempo.Fecing   = pl-flg-mes.fecing
        x-tempo.Vcontr   = pl-flg-mes.vcontr.
    END.
    
 end.
 
end.

/**************Obreros********************/
for each pl-flg-sem where pl-flg-sem.codcia = S-CODCIA and
                    YEAR(pl-flg-sem.vcontr) = S-PERIODO no-lock 
                       break by pl-flg-sem.codper
                             by pl-flg-sem.vcontr:
 if last-of(pl-flg-sem.codper) then do:
    FIND pl-pers WHERE pl-pers.codcia = S-CODCIA AND
                       pl-pers.codper = pl-flg-sem.codper
                       NO-LOCK NO-ERROR.
    IF NOT AVAILABLE pl-pers THEN NEXT.
    FIND x-tempo WHERE x-tempo.codper = pl-flg-sem.codper
         NO-ERROR.
    IF NOT AVAILABLE x-tempo THEN DO:        
        create x-tempo.
        ASSIGN
        x-tempo.Codper = pl-flg-sem.codper
        x-tempo.PatPer   = pl-pers.Patper
        x-tempo.MatPer   = pl-pers.matper
        x-tempo.Nomper   = pl-pers.Nomper
        x-tempo.Seccion  = pl-flg-sem.seccion
        x-tempo.Cargos   = pl-flg-sem.cargos
        x-tempo.Ccosto    = pl-flg-sem.ccosto
        x-tempo.Proyecto = pl-flg-sem.proyecto
        x-tempo.Fecing   = pl-flg-sem.fecing
        x-tempo.Vcontr   = pl-flg-sem.vcontr.
    END.
    
 end.
 
end.


{&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Certificado B-table-Win 
PROCEDURE Certificado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* 22/02/2024: Sugerencia de Soporte T�cnico */
DEF VAR x-Ruta AS CHAR NO-UNDO.

x-Ruta = "\\192.168.100.251\contratos\".

FIND FIRST VtaTabla WHERE Vtatabla.codcia = s-codcia AND
    Vtatabla.tabla = "CFG_RRHH_Certificado_Trabajo"
    NO-LOCK NO-ERROR.
IF AVAILABLE VtaTabla AND VtaTabla.Libre_c01 > "" THEN DO:
    x-Ruta = TRIM(VtaTabla.Libre_c01).
    /* Debe terminar en \ */
    IF R-INDEX(x-Ruta,'\') <> LENGTH(x-Ruta) THEN x-Ruta = x-Ruta + "\".
END.

/* Buscamos la plantilla dentro de los programas */
DEF VAR cNomPlantilla   AS CHAR NO-UNDO.
DEF VAR cPlantilla      AS CHAR NO-UNDO.

ASSIGN
    cNomPlantilla = "certraconti01.dot".

IF AVAILABLE VtaTabla AND VtaTabla.Libre_c03 > "" THEN DO:
   cNomPlantilla = TRIM(VtaTabla.Libre_c03).
END.

ASSIGN
    cPlantilla = SEARCH(x-Ruta + cNomPlantilla).

IF cPlantilla = ? THEN DO:
    MESSAGE 'No se encontr� la plantilla' cNomPlantilla 'en la carpeta' x-Ruta VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

IF x-tempo.vcontr = ? THEN RETURN.
DO:

DEF VAR x_archivo AS CHAR.
DEF VAR x_edad AS INTEGER.
DEF VAR x_codmov AS INTEGER INIT 101.
DEF VAR x-enletras AS CHAR.
DEF VAR x_codmon AS INTEGER INIT 1.
DEF VAR x-fecini AS DATE INIT 01/01/2003.
DEF VAR x-fecfin AS DATE INIT 12/31/2003.
DEF VAR x_vigcon AS CHAR .
DEF VAR x_fecini AS CHAR .
DEF VAR x_fecfin AS CHAR .
DEF VAR x_fecemi AS CHAR .
DEF VAR x_sueper AS CHAR .
DEf VAR x_secper AS CHAR .
DEF VAR x-ccosto AS CHAR.
DEF VAR I AS INTEGER.

x-fecini = x-tempo.fecing.
x-fecfin = x-tempo.vcontr.
x-ccosto = "".

DEF VAR x_meses AS CHAR INIT "Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Septiembre,Octubre,Noviembre,Diciembre".

CREATE "Word.Application" Word.

FIND Pl-pers WHERE Pl-Pers.Codper = x-tempo.codper
     NO-LOCK NO-ERROR.    

FIND cb-auxi WHERE cb-auxi.CodCia = cb-codcia and 
                   cb-auxi.CLFAUX = "CCO" and
                   cb-auxi.CodAUX = x-tempo.ccosto
                   no-lock no-error.
IF AVAILABLE cb-auxi THEN  x-ccosto = cb-auxi.nomaux.

x_fecini = STRING(DAY(x-fecini)) + " de " + ENTRY(MONTH(x-fecini),x_meses) + " del " + STRING(YEAR(x-fecini)).
x_fecfin = STRING(DAY(x-fecfin)) + " de " + ENTRY(MONTH(x-fecfin),x_meses) + " del " + STRING(YEAR(x-fecfin)).
x_fecemi = STRING(DAY(x-fecfin)) + " de " + ENTRY(MONTH(x-fecfin),x_meses) + " del " + STRING(YEAR(x-fecfin)).

/*Word:Documents:Add(cPlantilla).*/
Word:Documents:OPEN(cPlantilla).


RUN Remplazo(INPUT "NOMPER", INPUT (trim(pl-pers.nomper) + " " + trim(pl-pers.patper) + " " + trim(pl-pers.matper)), INPUT 1).     
RUN Remplazo(INPUT "CARGO" ,INPUT (x-tempo.cargos), INPUT 0).
RUN Remplazo(INPUT "CCOSTO" ,INPUT (x-ccosto), INPUT 0).
RUN Remplazo(INPUT "FECINI" ,INPUT (x_fecini), INPUT 0).
RUN Remplazo(INPUT "FECFIN" ,INPUT (x_fecfin), INPUT 0).
RUN Remplazo(INPUT "FECEMI" ,INPUT (x_fecemi), INPUT 0).

x-Ruta = "d:\rrhh".
IF AVAILABLE VtaTabla AND VtaTabla.Libre_c02 > "" THEN DO:
    x-Ruta = TRIM(VtaTabla.Libre_c02).
    /* Debe terminar en \ */
    IF R-INDEX(x-Ruta,'\') <> LENGTH(x-Ruta) THEN x-Ruta = x-Ruta + "\".
END.
x_archivo = x-Ruta + "CertificadoCONTI" + pl-pers.codper.
/*Word:ChangeFileOpenDirectory(x-Ruta).*/
Word:ActiveDocument:SaveAs(x_archivo).
Word:Quit().

RELEASE OBJECT Word NO-ERROR.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genera-certificado B-table-Win 
PROCEDURE genera-certificado :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE p-archivo AS CHARACTER.

/* Direccionamiento del STREAM */


DEFINE VAR I AS INTEGER.

CASE R-seleccion:
WHEN 1 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL IN FRAME F-Msg = "Seccion".
    GET FIRST br_pl-flg-m.
    DO WHILE AVAILABLE(x-tempo):
        DISPLAY x-tempo.seccion @ FILL-IN-Seccion 
                x-tempo.Codper @ FILL-IN-Codigo 
        WITH FRAME F-Msg.
        RUN certificado.
        GET NEXT br_pl-flg-m.
    END.
END.
WHEN 2 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL = "Secci�n".
    DO i = 1 TO br_pl-flg-m:NUM-SELECTED-ROWS IN FRAME F-Main:
        ASSIGN stat-reg  = br_pl-flg-m:FETCH-SELECTED-ROW(i).
        IF stat-reg THEN DO:
            DISPLAY x-tempo.seccion @ FILL-IN-Seccion 
                    x-tempo.Codper @ FILL-IN-Codigo 
            WITH FRAME F-Msg.
            RUN certificado.
        END.
    END.
    ASSIGN stat-reg = br_pl-flg-m:DESELECT-ROWS().
END.
WHEN 3 THEN DO:
    ASSIGN FILL-IN-Seccion:LABEL = "Secci�n".
    GET FIRST br_pl-flg-m.
    DO WHILE AVAILABLE(x-tempo):
        IF x-tempo.vcontr >= F-DESDE AND
           x-tempo.vcontr <= F-HASTA THEN DO:
           DISPLAY x-tempo.seccion @ FILL-IN-Seccion
                   x-tempo.Codper @ FILL-IN-Codigo 
           WITH FRAME F-Msg.         
           RUN certificado.           
        END.
        GET NEXT br_pl-flg-m.   
    END.
END.

END CASE.

HIDE FRAME F-Msg.

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
  APPLY "VALUE-CHANGED" TO R-seleccion IN FRAME F-Main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Remplazo B-table-Win 
PROCEDURE Remplazo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER campo AS CHARACTER.
DEFINE INPUT PARAMETER registro AS CHARACTER.
DEFINE INPUT PARAMETER mayuscula AS LOGICAL.
DEFINE VAR cBuffer AS CHARACTER.

Word:Selection:Goto(-1 BY-VARIANT-POINTER,,,campo BY-VARIANT-POINTER).
Word:Selection:Select().
IF mayuscula = TRUE THEN cBuffer = CAPS(registro).
ELSE cBuffer = registro.
Word:Selection:Typetext(cBuffer).

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
  {src/adm/template/snd-list.i "x-tempo"}

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

