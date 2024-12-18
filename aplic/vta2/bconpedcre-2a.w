&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE PEDIDO LIKE FacCPedi.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

DEF VAR x-Situacion AS CHAR NO-UNDO.

DEF VAR x-Estado AS CHAR NO-UNDO.
DEF VAR x-Motivo AS CHAR NO-UNDO.

DEF VAR x-NroBultos AS INT NO-UNDO.
DEF VAR x-Peso AS DEC NO-UNDO.
DEF VAR x-Volumen AS DEC NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES FacCPedi
&Scoped-define FIRST-EXTERNAL-TABLE FacCPedi


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR FacCPedi.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES vtadtrkped TabGener DI-RutaC

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table vtadtrkped.CodDiv vtadtrkped.CodRef ~
vtadtrkped.NroRef vtadtrkped.FechaI vtadtrkped.FechaT TabGener.Descripcion ~
fFlgSit(vtadtrkped.FlgSit) @ x-Situacion DI-RutaC.NroDoc DI-RutaC.FchDoc ~
fEstado() @ x-Estado fEstadoDet() @ x-Motivo fNroBultos() @ x-NroBultos ~
fPeso() @ x-Peso fVolumen() @ x-Volumen 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH vtadtrkped WHERE vtadtrkped.CodCia = FacCPedi.CodCia ~
  AND vtadtrkped.CodDoc = FacCPedi.CodDoc ~
  AND vtadtrkped.NroPed = FacCPedi.NroPed NO-LOCK, ~
      FIRST TabGener WHERE TabGener.CodCia = vtadtrkped.CodCia ~
  AND TabGener.Codigo = vtadtrkped.CodUbic ~
      AND TabGener.Clave = "TRKPED" OUTER-JOIN NO-LOCK, ~
      FIRST DI-RutaC WHERE DI-RutaC.CodCia = vtadtrkped.CodCia ~
  AND DI-RutaC.CodDiv = vtadtrkped.CodDiv ~
  AND DI-RutaC.CodDoc = vtadtrkped.Libre_c01 ~
  AND DI-RutaC.NroDoc = vtadtrkped.Libre_c02 OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH vtadtrkped WHERE vtadtrkped.CodCia = FacCPedi.CodCia ~
  AND vtadtrkped.CodDoc = FacCPedi.CodDoc ~
  AND vtadtrkped.NroPed = FacCPedi.NroPed NO-LOCK, ~
      FIRST TabGener WHERE TabGener.CodCia = vtadtrkped.CodCia ~
  AND TabGener.Codigo = vtadtrkped.CodUbic ~
      AND TabGener.Clave = "TRKPED" OUTER-JOIN NO-LOCK, ~
      FIRST DI-RutaC WHERE DI-RutaC.CodCia = vtadtrkped.CodCia ~
  AND DI-RutaC.CodDiv = vtadtrkped.CodDiv ~
  AND DI-RutaC.CodDoc = vtadtrkped.Libre_c01 ~
  AND DI-RutaC.NroDoc = vtadtrkped.Libre_c02 OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table vtadtrkped TabGener DI-RutaC
&Scoped-define FIRST-TABLE-IN-QUERY-br_table vtadtrkped
&Scoped-define SECOND-TABLE-IN-QUERY-br_table TabGener
&Scoped-define THIRD-TABLE-IN-QUERY-br_table DI-RutaC


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEstado B-table-Win 
FUNCTION fEstado RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEstadoDet B-table-Win 
FUNCTION fEstadoDet RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFlgSit B-table-Win 
FUNCTION fFlgSit RETURNS CHARACTER
  ( INPUT pFlgSit AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNroBultos B-table-Win 
FUNCTION fNroBultos RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPeso B-table-Win 
FUNCTION fPeso RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fVolumen B-table-Win 
FUNCTION fVolumen RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      vtadtrkped, 
      TabGener, 
      DI-RutaC SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      vtadtrkped.CodDiv FORMAT "x(5)":U
      vtadtrkped.CodRef FORMAT "x(3)":U
      vtadtrkped.NroRef FORMAT "X(12)":U WIDTH 12.14
      vtadtrkped.FechaI COLUMN-LABEL "Inicio" FORMAT "99/99/9999 HH:MM":U
      vtadtrkped.FechaT COLUMN-LABEL "T�rmino" FORMAT "99/99/9999 HH:MM":U
      TabGener.Descripcion COLUMN-LABEL "Ubicacion" FORMAT "X(40)":U
      fFlgSit(vtadtrkped.FlgSit) @ x-Situacion COLUMN-LABEL "Situaci�n" FORMAT "x(20)":U
      DI-RutaC.NroDoc COLUMN-LABEL "Hoja de Ruta" FORMAT "X(9)":U
      DI-RutaC.FchDoc COLUMN-LABEL "Fecha de H/R" FORMAT "99/99/9999":U
      fEstado() @ x-Estado COLUMN-LABEL "Situacion" FORMAT "x(15)":U
      fEstadoDet() @ x-Motivo COLUMN-LABEL "Motivo" FORMAT "x(20)":U
      fNroBultos() @ x-NroBultos COLUMN-LABEL "No. Bultos"
      fPeso() @ x-Peso COLUMN-LABEL "Peso kg"
      fVolumen() @ x-Volumen COLUMN-LABEL "Vol. m3"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 144 BY 7.77
         FONT 4
         TITLE "TRACKING POR PEDIDOS".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1.04 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: INTEGRAL.FacCPedi
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: PEDIDO T "SHARED" ? INTEGRAL FacCPedi
   END-TABLES.
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
         HEIGHT             = 9.54
         WIDTH              = 144.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm-vm/method/vmbrowser.i}
{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.vtadtrkped WHERE INTEGRAL.FacCPedi ...,INTEGRAL.TabGener WHERE INTEGRAL.vtadtrkped ...,INTEGRAL.DI-RutaC WHERE INTEGRAL.vtadtrkped ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER, FIRST OUTER"
     _JoinCode[1]      = "INTEGRAL.vtadtrkped.CodCia = INTEGRAL.FacCPedi.CodCia
  AND INTEGRAL.vtadtrkped.CodDoc = INTEGRAL.FacCPedi.CodDoc
  AND INTEGRAL.vtadtrkped.NroPed = INTEGRAL.FacCPedi.NroPed"
     _JoinCode[2]      = "TabGener.CodCia = vtadtrkped.CodCia
  AND TabGener.Codigo = vtadtrkped.CodUbic"
     _Where[2]         = "TabGener.Clave = ""TRKPED"""
     _JoinCode[3]      = "INTEGRAL.DI-RutaC.CodCia = INTEGRAL.vtadtrkped.CodCia
  AND INTEGRAL.DI-RutaC.CodDiv = INTEGRAL.vtadtrkped.CodDiv
  AND INTEGRAL.DI-RutaC.CodDoc = INTEGRAL.vtadtrkped.Libre_c01
  AND INTEGRAL.DI-RutaC.NroDoc = INTEGRAL.vtadtrkped.Libre_c02"
     _FldNameList[1]   = INTEGRAL.vtadtrkped.CodDiv
     _FldNameList[2]   = INTEGRAL.vtadtrkped.CodRef
     _FldNameList[3]   > INTEGRAL.vtadtrkped.NroRef
"vtadtrkped.NroRef" ? "X(12)" "character" ? ? ? ? ? ? no ? no no "12.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.vtadtrkped.FechaI
"vtadtrkped.FechaI" "Inicio" "99/99/9999 HH:MM" "datetime" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.vtadtrkped.FechaT
"vtadtrkped.FechaT" "T�rmino" "99/99/9999 HH:MM" "datetime" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.TabGener.Descripcion
"TabGener.Descripcion" "Ubicacion" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"fFlgSit(vtadtrkped.FlgSit) @ x-Situacion" "Situaci�n" "x(20)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > INTEGRAL.DI-RutaC.NroDoc
"DI-RutaC.NroDoc" "Hoja de Ruta" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > INTEGRAL.DI-RutaC.FchDoc
"DI-RutaC.FchDoc" "Fecha de H/R" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"fEstado() @ x-Estado" "Situacion" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"fEstadoDet() @ x-Motivo" "Motivo" "x(20)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"fNroBultos() @ x-NroBultos" "No. Bultos" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"fPeso() @ x-Peso" "Peso kg" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"fVolumen() @ x-Volumen" "Vol. m3" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main /* TRACKING POR PEDIDOS */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* TRACKING POR PEDIDOS */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* TRACKING POR PEDIDOS */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
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
  {src/adm/template/row-list.i "FacCPedi"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "FacCPedi"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargar-data B-table-Win 
PROCEDURE cargar-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
EMPTY TEMP-TABLE tt-vtadtrkped.

FOR EACH INTEGRAL.vtadtrkped WHERE INTEGRAL.vtadtrkped.CodCia = INTEGRAL.FacCPedi.CodCia
            AND INTEGRAL.vtadtrkped.CodDoc = INTEGRAL.FacCPedi.CodDoc
            AND INTEGRAL.vtadtrkped.NroPed = INTEGRAL.FacCPedi.NroPed NO-LOCK:
    
    CREATE tt-vtadtrkped.
    BUFFER-COPY vtadtrkped TO tt-vtadtrkped.
END.

{&open-query-br_table-2}
*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-busca B-table-Win 
PROCEDURE local-busca :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OK-WAIT-STATE AS LOGICAL NO-UNDO.
  ASSIGN  input-var-1 = ""
          input-var-2 = ""
          input-var-3 = ""
          output-var-1 = ?
          OK-WAIT-STATE = SESSION:SET-WAIT-STATE("GENERAL").

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'busca':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
    /*RUN PL/C-XXX.W("").*/
    IF OUTPUT-VAR-1 <> ? THEN DO:
         FIND {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} WHERE
              ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = OUTPUT-VAR-1
              NO-LOCK NO-ERROR.
         IF AVAIL {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
            REPOSITION {&BROWSE-NAME}  TO ROWID OUTPUT-VAR-1.
         END.
    END.
  END.
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /*RUN cargar-data.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros B-table-Win 
PROCEDURE procesa-parametros :
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
        WHEN "" THEN.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros B-table-Win 
PROCEDURE recoge-parametros :
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
        WHEN "" THEN .
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
  {src/adm/template/snd-list.i "FacCPedi"}
  {src/adm/template/snd-list.i "vtadtrkped"}
  {src/adm/template/snd-list.i "TabGener"}
  {src/adm/template/snd-list.i "DI-RutaC"}

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

  IF p-state = 'update-begin':U THEN DO:
     RUN valida-update.
     IF RETURN-VALUE = "ADM-ERROR" THEN RETURN.
  END.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tracking-a-excel B-table-Win 
PROCEDURE tracking-a-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE lFileXls                 AS CHARACTER.
DEFINE VARIABLE lNuevoFile               AS LOG.
define VAR cValue as char.


lFileXls = "".          /* Nombre el archivo a abrir o crear, vacio es valido solo para nuevos */
lNuevoFile = YES.       /* YES : Si va crear un nuevo archivo o abrir */


{lib\excel-open-file.i}

chExcelApplication:Visible = NO.

lMensajeAlTerminar = NO. /*  */
lCerrarAlTerminar = NO. /* Si permanece abierto el Excel luego de concluir el proceso */
/*cColList - Array Columnas (A,B,C...AA,AB,AC...) */

chWorkSheet = chExcelApplication:Sheets:Item(1).  
iRow = 1.

SESSION:SET-WAIT-STATE('GENERAL').

DEFINE BUFFER b-pedido FOR pedido.
DEFINE BUFFER b-faccpedi FOR faccpedi.
DEFINE BUFFER b-vtadtrkped FOR vtadtrkped.
DEFINE BUFFER b-tabgener FOR tabgener.
DEFINE BUFFER b-di-rutaC FOR di-rutaC.

/* Cabecera */
cRange = "A" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Origen".
cRange = "B" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Lista**".
cRange = "C" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Numero".
cRange = "D" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "PDD".
cRange = "E" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Emision".
cRange = "F" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Vcto".
cRange = "G" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Entrega".
cRange = "H" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "O/D".
cRange = "I" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Cliente".
cRange = "J" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Nombre / Razon Social".
cRange = "K" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Vendedor".
cRange = "L" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Importe".
cRange = "M" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Estado".
cRange = "N" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "% Avance".
cRange = "O" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Total Items".
cRange = "P" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Total Peso Kgrs".
cRange = "Q" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Imp. Atendido".
cRange = "R" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Imp x Atender".

/* Detalle */
cRange = "S" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Division".
cRange = "T" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Codigo".
cRange = "U" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Numero".
cRange = "V" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Inicion".
cRange = "W" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Termino".
cRange = "X" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Ubicacion".
cRange = "Y" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Situacion".
cRange = "Z" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Hoja de Ruta".
cRange = "AA" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Fecha H/R".
cRange = "AB" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Situacion".
cRange = "AC" + TRIM(STRING(iRow)).
chWorkSheet:Range(cRange):VALUE = "Motivos".

iRow = iRow + 1.


/* Los Pedidos */ 
FOR EACH b-PEDIDO NO-LOCK,
      FIRST b-FacCPedi OF b-PEDIDO NO-LOCK :

    /* Tracking */
    FOR EACH b-vtadtrkped WHERE b-vtadtrkped.CodCia = b-FacCPedi.CodCia
                                AND b-vtadtrkped.CodDoc = b-FacCPedi.CodDoc
                                AND b-vtadtrkped.NroPed = b-FacCPedi.NroPed NO-LOCK :
        /* Cabecera */
        cRange = "A" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-faccpedi.coddiv.
        cRange = "B" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-pedido.libre_c01.
        cRange = "C" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-faccpedi.nroped.
        cRange = "D" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-faccpedi.codalm.
        cRange = "E" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-faccpedi.fchped.
        cRange = "F" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-faccpedi.fchven.
        cRange = "G" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-faccpedi.fchent.
        cRange = "H" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-pedido.ncmpbnte.
        cRange = "I" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-faccpedi.codcli.
        cRange = "J" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-faccpedi.nomcli.
        cRange = "K" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-faccpedi.codven.
        cRange = "L" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-faccpedi.imptot.
        cRange = "M" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-pedido.libre_c02.
        cRange = "N" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-pedido.libre_d01.
        cRange = "O" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-pedido.acubon[2].
        cRange = "P" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-pedido.acubon[3].
        cRange = "Q" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-pedido.acubon[4].
        cRange = "R" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-pedido.acubon[5].


        FIND FIRST b-TabGener WHERE b-TabGener.CodCia = b-vtadtrkped.CodCia
                            AND b-TabGener.Codigo = b-vtadtrkped.CodUbic
                            AND b-TabGener.Clave = "TRKPED" NO-LOCK NO-ERROR.

        FIND FIRST b-DI-RutaC WHERE b-DI-RutaC.CodCia = b-vtadtrkped.CodCia
                            AND b-DI-RutaC.CodDiv = b-vtadtrkped.CodDiv
                            AND b-DI-RutaC.CodDoc = b-vtadtrkped.Libre_c01
                            AND b-DI-RutaC.NroDoc = b-vtadtrkped.Libre_c02 NO-LOCK NO-ERROR.
        /* Detalle */
        cRange = "S" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-vtadtrkped.coddiv.
        cRange = "T" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-vtadtrkped.codref.
        cRange = "U" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + b-vtadtrkped.nroref.
        cRange = "V" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-vtadtrkped.fechaI.
        cRange = "W" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = b-vtadtrkped.fechaT.
        cRange = "X" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = if(AVAILABLE b-tabgener) THEN b-tabgener.descripcion ELSE "".
        cRange = "Y" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = fFlgSit(b-vtadtrkped.flgsit).
        cRange = "Z" + TRIM(STRING(iRow)).
        chWorkSheet:Range(cRange):VALUE = "'" + (if(AVAILABLE b-di-rutac) THEN b-di-rutac.nrodoc ELSE "").
        cRange = "AA" + TRIM(STRING(iRow)).
        IF AVAILABLE b-di-rutac THEN DO:
            chWorkSheet:Range(cRange):VALUE =  b-di-rutac.fchdoc.
            cValue = "".
            RUN alm/f-flgrut ("D", b-vtadtrkped.Libre_c03, OUTPUT cValue).

            cRange = "AB" + TRIM(STRING(iRow)).
            chWorkSheet:Range(cRange):VALUE = "'" + cValue.

            cRange = "AC" + TRIM(STRING(iRow)).
            FIND AlmTabla WHERE AlmTabla.Tabla = 'HR'
                AND AlmTabla.Codigo = b-vtadtrkped.Libre_c04
                AND almtabla.NomAnt = 'N'
                NO-LOCK NO-ERROR.
            IF AVAILABLE AlmTabla THEN chWorkSheet:Range(cRange):VALUE = "'" + almtabla.Nombre.

        END.
        iRow = iRow + 1.
    END.
END.
chExcelApplication:Visible = TRUE.
{lib\excel-close-file.i} 

SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida B-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update B-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEstado B-table-Win 
FUNCTION fEstado RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF AVAILABLE Di-RutaC THEN DO:
      RUN alm/f-flgrut ("D", vtadtrkped.Libre_c03, OUTPUT x-Estado).
      RETURN x-Estado.
/*       FIND Di-RutaD OF Di-RutaC WHERE Di-RutaD.codref = vtadtrkped.CodRef */
/*           AND Di-RutaD.nroref = vtadtrkped.NroRef                         */
/*           NO-LOCK NO-ERROR.                                               */
/*       IF AVAILABLE Di-RutaD THEN DO:                                      */
/*           RUN alm/f-flgrut ("D", Di-RutaD.flgest, OUTPUT x-Estado).       */
/*           RETURN x-Estado.                                                */
/*       END.                                                                */
  END.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEstadoDet B-table-Win 
FUNCTION fEstadoDet RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

IF AVAILABLE Di-RutaC THEN DO:
    FIND AlmTabla WHERE AlmTabla.Tabla = 'HR'
        AND AlmTabla.Codigo = vtadtrkped.Libre_c04
        AND almtabla.NomAnt = 'N'
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmTabla THEN RETURN almtabla.Nombre.
/*     FIND Di-RutaD OF Di-RutaC WHERE Di-RutaD.codref = vtadtrkped.CodRef */
/*         AND Di-RutaD.nroref = vtadtrkped.NroRef                         */
/*         NO-LOCK NO-ERROR.                                               */
/*     IF AVAILABLE Di-RutaD THEN DO:                                      */
/*         FIND AlmTabla WHERE AlmTabla.Tabla = 'HR'                       */
/*             AND AlmTabla.Codigo = DI-RutaD.FlgEstDet                    */
/*             AND almtabla.NomAnt = 'N'                                   */
/*             NO-LOCK NO-ERROR.                                           */
/*         IF AVAILABLE AlmTabla THEN RETURN almtabla.Nombre.              */
/*     END.                                                                */
END.
RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFlgSit B-table-Win 
FUNCTION fFlgSit RETURNS CHARACTER
  ( INPUT pFlgSit AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR pEstado AS CHAR NO-UNDO.

RUN gn/fFlgSitTracking(pFlgSIt, OUTPUT pEstado).

RETURN pEstado.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNroBultos B-table-Win 
FUNCTION fNroBultos RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEF VAR pParam AS INT NO-UNDO.

  FOR EACH Ccbcbult NO-LOCK WHERE CcbCBult.CodCia = vtadtrkped.CodCia
      AND CcbCBult.CodDoc = vtadtrkped.CodRef 
      AND CcbCBult.NroDoc = vtadtrkped.NroRef :
      pParam = pParam + CcbCBult.Bultos.
  END.
  RETURN pParam.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPeso B-table-Win 
FUNCTION fPeso RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pParam AS DEC NO-UNDO.

  FOR EACH Facdpedi NO-LOCK WHERE FacDPedi.CodCia = vtadtrkped.CodCia
      AND FacDPedi.CodDoc = vtadtrkped.CodRef
      AND FacDPedi.NroPed = vtadtrkped.NroRef,
      FIRST Almmmatg OF Facdpedi NO-LOCK:
      pParam = pParam + (Facdpedi.canped * Facdpedi.factor * Almmmatg.Pesmat).
  END.
  RETURN pParam.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fVolumen B-table-Win 
FUNCTION fVolumen RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pParam AS DEC NO-UNDO.

  FOR EACH Facdpedi NO-LOCK WHERE FacDPedi.CodCia = vtadtrkped.CodCia
      AND FacDPedi.CodDoc = vtadtrkped.CodRef
      AND FacDPedi.NroPed = vtadtrkped.NroRef,
      FIRST Almmmatg OF Facdpedi NO-LOCK:
      pParam = pParam + (Facdpedi.canped * Facdpedi.factor * Almmmatg.Libre_d02).
  END.
  RETURN (pParam / 1000000).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

