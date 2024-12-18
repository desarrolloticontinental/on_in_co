&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tgre_cmpte NO-UNDO LIKE gre_cmpte
       field trowid as rowid
       field fchemipgre as date
       field nropgre as int.



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

/*

    ESTE PROGRAMA TBM ES INVOCADO DESDE LA CONSULTA DE COMPROBANTES SIN PGRE

*/

/* Parameters Definitions ---                                           */
DEFINE SHARED VAR s-acceso-total  AS LOG.   /* Control GRE */

/* Local Variable Definitions ---                                       */

DEFINE SHARED VAR s-coddiv AS CHAR.
DEFINE SHARED VAR s-codcia AS INT.
DEFINE SHARED VAR lh_handle AS HANDLE.

DEFINE BUFFER b-gre_cmpte FOR gre_cmpte.
DEFINE BUFFER b-gre_header FOR gre_header.
DEFINE BUFFER b-gre_detail FOR gre_detail.
DEFINE BUFFER b-ccbcdocu FOR ccbcdocu.
DEFINE BUFFER x-ccbcdocu FOR ccbcdocu.
DEFINE BUFFER b-ccbddocu FOR ccbddocu.

define var x-sort-column-current as char.

DEFINE VAR iRowsSelecteds AS INT.

DEFINE SHARED VARIABLE s-numero     AS INTEGER.

DEFINE VAR lGRE_ONLINE AS LOG.
DEFINE VAR cPHR AS CHAR.

/* Para la exportacion a Excel */
DEFINE TEMP-TABLE tmpTable
    FIELD coddivvta    AS  CHAR    FORMAT 'x(8)'   COLUMN-LABEL "Cod.div.vta"
    FIELD nomdivvta    AS  CHAR    FORMAT 'x(80)'   COLUMN-LABEL "Nombre div. vta"
    FIELD nropedcom    AS  CHAR    FORMAT 'x(15)'   COLUMN-LABEL "Nro. Pedido Comercial"
    FIELD nropedlog    AS  CHAR    FORMAT 'x(15)'   COLUMN-LABEL "Nro. Pedido Logistico"
    FIELD codorddsp    AS  CHAR    FORMAT 'x(5)'   COLUMN-LABEL "Cod. Orden"
    FIELD nroorddsp    AS  CHAR    FORMAT 'x(15)'   COLUMN-LABEL "Nro. Orden"
    FIELD fchorddsp     AS DATE     COLUMN-LABEL "Fecha emision O/D"
    FIELD codcmpte     AS  CHAR    FORMAT 'x(5)'   COLUMN-LABEL "Cod.Comprobante"
    FIELD nrocmpte     AS  CHAR    FORMAT 'x(15)'   COLUMN-LABEL "Nro.Comprobante"
    FIELD fechaemi     AS  DATE       COLUMN-LABEL "Fecha emision cmpte"
    FIELD codclie    AS  CHAR    FORMAT 'x(15)'   COLUMN-LABEL "Cod.Cliente"
    FIELD nomclie    AS  CHAR    FORMAT 'x(80)'   COLUMN-LABEL "Nombre del cliente"
    FIELD monevta    AS  CHAR    FORMAT 'x(5)'   COLUMN-LABEL "Moneda Vta"
    FIELD impte    AS  DEC    FORMAT '->>,>>>,>>9.99'   COLUMN-LABEL "Importe"
    FIELD coddivdsp    AS  CHAR    FORMAT 'x(5)'   COLUMN-LABEL "Cod.div. espacho"
    FIELD nomdivdsp    AS  CHAR    FORMAT 'x(80)'   COLUMN-LABEL "Nombre div. despacho"
    FIELD nropgre    AS  INT     COLUMN-LABEL "Nro. de PGRE"
    FIELD fchpgre    AS  DATE     COLUMN-LABEL "Fecha de PGRE"
    INDEX idx01 nroorddsp fchorddsp nomdivdsp
    INDEX idx02 codcmpte nrocmpte
    .

DEFINE VAR cFile AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tgre_cmpte CcbCDocu

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tgre_cmpte.coddoc tgre_cmpte.nrodoc ~
CcbCDocu.Libre_c01 CcbCDocu.Libre_c02 tgre_cmpte.fechahorareg ~
CcbCDocu.CodCli CcbCDocu.NomCli tgre_cmpte.coddivvta CcbCDocu.CodPed ~
CcbCDocu.NroPed tgre_cmpte.estado_sunat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3 
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tgre_cmpte ~
      WHERE tgre_cmpte.estado = 'CMPTE GENERADO' NO-LOCK, ~
      EACH CcbCDocu WHERE ccbcdocu.codcia = s-codcia and ~
ccbcdocu.coddoc = tgre_cmpte.coddoc and ~
ccbcdocu.nrodoc = tgre_cmpte.nrodoc NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY BROWSE-3 FOR EACH tgre_cmpte ~
      WHERE tgre_cmpte.estado = 'CMPTE GENERADO' NO-LOCK, ~
      EACH CcbCDocu WHERE ccbcdocu.codcia = s-codcia and ~
ccbcdocu.coddoc = tgre_cmpte.coddoc and ~
ccbcdocu.nrodoc = tgre_cmpte.nrodoc NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tgre_cmpte CcbCDocu
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tgre_cmpte
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-3 CcbCDocu


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-refrescar FILL-IN-fecha FILL-IN-dias ~
FILL-IN-phr BROWSE-3 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-fecha FILL-IN-dias FILL-IN-phr ~
FILL-IN-titulo 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-get-utf8 B-table-Win 
FUNCTION f-get-utf8 RETURNS CHARACTER
  ( INPUT pString AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-refrescar 
     LABEL "Refrescar data" 
     SIZE 15 BY .88.

DEFINE VARIABLE FILL-IN-dias AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Dias hacia atras" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha emision comprobante" 
     VIEW-AS FILL-IN 
     SIZE 12.29 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-phr AS CHARACTER FORMAT "X(15)":U 
     LABEL "# PHR" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-titulo AS CHARACTER FORMAT "X(256)":U INITIAL "COMPROBANTES ELECTRONICOS EMITIDOS - SIN PGRE/GRE" 
      VIEW-AS TEXT 
     SIZE 59.57 BY .85
     FGCOLOR 4 FONT 11 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      tgre_cmpte, 
      CcbCDocu SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 B-table-Win _STRUCTURED
  QUERY BROWSE-3 NO-LOCK DISPLAY
      tgre_cmpte.coddoc COLUMN-LABEL "Cod!Doc" FORMAT "x(5)":U
            WIDTH 4
      tgre_cmpte.nrodoc COLUMN-LABEL "Numero!Doc" FORMAT "x(15)":U
            WIDTH 10.57
      CcbCDocu.Libre_c01 COLUMN-LABEL "O/D" FORMAT "x(5)":U
      CcbCDocu.Libre_c02 COLUMN-LABEL "Nro.!O/D" FORMAT "x(15)":U
            WIDTH 10.86
      tgre_cmpte.fechahorareg COLUMN-LABEL "Fecha/Hora!Emision" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 14.43
      CcbCDocu.CodCli COLUMN-LABEL "Codigo!Cliente" FORMAT "x(11)":U
            WIDTH 12.43
      CcbCDocu.NomCli COLUMN-LABEL "Nombre!Cliente" FORMAT "x(50)":U
            WIDTH 30.43
      tgre_cmpte.coddivvta COLUMN-LABEL "Punto!Venta" FORMAT "x(6)":U
            WIDTH 7.43
      CcbCDocu.CodPed COLUMN-LABEL "PED" FORMAT "x(10)":U WIDTH 5.43
      CcbCDocu.NroPed COLUMN-LABEL "Nro.!Pedido" FORMAT "X(12)":U
      tgre_cmpte.estado_sunat COLUMN-LABEL "Sunat" FORMAT "x(50)":U
            WIDTH 33.57
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 117.57 BY 17.77
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-refrescar AT ROW 2.04 COL 99 WIDGET-ID 12
     FILL-IN-fecha AT ROW 2.08 COL 26.72 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-dias AT ROW 2.08 COL 58 COLON-ALIGNED WIDGET-ID 24
     FILL-IN-phr AT ROW 2.08 COL 71.29 COLON-ALIGNED WIDGET-ID 20
     BROWSE-3 AT ROW 3.15 COL 1.57 WIDGET-ID 200
     FILL-IN-titulo AT ROW 1.12 COL 37 NO-LABEL WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 3 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tgre_cmpte T "?" NO-UNDO INTEGRAL gre_cmpte
      ADDITIONAL-FIELDS:
          field trowid as rowid
          field fchemipgre as date
          field nropgre as int
      END-FIELDS.
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
         HEIGHT             = 20.42
         WIDTH              = 119.
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
/* BROWSE-TAB BROWSE-3 FILL-IN-phr F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-3:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-titulo IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _TblList          = "Temp-Tables.tgre_cmpte,INTEGRAL.CcbCDocu WHERE Temp-Tables.tgre_cmpte ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "tgre_cmpte.estado = 'CMPTE GENERADO'"
     _JoinCode[2]      = "ccbcdocu.codcia = s-codcia and
ccbcdocu.coddoc = tgre_cmpte.coddoc and
ccbcdocu.nrodoc = tgre_cmpte.nrodoc"
     _FldNameList[1]   > Temp-Tables.tgre_cmpte.coddoc
"tgre_cmpte.coddoc" "Cod!Doc" ? "character" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tgre_cmpte.nrodoc
"tgre_cmpte.nrodoc" "Numero!Doc" ? "character" ? ? ? ? ? ? no ? no no "10.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.CcbCDocu.Libre_c01
"CcbCDocu.Libre_c01" "O/D" "x(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.CcbCDocu.Libre_c02
"CcbCDocu.Libre_c02" "Nro.!O/D" "x(15)" "character" ? ? ? ? ? ? no ? no no "10.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tgre_cmpte.fechahorareg
"tgre_cmpte.fechahorareg" "Fecha/Hora!Emision" "99/99/9999 HH:MM:SS" "datetime" ? ? ? ? ? ? no ? no no "14.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.CcbCDocu.CodCli
"CcbCDocu.CodCli" "Codigo!Cliente" ? "character" ? ? ? ? ? ? no ? no no "12.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > INTEGRAL.CcbCDocu.NomCli
"CcbCDocu.NomCli" "Nombre!Cliente" ? "character" ? ? ? ? ? ? no ? no no "30.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tgre_cmpte.coddivvta
"tgre_cmpte.coddivvta" "Punto!Venta" ? "character" ? ? ? ? ? ? no ? no no "7.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > INTEGRAL.CcbCDocu.CodPed
"CcbCDocu.CodPed" "PED" ? "character" ? ? ? ? ? ? no ? no no "5.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > INTEGRAL.CcbCDocu.NroPed
"CcbCDocu.NroPed" "Nro.!Pedido" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.tgre_cmpte.estado_sunat
"tgre_cmpte.estado_sunat" "Sunat" ? "character" ? ? ? ? ? ? no ? no no "33.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 B-table-Win
ON START-SEARCH OF BROWSE-3 IN FRAME F-Main
DO:
    DEFINE VAR x-sql AS CHAR.

    x-SQL = "FOR EACH tgre_cmpte WHERE tgre_cmpte.estado = 'CMPTE GENERADO' NO-LOCK, " + 
            "FIRST INTEGRAL.CcbCDocu WHERE ccbcdocu.codcia = " + string(s-codcia) + " and " +
            "ccbcdocu.coddoc = tgre_cmpte.coddoc AND ccbcdocu.nrodoc = tgre_cmpte.nrodoc NO-LOCK ".

    {gn/sort-browse.i &ThisBrowse="browse-3" &ThisSQL = x-SQL}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 B-table-Win
ON VALUE-CHANGED OF BROWSE-3 IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

    DEFINE VAR cEstado AS CHAR.

    iRowsSelecteds = browse-3:NUM-SELECTED-ROWS.
    IF iRowsSelecteds > s-numero THEN DO:
        browse-3:DESELECT-FOCUSED-ROW().        
    END.
    ELSE DO:
        /*
        /*cEstado = gre_cmpte.estado_sunat:SCREEN-VALUE IN FRAME {&FRAME-NAME}.*/
        IF gre_cmpte.estado_sunat <> "Aceptado por sunat" THEN DO:
            MESSAGE "Comprobante " gre_cmpte.coddoc " " gre_cmpte.nrodoc SKIP
                "Aun NO esta  ACEPTADO por SUNAT"
                VIEW-AS ALERT-BOX INFORMATION.
            br_table:DESELECT-FOCUSED-ROW().
        END.
        */
    END.
    iRowsSelecteds = browse-3:NUM-SELECTED-ROWS.

    RUN mostrar-registros-seleccionados IN lh_handle(iRowsSelecteds).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-refrescar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-refrescar B-table-Win
ON CHOOSE OF BUTTON-refrescar IN FRAME F-Main /* Refrescar data */
DO:
    ASSIGN fill-in-phr fill-in-fecha fill-in-dias.
    DEFINE VAR iDias AS INT.
    iDias = 31.
    IF fill-in-dias > iDias  THEN DO:
        MESSAGE "No debe ser mayor a " + STRING(iDias) + " dias" VIEW-AS ALERT-BOX INFORMATION.
        RETURN NO-APPLY.
    END.

  RUN extraeDataFromDB.

  /*RUN dispatch IN h_b-genera-gre-desde-cmpte ('open-query':U).*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualizar-estado-sunat B-table-Win 
PROCEDURE actualizar-estado-sunat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exportar-excel B-table-Win 
PROCEDURE exportar-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pSoloSeleccionados AS LOG.

DEFINE VAR rpta AS LOG.
cFile = "".

SYSTEM-DIALOG GET-FILE cFile
    FILTERS 'Excel (*.xlsx)' '*.xlsx'
    ASK-OVERWRITE
    CREATE-TEST-FILE
    DEFAULT-EXTENSION '.xlxs'
    RETURN-TO-START-DIR
    SAVE-AS
    TITLE 'Exportar a Excel'
    UPDATE rpta.
IF rpta = NO OR cFile = '' THEN RETURN.

/*  */
EMPTY TEMP-TABLE tmpTable.

SESSION:SET-WAIT-STATE("GENERAL").

DEFINE BUFFER b-ccbcdocu FOR ccbcdocu.
DEFINE BUFFER b-faccpedi FOR faccpedi.
DEFINE BUFFER b-gn-divi FOR gn-divi.

 FOR EACH tgre_cmpte BY tgre_cmpte.coddoc BY tgre_cmpte.nrodoc BY tgre_cmpte.fechaemision DESC:
        FIND FIRST tmpTable WHERE tmpTable.codcmpte = tgre_cmpte.coddoc AND tmpTable.nrocmpte = tgre_cmpte.nrodoc NO-LOCK NO-ERROR.
        IF AVAILABLE tmpTable THEN NEXT.
        /* Comprobante */
        FIND FIRST b-ccbcdocu WHERE b-ccbcdocu.codcia = 1 AND b-ccbcdocu.coddoc = tgre_cmpte.coddoc AND
                            b-ccbcdocu.nrodoc = tgre_cmpte.nrodoc NO-LOCK NO-ERROR.

        CREATE tmpTable.
            ASSIGN tmpTable.coddivvta = tgre_cmpte.coddivvta
                tmpTable.nropedcom = ''
                tmpTable.nropedlog = ''
                tmpTable.codorddsp = ''
                tmpTable.nroorddsp = ''
                tmpTable.fechaemi = tgre_cmpte.fechahorareg
                tmpTable.codcmpte = tgre_cmpte.coddoc
                tmpTable.nrocmpte = tgre_cmpte.nrodoc
                tmpTable.codclie = IF AVAILABLE b-ccbcdocu THEN b-ccbcdocu.codcli ELSE '<No existe>'
                tmpTable.nomclie = IF AVAILABLE b-ccbcdocu THEN b-ccbcdocu.nomcli ELSE '<No existe>'
                tmpTable.impte = IF AVAILABLE b-ccbcdocu THEN b-ccbcdocu.totalprecioventa ELSE 0
                tmpTable.coddivdsp = IF AVAILABLE b-ccbcdocu THEN b-ccbcdocu.coddiv ELSE '<No existe>'
                tmpTable.nomdivdsp = '<No existe>'
                tmpTable.nomdivvta = '<No existe>'
                tmpTable.monevta = 'No existe'
                tmpTable.nropgre = tgre_cmpte.nropgre
                tmpTable.fchpgre = tgre_cmpte.fchemipgre.

            IF AVAILABLE b-ccbcdocu THEN DO:
                ASSIGN tmpTable.monevta =  IF b-ccbcdocu.codmon = 1 THEN 'Soles' ELSE 'Dolares'
                        tmpTable.nropedlog = b-ccbcdocu.nroped
                        tmpTable.codorddsp = b-ccbcdocu.libre_C01
                        tmpTable.nroorddsp = b-ccbcdocu.libre_C02.
                  /* Pedido Logistico */
                  FIND FIRST b-faccpedi WHERE b-faccpedi.codcia = 1 AND b-faccpedi.coddoc = 'PED' AND 
                                              b-faccpedi.nroped = b-ccbcdocu.nroped NO-LOCK NO-ERROR.
                  IF AVAILABLE b-faccpedi THEN DO:
                        ASSIGN tmpTable.nropedcom = b-faccpedi.nroref.
                  END.
                  /* Orden de despacho */
                  FIND FIRST b-faccpedi WHERE b-faccpedi.codcia = 1 AND b-faccpedi.coddoc = b-ccbcdocu.libre_C01 AND 
                                              b-faccpedi.nroped = b-ccbcdocu.libre_C02 NO-LOCK NO-ERROR.
                  IF AVAILABLE b-faccpedi THEN DO:
                        ASSIGN tmpTable.fchorddsp = b-faccpedi.fchped.
                  END.

            END.
                
        FIND FIRST b-gn-divi WHERE b-gn-divi.codcia = 1 AND b-gn-divi.coddiv = tmpTable.coddivvta NO-LOCK NO-ERROR.
        IF AVAILABLE b-gn-divi THEN ASSIGN tmpTable.nomdivvta = b-gn-divi.desdiv.
        FIND FIRST b-gn-divi WHERE b-gn-divi.codcia = 1 AND b-gn-divi.coddiv = tmpTable.coddivdsp NO-LOCK NO-ERROR.
        IF AVAILABLE b-gn-divi THEN ASSIGN tmpTable.nomdivdsp = b-gn-divi.desdiv.
 END.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN lib\Tools-to-excel PERSISTENT SET hProc.

def var c-csv-file as char no-undo.
def var c-xls-file as char no-undo. /* will contain the XLS file path created */

c-xls-file = cFile.

run pi-crea-archivo-csv IN hProc (input  buffer tmpTable:handle,
                        /*input  session:temp-directory + "file"*/ c-xls-file,
                        output c-csv-file) .

run pi-crea-archivo-xls  IN hProc (input  buffer tmpTable:handle,
                        input  c-csv-file,
                        output c-xls-file) .

DELETE PROCEDURE hProc.

SESSION:SET-WAIT-STATE("").

END PROCEDURE.


/*
DEFINE TEMP-TABLE tmpTable
    FIELD coddivvta    AS  CHAR    FORMAT 'x(8)'   COLUMN-LABEL "Cod.div.vta"
    FIELD nomdivvta    AS  CHAR    FORMAT 'x(80)'   COLUMN-LABEL "Nombre div. vta"
    FIELD nropedcom    AS  CHAR    FORMAT 'x(15)'   COLUMN-LABEL "Nro. Pedido Comercial"
    FIELD nropedlog    AS  CHAR    FORMAT 'x(15)'   COLUMN-LABEL "Nro. Pedido Logistico"
    FIELD fechaemi     AS  DATE       COLUMN-LABEL "Fecha emision cmpte"
    FIELD codcmpte     AS  CHAR    FORMAT 'x(5)'   COLUMN-LABEL "Cod.doc"
    FIELD nrocmpte     AS  CHAR    FORMAT 'x(15)'   COLUMN-LABEL "Nro.doc"
    FIELD codclie    AS  CHAR    FORMAT 'x(15)'   COLUMN-LABEL "Cod.Cliente"
    FIELD nomclie    AS  CHAR    FORMAT 'x(80)'   COLUMN-LABEL "Nombre del cliente"
    FIELD monevta    AS  CHAR    FORMAT 'x(5)'   COLUMN-LABEL "Moneda Vta"
    FIELD impte    AS  DEC    FORMAT '->>,>>>,>>9.99'   COLUMN-LABEL "Importe"
    FIELD coddivdsp    AS  CHAR    FORMAT 'x(5)'   COLUMN-LABEL "Cod.div. espacho"
    FIELD nomdivdsp    AS  CHAR    FORMAT 'x(80)'   COLUMN-LABEL "Nombre div. despacho".
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extraeDataFromDB B-table-Win 
PROCEDURE extraeDataFromDB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR dFechaFinal AS DATE.
DEFINE VAR dFechaInicial AS DATE.
DEFINE VAR dFecha AS DATE.
DEFINE VAR cDocumentos AS CHAR.
DEFINE VAR cDocumento AS CHAR.
DEFINE VAR iConteo AS INT.
DEFINE VAR iConteo1 AS INT.

DEFINE VAR iSerieDoc AS INT.
DEFINE VAR iNumeroDoc AS INT.
DEFINE VAR fchEmiPgre AS DATE.
DEFINE VAR iNroPgre AS INT.

dFechaInicial = fill-in-fecha - fill-in-dias.
dFechaFinal = fill-in-fecha.

cDocumentos = "FAC,BOL".

SESSION:SET-WAIT-STATE('GENERAL').

EMPTY TEMP-TABLE tgre_cmpte.
FOR EACH gre_cmpte WHERE (gre_cmpte.fechaemision >= dFechaInicial AND gre_cmpte.fechaemision <= dFechaFinal) NO-LOCK:

    IF LOOKUP(gre_cmpte.estado,"PGRE GENERADA,CMPTE GENERADO") = 0 THEN NEXT.
    FIND FIRST ccbcdocu WHERE ccbcdocu.codcia = 1 AND ccbcdocu.coddoc = gre_cmpte.coddoc AND 
                                ccbcdocu.nrodoc = gre_cmpte.nrodoc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbcdocu THEN NEXT.
    IF ccbcdocu.flgest = 'A' THEN NEXT. 

    IF cPHR <> "" THEN DO:
        FIND FIRST DI-RutaD WHERE (di-rutaD.codcia = 1 and di-rutaD.coddoc = 'PHR' 
                                    and di-rutaD.codref = ccbcdocu.libre_c01
                                    and di-rutaD.nroref = ccbcdocu.libre_c02
                                    and di-rutaD.nrodoc = cPHR) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE di-rutaD THEN NEXT.
    END.
    
    iSerieDoc = INT(SUBSTRING(ccbcdocu.nrodoc,1,3)) NO-ERROR.
    iNumeroDoc = INT(SUBSTRING(ccbcdocu.nrodoc,4)) NO-ERROR.
    fchEmiPgre = ?.
    iNroPgre = 0.
    /* Buscar su GRE */
    FIND FIRST gre_header WHERE gre_header.m_coddoc = ccbcdocu.coddoc AND gre_header.m_nroser = iSerieDoc AND
                                gre_header.m_nrodoc = iNumeroDoc AND gre_header.m_rspta_sunat = 'ACEPTADO POR SUNAT'
                                 NO-LOCK NO-ERROR.
    IF AVAILABLE gre_header THEN NEXT.
    /* Si no tiene GRE, entonces la PGRE */
    /*
    FIND FIRST gre_header WHERE gre_header.m_coddoc = ccbcdocu.coddoc AND gre_header.m_nroser = iSerieDoc AND
                                gre_header.m_nrodoc = iNumeroDoc NO-LOCK NO-ERROR.
    */
    IF gre_cmpte.estado = "PGRE GENERADA" THEN DO:
        PGRE:
        FOR EACH gre_header WHERE gre_header.m_coddoc = ccbcdocu.coddoc AND gre_header.m_nroser = iSerieDoc AND
                                    gre_header.m_nrodoc = iNumeroDoc BY m_fechahorareg DESC :
            iNroPgre = gre_header.ncorrelatio.
            fchEmiPgre = gre_header.fechaEmisionGuia.
            LEAVE PGRE.
        END.
    END.
        
    /**/
    CREATE tgre_cmpte.
    BUFFER-COPY gre_cmpte TO tgre_cmpte.

    ASSIGN tgre_cmpte.trowid = rowid(gre_cmpte)
            tgre_cmpte.fchemipgre = fchEmiPgre
            tgre_cmpte.nropgre = iNroPgre.
END.

SESSION:SET-WAIT-STATE('').

{&OPEN-QUERY-browse-3}




END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extraeDataFromDBborrar B-table-Win 
PROCEDURE extraeDataFromDBborrar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR dFechaFinal AS DATE.
DEFINE VAR dFechaInicial AS DATE.
DEFINE VAR dFecha AS DATE.
DEFINE VAR cDocumentos AS CHAR.
DEFINE VAR cDocumento AS CHAR.
DEFINE VAR iConteo AS INT.
DEFINE VAR iConteo1 AS INT.

DEFINE VAR iSerieDoc AS INT.
DEFINE VAR iNumeroDoc AS INT.

dFechaInicial = fill-in-fecha - fill-in-dias.
dFechaFinal = fill-in-fecha.

cDocumentos = "FAC,BOL".

SESSION:SET-WAIT-STATE('GENERAL').

EMPTY TEMP-TABLE tgre_cmpte.
/*
REPEAT iConteo = 1 TO NUM-ENTRIES(cDocumentos,","):
    cDocumento = ENTRY(iConteo,cDocumentos,",").
    */
    /*FOR EACH ccbcdocu WHERE ccbcdocu.codcia = 1 AND ccbcdocu.fchdoc = dFecha AND ccbcdocu.coddoc = cDocumento NO-LOCK:*/
    FOR EACH ccbcdocu WHERE ccbcdocu.codcia = 1 
                AND (ccbcdocu.fchdoc >= dFechaInicial AND ccbcdocu.fchdoc <= dFechaFinal)
             /*AND ccbcdocu.coddoc = cDocumento*/ NO-LOCK:

        IF LOOKUP(ccbcdocu.coddoc,cDocumentos) = 0 THEN NEXT.
        IF ccbcdocu.flgest = 'A' THEN NEXT.        
        IF cPHR <> "" THEN DO:
            FIND FIRST DI-RutaD WHERE (di-rutaD.codcia = 1 and di-rutaD.coddoc = 'PHR' 
                                        and di-rutaD.codref = ccbcdocu.libre_c01
                                        and di-rutaD.nroref = ccbcdocu.libre_c02
                                        and di-rutaD.nrodoc = cPHR) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE di-rutaD THEN NEXT.
        END.

        FIND FIRST gre_cmpte WHERE gre_cmpte.coddoc = ccbcdocu.coddoc AND gre_cmpte.nrodoc = ccbcdocu.nrodoc AND
                    gre_cmpte.estado = "PGRE_GENERADA" NO-LOCK NO-ERROR.
        IF NOT AVAILABLE gre_cmpte THEN DO:
            FIND FIRST gre_cmpte WHERE gre_cmpte.coddoc = ccbcdocu.coddoc AND gre_cmpte.nrodoc = ccbcdocu.nrodoc AND
                        gre_cmpte.estado = "CMPTE_GENERADO" NO-LOCK NO-ERROR.
        END.
        
        IF NOT AVAILABLE gre_cmpte THEN NEXT.
        
        iSerieDoc = INT(SUBSTRING(ccbcdocu.nrodoc,1,3)).
        iNumeroDoc = INT(SUBSTRING(ccbcdocu.nrodoc,1,4)).
        FIND FIRST gre_header WHERE gre_header.m_coddoc = ccbcdocu.coddoc AND gre_header.m_nroser = iSerieDoc AND
                                    gre_header.m_nrodoc = iNumeroDoc AND gre_header.m_rspta_sunat = 'ACEPTADO POR SUNAT'
                                     NO-LOCK NO-ERROR.
        IF AVAILABLE gre_header THEN NEXT.
        /**/
        CREATE tgre_cmpte.
        BUFFER-COPY gre_cmpte TO tgre_cmpte.

        ASSIGN tgre_cmpte.trowid = rowid(gre_cmpte).
    END.
/*
END.
*/

SESSION:SET-WAIT-STATE('').

{&OPEN-QUERY-browse-3}

    MESSAGE "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE generar-pgre B-table-Win 
PROCEDURE generar-pgre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR iRow AS INT.
DEFINE VAR iRowsSelected AS INT.
DEFINE VAR iRowsOk AS INT.

DEFINE VAR cCoddoc AS CHAR.
DEFINE VAR cNrodoc AS CHAR.
DEFINE VAR cCoddiv AS CHAR.
DEFINE VAR cEstadoSUNAT AS CHAR.

DEFINE VAR rRowId AS ROWID.

/* RUN gn/gre-online.r(OUTPUT lGRE_ONLINE).                             */
/*                                                                      */
/* IF lGRE_ONLINE = NO THEN DO:                                         */
/*     MESSAGE 'Proceso de GRE no esta ACTIVO' VIEW-AS ALERT-BOX ERROR. */
/*     RETURN 'ADM-ERROR'.                                              */
/* END.                                                                 */

/* ******************************************** */
/* 23/11/2023 */
/* ******************************************** */
IF s-acceso-total = NO THEN DO:
    RUN gre/d-error-gre-noactiva.w.
    RETURN 'ADM-ERROR'.
END.
/* ******************************************** */
/* ******************************************** */

DO WITH FRAME {&FRAME-NAME}:
    iRowsSelected = browse-3:NUM-SELECTED-ROWS.

    IF iRowsSelected > 0 THEN DO:
        /*
        VERIFICA:
        DO iRow = 1 TO iRowsSelected :
            IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(iRow) THEN DO:
                cCoddiv = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.coddivdesp.
                cCoddoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Coddoc.
                cNrodoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.nrodoc.

                cEstadoSUNAT = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Estado_sunat.

                IF cEstadoSUNAT <> "Aceptado por sunat" THEN DO:                
                    MESSAGE "Ha seleccionado comprobantes que NO ESTAN ACEPTADOS por SUNAT"
                        VIEW-AS ALERT-BOX INFORMATION.
                    iRowsSelected = -99.
                    LEAVE VERIFICA.
                END.
            END.
        END.
        IF iRowsSelected = -99 THEN RETURN "ADM-ERROR".
        */
        /**/
        MESSAGE 'Seguro de procesar los ' + STRING(iRowsSelected) + ' comprobantes' VIEW-AS ALERT-BOX QUESTION
                    BUTTONS YES-NO UPDATE rpta AS LOG.
            IF rpta = YES THEN DO:
            SESSION:SET-WAIT-STA("GENERAL").        
            DO iRow = 1 TO iRowsSelected :
                IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(iRow) THEN DO:
                    cCoddiv = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.coddivdesp.
                    cCoddoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Coddoc.
                    cNrodoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.nrodoc.
    
                    cEstadoSUNAT = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Estado_sunat.
                    /*ASSIGN tgre_cmpte.estado = "XXXXXX".*/
                    
                    RUN grabar-pre-gre(cCoddiv, cCoddoc, cNroDoc).
                    IF RETURN-VALUE = "OK" THEN DO:
                        ASSIGN tgre_cmpte.estado = "XXXXXX"
                        iRowsOk = iRowsOk + 1.
                    END.
                    
                END.
            END.
            IF iRowsSelected <> -99 THEN DO:
                {&open-query-browse-3}
            END.        
            SESSION:SET-WAIT-STA("").
    
            MESSAGE "Se generaron " + STRING(iRowsOk) + " pre-guia(s) de remision, de " + STRING(iRowsSelected) + " seleccionado(s)"
                VIEW-AS ALERT-BOX INFORMATION.

        END.

    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grabar-pre-gre B-table-Win 
PROCEDURE grabar-pre-gre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pcCoddiv AS CHAR.    /* Division de despacho */
DEFINE INPUT PARAMETER pcCoddoc AS CHAR.
DEFINE INPUT PARAMETER pcNroDoc AS CHAR.

DEFINE VAR pcOtros AS CHAR.
DEFINE VAR pcRetVal AS CHAR.
DEFINE VAR rRowId AS ROWID.
/*
    rRowId = tgre_cmpte.trowid.
    FIND FIRST b-gre_cmpte WHERE ROWID(b-gre_cmpte) = rRowid EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE b-gre_cmpte THEN DO:
        MESSAGE "Si esta".
        RELEASE b-gre_cmpte.
    END.
    ELSE DO:
        MESSAGE "NOO esta".
    END.

RETURN "ADM-ERROR".
*/

RUN gre/p-graba-pre_gre-desde-cmpte.r(pcCoddiv, pcCoddoc, pcNroDoc, INPUT-OUTPUT pcOtros, OUTPUT pcRetVal).

IF pcRetVal = "OK" THEN DO:
    /* Cambio el estado de la tabla de control */
    DO WITH FRAME {&FRAME-NAME}:
        /*rRowId = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).*/
        rRowId = tgre_cmpte.trowid.
        FIND FIRST b-gre_cmpte WHERE ROWID(b-gre_cmpte) = rRowid EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE b-gre_cmpte THEN DO:
            ASSIGN b-gre_cmpte.estado = "PGRE GENERADA".
            RELEASE b-gre_cmpte.
        END.
    END.
END.
ELSE DO:
    pcRetVal = "ERROR".
    RETURN "ADM-ERROR".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Invalidar B-table-Win 
PROCEDURE Invalidar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

/* ******************************************** */
/* 23/11/2023 */
/* ******************************************** */
IF s-acceso-total = NO THEN DO:
    RUN gre/d-error-gre-noactiva.w.
    RETURN.
END.
/* ******************************************** */
/* ******************************************** */

DEF VAR iRow AS INTE NO-UNDO.
DEF VAR iRowsSelected AS INTE NO-UNDO.
DEF VAR rRowId AS ROWID.

DO WITH FRAME {&FRAME-NAME}:
    /* Verificamos informaci�n */
    iRowsSelected = {&browse-name}:NUM-SELECTED-ROWS.
    DO iRow = 1 TO iRowsSelected:
        IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(iRow) THEN DO:
            /* Si es por reprogramaci�n debe tener parte de ingreso al almac�n */
            FIND LogTabla WHERE logtabla.codcia = s-codcia AND
                logtabla.Tabla = 'ALMCDOCU' AND 
                logtabla.Evento = 'REPROGRAMACION' AND
                logtabla.ValorLlave BEGINS TRIM(s-coddiv) + '|' + Ccbcdocu.Libre_c01 + '|' + Ccbcdocu.Libre_c02
                NO-LOCK NO-ERROR.
            IF AVAILABLE LogTabla THEN DO:
                FIND Almcmov WHERE Almcmov.CodCia = s-codcia AND
                    Almcmov.CodRef = Ccbcdocu.coddoc AND 
                    Almcmov.NroRef = Ccbcdocu.nrodoc AND
                    Almcmov.TipMov = "I" AND            /* Ingreso por Devoluci�n de Mercader�a */
                    Almcmov.CodMov = 09 AND
                    Almcmov.FlgEst <> "A"
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Almcmov THEN DO:
                    pMensaje = 'El comprobante ' + Ccbcdocu.coddoc + ' ' + ccbcdocu.nrodoc + ' NO tiene Parte de Ingreso al almac�n' + CHR(10) +
                        'Proceso Abortado'.
                    RETURN 'ADM-ERROR'.
                END.
            END.
            ELSE DO:
                FIND Almcmov WHERE Almcmov.CodCia = s-codcia AND
                    Almcmov.CodRef = Ccbcdocu.coddoc AND 
                    Almcmov.NroRef = Ccbcdocu.nrodoc AND
                    Almcmov.TipMov = "I" AND            /* Ingreso por Devoluci�n de Mercader�a */
                    Almcmov.CodMov = 09 AND
                    Almcmov.FlgEst <> "A" NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Almcmov THEN DO:
                    /* Ver si la factura est� anulada */
                    IF Ccbcdocu.flgest <> "A" THEN DO:
                        pMensaje = 'El comprobante ' + Ccbcdocu.coddoc + ' ' + Ccbcdocu.nrodoc + ' NO est� ANULADO y NO tiene parte de ingreso' + CHR(10) +
                            'Proceso Abortado'.
                        RETURN 'ADM-ERROR'.
                    END.
                    /*
                    pMensaje = '(*) El comprobante ' + Ccbcdocu.coddoc + ' ' + ccbcdocu.nrodoc + ' NO tiene Parte de Ingreso al almac�n' + CHR(10) +
                        'Proceso Abortado'.
                    RETURN 'ADM-ERROR'.
                    */
                END.
            END.
        END.
    END.
END.
/* Una vez verificado procedemos a INVALIDAR los registros */
RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR' WITH FRAME {&FRAME-NAME}:
    DO iRow = 1 TO iRowsSelected:
        IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(iRow) THEN DO:

            rRowId = tgre_cmpte.trowid.
            FIND FIRST gre_cmpte WHERE rowid(gre_cmpte) = rRowid EXCLUSIVE-LOCK NO-ERROR.

            IF ERROR-STATUS:ERROR = YES THEN DO:
                {lib/mensaje-de-error.i &MensajeError="pMensaje"}
                UNDO RLOOP, RETURN 'ADM-ERROR'.
            END.
            ASSIGN
                gre_cmpte.estado = "RECHAZADO X CLIENTE"
                gre_cmpte.fechahora_exclusion = NOW
                gre_cmpte.user_exclusion = USERID("dictdb").
            /* LOG */
            CREATE gre_header_log.
            ASSIGN
                gre_header_log.m_coddoc = gre_cmpte.coddoc 
                gre_header_log.m_nroser = INTEGER(SUBSTRING(gre_cmpte.nrodoc,1,3))
                gre_header_log.m_nrodoc = INTEGER(SUBSTRING(gre_cmpte.nrodoc,4))
                gre_header_log.m_divdestino = gre_cmpte.coddivvta 
                gre_header_log.m_divorigen = gre_cmpte.coddivdesp 
                gre_header_log.m_fechahorareg = gre_cmpte.fechahorareg 
                gre_header_log.m_rspta_sunat = gre_cmpte.estado_sunat 
                gre_header_log.m_estado_mov_almacen = gre_cmpte.estado 
                gre_header_log.fechaemisionguia = gre_cmpte.fechaemision 
                gre_header_log.fechahora_envio_a_sunat = gre_cmpte.fechaEnvioSunat 
                .
            ASSIGN
                gre_header_log.m_fechahorareg_log = NOW
                gre_header_log.USER_log = USERID("dictdb")
                gre_header_log.m_motivo_log = "INVALIDADO"
                gre_header_log.cmotivo = "99"                       /* cMotivo */
                gre_header_log.dmotivo_detalle = "GENERA GR DESDE COMPROBANTE".   /*cMotivoDetalle*/

            /**/
            ASSIGN tgre_cmpte.estado = "XXXXXX".

            IF AVAILABLE(gre_cmpte) THEN RELEASE gre_cmpte.
            IF AVAILABLE(gre_header_log) THEN RELEASE gre_header_log.
        END.
    END.
    IF AVAILABLE(gre_cmpte) THEN RELEASE gre_cmpte.
    IF AVAILABLE(gre_header_log) THEN RELEASE gre_header_log.
END.

{&OPEN-QUERY-browse-3}

RETURN 'OK'.

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

/*RUN extraeDataFromDB.*/

  DO WITH FRAME {&FRAME-NAME}:
      fill-in-fecha:SCREEN-VALUE = STRING(TODAY,"99/99/99999").
      fill-in-dias:SCREEN-VALUE = "7".
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NO-generan-pgre B-table-Win 
PROCEDURE NO-generan-pgre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR iRow AS INT.
DEFINE VAR iRowsSelected AS INT.
DEFINE VAR iRowsOk AS INT.

DEFINE VAR cCoddoc AS CHAR.
DEFINE VAR cNrodoc AS CHAR.
DEFINE VAR cCoddiv AS CHAR.
DEFINE VAR cEstadoSUNAT AS CHAR.

DEFINE VAR rRowId AS ROWID.

RUN gn/gre-online.r(OUTPUT lGRE_ONLINE).

IF lGRE_ONLINE = NO THEN DO:
    MESSAGE 'Proceso de GRE no esta ACTIVO' VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
END.

DO WITH FRAME {&FRAME-NAME}:
    iRowsSelected = browse-3:NUM-SELECTED-ROWS.

    IF iRowsSelected > 0 THEN DO:        
        /*
        VERIFICA:
        DO iRow = 1 TO iRowsSelected :
            IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(iRow) THEN DO:
                cCoddiv = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.coddivdesp.
                cCoddoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Coddoc.
                cNrodoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.nrodoc.

                cEstadoSUNAT = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Estado_sunat.

                IF cEstadoSUNAT <> "Aceptado por sunat" THEN DO:                
                    MESSAGE "Ha seleccionado comprobantes que NO ESTAN ACEPTADOS por SUNAT"
                        VIEW-AS ALERT-BOX INFORMATION.
                    iRowsSelected = -99.
                    LEAVE VERIFICA.
                END.
            END.
        END.
        IF iRowsSelected = -99 THEN RETURN "ADM-ERROR".
        */
        /**/
        MESSAGE 'Seguro de NO Generar PGRE a los ' + STRING(iRowsSelected) + ' comprobantes seleccionados?' VIEW-AS ALERT-BOX QUESTION
                    BUTTONS YES-NO UPDATE rpta AS LOG.

        IF rpta = YES THEN DO:
        SESSION:SET-WAIT-STA("GENERAL").        
        DO iRow = 1 TO iRowsSelected :
            IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(iRow) THEN DO:
                cCoddiv = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.coddivdesp.
                cCoddoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Coddoc.
                cNrodoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.nrodoc.

                rRowId = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).

                FIND FIRST b-gre_cmpte WHERE ROWID(b-gre_cmpte) = rRowid EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE b-gre_cmpte THEN DO:
                    ASSIGN b-gre_cmpte.estado = "EXCLUIDO MANUAL"
                            b-gre_cmpte.USER_exclusion = USERID("dictdb")
                            b-gre_cmpte.fechahora_exclusion = NOW
                        .
                    iRowsOk = iRowsOk + 1.
                END.
                RELEASE b-gre_header.
            END.
        END.
        IF iRowsSelected <> -99 THEN DO:
            {&open-query-browse-3}
        END.        
        SESSION:SET-WAIT-STA("").

        MESSAGE "Se EXCLUYEON " + STRING(iRowsOk) + " comprobante(s), de " + STRING(iRowsSelected) + " seleccionado(s)"
            VIEW-AS ALERT-BOX INFORMATION.

        END.
    END.
END.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesar-pre-gre B-table-Win 
PROCEDURE procesar-pre-gre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR cEstadoBizLinks AS CHAR.  
DEFINE VAR cEstadoSUNAT AS CHAR.
DEFINE VAR cEstadoDocumento AS CHAR.

DEFINE VAR iRow AS INT.
DEFINE VAR iRows AS INT.
DEFINE VAR iRowsSelected AS INT.

DEFINE VAR cCoddoc AS CHAR.
DEFINE VAR cNrodoc AS CHAR.
DEFINE VAR cCoddiv AS CHAR.

DEFINE VAR rRowId AS ROWID.

DO WITH FRAME {&FRAME-NAME}:
    iRowsSelected = browse-3:NUM-SELECTED-ROWS.

    IF iRowsSelected > 0 THEN DO:
        SESSION:SET-WAIT-STA("GENERAL").
        DO iRow = 1 TO iRowsSelected :
            IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(iRow) THEN DO:
                cEstadoSUNAT = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Estado_sunat.
                IF cEstadoSUNAT = 'Aceptado por sunat' THEN DO:
                    iRows = iRows + 1.
                END.
            END.
        END.
        IF iRows <= 0 THEN DO:
            MESSAGE "De los documentos seleccionados" SKIP
                    "ninguno de ellos esta aceptado por sunat" SKIP
                    "verifique correctamente la seleccion de registros"
                    VIEW-AS ALERT-BOX INFORMATION.
            RETURN "ADM-ERROR".
        END.
            MESSAGE 'Se proceder a generar ' + STRING(iRows) + ' pre-guias de remision (PGRE)' SKIP
                '�Seguro de procesar?' VIEW-AS ALERT-BOX QUESTION
                    BUTTONS YES-NO UPDATE rpta AS LOG.
            IF rpta = NO THEN RETURN NO-APPLY.

        /**/
        DO iRow = 1 TO iRowsSelected :
            IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(iRow) THEN DO:
                cCoddiv = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.coddivdesp.
                cCoddoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Coddoc.
                cNrodoc = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.nrodoc.

                cEstadoSUNAT = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.Estado_sunat.
                IF cEstadoSUNAT = 'Aceptado por sunat' THEN DO:
                    RUN grabar-pre-gre(cCoddiv, cCoddoc, cNroDoc).
                END.
                /*
                IF TRUE <> (cEstadoSUNAT > "") OR  
                    LOOKUP(cEstadoSUNAT,"Aceptado por sunat, Rechazado por sunat") = 0 THEN DO:
                    RUN gn/p-estado-documento-electronico(cCoddoc, cNrodoc, cCoddiv,
                                                          OUTPUT cEstadoBizLinks, OUTPUT cEstadoSunat, OUTPUT cEstadoDocumento).
                    rRowId = ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                    FIND FIRST b-gre_cmpte WHERE ROWID(b-gre_cmpte) = rRowid EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE b-gre_cmpte THEN DO:
                        ASSIGN b-gre_cmpte.estado_sunat = cEstadoDocumento.
                        RELEASE b-gre_cmpte.
                        br_table:REFRESH().
                    END.
                    
                END.
                */
            END.
        END.
        SESSION:SET-WAIT-STA("").
    END.
END.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refrescar B-table-Win 
PROCEDURE refrescar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pNoPHR AS CHAR NO-UNDO.

cPHR = pNoPHR.

SESSION:SET-WAIT-STATE('GENERAL').

EMPTY TEMP-TABLE tgre_cmpte.

/* Comprobantes sin PGRE */
FOR EACH gre_cmpte WHERE gre_cmpte.estado = 'CMPTE GENERADO' AND gre_cmpte.coddivdesp = s-coddiv NO-LOCK:
    FIND FIRST CcbCDocu WHERE ccbcdocu.codcia = s-codcia and
            ccbcdocu.coddoc = gre_cmpte.coddoc and
            ccbcdocu.nrodoc = gre_cmpte.nrodoc NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ccbcdocu THEN NEXT.
    IF cPHR <> "" THEN DO:
        FIND FIRST DI-RutaD WHERE (di-rutaD.codcia = 1 and di-rutaD.coddoc = 'PHR' 
                                    and di-rutaD.codref = ccbcdocu.libre_c01
                                    and di-rutaD.nroref = ccbcdocu.libre_c02
                                    and di-rutaD.nrodoc = cPHR) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE di-rutaD THEN NEXT.
    END.
    CREATE tgre_cmpte.
    BUFFER-COPY gre_cmpte TO tgre_cmpte.

    ASSIGN tgre_cmpte.trowid = rowid(gre_cmpte).
END.

/* Comprobantes sin GRE */


SESSION:SET-WAIT-STATE('').

{&OPEN-QUERY-browse-3}

iRowsSelecteds = BROWSE-3:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.

 RUN mostrar-registros-seleccionados IN lh_handle(iRowsSelecteds).


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
  {src/adm/template/snd-list.i "tgre_cmpte"}
  {src/adm/template/snd-list.i "CcbCDocu"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-get-utf8 B-table-Win 
FUNCTION f-get-utf8 RETURNS CHARACTER
  ( INPUT pString AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR lRetVal AS CHAR.
DEF VAR x-dice AS CHAR NO-UNDO.
DEF VAR x-debedecir AS CHAR NO-UNDO.

/* UTF-8 */
x-dice = TRIM(pString).
RUN lib\limpiar-texto(x-dice,'',OUTPUT x-debedecir).
lRetVal = CODEPAGE-CONVERT(x-debedecir, "utf-8", SESSION:CHARSET).
lRetVal = REPLACE(x-debedecir,"�","'").
lRetVal = REPLACE(x-debedecir,"�","a").
lRetVal = REPLACE(x-debedecir,"�","e").
lRetVal = REPLACE(x-debedecir,"�","i").
lRetVal = REPLACE(x-debedecir,"�","o").
lRetVal = REPLACE(x-debedecir,"�","u").
lRetVal = REPLACE(x-debedecir,"�","A").
lRetVal = REPLACE(x-debedecir,"�","E").
lRetVal = REPLACE(x-debedecir,"�","I").
lRetVal = REPLACE(x-debedecir,"�","O").
lRetVal = REPLACE(x-debedecir,"�","U").
lRetVal = REPLACE(x-debedecir,"�","u").
lRetVal = REPLACE(x-debedecir,"�","U").
lRetVal = REPLACE(x-debedecir,"�","u").
lRetVal = REPLACE(x-debedecir,"�"," ").
lRetVal = REPLACE(x-debedecir,"�","'").
lRetVal = REPLACE(x-debedecir,"�"," ").
lRetVal = REPLACE(x-debedecir,"�"," ").
lRetVal = REPLACE(x-debedecir,"�"," ").
lRetVal = REPLACE(x-debedecir,"�"," ").
lRetVal = REPLACE(x-debedecir,"�"," ").

RETURN lRetVal.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

