&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
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

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-user-id AS CHAR.

DEF VAR x-Margen AS DECI NO-UNDO.

DEF TEMP-TABLE Detalle
    FIELD CodMat        AS CHAR     FORMAT 'x(10)'      LABEL 'SKU'
    FIELD DesMat        AS CHAR     FORMAT 'x(80)'      LABEL 'DESCRIPCION'
    FIELD DesMar        AS CHAR     FORMAT 'x(30)'      LABEL 'MARCA'
    FIELD CHR__01       AS CHAR     FORMAT 'x(10)'      LABEL 'UNIDAD'
    FIELD CodFam        AS CHAR     FORMAT 'x(8)'       LABEL 'LINEA'
    FIELD SubFam        AS CHAR     FORMAT 'x(8)'       LABEL 'SUBLINEA'
    FIELD CtoTot        AS DEC      FORMAT '>>>,>>9.9999'   LABEL 'COSTO REPOSICION'
    FIELD PreBas        AS DEC      FORMAT '>>>,>>9.9999'   LABEL 'PRECIO BASE'
    FIELD Margen        AS DEC      FORMAT '->>>,>>9.99'    LABEL 'MARGEN'
    FIELD Canal         AS CHAR     FORMAT 'x(30)'      LABEL 'CANAL'
    FIELD Factor-1      AS DEC      FORMAT '>>9.99'     LABEL 'FACTOR-1'
    FIELD Precio-1      AS DECI     FORMAT '->>>,>>9.9999'  LABEL 'PRECIO-1'
    FIELD Grupo         AS CHAR     FORMAT 'x(30)'      LABEL 'GRUPO'
    FIELD Factor-2      AS DEC      FORMAT '>>9.99'     LABEL 'FACTOR-2'
    FIELD Precio-2      AS DECI     FORMAT '->>>,>>9.9999'  LABEL 'PRECIO-2'
    .

DEF TEMP-TABLE T-Precios
    FIELD Fila   AS INTE
    FIELD CodMat LIKE prilistapreciostemp.CodMat 
    FIELD Canal LIKE prilistapreciostemp.Canal 
    FIELD Grupo LIKE prilistapreciostemp.Grupo 
    FIELD PreUni LIKE prilistapreciostemp.PreUni 
    FIELD PreGrupo LIKE prilistapreciostemp.PreGrupo
    FIELD FactorGrupo LIKE prilistapreciostemp.FactorGrupo.

DEF TEMP-TABLE T-Resumen
    FIELD CodMat LIKE prilistapreciostemp.CodMat.

DEF TEMP-TABLE T-Errores 
    FIELD Fila   AS INTE FORMAT ">>>>>>>>9"     LABEL 'Fila'
    FIELD CodMat AS CHAR FORMAT 'x(8)'          LABEL 'Articulo'
    FIELD Glosa  AS CHAR FORMAT 'x(80)'         LABEL 'Glosa'.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES prilistapreciostemp Almmmatg pricanal ~
prigrupo

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table prilistapreciostemp.CodMat ~
Almmmatg.DesMat Almmmatg.DesMar prilistapreciostemp.UndBas Almmmatg.codfam ~
Almmmatg.subfam prilistapreciostemp.CtoTot prilistapreciostemp.PreBas ~
x-Margen @ x-Margen pricanal.Descripcion prilistapreciostemp.FactorCanal ~
prilistapreciostemp.PreCanal prigrupo.Descripcion ~
prilistapreciostemp.FactorGrupo prilistapreciostemp.PreGrupo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table ~
prilistapreciostemp.PreGrupo 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table prilistapreciostemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table prilistapreciostemp
&Scoped-define QUERY-STRING-br_table FOR EACH prilistapreciostemp WHERE ~{&KEY-PHRASE} ~
      AND prilistapreciostemp.CodCia = s-codcia ~
 AND prilistapreciostemp.User-Id = s-user-id NO-LOCK, ~
      FIRST Almmmatg OF prilistapreciostemp OUTER-JOIN NO-LOCK, ~
      FIRST pricanal OF prilistapreciostemp OUTER-JOIN NO-LOCK, ~
      FIRST prigrupo OF prilistapreciostemp OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH prilistapreciostemp WHERE ~{&KEY-PHRASE} ~
      AND prilistapreciostemp.CodCia = s-codcia ~
 AND prilistapreciostemp.User-Id = s-user-id NO-LOCK, ~
      FIRST Almmmatg OF prilistapreciostemp OUTER-JOIN NO-LOCK, ~
      FIRST pricanal OF prilistapreciostemp OUTER-JOIN NO-LOCK, ~
      FIRST prigrupo OF prilistapreciostemp OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table prilistapreciostemp Almmmatg ~
pricanal prigrupo
&Scoped-define FIRST-TABLE-IN-QUERY-br_table prilistapreciostemp
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg
&Scoped-define THIRD-TABLE-IN-QUERY-br_table pricanal
&Scoped-define FOURTH-TABLE-IN-QUERY-br_table prigrupo


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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      prilistapreciostemp, 
      Almmmatg
    FIELDS(Almmmatg.DesMat
      Almmmatg.DesMar
      Almmmatg.codfam
      Almmmatg.subfam), 
      pricanal
    FIELDS(pricanal.Descripcion), 
      prigrupo
    FIELDS(prigrupo.Descripcion) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      prilistapreciostemp.CodMat COLUMN-LABEL "Articulo" FORMAT "X(6)":U
      Almmmatg.DesMat FORMAT "X(60)":U
      Almmmatg.DesMar COLUMN-LABEL "Marca" FORMAT "X(20)":U
      prilistapreciostemp.UndBas COLUMN-LABEL "Unidad" FORMAT "x(8)":U
      Almmmatg.codfam COLUMN-LABEL "L�nea" FORMAT "X(3)":U
      Almmmatg.subfam COLUMN-LABEL "SubL�nea" FORMAT "X(3)":U
      prilistapreciostemp.CtoTot COLUMN-LABEL "Costo!con IGV" FORMAT ">>>,>>>,>>9.9999":U
            WIDTH 8.43
      prilistapreciostemp.PreBas COLUMN-LABEL "Precio Base!con IGV" FORMAT ">>>,>>>,>>9.9999":U
            WIDTH 9.29
      x-Margen @ x-Margen COLUMN-LABEL "Margen!(%)" FORMAT "->>>,>>9.99":U
            WIDTH 7
      pricanal.Descripcion COLUMN-LABEL "Canal de Negociaci�n" FORMAT "x(30)":U
            WIDTH 17.72
      prilistapreciostemp.FactorCanal COLUMN-LABEL "% I-II" FORMAT ">>>,>>9.999999":U
            WIDTH 8.86
      prilistapreciostemp.PreCanal COLUMN-LABEL "Precio Canal" FORMAT ">>>,>>>,>>9.9999":U
            WIDTH 9.86
      prigrupo.Descripcion COLUMN-LABEL "Grupo de Divisi�n" FORMAT "x(30)":U
            WIDTH 16.43
      prilistapreciostemp.FactorGrupo COLUMN-LABEL "% II-III" FORMAT ">>>,>>9.999999":U
            WIDTH 9
      prilistapreciostemp.PreGrupo COLUMN-LABEL "Precio Grupo" FORMAT ">>>,>>>,>>9.9999":U
            COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 11
  ENABLE
      prilistapreciostemp.PreGrupo
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 188 BY 23.69
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


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
         HEIGHT             = 24.38
         WIDTH              = 189.43.
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.prilistapreciostemp,INTEGRAL.Almmmatg OF INTEGRAL.prilistapreciostemp,INTEGRAL.pricanal OF INTEGRAL.prilistapreciostemp,INTEGRAL.prigrupo OF INTEGRAL.prilistapreciostemp"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER USED, FIRST OUTER USED, FIRST OUTER USED"
     _Where[1]         = "prilistapreciostemp.CodCia = s-codcia
 AND prilistapreciostemp.User-Id = s-user-id"
     _FldNameList[1]   > INTEGRAL.prilistapreciostemp.CodMat
"prilistapreciostemp.CodMat" "Articulo" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.Almmmatg.DesMat
"Almmmatg.DesMat" ? "X(60)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.Almmmatg.DesMar
"Almmmatg.DesMar" "Marca" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.prilistapreciostemp.UndBas
"prilistapreciostemp.UndBas" "Unidad" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.Almmmatg.codfam
"Almmmatg.codfam" "L�nea" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.Almmmatg.subfam
"Almmmatg.subfam" "SubL�nea" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > INTEGRAL.prilistapreciostemp.CtoTot
"prilistapreciostemp.CtoTot" "Costo!con IGV" ? "decimal" ? ? ? ? ? ? no ? no no "8.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > INTEGRAL.prilistapreciostemp.PreBas
"prilistapreciostemp.PreBas" "Precio Base!con IGV" ? "decimal" ? ? ? ? ? ? no ? no no "9.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"x-Margen @ x-Margen" "Margen!(%)" "->>>,>>9.99" ? ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > INTEGRAL.pricanal.Descripcion
"pricanal.Descripcion" "Canal de Negociaci�n" ? "character" ? ? ? ? ? ? no ? no no "17.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > INTEGRAL.prilistapreciostemp.FactorCanal
"prilistapreciostemp.FactorCanal" "% I-II" ? "decimal" ? ? ? ? ? ? no ? no no "8.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > INTEGRAL.prilistapreciostemp.PreCanal
"prilistapreciostemp.PreCanal" "Precio Canal" ? "decimal" ? ? ? ? ? ? no ? no no "9.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > INTEGRAL.prigrupo.Descripcion
"prigrupo.Descripcion" "Grupo de Divisi�n" ? "character" ? ? ? ? ? ? no ? no no "16.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > INTEGRAL.prilistapreciostemp.FactorGrupo
"prilistapreciostemp.FactorGrupo" "% II-III" ? "decimal" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > INTEGRAL.prilistapreciostemp.PreGrupo
"prilistapreciostemp.PreGrupo" "Precio Grupo" ? "decimal" 11 0 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prilistapreciostemp.PreBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prilistapreciostemp.PreBas br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF prilistapreciostemp.PreBas IN BROWSE br_table /* Precio Base!con IGV */
DO:
/*     x-Margen = 0.                                                                                                               */
/*     IF prilistapreciostemp.CtoTot > 0                                                                                           */
/*         THEN x-Margen = ROUND((DECIMAL(SELF:SCREEN-VALUE) - prilistapreciostemp.CtoTot) / prilistapreciostemp.CtoTot * 100, 2). */
/*     DISPLAY x-Margen WITH BROWSE {&browse-name}.                                                                                */
/*     ASSIGN                                                                                                                      */
/*         prilistapreciostemp.PreCanal:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(                                            */
/*             ROUND(DECIMAL(prilistapreciostemp.FactorCanal:SCREEN-VALUE IN BROWSE {&browse-name}) *                              */
/*                   DECIMAL(prilistapreciostemp.PreBas:SCREEN-VALUE IN BROWSE {&browse-name})                                     */
/*                   / 100, 4)                                                                                                     */
/*             ).                                                                                                                  */
/*     ASSIGN                                                                                                                      */
/*         prilistapreciostemp.PreGrupo:SCREEN-VALUE IN BROWSE {&browse-name} =  STRING(                                           */
/*             ROUND(DECIMAL(prilistapreciostemp.PreCanal:SCREEN-VALUE IN BROWSE {&browse-name}) *                                 */
/*                   DECIMAL(prilistapreciostemp.FactorGrupo:SCREEN-VALUE IN BROWSE {&browse-name})                                */
/*                   / 100, 4)                                                                                                     */
/*             ).                                                                                                                  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON 'RETURN':U OF prilistapreciostemp.PreGrupo
DO:
    APPLY 'TAB':U.
    RETURN NO-APPLY.
END.

ON FIND OF prilistapreciostemp DO:
    x-Margen = 0.
    IF prilistapreciostemp.CtoTot > 0 
        THEN x-Margen = ROUND((prilistapreciostemp.PreBas - prilistapreciostemp.CtoTot) / prilistapreciostemp.CtoTot * 100, 2).
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal B-table-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-margen AS DEC.

GET FIRST {&browse-name}.
REPEAT WHILE AVAILABLE PriListaPreciosTemp:
    CREATE Detalle.
    BUFFER-COPY PriListaPreciosTemp TO Detalle.
    ASSIGN
        Detalle.CHR__01 = prilistapreciostemp.UndBas
        Detalle.DesMat = (IF AVAILABLE Almmmatg THEN Almmmatg.DesMat ELSE '')
        Detalle.DesMar = (IF AVAILABLE Almmmatg THEN Almmmatg.DesMar ELSE '')
        Detalle.CodFam = (IF AVAILABLE Almmmatg THEN Almmmatg.CodFam ELSE '')
        Detalle.SubFam = (IF AVAILABLE Almmmatg THEN Almmmatg.SubFam ELSE '').
    x-Margen = 0.
    IF Detalle.ctotot > 0 THEN x-margen = (Detalle.prebas - Detalle.ctotot) / Detalle.ctotot * 100.
    ASSIGN
        Detalle.Margen = x-Margen.
    ASSIGN
        Detalle.Factor-1 = prilistapreciostemp.FactorCanal
        Detalle.Precio-1 = prilistapreciostemp.PreCanal
        Detalle.Factor-2 = prilistapreciostemp.FactorGrupo
        Detalle.Precio-2 = prilistapreciostemp.PreGrupo.

    FIND PriCanal WHERE pricanal.CodCia = prilistapreciostemp.CodCia AND 
        pricanal.Canal = prilistapreciostemp.Canal
        NO-LOCK NO-ERROR.
    IF AVAILABLE PriCanal THEN Detalle.Canal = pricanal.Canal + " - " + pricanal.Descripcion.
    FIND PriGrupo WHERE prigrupo.CodCia = prilistapreciostemp.CodCia AND
        prigrupo.Grupo = prilistapreciostemp.Grupo
        NO-LOCK NO-ERROR.
    IF AVAILABLE PriGrupo THEN Detalle.Grupo = prigrupo.Grupo + " - " + prigrupo.Descripcion.
    GET NEXT {&browse-name}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Excel B-table-Win 
PROCEDURE Genera-Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Archivo de Salida */
DEF VAR c-csv-file AS CHAR NO-UNDO.
DEF VAR c-xls-file AS CHAR INIT 'Archivo_Excel' NO-UNDO.
DEF VAR rpta AS LOG INIT NO NO-UNDO.

SYSTEM-DIALOG GET-FILE c-xls-file
    FILTERS 'Libro de Excel' '*.xlsx'
    INITIAL-FILTER 1
    ASK-OVERWRITE
    CREATE-TEST-FILE
    DEFAULT-EXTENSION ".xlsx"
    SAVE-AS
    TITLE "Guardar como"
    USE-FILENAME
    UPDATE rpta.
IF rpta = NO THEN RETURN.

SESSION:SET-WAIT-STATE('GENERAL').

/* Variable de memoria */
DEFINE VAR hProc AS HANDLE NO-UNDO.

/* Levantamos la libreria a memoria */
RUN lib\Tools-to-excel PERSISTENT SET hProc.

/* Cargamos la informacion al temporal */
EMPTY TEMP-TABLE Detalle.
RUN Carga-Temporal.

/* Programas que generan el Excel */
RUN pi-crea-archivo-csv IN hProc (INPUT BUFFER Detalle:HANDLE,
                                  INPUT c-xls-file,
                                  OUTPUT c-csv-file) .

RUN pi-crea-archivo-xls IN hProc (INPUT BUFFER Detalle:handle,
                                  INPUT  c-csv-file,
                                  OUTPUT c-xls-file) .

/* Borramos librerias de la memoria */
DELETE PROCEDURE hProc.
SESSION:SET-WAIT-STATE('').
MESSAGE 'Proceso Terminado' VIEW-AS ALERT-BOX INFORMATION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Excel-Plantilla B-table-Win 
PROCEDURE Genera-Excel-Plantilla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Archivo de Salida */
DEF VAR c-csv-file AS CHAR NO-UNDO.
DEF VAR c-xls-file AS CHAR INIT 'Hoja_de_Planning_2' NO-UNDO.
DEF VAR rpta AS LOG INIT NO NO-UNDO.

SYSTEM-DIALOG GET-FILE c-xls-file
    FILTERS 'Libro de Excel' '*.xlsx'
    INITIAL-FILTER 1
    ASK-OVERWRITE
    CREATE-TEST-FILE
    DEFAULT-EXTENSION ".xlsx"
    SAVE-AS
    TITLE "Guardar como"
    USE-FILENAME
    UPDATE rpta.
IF rpta = NO THEN RETURN.

/* ******************************************************************************** */
/* Buscamos la plantilla */
/* ******************************************************************************** */
DEF VAR lFileXls AS CHAR NO-UNDO.
DEF VAR lNuevoFile AS LOG NO-UNDO.
GET-KEY-VALUE SECTION 'Plantillas' KEY 'Carpeta'  VALUE lFileXls.
lFileXls = lFileXls + 'hoja_de_planning.xltx'.
FILE-INFO:FILE-NAME = lFileXls.
IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
    MESSAGE 'La plantilla ' lFileXls SKIP 'NO existe' VIEW-AS ALERT-BOX.
    RETURN.
END.
SESSION:SET-WAIT-STATE('GENERAL').
/* ******************************************************************************** */
/* Cargamos la informacion al temporal */
/* ******************************************************************************** */
EMPTY TEMP-TABLE Detalle.
RUN Carga-Temporal.
/* ******************************************************************************** */
/* Programas que generan el Excel */
/* ******************************************************************************** */
lNuevoFile = NO.    /* Abre la plantilla lFileXls */
{lib/excel-open-file.i}
/* ******************************************************************************** */
/* LOGICA PRINCIPAL: CARGA DEL EXCEL */
/* ******************************************************************************** */
/* Select a worksheet */
chWorkbook:Worksheets(1):Activate.
chWorksheet = chWorkbook:Worksheets(1).
/* Cargamos al rev�s */
DEF VAR LocalRow AS INT NO-UNDO.
iRow = 2.
LocalRow = 2.
FOR EACH Detalle NO-LOCK BY Detalle.CodFam DESCENDING
    BY Detalle.SubFam DESCENDING
    BY Detalle.CodMat DESCENDING
    BY Detalle.Canal DESCENDING
    BY Detalle.Grupo DESCENDING:
    /*Agrega Row*/
    chWorkSheet:Range("A2"):EntireRow:INSERT.
    /* Grabar */
    /*iRow = iRow + 1.*/
    LocalRow = LocalRow + 1.
    cColumn = STRING(iRow).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.CodMat.
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.DesMat.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.DesMar.
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.CHR__01.
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.CodFam.
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.SubFam.
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.CtoTot.
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.PreBas.
/*     cRange = "I" + cColumn.                           */
/*     chWorkSheet:Range(cRange):Value = Detalle.Margen. */
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.Canal.
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.Factor-1.
/*     cRange = "L" + cColumn.                             */
/*     chWorkSheet:Range(cRange):Value = Detalle.Precio-1. */
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.Grupo.
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = Detalle.Factor-2.
/*     cRange = "O" + cColumn.                             */
/*     chWorkSheet:Range(cRange):Value = Detalle.Precio-2. */
END.
/* Borramos librerias de la memoria */
/* Delete Row*/
chWorkSheet:Range("A" + STRING(LocalRow)):EntireRow:DELETE.
SESSION:SET-WAIT-STATE('').
lNuevoFile = YES.           /* Graba la plantilla en el nuevo archivo */
lFileXls = c-xls-file.
lCerrarAlTerminar = YES.     /* NO Se hace visible al terminar */
lMensajeAlTerminar = YES.   /* Aviso que termin� el proceso */
{lib/excel-close-file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      prilistapreciostemp.PreCanal = ROUND(prilistapreciostemp.FactorCanal * prilistapreciostemp.PreBas / 100, 4).
/*   ASSIGN                                                                                                             */
/*       prilistapreciostemp.PreGrupo = ROUND(prilistapreciostemp.PreCanal * prilistapreciostemp.FactorGrupo / 100, 4). */
  ASSIGN
      PriListaPreciosTemp.FactorGrupo = ROUND(PriListaPreciosTemp.PreGrupo / PriListaPreciosTemp.PreCanal * 100, 6).
  ASSIGN
      prilistapreciostemp.PreUni = prilistapreciostemp.PreGrupo.
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

  /* Validaciones */
  IF PriListaPreciosTemp.PreUni < PriListaPreciosTemp.CtoTot THEN DO:
      MESSAGE 'El Precio Final es menor al Costo' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO prilistapreciostemp.PreBas IN BROWSE {&browse-name}.
      UNDO, RETURN 'ADM-ERROR'.
  END.

  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES' THEN
      ASSIGN
      prilistapreciostemp.HoraCreacion = STRING(TIME, 'HH:MM:SS')
      prilistapreciostemp.FchCreacion = TODAY
      prilistapreciostemp.UsrCreacion = s-user-id.
  ELSE 
      ASSIGN
          prilistapreciostemp.HoraModificacion = STRING(TIME, 'HH:MM:SS')
          prilistapreciostemp.FchModificacion = TODAY
          prilistapreciostemp.UsrModificacion = s-user-id.

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
  {src/adm/template/snd-list.i "prilistapreciostemp"}
  {src/adm/template/snd-list.i "Almmmatg"}
  {src/adm/template/snd-list.i "pricanal"}
  {src/adm/template/snd-list.i "prigrupo"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar-Canales-Grupos B-table-Win 
PROCEDURE Validar-Canales-Grupos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE T-Precios.
EMPTY TEMP-TABLE t-Resumen.
EMPTY TEMP-TABLE T-Errores.

DEF VAR LocalFila AS INT NO-UNDO.

/* Cargamos Temporales */
SESSION:SET-WAIT-STATE('GENERAL').
LocalFila = 0.
GET FIRST {&browse-name}.
REPEAT WHILE AVAILABLE PriListaPreciosTemp:
    LocalFila = LocalFila + 1.
    FIND T-Resumen WHERE T-Resumen.codmat = prilistapreciostemp.CodMat NO-ERROR.
    IF NOT AVAILABLE T-Resumen THEN CREATE T-Resumen.
    T-Resumen.CodMat = prilistapreciostemp.CodMat.
    FIND PriCanal WHERE pricanal.CodCia = s-CodCia AND
        pricanal.Canal = prilistapreciostemp.Canal 
        NO-LOCK.
    FIND PriCanalGrupo WHERE pricanalgrupo.CodCia = s-CodCia AND
        pricanalgrupo.Canal = prilistapreciostemp.Canal AND
        pricanalgrupo.Grupo = prilistapreciostemp.Grupo
        NO-LOCK.
    CREATE T-Precios.
    ASSIGN
        T-Precios.Fila = LocalFila
        T-Precios.CodMat = prilistapreciostemp.CodMat 
        T-Precios.Canal = prilistapreciostemp.Canal 
        T-Precios.Grupo = prilistapreciostemp.Grupo 
        T-Precios.PreUni = prilistapreciostemp.PreUni 
        T-Precios.PreGrupo = prilistapreciostemp.PreGrupo
        T-Precios.FactorGrupo = (pricanal.Factor / 100) * (pricanalgrupo.Factor / 100).
    GET NEXT {&browse-name}.
END.
/* Validamos */
DEF VAR x-PreGrupo LIKE prilistapreciostemp.PreGrupo NO-UNDO.
RLOOP:
FOR EACH T-Resumen NO-LOCK:
    x-PreGrupo = 0.
    FOR EACH T-Precios NO-LOCK WHERE T-Precios.CodMat = T-Resumen.CodMat
        BREAK BY T-Precios.CodMat BY T-Precios.FactorGrupo DESCENDING:
        IF FIRST-OF(T-Precios.CodMat) THEN x-PreGrupo = T-Precios.PreGrupo.
        IF x-PreGrupo < T-Precios.PreGrupo THEN DO:
            CREATE T-Errores.
            ASSIGN
                T-Errores.Fila   = T-Precios.Fila
                T-Errores.CodMat = T-Resumen.CodMat
                T-Errores.Glosa  = "Error Precio entre Canales de Negociaci�n".
        END.
        x-PreGrupo = T-Precios.PreGrupo.
    END.
END.
SESSION:SET-WAIT-STATE('').

IF CAN-FIND(FIRST T-Errores NO-LOCK) THEN DO:
    RUN pri/d-errores (INPUT TABLE T-Errores).
END.
ELSE DO:
    MESSAGE 'NO hay incosistencia entre de los precios de los canales de negociaci�n'
        VIEW-AS ALERT-BOX INFORMATION.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
