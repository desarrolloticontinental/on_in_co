&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER SUBZONA FOR VtaDTabla.
DEFINE TEMP-TABLE T-FacTabla NO-UNDO LIKE FacTabla
       INDEX Llave01 AS PRIMARY CodCia Tabla Codigo.
DEFINE BUFFER ZONA FOR VtaCTabla.



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
DEF SHARED VAR s-Llave-Zona AS CHAR.
/* Local Variable Definitions ---                                       */

DEF SHARED VAR s-codcia AS INT.

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
&Scoped-define EXTERNAL-TABLES GN-DIVI
&Scoped-define FIRST-EXTERNAL-TABLE GN-DIVI


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR GN-DIVI.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES T-FacTabla ZONA SUBZONA

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ZONA.Descripcion SUBZONA.Libre_c02 ~
T-FacTabla.Campo-L[2] T-FacTabla.Campo-L[3] T-FacTabla.Campo-L[4] ~
T-FacTabla.Campo-L[5] T-FacTabla.Campo-L[6] T-FacTabla.Campo-L[7] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table T-FacTabla.Campo-L[2] ~
T-FacTabla.Campo-L[3] T-FacTabla.Campo-L[4] T-FacTabla.Campo-L[5] ~
T-FacTabla.Campo-L[6] T-FacTabla.Campo-L[7] 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table T-FacTabla
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table T-FacTabla
&Scoped-define QUERY-STRING-br_table FOR EACH T-FacTabla WHERE TRUE /* Join to GN-DIVI incomplete */ NO-LOCK, ~
      EACH ZONA WHERE ZONA.CodCia = T-FacTabla.CodCia ~
  AND ZONA.Llave = T-FacTabla.Tabla ~
      AND ZONA.CodCia = T-FacTabla.CodCia ~
  AND ZONA.Llave = T-FacTabla.Tabla ~
 AND ZONA.Tabla = "ZGHR" NO-LOCK, ~
      EACH SUBZONA WHERE SUBZONA.CodCia = T-FacTabla.CodCia ~
  AND SUBZONA.Llave = T-FacTabla.Tabla ~
  AND SUBZONA.Tipo = T-FacTabla.Codigo ~
      AND SUBZONA.Tabla = "SZGHR" ~
 AND SUBZONA.LlaveDetalle = "C" NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH T-FacTabla WHERE TRUE /* Join to GN-DIVI incomplete */ NO-LOCK, ~
      EACH ZONA WHERE ZONA.CodCia = T-FacTabla.CodCia ~
  AND ZONA.Llave = T-FacTabla.Tabla ~
      AND ZONA.CodCia = T-FacTabla.CodCia ~
  AND ZONA.Llave = T-FacTabla.Tabla ~
 AND ZONA.Tabla = "ZGHR" NO-LOCK, ~
      EACH SUBZONA WHERE SUBZONA.CodCia = T-FacTabla.CodCia ~
  AND SUBZONA.Llave = T-FacTabla.Tabla ~
  AND SUBZONA.Tipo = T-FacTabla.Codigo ~
      AND SUBZONA.Tabla = "SZGHR" ~
 AND SUBZONA.LlaveDetalle = "C" NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table T-FacTabla ZONA SUBZONA
&Scoped-define FIRST-TABLE-IN-QUERY-br_table T-FacTabla
&Scoped-define SECOND-TABLE-IN-QUERY-br_table ZONA
&Scoped-define THIRD-TABLE-IN-QUERY-br_table SUBZONA


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
      T-FacTabla, 
      ZONA, 
      SUBZONA SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      ZONA.Descripcion COLUMN-LABEL "Zona" FORMAT "x(25)":U
      SUBZONA.Libre_c02 COLUMN-LABEL "SubZona" FORMAT "x(25)":U
      T-FacTabla.Campo-L[2] COLUMN-LABEL "Lunes" FORMAT "yes/no":U
            WIDTH 8.29 VIEW-AS TOGGLE-BOX
      T-FacTabla.Campo-L[3] COLUMN-LABEL "Martes" FORMAT "yes/no":U
            WIDTH 8.43 VIEW-AS TOGGLE-BOX
      T-FacTabla.Campo-L[4] COLUMN-LABEL "Mi�rcoles" FORMAT "yes/no":U
            WIDTH 9.43 VIEW-AS TOGGLE-BOX
      T-FacTabla.Campo-L[5] COLUMN-LABEL "Jueves" FORMAT "yes/no":U
            WIDTH 8.43 VIEW-AS TOGGLE-BOX
      T-FacTabla.Campo-L[6] COLUMN-LABEL "Viernes" FORMAT "yes/no":U
            WIDTH 8.43 VIEW-AS TOGGLE-BOX
      T-FacTabla.Campo-L[7] COLUMN-LABEL "S�bado" FORMAT "yes/no":U
            WIDTH 8.43 COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 14 VIEW-AS TOGGLE-BOX
  ENABLE
      T-FacTabla.Campo-L[2]
      T-FacTabla.Campo-L[3]
      T-FacTabla.Campo-L[4]
      T-FacTabla.Campo-L[5]
      T-FacTabla.Campo-L[6]
      T-FacTabla.Campo-L[7]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 96 BY 13.46
         FONT 4.


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
   External Tables: INTEGRAL.GN-DIVI
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: SUBZONA B "?" ? INTEGRAL VtaDTabla
      TABLE: T-FacTabla T "?" NO-UNDO INTEGRAL FacTabla
      ADDITIONAL-FIELDS:
          INDEX Llave01 AS PRIMARY CodCia Tabla Codigo
      END-FIELDS.
      TABLE: ZONA B "?" ? INTEGRAL VtaCTabla
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
         HEIGHT             = 15.54
         WIDTH              = 119.14.
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
     _TblList          = "Temp-Tables.T-FacTabla Where INTEGRAL.GN-DIVI ...,ZONA WHERE Temp-Tables.T-FacTabla ...,SUBZONA WHERE Temp-Tables.T-FacTabla ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _JoinCode[2]      = "ZONA.CodCia = Temp-Tables.T-FacTabla.CodCia
  AND ZONA.Llave = Temp-Tables.T-FacTabla.Tabla"
     _Where[2]         = "ZONA.CodCia = Temp-Tables.T-FacTabla.CodCia
  AND ZONA.Llave = Temp-Tables.T-FacTabla.Tabla
 AND ZONA.Tabla = ""ZGHR"""
     _JoinCode[3]      = "SUBZONA.CodCia = Temp-Tables.T-FacTabla.CodCia
  AND SUBZONA.Llave = Temp-Tables.T-FacTabla.Tabla
  AND SUBZONA.Tipo = Temp-Tables.T-FacTabla.Codigo"
     _Where[3]         = "SUBZONA.Tabla = ""SZGHR""
 AND SUBZONA.LlaveDetalle = ""C"""
     _FldNameList[1]   > Temp-Tables.ZONA.Descripcion
"ZONA.Descripcion" "Zona" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.SUBZONA.Libre_c02
"SUBZONA.Libre_c02" "SubZona" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.T-FacTabla.Campo-L[2]
"T-FacTabla.Campo-L[2]" "Lunes" ? "logical" ? ? ? ? ? ? yes ? no no "8.29" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[4]   > Temp-Tables.T-FacTabla.Campo-L[3]
"T-FacTabla.Campo-L[3]" "Martes" ? "logical" ? ? ? ? ? ? yes ? no no "8.43" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[5]   > Temp-Tables.T-FacTabla.Campo-L[4]
"T-FacTabla.Campo-L[4]" "Mi�rcoles" ? "logical" ? ? ? ? ? ? yes ? no no "9.43" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[6]   > Temp-Tables.T-FacTabla.Campo-L[5]
"T-FacTabla.Campo-L[5]" "Jueves" ? "logical" ? ? ? ? ? ? yes ? no no "8.43" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[7]   > Temp-Tables.T-FacTabla.Campo-L[6]
"T-FacTabla.Campo-L[6]" "Viernes" ? "logical" ? ? ? ? ? ? yes ? no no "8.43" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[8]   > Temp-Tables.T-FacTabla.Campo-L[7]
"T-FacTabla.Campo-L[7]" "S�bado" ? "logical" 14 0 ? ? ? ? yes ? no no "8.43" yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
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
  {src/adm/template/row-list.i "GN-DIVI"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "GN-DIVI"}

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

EMPTY TEMP-TABLE T-Factabla.
FOR EACH VtaCTabla NO-LOCK WHERE VtaCTabla.CodCia = s-codcia
    AND VtaCTabla.Tabla = 'ZGHR',
    EACH VtaDTabla NO-LOCK WHERE VtaDTabla.CodCia = s-codcia 
    AND VtaDTabla.Tabla = "SZGHR"    /* Sub-Zona Geografica por Hoja de Ruta */
    AND VtaDTabla.Llave = VtaCTabla.Llave   /* Zona */
    AND VtaDTabla.LlaveDetalle = "C":       /* A nivel de SubZonas */
    CREATE T-Factabla.
    ASSIGN
        T-FacTabla.CodCia = s-codcia
        T-FacTabla.Tabla  = VtaDTabla.Llave     /* Zona */
        T-FacTabla.Codigo = VtaDTabla.Tipo      /* SubZona */
        .
END.
FOR EACH T-FacTabla:
    FOR EACH Vtatabla NO-LOCK WHERE VtaTabla.CodCia = s-codcia
        AND VtaTabla.Tabla = s-Llave-Zona       /*"SZGHRXDIA"*/
        AND VtaTabla.Llave_c1 = gn-divi.coddiv  /*s-coddiv*/
        AND VtaTabla.Llave_c2 = T-FacTabla.Tabla    /* Zona */
        AND VtaTabla.Llave_c3 = T-FacTabla.Codigo:  /* SubZona */
        T-FacTabla.Campo-L[INT(VtaTabla.Valor[1])] = YES.
    END.
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
  /* 1ro. Borramos */
  FOR EACH Vtatabla WHERE VtaTabla.CodCia = s-codcia
      AND VtaTabla.Tabla = s-Llave-Zona     /*"SZGHRXDIA"*/
      AND VtaTabla.Llave_c1 = gn-div.coddiv     /*s-coddiv*/
      AND VtaTabla.Llave_c2 = T-FacTabla.Tabla
      AND VtaTabla.Llave_c3 = T-FacTabla.Codigo:
      DELETE VtaTabla.
  END.
  /* 2do. Grabamos */
  DEF VAR k AS INT NO-UNDO.
  DO k = 1 TO 7:
      IF T-FacTabla.Campo-L[k] THEN DO:
          CREATE VtaTabla.
          ASSIGN
              VtaTabla.CodCia = s-codcia
              VtaTabla.Tabla  = s-Llave-Zona     /*"SZGHRXDIA"*/
              VtaTabla.Llave_c1 = gn-divi.coddiv /*s-coddiv */
              VtaTabla.Llave_c2 = T-FacTabla.Tabla
              VtaTabla.Llave_c3 = T-FacTabla.Codigo
              VtaTabla.Valor[1] = k.
      END.
  END.
  RELEASE VtaTabla.

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
  RUN Carga-Temporal.

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
  {src/adm/template/snd-list.i "GN-DIVI"}
  {src/adm/template/snd-list.i "T-FacTabla"}
  {src/adm/template/snd-list.i "ZONA"}
  {src/adm/template/snd-list.i "SUBZONA"}

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

