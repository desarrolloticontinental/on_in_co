&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-w-report NO-UNDO LIKE w-report.



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
DEFINE SHARED VAR s-codcia AS INT.

/* Local Variable Definitions ---                                       */
DEFINE VAR x-todas-lineas AS LOG INIT NO.

DEFINE VAR x-proceso AS CHAR.
DEFINE VAR x-motivo AS CHAR.

DEFINE BUFFER xtt-w-report FOR tt-w-report.
DEFINE TEMP-TABLE ytt-w-report LIKE w-report.
DEFINE BUFFER x-vtatabla FOR vtatabla.

DEFINE VAR x-hay-cambios AS LOG INIT NO.

DEFINE VAR x-tabla AS CHAR INIT "TIPO_DSCTO_CAMPANA".

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
&Scoped-define INTERNAL-TABLES tt-w-report

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-w-report.Campo-C[1] ~
tt-w-report.Campo-D[1] tt-w-report.Campo-D[2] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tt-w-report.Campo-C[1] ~
tt-w-report.Campo-D[1] tt-w-report.Campo-D[2] 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tt-w-report
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tt-w-report
&Scoped-define QUERY-STRING-br_table FOR EACH tt-w-report WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH tt-w-report WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table tt-w-report
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-w-report


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
      tt-w-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      tt-w-report.Campo-C[1] COLUMN-LABEL "Campa�a" FORMAT "X(6)":U
            WIDTH 7.29
      tt-w-report.Campo-D[1] COLUMN-LABEL "Desde" FORMAT "99/99/9999":U
      tt-w-report.Campo-D[2] COLUMN-LABEL "Hasta" FORMAT "99/99/9999":U
  ENABLE
      tt-w-report.Campo-C[1]
      tt-w-report.Campo-D[1]
      tt-w-report.Campo-D[2]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 35.86 BY 13.46
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1.04 COL 1.14
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
   Temp-Tables and Buffers:
      TABLE: tt-w-report T "?" NO-UNDO INTEGRAL w-report
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
         HEIGHT             = 14.08
         WIDTH              = 38.72.
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
       tt-w-report.Campo-D[1]:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       tt-w-report.Campo-D[2]:COLUMN-READ-ONLY IN BROWSE br_table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.tt-w-report"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > Temp-Tables.tt-w-report.Campo-C[1]
"tt-w-report.Campo-C[1]" "Campa�a" "X(6)" "character" ? ? ? ? ? ? yes ? no no "7.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tt-w-report.Campo-D[1]
"tt-w-report.Campo-D[1]" "Desde" ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tt-w-report.Campo-D[2]
"tt-w-report.Campo-D[2]" "Hasta" ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME tt-w-report.Campo-C[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-w-report.Campo-C[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF tt-w-report.Campo-C[1] IN BROWSE br_table /* Campa�a */
DO:

    DEFINE VAR x-campana AS CHAR.

    x-campana = SELF:SCREEN-VALUE IN BROWSE {&browse-name}.
    tt-w-report.campo-d[1]:SCREEN-VALUE IN BROWSE {&browse-name} = " / / ".
    tt-w-report.campo-d[2]:SCREEN-VALUE IN BROWSE {&browse-name} = " / / ".


    FIND FIRST vtatabla WHERE vtatabla.codcia = s-codcia AND 
                                vtatabla.tabla = "CAMPA�AS" AND
                                vtatabla.llave_c1 = x-campana NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vtatabla THEN DO:
        MESSAGE "Codigo de campa�a no existe" VIEW-AS ALERT-BOX INFORMATION.
        RETURN "ADM-ERROR".    
    END.

    tt-w-report.campo-d[1]:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(vtatabla.rango_fecha[1],"99/99/9999").
    tt-w-report.campo-d[2]:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(vtatabla.rango_fecha[2],"99/99/9999").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tt-w-report.Campo-C[1] br_table _BROWSE-COLUMN B-table-Win
ON LEFT-MOUSE-DBLCLICK OF tt-w-report.Campo-C[1] IN BROWSE br_table /* Campa�a */
DO:
 /*
  ASSIGN
    input-var-1 = ?
    input-var-2 = ?
    input-var-3 = ?
    output-var-1 = ?
    output-var-2 = ''
    output-var-3 = ''.


  RUN lkup/c-famili.r('LINEAS DE PRODUCTO').
  IF output-var-1 = ? THEN RETURN 'ADM-ERROR'.

  ASSIGN SELF:SCREEN-VALUE = output-var-2.

  */

END.

/*
APLIC\LKUP\C-ALMTAB.W
ASSIGN input-var-1 = "MK".
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON 'RETURN':U OF tt-w-report.campo-c[1], tt-w-report.campo-d[1], 
                tt-w-report.campo-d[2] IN BROWSE {&BROWSE-NAME} DO:
    APPLY 'TAB':U.
    RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-data B-table-Win 
PROCEDURE carga-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  SESSION:SET-WAIT-STATE("GENERAL").

  DO WITH FRAME {&FRAME-NAME}:

      EMPTY TEMP-TABLE tt-w-report.

      FOR EACH vtatabla WHERE vtatabla.codcia = s-codcia AND
                                vtatabla.tabla = x-tabla AND
                                vtatabla.llave_c1 = x-proceso NO-LOCK:
          CREATE tt-w-report.
          ASSIGN    tt-w-report.campo-c[1] = vtatabla.llave_c2
                    tt-w-report.campo-d[1] = ?
                    tt-w-report.campo-d[2] = ?
                    .
          
          FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND 
                                      x-vtatabla.tabla = "CAMPA�AS" AND
                                      x-vtatabla.llave_c1 = vtatabla.llave_c2 NO-LOCK NO-ERROR.
          IF AVAILABLE x-vtatabla THEN DO:
              ASSIGN tt-w-report.campo-d[1] = x-vtatabla.rango_fecha[1]
                    tt-w-report.campo-d[2] = x-vtatabla.rango_fecha[2].
          END.
      END.

      {&open-query-br_table}

  END.

  SESSION:SET-WAIT-STATE("").



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-valores B-table-Win 
PROCEDURE carga-valores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pProceso AS CHAR.
DEFINE INPUT PARAMETER pMotivo AS CHAR.

x-proceso = pProceso.
x-motivo = pMotivo.

x-hay-cambios = NO.

RUN carga-data.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-existen-cambios B-table-Win 
PROCEDURE get-existen-cambios :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER pRetVal AS LOG.

pRetVal = x-hay-cambios.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grabar B-table-Win 
PROCEDURE grabar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    MESSAGE 'Seguro de GRABAR las CAMPA�AS ?' VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO UPDATE rpta AS LOG.
    IF rpta = NO THEN RETURN NO-APPLY.


  SESSION:SET-WAIT-STATE("GENERAL").

  /*
    xtt-.... : Data que hemos modificado
    ytt-.... : Data como esta en la base de datos
  */

  DEFINE VAR x-msg AS CHAR INIT "OK".

  /* Cargar en memoria la data actual */
  EMPTY TEMP-TABLE ytt-w-report.

  FOR EACH vtatabla WHERE vtatabla.codcia = s-codcia AND
                                        vtatabla.tabla = x-tabla AND 
                                        vtatabla.llave_c1 = x-proceso NO-LOCK:
      CREATE ytt-w-report.
      ASSIGN    ytt-w-report.campo-c[1] = vtatabla.llave_c2
          .
  END.

  /* Seleccionamos aquellos que hay que eliminar */
  FOR EACH ytt-w-report :
      ASSIGN ytt-w-report.campo-c[10] = "".

      FIND FIRST xtt-w-report WHERE xtt-w-report.campo-c[1] = ytt-w-report.campo-c[1]  NO-LOCK NO-ERROR.

      IF NOT AVAILABLE xtt-w-report THEN DO:
            /* Ya no existe en la actualizacion que estamos realizando, Marcado para su eliminacion */
            ASSIGN ytt-w-report.campo-c[10] = "X".
      END.
  END.


  GRABACION:
  DO TRANSACTION ON ERROR UNDO GRABACION, LEAVE GRABACION:  
      DO:
    
          /* Procedemos a eliminar*/
          FOR EACH ytt-w-report WHERE ytt-w-report.campo-c[10] = "X" :
              FIND FIRST vtatabla WHERE vtatabla.codcia = s-codcia AND
                                            vtatabla.tabla = x-tabla AND
                                            vtatabla.llave_c1 = x-proceso AND
                                            vtatabla.llave_c2 = ytt-w-report.campo-c[1]
                                            EXCLUSIVE-LOCK NO-ERROR.
              IF AVAILABLE vtatabla THEN DO:
                  DELETE vtatabla NO-ERROR.
              END.

              IF ERROR-STATUS:ERROR THEN DO:
                  x-msg = ERROR-STATUS:GET-MESSAGE(1).
                  UNDO GRABACION, LEAVE GRABACION.
              END.
          END.
    
          /* Actualizacon/adicionamos el resto */
          FOR EACH xtt-w-report :
 
              FIND FIRST vtatabla WHERE vtatabla.codcia = s-codcia AND
                                            vtatabla.tabla = x-tabla AND
                                            vtatabla.llave_c1 = x-proceso AND
                                            vtatabla.llave_c2 = xtt-w-report.campo-c[1]
                                            EXCLUSIVE-LOCK NO-ERROR.
              IF NOT AVAILABLE vtatabla THEN DO:
                  CREATE vtatabla.
                    ASSIGN vtatabla.codcia = s-codcia
                            vtatabla.tabla = x-tabla
                            vtatabla.llave_c1 = x-proceso
                            vtatabla.llave_c2 = xtt-w-report.campo-c[1] NO-ERROR
                        .
              END.

              IF ERROR-STATUS:ERROR THEN DO:
                  x-msg = ERROR-STATUS:GET-MESSAGE(1).
                  UNDO GRABACION, LEAVE GRABACION.
              END.


              ASSIGN vtatabla.libre_c02 = USERID("DICTDB") + "|" + STRING(TODAY,"99/99/9999") + "|" + STRING(TIME,"HH:MM:SS")
                  NO-ERROR.
    
              IF ERROR-STATUS:ERROR THEN DO:
                  x-msg = ERROR-STATUS:GET-MESSAGE(1).
                  UNDO GRABACION, LEAVE GRABACION.
              END.

          END.
      END.
  END.

  RELEASE vtatabla.

  IF x-msg = 'OK' THEN DO:
      x-hay-cambios = NO.
      MESSAGE "Se grabaron los datos CORRECTAMENTE" VIEW-AS ALERT-BOX INFORMATION.
  END.
  ELSE DO:
      MESSAGE "Problemas al grabar los datos" SKIP
            x-msg
           VIEW-AS ALERT-BOX INFORMATION.
  END.

  SESSION:SET-WAIT-STATE("").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /*
  DEFINE VARIABLE hColumn AS HANDLE      NO-UNDO.

  hColumn = BROWSE br_table:GET-BROWSE-COLUMN(5).      /* Signo */

  IF x-todas-lineas = YES THEN DO:
      hColumn:READ-ONLY = YES.
  END.
  ELSE DO:
      hColumn:READ-ONLY = NO.
  END.
  */

END PROCEDURE.
/*
        DEFINE VARIABLE hColumn AS HANDLE      NO-UNDO.

  hColumn = BROWSE bCust:GET-BROWSE-COLUMN(INTEGER(cbColumns:SCREEN-VALUE)).
  hColumn:VISIBLE = (IF hColumn:VISIBLE EQ TRUE THEN FALSE ELSE TRUE).
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry B-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

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

  
  x-hay-cambios = YES.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  DEFINE VAR x-linea AS CHAR.

  x-linea = tt-w-report.campo-c[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  IF x-linea = '*' THEN x-todas-lineas = NO.
  
  x-hay-cambios = YES.

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

    
    CASE HANDLE-CAMPO:LABEL:
        WHEN "MARCA" THEN 
            ASSIGN input-var-1 = "MK".
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
  {src/adm/template/snd-list.i "tt-w-report"}

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

DEFINE VAR x-campana AS CHAR.

x-campana = tt-w-report.campo-c[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

IF TRUE <> (x-campana > "")  THEN DO:
    MESSAGE "Ingrese la Campa�a" VIEW-AS ALERT-BOX INFORMATION.
    RETURN "ADM-ERROR".    
END.

FIND FIRST vtatabla WHERE vtatabla.codcia = s-codcia AND 
                            vtatabla.tabla = "CAMPA�AS" AND
                            vtatabla.llave_c1 = x-campana NO-LOCK NO-ERROR.
IF NOT AVAILABLE vtatabla THEN DO:
    MESSAGE "Codigo de campa�a no existe" VIEW-AS ALERT-BOX INFORMATION.
    RETURN "ADM-ERROR".    
END.

FIND FIRST xtt-w-report WHERE xtt-w-report.campo-c[1] = x-campana NO-LOCK NO-ERROR.
IF AVAILABLE xtt-w-report THEN DO:
    MESSAGE "Codigo de campa�a ya esta registrado" VIEW-AS ALERT-BOX INFORMATION.
    RETURN "ADM-ERROR".    
END.

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

MESSAGE "No habilitado!!!" VIEW-AS ALERT-BOX INFORMATION.

RETURN "ADM-ERROR".

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
