&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-Mserv FOR almmserv.



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

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-user-id AS CHAR.

DEF VAR s-GrabaLog AS LOG INIT FALSE NO-UNDO.

DEF TEMP-TABLE Detalle 
    FIELD codmat LIKE almmserv.codmat      FORMAT 'x(6)'       COLUMN-LABEL 'Cuenta'
    FIELD estado LIKE almmserv.estado     FORMAT 'x'          COLUMN-LABEL 'Estado'
    FIELD desmat LIKE almmserv.desmat      FORMAT 'x(45)'      COLUMN-LABEL 'Descripcion'
    FIELD undbas LIKE almmserv.undbas      FORMAT 'x(6)'       COLUMN-LABEL 'Unidad Basica'
    FIELD fching LIKE almmserv.fching      FORMAT '99/99/9999' COLUMN-LABEL 'Fecha Ing.'
    FIELD fchact LIKE almmserv.fchact      FORMAT '99/99/9999' COLUMN-LABEL 'Fecha Act.'
    FIELD usuario LIKE almmserv.usuario     FORMAT 'x(12)'      COLUMN-LABEL 'Usuario'
    FIELD aftigv LIKE almmserv.aftigv      FORMAT 'Si/No'      COLUMN-LABEL 'I.G.V.'
    FIELD aftdetraccion LIKE almmserv.aftdetraccion FORMAT 'Si/No'     COLUMN-LABEL 'Detraccion'
    FIELD monvta LIKE almmserv.monvta       FORMAT '9'          COLUMN-LABEL 'Moneda Venta'
    FIELD ctolis LIKE almmserv.ctolis       FORMAT "->>>,>>9.9999"  COLUMN-LABEL 'Precio Costo Lista S/IGV'
    FIELD ctotot LIKE almmserv.ctotot       FORMAT "->>>,>>9.9999"  COLUMN-LABEL 'Precio Costo Lista Total'
    FIELD preofi LIKE almmserv.preofi       FORMAT ">,>>>,>>9.9999" COLUMN-LABEL 'Precio Oficina'

    .

&SCOPED-DEFINE Condicion (almmserv.codcia = s-codcia)

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES almmserv

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table almmserv.codmat almmserv.Estado ~
almmserv.DesMat almmserv.UndBas almmserv.FchIng almmserv.FchAct ~
almmserv.usuario almmserv.AftIgv almmserv.AftDetraccion almmserv.MonVta ~
almmserv.CtoLis almmserv.CtoTot almmserv.PreOfi 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table almmserv.Estado ~
almmserv.DesMat almmserv.UndBas almmserv.AftIgv almmserv.AftDetraccion ~
almmserv.MonVta almmserv.CtoLis almmserv.CtoTot almmserv.PreOfi 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table almmserv
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table almmserv
&Scoped-define QUERY-STRING-br_table FOR EACH almmserv WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH almmserv WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table almmserv
&Scoped-define FIRST-TABLE-IN-QUERY-br_table almmserv


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
      almmserv SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      almmserv.codmat COLUMN-LABEL "Cuenta" FORMAT "X(6)":U WIDTH 6
      almmserv.Estado FORMAT "X(1)":U
      almmserv.DesMat FORMAT "X(45)":U
      almmserv.UndBas FORMAT "X(4)":U
      almmserv.FchIng FORMAT "99/99/9999":U
      almmserv.FchAct COLUMN-LABEL "Fecha Act." FORMAT "99/99/9999":U
      almmserv.usuario COLUMN-LABEL "Usuario" FORMAT "x(11)":U
      almmserv.AftIgv FORMAT "Si/No":U VIEW-AS TOGGLE-BOX
      almmserv.AftDetraccion COLUMN-LABEL "Detraccion" FORMAT "yes/no":U
            VIEW-AS TOGGLE-BOX
      almmserv.MonVta FORMAT "9":U
      almmserv.CtoLis COLUMN-LABEL "Precio Costo Lista!S/IGV" FORMAT "->>>,>>9.9999":U
      almmserv.CtoTot COLUMN-LABEL "Precio Costo Lista!Total" FORMAT "->>>,>>9.9999":U
      almmserv.PreOfi FORMAT ">,>>>,>>9.9999":U WIDTH 14.14
  ENABLE
      almmserv.Estado HELP "A: Activado  D: Desactivado"
      almmserv.DesMat
      almmserv.UndBas HELP "Unidad Base"
      almmserv.AftIgv HELP "Incluido IGV"
      almmserv.AftDetraccion
      almmserv.MonVta HELP "1: Soles   2: D�lares"
      almmserv.CtoLis
      almmserv.CtoTot
      almmserv.PreOfi
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 139 BY 21.81
         FONT 4
         TITLE "Maestro de Servicios" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 2
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
   Temp-Tables and Buffers:
      TABLE: B-Mserv B "?" ? INTEGRAL almmserv
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
         HEIGHT             = 22.15
         WIDTH              = 145.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmbrowser.i}
{src/adm/method/browser.i}
{src/adm-vm/method/vmviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 5
       br_table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.almmserv"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "{&Condicion}"
     _FldNameList[1]   > INTEGRAL.almmserv.codmat
"almmserv.codmat" "Cuenta" ? "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.almmserv.Estado
"almmserv.Estado" ? ? "character" ? ? ? ? ? ? yes "A: Activado  D: Desactivado" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.almmserv.DesMat
"almmserv.DesMat" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.almmserv.UndBas
"almmserv.UndBas" ? ? "character" ? ? ? ? ? ? yes "Unidad Base" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = INTEGRAL.almmserv.FchIng
     _FldNameList[6]   > INTEGRAL.almmserv.FchAct
"almmserv.FchAct" "Fecha Act." ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > INTEGRAL.almmserv.usuario
"almmserv.usuario" "Usuario" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > INTEGRAL.almmserv.AftIgv
"almmserv.AftIgv" ? ? "logical" ? ? ? ? ? ? yes "Incluido IGV" no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[9]   > INTEGRAL.almmserv.AftDetraccion
"almmserv.AftDetraccion" "Detraccion" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[10]   > INTEGRAL.almmserv.MonVta
"almmserv.MonVta" ? ? "integer" ? ? ? ? ? ? yes "1: Soles   2: D�lares" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > INTEGRAL.almmserv.CtoLis
"almmserv.CtoLis" "Precio Costo Lista!S/IGV" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > INTEGRAL.almmserv.CtoTot
"almmserv.CtoTot" "Precio Costo Lista!Total" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > INTEGRAL.almmserv.PreOfi
"almmserv.PreOfi" ? ? "decimal" ? ? ? ? ? ? yes ? no no "14.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Maestro de Servicios */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Maestro de Servicios */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Maestro de Servicios */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME almmserv.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL almmserv.Estado br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF almmserv.Estado IN BROWSE br_table /* Estado */
DO:
 IF LOOKUP (Almmserv.Estado:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},'A,D') = 0 THEN DO:
    MESSAGE "Ingrese Estado valido" VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
 END.
 Almmserv.Estado:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = CAPS(Almmserv.Estado:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME almmserv.UndBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL almmserv.UndBas br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF almmserv.UndBas IN BROWSE br_table /* Unidad!Basica */
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME almmserv.MonVta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL almmserv.MonVta br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF almmserv.MonVta IN BROWSE br_table /* Moneda!venta */
DO:
 IF LOOKUP (Almmserv.Monvta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},'1,2') = 0 THEN DO:
    MESSAGE "Ingrese Monvta valido" VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
 END.
 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Export-to-Excel B-table-Win 
PROCEDURE Export-to-Excel :
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
    FOR EACH almmserv NO-LOCK WHERE {&Condicion}:
        CREATE Detalle.
        BUFFER-COPY almmserv TO Detalle.
    END.

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

    MESSAGE 'Proceso Concluido' VIEW-AS ALERT-BOX INFORMATION.
    RUN dispatch IN THIS-PROCEDURE ('open-query':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Log B-table-Win 
PROCEDURE Graba-Log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF s-GrabaLog THEN DO:
    CREATE Almlserv.
    BUFFER-COPY Almmserv TO Almlserv
        ASSIGN
            almlserv.FchAct = TODAY
            almlserv.HorAct = STRING(TIME, 'HH:MM')
            almlserv.usuario = s-user-id.
    RELEASE Almlserv.            
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR x-CodMat AS INT INIT 1 NO-UNDO.
  
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES'
  THEN DO:      /* Buscamos correlativo */
    FIND LAST B-MSERV WHERE B-MSERV.codcia = s-codcia AND
        B-MSERV.codmat <> '' EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE B-MSERV THEN x-CodMat = INTEGER(B-MSERV.CodMat) + 1.
  END.
  ELSE DO:
    x-CodMat = INTEGER(Almmserv.codmat).
  END.
  
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES'
  THEN DO:
    ASSIGN
        Almmserv.codcia = s-codcia
        Almmserv.codmat = STRING(x-codmat, '999999')
        Almmserv.fching = TODAY.
  END.        
  ELSE
    ASSIGN
        Almmserv.fchact = TODAY.
  ASSIGN
    Almmserv.usuario = s-user-id.

  RUN Graba-Log.
  
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

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

  DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
    /*RUN PL/C-XXX.W("").*/
    RUN VTA/C-mserv.w .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
/*  IF RETURN-VALUE = "ADM-ERROR" THEN UNDO,
 *   RETURN "ADM-ERROR".*/
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

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
  IF RETURN-VALUE = "ADM-ERROR" THEN UNDO, RETURN "ADM-ERROR".
  
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
  {src/adm/template/snd-list.i "almmserv"}

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
  Notes:  EN CASO DE ERROR RETORNAR : RETURN "ADM-ERROR"
------------------------------------------------------------------------------*/
  IF almmser.desmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "" THEN DO:
    MESSAGE "Ingrese Descripci�n de Servicio" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO Almmserv.DesMat.
    RETURN "ADM-ERROR".
  END.
  FIND Unidades WHERE Unidades.Codunid = Almmserv.UndBas:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Unidades THEN DO:
        MESSAGE "Unidad no registrada ..." VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO Almmserv.UndBas.
        RETURN "ADM-ERROR".   
  END.
/*   IF DECIMAL(almmser.CtoLis:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO: */
/*     MESSAGE "Ingrese Precio diferente a 0" VIEW-AS ALERT-BOX ERROR.             */
/*     APPLY "ENTRY" TO Almmserv.CtoLis.                                           */
/*     RETURN "ADM-ERROR".                                                         */
/*   END.                                                                          */
  IF DECIMAL(almmser.PreOfi:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
    MESSAGE "Ingrese Precio diferente a 0" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO Almmserv.PreOfi.
    RETURN "ADM-ERROR".
  END.
  /* DEFINIMOS SI ACTUALIZAMOS EL LOG DE DATOS */
  s-GrabaLog = NO.
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'NO' THEN DO:
    IF Almmserv.desmat <> Almmserv.desmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} THEN s-GrabaLog = YES.
    IF Almmserv.undbas <> Almmserv.undbas:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} THEN s-GrabaLog = YES.
    IF Almmserv.ctolis <> DECIMAL(Almmserv.ctolis:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN s-GrabaLog = YES.
    IF Almmserv.preofi <> DECIMAL(Almmserv.preofi:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN s-GrabaLog = YES.
  END.
  RETURN "OK".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida-Datos B-table-Win 
PROCEDURE Valida-Datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
 *     IF almmser.desmat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "" THEN DO:
 *       MESSAGE "Ingrese Descripci�n de Servicio" VIEW-AS ALERT-BOX WARNING.
 *       APPLY "ENTRY" TO Almmserv.DesMat.
 *       RETURN "ADM-ERROR".
 *     END.
 *   
 *     IF DECIMAL(almmser.CtoLis:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
 *       MESSAGE "Ingrese Precio" VIEW-AS ALERT-BOX WARNING.
 *       APPLY "ENTRY" TO Almmserv.CtoLis.
 *       RETURN "ADM-ERROR".
 *     END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update B-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Consistenciar la modificacion de la fila
  Parameters:  Retornar "ADM-ERROR" en caso de bloquear la modificacion
  Notes:       
------------------------------------------------------------------------------*/
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

