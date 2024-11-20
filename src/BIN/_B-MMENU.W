&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"browse generador de menús"
*/
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

DEFINE VARIABLE x-aplic-id AS CHARACTER.

DEFINE VARIABLE OK   AS LOGICAL NO-UNDO.
DEFINE VARIABLE FILA AS ROWID   NO-UNDO. 


DEFINE BUFFER b-G002 FOR PF-G002.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES PF-G003
&Scoped-define FIRST-EXTERNAL-TABLE PF-G003


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR PF-G003.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PF-G002

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table ~
FILL(' ',LENGTH(PF-G002.CODMNU) * 3 ) + integral.PF-G002.Etiqueta @ PF-G002.Etiqueta 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH PF-G002 OF PF-G003 WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH PF-G002 OF PF-G003 WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table PF-G002
&Scoped-define FIRST-TABLE-IN-QUERY-br_table PF-G002


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-1 br_table CMB-nivel Btn-arriba ~
Btn-abajo 
&Scoped-Define DISPLAYED-FIELDS PF-G003.Detalle 
&Scoped-define DISPLAYED-TABLES PF-G003
&Scoped-define FIRST-DISPLAYED-TABLE PF-G003
&Scoped-Define DISPLAYED-OBJECTS CMB-nivel 

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
<FOREIGN-KEYS></FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = ':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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
DEFINE BUTTON Btn-abajo 
     LABEL "A&bajo" 
     SIZE 8.43 BY .85.

DEFINE BUTTON Btn-arriba 
     LABEL "&Arriba" 
     SIZE 8.43 BY .85.

DEFINE VARIABLE CMB-nivel AS INTEGER FORMAT "9":U INITIAL 1 
     LABEL "Nivel" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3" 
     DROP-DOWN-LIST
     SIZE 5.14 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47.43 BY 11.15.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 11 BY 11.15.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      PF-G002 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      FILL(' ',LENGTH(PF-G002.CODMNU) * 3 ) + integral.PF-G002.Etiqueta
@ PF-G002.Etiqueta COLUMN-LABEL "O P C I O N E S    D E L    M E N U" FORMAT "X(70)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-ASSIGN SIZE 45.29 BY 10.65
         BGCOLOR 15 FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     PF-G003.Detalle AT ROW 1 COL 10.86 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP
          SIZE 49.57 BY .81
     br_table AT ROW 2.46 COL 2
     CMB-nivel AT ROW 3.5 COL 52.14 COLON-ALIGNED
     Btn-arriba AT ROW 6.85 COL 50.86
     Btn-abajo AT ROW 7.85 COL 50.86
     " Mover a:" VIEW-AS TEXT
          SIZE 7.14 BY .5 AT ROW 1.92 COL 50.43
     RECT-4 AT ROW 2.15 COL 49.29
     RECT-1 AT ROW 2.15 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: integral.PF-G003
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
         HEIGHT             = 12.46
         WIDTH              = 59.43.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table RECT-1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR PF-G003.Detalle IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       PF-G003.Detalle:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "integral.PF-G002 OF integral.PF-G003"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > "_<CALC>"
"FILL(' ',LENGTH(PF-G002.CODMNU) * 3 ) + integral.PF-G002.Etiqueta
@ PF-G002.Etiqueta" "O P C I O N E S    D E L    M E N U" "X(70)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    CMB-nivel:SCREEN-VALUE = STRING(LENGTH(PF-G002.CODMNU) / 2, "9").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-abajo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-abajo B-table-Win
ON CHOOSE OF Btn-abajo IN FRAME F-Main /* Abajo */
DO:
    OK = SESSION:SET-WAIT-STATE("GENERAL").
    IF br_table:SENSITIVE IN FRAME {&FRAME-NAME} THEN 
    DO on error undo, leave:
        RUN baja-registro.
        IF RETURN-VALUE <> "ERROR"
        THEN  DO:
            {&OPEN-QUERY-{&BROWSE-NAME}}
            REPOSITION {&BROWSE-NAME} TO ROWID FILA.
        END.
        ELSE BELL.
    END.
    OK = SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-arriba
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-arriba B-table-Win
ON CHOOSE OF Btn-arriba IN FRAME F-Main /* Arriba */
DO:
    OK = SESSION:SET-WAIT-STATE("GENERAL").  
    IF br_table:SENSITIVE IN FRAME {&FRAME-NAME} THEN 
    DO ON ERROR UNDO, LEAVE:
        RUN sube-registro.
        IF RETURN-VALUE <> "ERROR"
        THEN   DO:
            {&OPEN-QUERY-{&BROWSE-NAME}}
            REPOSITION {&BROWSE-NAME} TO ROWID FILA.
        END.
        ELSE BELL.
    END.
    OK = SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-nivel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-nivel B-table-Win
ON VALUE-CHANGED OF CMB-nivel IN FRAME F-Main /* Nivel */
DO:
    ASSIGN CMB-nivel.
    OK = SESSION:SET-WAIT-STATE("GENERAL").
    IF br_table:SENSITIVE IN FRAME {&FRAME-NAME} THEN 
    DO on error undo, leave:
        RUN cambia-nivel.
        IF RETURN-VALUE <> "ERROR"
        THEN  DO:
            {&OPEN-QUERY-{&BROWSE-NAME}}
            REPOSITION {&BROWSE-NAME} TO ROWID FILA.
        END.
        ELSE BELL.
    END.
    OK = SESSION:SET-WAIT-STATE("").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "PF-G003"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "PF-G003"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE baja-registro B-table-Win 
PROCEDURE baja-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE x-CodMnu-new  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE x-SubMnu      AS CHARACTER NO-UNDO INITIAL "".
    
    DEFINE VARIABLE x-CodMnu-old AS CHARACTER NO-UNDO.
    
    IF NOT AVAILABLE PF-G002 THEN RETURN "ERROR".
    
    FILA = ROWID( PF-G002).
    
    IF LENGTH(PF-G002.CODMNU) > 2
    THEN x-SubMnu = SUBSTR( PF-G002.CODMNU, 1, LENGTH(PF-G002.CODMNU) - 2).
    
    /* Buscando donde debe bajar */
    FIND b-G002 WHERE ROWID( b-g002 ) = FILA NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b-G002  THEN RETURN "ERROR".

    REPEAT:
        FIND NEXT b-G002 WHERE  b-G002.aplic-id = x-aplic-id NO-ERROR.
        IF NOT AVAILABLE b-G002  THEN RETURN "ERROR".
        /* sin incluir sus hijos */
        IF NOT b-G002.CodMnu BEGINS PF-G002.CodMnu THEN LEAVE.
    END.

/**** CASO 1 SE MUEVE DENTRO DEL MISMO SUB-MENU (PERMUTAR CODIGOS ) ****/
    IF     b-G002.CODMNU BEGINS x-SubMnu 
       AND LENGTH(b-G002.CODMNU) = LENGTH(PF-G002.CODMNU)
    THEN DO:
        FIND PF-G002 WHERE ROWID( pf-g002 ) = FILA NO-ERROR.
        IF NOT AVAILABLE PF-G002  THEN RETURN "ERROR".
        x-CodMnu-new =  B-G002.CODMNU.
        x-CodMnu-old = PF-G002.CODMNU.
        ASSIGN PF-G002.CODMNU = x-CodMnu-new.
        ASSIGN B-G002.CODMNU  = x-CodMnu-old.
        RUN MUEVE-SubMenu(x-CodMnu-new, x-CodMnu-old). 
        RETURN.
    END.

/**** CASO 2 SE MUEVE DENTRO DE OTRO SUB-MENU (ADICIONAR UN CODIGO) ****/
    IF B-G002.TIPO = "SUB-MENU" 
    THEN x-codmnu-new = B-G002.CODMNU + STRING( 1 , "99" ).
    ELSE x-codmnu-new = B-G002.CODMNU.
    
    RUN Renumera(x-codmnu-new).
    
    x-CodMnu-old = PF-G002.CODMNU.
    FIND PF-G002 WHERE ROWID( pf-g002 ) = FILA NO-ERROR.
    IF NOT AVAILABLE PF-G002  THEN RETURN "ERROR".
    ASSIGN PF-G002.CODMNU = x-CodMnu-new.
    RUN MUEVE-SubMenu(x-CodMnu-new, x-CodMnu-old). 
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cambia-nivel B-table-Win 
PROCEDURE cambia-nivel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE x-codmnu-new AS CHARACTER NO-UNDO.
    DEFINE VARIABLE x-CodMnu-old AS CHARACTER NO-UNDO.
    
    IF CMB-nivel = 1 AND
      (PF-G002.TIPO = "LINEA" OR
        PF-G002.TIPO = "SEPARADOR" )
    THEN DO:
        MESSAGE "La opción actual de menú" SKIP "no puede ser de primer nivel"
            VIEW-AS ALERT-BOX ERROR.
        RETURN "ERROR".
    END.
    
    IF CMB-nivel * 2 > LENGTH(PF-G002.CODMNU)
    THEN DO:
        MESSAGE "Invalido nivel seleccionado"
            VIEW-AS ALERT-BOX ERROR.
        RETURN "ERROR".
    END.

    FILA = ROWID( PF-G002).    
    x-CodMnu-new = SUBSTR(PF-G002.CODMNU, 1, CMB-nivel * 2 ).

    RUN Renumera(x-codmnu-new).
    
    x-CodMnu-old = PF-G002.CODMNU.
    FIND PF-G002 WHERE ROWID( pf-g002 ) = FILA NO-ERROR.
    IF NOT AVAILABLE PF-G002  THEN RETURN "ERROR".
    ASSIGN PF-G002.CODMNU = x-CodMnu-new.
    RUN MUEVE-SubMenu(x-CodMnu-new, x-CodMnu-old). 

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
  IF AVAILABLE PF-G003 
  THEN DO:
    DISPLAY PF-G003.DETALLE WITH FRAME {&FRAME-NAME}.  
    x-aplic-id = PF-G003.APLIC-ID.
  END. 
  ELSE DO:
    PF-G003.DETALLE:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
    x-aplic-id = ?.
  END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mueve-SubMenu B-table-Win 
PROCEDURE Mueve-SubMenu :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER x-CodMnu-new AS CHARACTER.
    DEFINE INPUT PARAMETER x-CodMnu-old AS CHARACTER.
    
    
    /* MEMORIA LOS SUB-MENUS DE ITEM A PERMUTAR */
    REPEAT:
        FIND FIRST b-G002 WHERE  b-G002.aplic-id = x-aplic-id 
                             AND b-G002.CodMnu BEGINS x-CodMnu-new
                             AND LENGTH(b-G002.CodMnu) > LENGTH(x-CodMnu-new)
                       NO-ERROR.
        IF NOT AVAILABLE b-G002  THEN LEAVE.
        ASSIGN b-G002.CodMnu = "_" + b-G002.CodMnu.
    END.
    
    /* MOVIENDO LOS SUB-MENUS DE ITEM ACTUAL */
    REPEAT:
        FIND FIRST b-G002 WHERE  b-G002.aplic-id = x-aplic-id 
                             AND b-G002.CodMnu BEGINS x-CodMnu-old
                             AND LENGTH(b-G002.CodMnu) > LENGTH(x-CodMnu-old)
                       NO-ERROR.
        IF NOT AVAILABLE b-G002  THEN LEAVE.
        ASSIGN b-G002.CodMnu = x-CodMnu-new + 
                SUBSTR( b-G002.CodMnu, LENGTH(x-CodMnu-old) + 1).
    END.

    /* MOVIENDO LOS SUB-MENUS DE ITEM A PERMUTAR */
    x-CodMnu-new = "_" + x-CodMnu-new.
    REPEAT:
        FIND FIRST b-G002 WHERE  b-G002.aplic-id = x-aplic-id 
                             AND b-G002.CodMnu BEGINS x-CodMnu-new
                       NO-ERROR.
        IF NOT AVAILABLE b-G002  THEN LEAVE.
        ASSIGN b-G002.CodMnu = x-CodMnu-old + 
                SUBSTR( b-G002.CodMnu, LENGTH(x-CodMnu-new) + 1).
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE renumera B-table-Win 
PROCEDURE renumera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER x-codmnu-new AS CHARACTER NO-UNDO.

    DEFINE VARIABLE x-SubMnu        AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE x-codMnu        AS CHARACTER NO-UNDO INITIAL "".
    DEFINE VARIABLE UltFila         AS ROWID NO-UNDO.
    DEFINE VARIABLE i               AS INTEGER NO-UNDO.

    IF LENGTH(x-CODMNU-NEW) > 2
    THEN x-SubMnu = SUBSTR( x-CODMNU-new, 1, LENGTH(x-CODMNU-new) - 2).
    
    FIND FIRST b-G002 WHERE  b-G002.aplic-id = x-aplic-id 
                       AND  b-G002.codmnu    = x-CodMnu-new  NO-ERROR.
    IF NOT AVAILABLE b-G002  THEN RETURN.
                           
    /* Buscando el final del sub-menu */    
    FIND LAST b-G002 WHERE  b-G002.aplic-id = x-aplic-id 
                       AND  b-G002.codmnu   BEGINS x-SubMnu  NO-ERROR.
    IF NOT AVAILABLE b-G002  THEN RETURN.

    /* RENUMERANDO DE ATRAS A ADELANTE */
    REPEAT:
        i = INTEGER(SUBSTR( b-G002.CODMNU, LENGTH(x-submnu) + 1, 2 )) + 1.
        x-codmnu = x-submnu + STRING( i, "99" ) + 
            SUBSTR( b-G002.CODMNU, LENGTH(x-submnu) + 3).
        ASSIGN b-G002.CODMNU = x-codmnu.
        FIND PREV b-g002 WHERE  b-g002.aplic-id  = x-aplic-id NO-ERROR.
        IF NOT AVAILABLE b-g002 THEN LEAVE.    
        IF NOT b-g002.codmnu  begins x-SubMnu THEN LEAVE.
        IF b-G002.codmnu < x-codmnu-new  THEN LEAVE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* There are no foreign keys supplied by this SmartObject. */

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
  {src/adm/template/snd-list.i "PF-G003"}
  {src/adm/template/snd-list.i "PF-G002"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sube-registro B-table-Win 
PROCEDURE sube-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE x-CodMnu-new  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE x-SubMnu      AS CHARACTER NO-UNDO INITIAL "".
    
    DEFINE VARIABLE x-CodMnu-old  AS CHARACTER NO-UNDO.
    
    IF NOT AVAILABLE PF-G002 THEN RETURN "ERROR".
    
    FILA = ROWID( PF-G002).
    
    IF LENGTH(PF-G002.CODMNU) > 2
    THEN x-SubMnu = SUBSTR( PF-G002.CODMNU, 1, LENGTH(PF-G002.CODMNU) - 2).
    
    /* Buscando donde debe subir */
    FIND b-G002 WHERE ROWID( b-g002 ) = FILA NO-LOCK NO-ERROR.
    IF NOT AVAILABLE b-G002  THEN RETURN "ERROR".

    FIND PREV b-G002 WHERE  b-G002.aplic-id = x-aplic-id NO-ERROR.
    IF NOT AVAILABLE b-G002  THEN RETURN "ERROR".

    IF LENGTH(B-G002.CODMNU) > 2
    THEN x-CodMnu-new = SUBSTR( B-G002.CODMNU, 1, LENGTH(PF-G002.CODMNU)).
    ELSE x-CodMnu-new = ?.
    
    IF     b-G002.CODMNU BEGINS x-SubMnu 
       AND LENGTH(b-G002.CODMNU) > LENGTH(PF-G002.CODMNU)
       AND x-CodMnu-new <> ?
    THEN REPEAT: /* Buscando el padre */
        FIND PREV b-G002 WHERE  b-G002.aplic-id = x-aplic-id NO-ERROR.
        IF NOT AVAILABLE b-G002  THEN RETURN "ERROR".
        
        IF b-G002.CodMnu = x-CodMnu-new THEN LEAVE.    
    END.

/**** CASO 1 SE MUEVE DENTRO DEL MISMO SUB-MENU (PERMUTAR CODIGOS ) ****/
    IF     b-G002.CODMNU BEGINS x-SubMnu 
       AND LENGTH(b-G002.CODMNU) = LENGTH(PF-G002.CODMNU)
    THEN DO:
        FIND PF-G002 WHERE ROWID( pf-g002 ) = FILA NO-ERROR.
        IF NOT AVAILABLE PF-G002  THEN RETURN "ERROR".
        x-CodMnu-new =  B-G002.CODMNU.
        x-CodMnu-old = PF-G002.CODMNU.
        ASSIGN PF-G002.CODMNU = x-CodMnu-new.
        ASSIGN B-G002.CODMNU  = x-CodMnu-old.
        RUN MUEVE-SubMenu(x-CodMnu-new, x-CodMnu-old). 
        RETURN.
    END.

/**** CASO 2 SE MUEVE A OTRO SUB-MENU (ADICIONAR UN CODIGO) ****/
    IF b-G002.CODMNU = x-SubMnu 
    THEN DO:
        FIND PREV b-G002 WHERE  b-G002.aplic-id = x-aplic-id NO-ERROR.
        IF NOT AVAILABLE b-G002  THEN RETURN "ERROR".
    END.
    IF b-G002.TIPO = "SUB-MENU"
    THEN x-codmnu-new = b-G002.CODMNU + STRING(1, "99").
    ELSE x-codmnu-new = SUBSTR( b-G002.CODMNU, 1, LENGTH(b-G002.CODMNU) - 2)
      + STRING( INTEGER(SUBSTR( b-G002.CODMNU, LENGTH(b-G002.CODMNU) - 1, 2 )) + 1, "99" ).
    x-CodMnu-old = PF-G002.CODMNU.
    FIND PF-G002 WHERE ROWID( pf-g002 ) = FILA NO-ERROR.
    IF NOT AVAILABLE PF-G002  THEN RETURN "ERROR".
    ASSIGN PF-G002.CODMNU = x-CodMnu-new.
    RUN MUEVE-SubMenu(x-CodMnu-new, x-CodMnu-old). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

