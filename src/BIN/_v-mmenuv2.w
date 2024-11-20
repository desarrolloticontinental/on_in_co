&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"Visualizador generador de menús"
*/
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

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

DEFINE VARIABLE X-aplic-id AS CHARACTER.

DEFINE VARIABLE OK            AS LOGICAL NO-UNDO.
DEFINE VARIABLE OKpressed     AS LOGICAL INITIAL TRUE NO-UNDO.
DEFINE VARIABLE path-main     AS CHARACTER NO-UNDO.
DEFINE VARIABLE procname      AS CHARACTER NO-UNDO.
DEFINE VARIABLE direc-inicial AS CHARACTER NO-UNDO.
DEFINE VARIABLE codmnu-new    AS CHARACTER NO-UNDO.
DEFINE VARIABLE opcion-act    AS CHARACTER NO-UNDO.
DEFINE VARIABLE tipo-act      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lista-grupos  AS CHARACTER NO-UNDO.
DEFINE VARIABLE i             AS INTEGER   NO-UNDO.

FILE-INFO:FILE-NAME = ".".
path-main = FILE-INFO:FULL-PATHNAME.

DEFINE BUFFER b-G002 FOR PF-G002a.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES PF-G002a PF-G003
&Scoped-define FIRST-EXTERNAL-TABLE PF-G002a


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR PF-G002a, PF-G003.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS pf-g002a.Tipo pf-g002a.Etiqueta ~
pf-g002a.Persistente pf-g002a.Programa pf-g002a.Parametros ~
pf-g002a.Seguridad-Grupos 
&Scoped-define ENABLED-TABLES pf-g002a
&Scoped-define FIRST-ENABLED-TABLE pf-g002a
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-1 
&Scoped-Define DISPLAYED-FIELDS pf-g002a.Tipo pf-g002a.Etiqueta ~
pf-g002a.Persistente pf-g002a.Programa pf-g002a.Parametros ~
pf-g002a.Seguridad-Grupos 
&Scoped-define DISPLAYED-TABLES pf-g002a
&Scoped-define FIRST-DISPLAYED-TABLE pf-g002a
&Scoped-Define DISPLAYED-OBJECTS COMBO-nivel 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-grupos 
     IMAGE-UP FILE "img\pvbuscar":U
     LABEL "" 
     SIZE 4.29 BY 1.

DEFINE BUTTON Btn-program 
     IMAGE-UP FILE "img\pvbuscar":U
     LABEL "" 
     SIZE 4.29 BY 1.

DEFINE VARIABLE COMBO-nivel AS INTEGER FORMAT "9":U INITIAL 1 
     LABEL "Nivel" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3" 
     DROP-DOWN-LIST
     SIZE 5.14 BY 1
     BGCOLOR 0 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 2.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 3.77.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-nivel AT ROW 1.23 COL 6.57 COLON-ALIGNED
     pf-g002a.Tipo AT ROW 1.23 COL 20.72 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 6
          LIST-ITEMS "SUB-MENU","SEPARADOR","LINEA","PROCESO" 
          DROP-DOWN-LIST
          SIZE 24.14 BY 1
     pf-g002a.Etiqueta AT ROW 2.12 COL 20.72 COLON-ALIGNED
          LABEL "Etiqueta" FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 43 BY .81
     pf-g002a.Persistente AT ROW 3.31 COL 3
          VIEW-AS TOGGLE-BOX
          SIZE 11.14 BY .69
     pf-g002a.Programa AT ROW 3.31 COL 21 COLON-ALIGNED FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 43 BY .81
     Btn-program AT ROW 3.35 COL 66
     pf-g002a.Parametros AT ROW 4.08 COL 21 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 22.86 BY .81
     Btn-grupos AT ROW 4.85 COL 66.29
     pf-g002a.Seguridad-Grupos AT ROW 5 COL 23 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 43 BY 1.73
     "Grupos de Acceso" VIEW-AS TEXT
          SIZE 13 BY .5 AT ROW 5.04 COL 9
     RECT-2 AT ROW 3.19 COL 1
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.PF-G002a,integral.PF-G003
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 9.42
         WIDTH              = 74.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn-grupos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn-program IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX COMBO-nivel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN pf-g002a.Etiqueta IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN pf-g002a.Programa IN FRAME F-Main
   EXP-FORMAT                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-grupos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-grupos V-table-Win
ON CHOOSE OF Btn-grupos IN FRAME F-Main
DO:
    lista-grupos = "".
    RUN bin/_grupos.w( OUTPUT lista-grupos ).
    IF lista-grupos <> "" OR lista-grupos <> ? THEN
        DO i = 1 TO NUM-ENTRIES(lista-grupos):
            IF LOOKUP(ENTRY(i,lista-grupos),PF-G002a.Seguridad-Grupos:SCREEN-VALUE) = 0 THEN DO:
                IF PF-G002a.Seguridad-Grupos:SCREEN-VALUE = "" THEN
                    PF-G002a.Seguridad-Grupos:SCREEN-VALUE = ENTRY(i,lista-grupos).
                ELSE
                    PF-G002a.Seguridad-Grupos:SCREEN-VALUE =
                    PF-G002a.Seguridad-Grupos:SCREEN-VALUE + "," +
                    ENTRY(i,lista-grupos).
            END.
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-program
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-program V-table-Win
ON CHOOSE OF Btn-program IN FRAME F-Main
DO:
 
    FILE-INFO:FILE-NAME = PF-G002a.Programa:SCREEN-VALUE.
    direc-inicial =
        IF ( FILE-INFO:FULL-PATHNAME = ? OR PF-G002a.Programa:SCREEN-VALUE = "") THEN "apl"
        ELSE SUBSTR( FILE-INFO:FULL-PATHNAME, 1, R-INDEX(FILE-INFO:FULL-PATHNAME, "\") ).
    procname = SUBSTR( PF-G002a.Programa:SCREEN-VALUE,
        R-INDEX(PF-G002a.Programa:SCREEN-VALUE, "\") + 1 ).

    SYSTEM-DIALOG GET-FILE procname
    TITLE   "Selecione el programa a ejecutar ..."
    FILTERS "SmartWindows  (w-*.r)"   "w-*.r",
            "Otros programas (*.r)"   "*.r"
    INITIAL-DIR direc-inicial
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.

    IF OKpressed = TRUE THEN DO:
        IF procname BEGINS path-main THEN
            procname = SUBSTR( procname, LENGTH(path-main) + 2).
        IF procname BEGINS "apl" THEN procname = SUBSTR( procname, 7).
        PF-G002a.Programa:SCREEN-VALUE = procname.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-nivel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-nivel V-table-Win
ON VALUE-CHANGED OF COMBO-nivel IN FRAME F-Main /* Nivel */
DO:
    ASSIGN COMBO-nivel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pf-g002a.Etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pf-g002a.Etiqueta V-table-Win
ON ENTRY OF pf-g002a.Etiqueta IN FRAME F-Main /* Etiqueta */
DO:
/*
  IF integral.PF-G002a.Tipo:SCREEN-VALUE = "SEPARADOR"
  THEN DO:
    ASSIGN integral.PF-G002a.Etiqueta:SCREEN-VALUE = "".  
    RETURN NO-APPLY.
  END.
  IF integral.PF-G002a.Tipo:SCREEN-VALUE = "LINEA"
  THEN DO:
    ASSIGN integral.PF-G002a.Etiqueta:SCREEN-VALUE = FILL("-",80).
    RETURN NO-APPLY.
  END.  
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pf-g002a.Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pf-g002a.Tipo V-table-Win
ON VALUE-CHANGED OF pf-g002a.Tipo IN FRAME F-Main /* Tipo */
DO:
    CASE PF-G002a.Tipo:SCREEN-VALUE:
        WHEN "LINEA" THEN DO:
            PF-G002a.Etiqueta:SCREEN-VALUE          = FILL("-",80).
            PF-G002a.Etiqueta:SENSITIVE             = NO.
            PF-G002a.Programa:SCREEN-VALUE          = "".
            PF-G002a.Programa:SENSITIVE             = NO.
            Btn-program:SENSITIVE                  = NO.
            Btn-grupos:SENSITIVE                   = NO.
/*             PF-G002a.Acceso-directo:SCREEN-VALUE    = "NO". */
/*             PF-G002a.Acceso-directo:SENSITIVE       = NO.   */
            PF-G002a.Persistente:SENSITIVE          = NO.
            PF-G002a.Seguridad-Grupos:SCREEN-VALUE  = "".
            PF-G002a.Seguridad-Grupos:SENSITIVE     = NO.
/*             PF-G002a.Tecla-Aceleradora:SCREEN-VALUE = "". */
/*             PF-G002a.Tecla-Aceleradora:SENSITIVE    = NO. */
        END.
        WHEN "SEPARADOR" THEN DO:
            PF-G002a.Etiqueta:SCREEN-VALUE          = FILL(" ",80).
            PF-G002a.Etiqueta:SENSITIVE             = NO.
            PF-G002a.Programa:SCREEN-VALUE          = "".
            PF-G002a.Programa:SENSITIVE             = NO.
            Btn-program:SENSITIVE                  = NO.
            Btn-grupos:SENSITIVE                   = NO.
/*             PF-G002a.Acceso-directo:SCREEN-VALUE    = "NO". */
/*             PF-G002a.Acceso-directo:SENSITIVE       = NO.   */
            PF-G002a.Persistente:SENSITIVE          = NO.
            PF-G002a.Seguridad-Grupos:SCREEN-VALUE  = "".
            PF-G002a.Seguridad-Grupos:SENSITIVE     = NO.
/*             PF-G002a.Tecla-Aceleradora:SCREEN-VALUE = "". */
/*             PF-G002a.Tecla-Aceleradora:SENSITIVE    = NO. */
        END.
        WHEN "SUB-MENU" THEN DO:
            PF-G002a.Etiqueta:SENSITIVE             = YES.
            PF-G002a.Programa:SCREEN-VALUE          = "".
            PF-G002a.Programa:SENSITIVE             = NO.
            Btn-program:SENSITIVE                  = NO.
            Btn-grupos:SENSITIVE                   = YES.
/*             PF-G002a.Acceso-directo:SCREEN-VALUE    = "NO". */
/*             PF-G002a.Acceso-directo:SENSITIVE       = NO.   */
            PF-G002a.Persistente:SENSITIVE          = NO.
            PF-G002a.Seguridad-Grupos:SENSITIVE     = YES. 
/*             PF-G002a.Tecla-Aceleradora:SCREEN-VALUE = "". */
/*             PF-G002a.Tecla-Aceleradora:SENSITIVE    = NO. */
        END.
        WHEN "PROCESO" THEN DO:
            PF-G002a.Etiqueta:SENSITIVE          = YES.
            PF-G002a.Programa:SENSITIVE          = YES.
            Btn-program:SENSITIVE               = YES.
            Btn-grupos:SENSITIVE                = YES.
/*             PF-G002a.Acceso-directo:SENSITIVE    = YES. */
            PF-G002a.Persistente:SENSITIVE       = YES.
            PF-G002a.Seguridad-Grupos:SENSITIVE  = YES.
/*             PF-G002a.Tecla-Aceleradora:SENSITIVE = YES. */
        END.
    END CASE.
/*     APPLY "VALUE-CHANGED" TO PF-G002a.Acceso-directo. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "PF-G002a"}
  {src/adm/template/row-list.i "PF-G003"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "PF-G002a"}
  {src/adm/template/row-find.i "PF-G003"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    IF AVAILABLE PF-G002a THEN DO:
        opcion-act = PF-G002a.codmnu.
        tipo-act   = PF-G002a.Tipo.
    END.
    ELSE DO:
        opcion-act = "".
        tipo-act   = "".
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    COMBO-nivel:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
    APPLY "VALUE-CHANGED" TO PF-G002a.Tipo IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    X-APLIC-ID = IF AVAILABLE PF-G003 THEN PF-G003.APLIC-ID ELSE ?.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUN get-attribute ('ADM-NEW-RECORD').
    IF RETURN-VALUE = 'YES' THEN DO:
        CASE COMBO-nivel:
            WHEN 1 THEN DO:
                FIND LAST b-G002 WHERE
                    b-G002.aplic-id = x-aplic-id AND
                    b-G002.CodMnu >= codmnu-new NO-ERROR.
                IF AVAILABLE b-G002 THEN DO:
                    SUBSTRING(b-G002.CodMnu,1,2,"CHARACTER") =
                        STRING((INTEGER(SUBSTRING(b-G002.CodMnu,1,2)) + 1), "99").
                    loopito1:
                    REPEAT:
                        FIND PREV b-G002 WHERE
                            b-G002.aplic-id = x-aplic-id AND
                            b-G002.CodMnu >= codmnu-new NO-ERROR.
                        IF AVAILABLE b-G002 THEN
                            SUBSTRING(b-G002.CodMnu,1,2,"CHARACTER") =
                                STRING((INTEGER(SUBSTRING(b-G002.CodMnu,1,2)) + 1), "99").
                        ELSE LEAVE loopito1.
                    END.
                END.
            END.
            WHEN 2 THEN DO:
                FIND LAST b-G002 WHERE
                    b-G002.aplic-id = x-aplic-id AND
                    b-G002.CodMnu BEGINS SUBSTRING(codmnu-new,1,2) AND
                    b-G002.CodMnu >= codmnu-new NO-ERROR.
                IF AVAILABLE b-G002 THEN DO:
                    SUBSTRING(b-G002.CodMnu,3,2,"CHARACTER") =
                        STRING((INTEGER(SUBSTRING(b-G002.CodMnu,3,2)) + 1), "99").
                    loopito2:
                    REPEAT:
                        FIND PREV b-G002 WHERE
                            b-G002.aplic-id = x-aplic-id AND
                            b-G002.CodMnu BEGINS SUBSTRING(codmnu-new,1,2) AND
                            b-G002.CodMnu >= codmnu-new NO-ERROR.
                        IF AVAILABLE b-G002 THEN
                            SUBSTRING(b-G002.CodMnu,3,2,"CHARACTER") =
                                STRING((INTEGER(SUBSTRING(b-G002.CodMnu,3,2)) + 1), "99").
                        ELSE LEAVE loopito2.
                    END.
                END.
            END.
            WHEN 3 THEN DO:
                FIND LAST b-G002 WHERE
                    b-G002.aplic-id = x-aplic-id AND
                    b-G002.CodMnu BEGINS SUBSTRING(codmnu-new,1,4) AND
                    b-G002.CodMnu >= codmnu-new NO-ERROR.
                IF AVAILABLE b-G002 THEN DO:
                    SUBSTRING(b-G002.CodMnu,5,2,"CHARACTER") =
                        STRING((INTEGER(SUBSTRING(b-G002.CodMnu,5,2)) + 1), "99").
                    loopito3:
                    REPEAT:
                        FIND PREV b-G002 WHERE
                            b-G002.aplic-id = x-aplic-id AND
                            b-G002.CodMnu BEGINS SUBSTRING(codmnu-new,1,4) AND
                            b-G002.CodMnu >= codmnu-new NO-ERROR.
                        IF AVAILABLE b-G002 THEN
                            SUBSTRING(b-G002.CodMnu,5,2,"CHARACTER") =
                                STRING((INTEGER(SUBSTRING(b-G002.CodMnu,5,2)) + 1), "99").
                        ELSE LEAVE loopito3.
                    END.
                END.
            END.
        END CASE.
        COMBO-nivel:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
        PF-G002a.APLIC-ID = x-aplic-id.
        PF-G002a.codmnu = codmnu-new.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    COMBO-nivel:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    X-APLIC-ID = IF AVAILABLE PF-G003 THEN PF-G003.APLIC-ID ELSE ?.
    
    /* Code placed here will execute PRIOR to standard behavior. */

    IF AVAILABLE PF-G002a AND PF-G002a.Tipo = "SUB-MENU" THEN DO:
        MESSAGE
            "También se van ha eliminar" SKIP
            "los hijos de esta opción" SKIP
            "De todas maneras desea continuar?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE rpta AS LOGICAL.
        IF rpta <> TRUE THEN RETURN.
        FOR EACH b-G002 WHERE
            b-G002.aplic-id = x-aplic-id AND
            LENGTH(b-G002.codmnu) > LENGTH(PF-G002a.codmnu) AND
            b-G002.codmnu BEGINS PF-G002a.codmnu:
            DELETE b-G002.
        END.
    END.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    IF AVAILABLE PF-G002a THEN
        COMBO-nivel = LENGTH(PF-G002a.codmnu) / 2.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).

    /* Code placed here will execute AFTER standard behavior.    */
/*     IF AVAILABLE PF-G002a THEN                                              */
/*         DO WITH FRAME {&FRAME-NAME}:                                        */
/*             FILE-INFO:FILE-NAME = PF-G002a.ico.                             */
/*             IF FILE-INFO:FILE-TYPE = ? THEN Ok = Btn-iconos:LOAD-IMAGE(""). */
/*             ELSE OK = Btn-iconos:LOAD-IMAGE(FILE-INFO:FILE-NAME).           */
/*         END.                                                                */
/*     ELSE OK = Btn-iconos:LOAD-IMAGE("").                                    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    APPLY "VALUE-CHANGED" TO PF-G002a.Tipo IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    X-APLIC-ID = IF AVAILABLE PF-G003 THEN PF-G003.APLIC-ID ELSE ?.

    RUN get-attribute ('ADM-NEW-RECORD').
    IF RETURN-VALUE = 'YES' THEN DO:
        CASE COMBO-nivel:
        WHEN 1 THEN DO:
            FIND b-G002 WHERE
                b-G002.aplic-id = x-aplic-id AND
                b-G002.codmnu = SUBSTRING(opcion-act,1,2) NO-LOCK NO-ERROR.
            IF NOT AVAILABLE b-G002 THEN DO:
                FIND LAST b-G002 WHERE
                    b-G002.aplic-id = x-aplic-id AND
                    LENGTH(b-G002.codmnu) = COMBO-nivel * 2 NO-LOCK NO-ERROR.
                IF NOT AVAILABLE b-G002 THEN codmnu-new = "01".
            END.
            IF AVAILABLE b-G002 THEN
                codmnu-new = STRING((INTEGER(b-G002.codmnu) + 1), "99").
        END.
        WHEN 2 THEN DO:
            /*
            IF opcion-act = "" THEN DO:
                BELL.
                MESSAGE "Debe existir un padre para" SKIP
                    "esta opción con nivel" COMBO-nivel
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            */
            FIND b-G002 WHERE
                b-G002.aplic-id = x-aplic-id AND
                b-G002.codmnu = SUBSTRING(opcion-act,1,4) NO-LOCK NO-ERROR.
            IF AVAILABLE b-G002 THEN DO:
                IF LENGTH(opcion-act) = 2 THEN DO:
                    IF tipo-act <> "SUB-MENU" THEN DO:
                        BELL.
                        MESSAGE "El padre para esta opción con nivel"
                            COMBO-nivel SKIP
                            "debe ser de tipo SUB-MENU"
                            VIEW-AS ALERT-BOX ERROR.
                        RETURN.
                    END.
                    codmnu-new = opcion-act + "01".
                END.
                ELSE codmnu-new = STRING((INTEGER(b-G002.codmnu) + 1), "9999").
            END.
            ELSE DO:
                FIND LAST b-G002 WHERE
                    b-G002.aplic-id = x-aplic-id AND
                    LENGTH(b-G002.codmnu) = COMBO-nivel * 2 AND
                    b-G002.codmnu BEGINS SUBSTRING(opcion-act,1,2) NO-LOCK NO-ERROR.
                IF AVAILABLE b-G002 THEN
                    codmnu-new = STRING((INTEGER(b-G002.codmnu) + 1), "9999").
                ELSE DO:
                    IF tipo-act <> "SUB-MENU" THEN DO:
                        BELL.
                        MESSAGE "El padre para esta opción con nivel"
                            COMBO-nivel SKIP
                            "debe ser de tipo SUB-MENU"
                            VIEW-AS ALERT-BOX ERROR.
                        RETURN.
                    END.
                    codmnu-new = SUBSTRING(opcion-act,1,2) + "01".
                END.
            END.
        END.
        WHEN 3 THEN DO:
            IF LENGTH(opcion-act) <= 2 THEN DO:
                BELL.
                MESSAGE "Debe existir un padre para" SKIP
                    "esta opción con nivel" COMBO-nivel
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            FIND b-G002 WHERE
                b-G002.aplic-id = x-aplic-id AND
                b-G002.codmnu = opcion-act NO-LOCK NO-ERROR.
            IF AVAILABLE b-G002 THEN DO:
                IF LENGTH(opcion-act) = 4 THEN DO:
                    IF tipo-act <> "SUB-MENU" THEN DO:
                        BELL.
                        MESSAGE "El padre para esta opción con nivel"
                            COMBO-nivel SKIP
                            "debe ser de tipo SUB-MENU"
                            VIEW-AS ALERT-BOX ERROR.
                        RETURN.
                    END.
                    codmnu-new = opcion-act + "01".
                END.
                ELSE codmnu-new = STRING((INTEGER(b-G002.codmnu) + 1), "999999").
            END.
            ELSE DO:
                FIND LAST b-G002 WHERE
                    b-G002.aplic-id = x-aplic-id AND
                    LENGTH(b-G002.codmnu) = COMBO-nivel * 2 AND
                    b-G002.codmnu BEGINS SUBSTRING(opcion-act,1,4) NO-LOCK NO-ERROR.
                IF AVAILABLE b-G002 THEN
                    codmnu-new = STRING((INTEGER(b-G002.codmnu) + 1), "999999").
                ELSE DO:
                    IF tipo-act <> "SUB-MENU" THEN DO:
                        BELL.
                        MESSAGE "El padre para esta opción con nivel"
                            COMBO-nivel SKIP
                            "debe ser de tipo SUB-MENU"
                            VIEW-AS ALERT-BOX ERROR.
                        RETURN.
                    END.
                    codmnu-new = SUBSTRING(opcion-act,1,4) + "01".
                END.
            END.
        END.
        END CASE.
    END.

    /* Code placed here will execute PRIOR to standard behavior. */
    RUN valida.
    IF RETURN-VALUE = "ERROR" THEN RETURN.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros V-table-Win 
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
        WHEN "" THEN .
    END CASE.
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros V-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "PF-G002a"}
  {src/adm/template/snd-list.i "PF-G003"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida V-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     Validacion de datos
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    CASE PF-G002a.Tipo:SCREEN-VALUE:
        WHEN "PROCESO" THEN DO:
            IF PF-G002a.Etiqueta:SCREEN-VALUE = "" THEN DO:
                BELL.
                MESSAGE "Debe ingresar la etiqueta"
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO PF-G002a.etiqueta.
                RETURN "ERROR".
            END.
            IF PF-G002a.PROGRAMA:SCREEN-VALUE = "" THEN DO:
                BELL.
                MESSAGE "Debe ingresar nombre de programa"
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO PF-G002a.programa.
                RETURN "ERROR".
            END.
/*             IF INPUT PF-G002a.Acceso-directo AND           */
/*                 PF-G002a.Icon:SCREEN-VALUE = "" THEN DO:   */
/*                 BELL.                                      */
/*                 MESSAGE "Debe ingresar el nombre de Icono" */
/*                     VIEW-AS ALERT-BOX ERROR.               */
/*                 APPLY "ENTRY" TO PF-G002a.icon.            */
/*                 RETURN "ERROR".                            */
/*             END.                                           */
        END.
        WHEN "SUB-MENU" THEN
            IF PF-G002a.Etiqueta:SCREEN-VALUE = "" THEN DO:
                BELL.
                MESSAGE "Debe ingresar la etiqueta"
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO PF-G002a.etiqueta.
                RETURN "ERROR".
            END. 
    END CASE.
END.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

