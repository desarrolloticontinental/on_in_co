&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r2 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral          PROGRESS
*/
&Scoped-define WINDOW-NAME W-maestro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-maestro 
/*------------------------------------------------------------------------

  File: 
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:  VALMIESA - DEPARTAMENTO DE TECNOLOGIA

  Created: 10/25/93 - 11:42 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*~ MAESTRO V1.00 FOR MS-WINDOWS ~*/
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE RECID-stack AS RECID NO-UNDO.
DEFINE VARIABLE RECID-tmp   AS RECID NO-UNDO.
DEFINE VARIABLE x-codigo    LIKE cb-tabl.codigo.
DEFINE VARIABLE pto AS LOGICAL.
pto = SESSION:SET-WAIT-STATE("").

/*   C A M B I O S   E N    L O S   P R E - P R O C E S A D O R E S */

/* Solo muestra los registros que cumplan con la condici�n : */
&Scoped-define RECORD-SCOPE cb-tabl.Tabla = x-codigo

/* Como Buscar un Registro en la Tabla ( Para Crear, modificar, anular etc.) */
&Scoped-define SEARCH-KEY cb-tabl.Codigo = cb-tabl.Codigo:SCREEN-VALUE IN FRAME F-maestro

/* Campos Ocultos que deben ser asignados en cada modificaci�n */
&Scoped-define ASSIGN-ADD cb-tabl.Tabla = x-codigo

/* Campos que no pueden ser modificados */
&Scoped-define NO-MODIFY cb-tabl.Codigo

/* Programa donde selecciona clasificaci�n de cb-tabl */
&Scoped-define q-modelo1 ast/q-scb-tabl.w ( "00", OUTPUT RECID-stack )

/* Programa donde realizara la Consulta */
&Scoped-define q-modelo ast/q-cb-tabl.w ( x-codigo, OUTPUT RECID-stack )

/* Campos por los cuales se puede hacer Busquedas */
&Scoped-define Query-Field cb-tabl.Codigo cb-tabl.Nombre

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-add

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES integral.cb-tabl

/* Definitions for FRAME F-maestro                                      */
&Scoped-define FIELDS-IN-QUERY-F-maestro integral.cb-tabl.Codigo ~
integral.cb-tabl.Nombre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F-maestro integral.cb-tabl.Codigo ~
integral.cb-tabl.Nombre 
&Scoped-define ENABLED-TABLES-IN-QUERY-F-maestro integral.cb-tabl
&Scoped-define FIELD-PAIRS-IN-QUERY-F-maestro ~
      ~{&FP1}Codigo         ~{&FP2}Codigo          ~{&FP3} ~
      ~{&FP1}Nombre         ~{&FP2}Nombre          ~{&FP3}
&Scoped-define OPEN-QUERY-F-maestro OPEN QUERY F-maestro FOR EACH integral.cb-tabl SHARE-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-F-maestro integral.cb-tabl
&Scoped-define TABLES-IN-QUERY-F-maestro integral.cb-tabl

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS R-navigaate-2 B-ok B-Cancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-maestro AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Cancel 
     LABEL "&Cancelar":L 
     SIZE 9 BY 1
     font 4.

DEFINE BUTTON B-ok 
     LABEL "&Aceptar":L 
     SIZE 9 BY 1
     font 4.

DEFINE RECTANGLE R-navigaate-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 90.57 BY 2
     BGCOLOR 8 FGCOLOR 15 .

DEFINE BUTTON B-add 
     LABEL "&Crear":L 
     SIZE 9 BY 1
     font 4.

DEFINE BUTTON B-browse 
     IMAGE-UP FILE "IMG/pvbrow"
     IMAGE-DOWN FILE "IMG/pvbrowd"
     IMAGE-INSENSITIVE FILE "IMG/pvbrowx"
     LABEL "&Consulta" 
     SIZE 5.72 BY 1.5.

DEFINE BUTTON B-delete 
     LABEL "&Eliminar":L 
     SIZE 9 BY 1
     font 4.

DEFINE BUTTON B-exit 
     LABEL "&Salir":L 
     SIZE 7 BY 1
     font 4.

DEFINE BUTTON B-first 
     IMAGE-UP FILE "IMG/pvfirst"
     IMAGE-DOWN FILE "IMG/pvfirstd"
     IMAGE-INSENSITIVE FILE "IMG/pvfirstx"
     LABEL "<<":L 
     SIZE 5 BY 1
     font 4.

DEFINE BUTTON B-last 
     IMAGE-UP FILE "IMG/pvlast"
     IMAGE-DOWN FILE "IMG/pvlastd"
     IMAGE-INSENSITIVE FILE "IMG/pvlastx"
     LABEL ">>":L 
     SIZE 4.57 BY 1
     font 4.

DEFINE BUTTON B-next 
     IMAGE-UP FILE "IMG/pvforw"
     IMAGE-DOWN FILE "IMG/pvforwd"
     IMAGE-INSENSITIVE FILE "IMG/pvforwx"
     LABEL ">":L 
     SIZE 5 BY 1
     font 4.

DEFINE BUTTON B-prev 
     IMAGE-UP FILE "IMG/pvback"
     IMAGE-DOWN FILE "IMG/pvbackd"
     IMAGE-INSENSITIVE FILE "IMG/pvbackx"
     LABEL "<":L 
     SIZE 5 BY 1
     font 4.

DEFINE BUTTON B-query 
     LABEL "&Buscar":L 
     SIZE 7.57 BY 1
     font 4.

DEFINE BUTTON B-update 
     LABEL "&Modificar":L 
     SIZE 9 BY 1
     font 4.

DEFINE RECTANGLE R-consulta
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 10 BY 2
     BGCOLOR 8 .

DEFINE RECTANGLE R-exit
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 11.57 BY 2
     BGCOLOR 8 FGCOLOR 15 .

DEFINE RECTANGLE R-modify
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 33 BY 2
     BGCOLOR 8 FGCOLOR 15 .

DEFINE RECTANGLE R-navigaate
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 26 BY 2
     BGCOLOR 8 FGCOLOR 15 .

DEFINE RECTANGLE R-query
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 10 BY 2
     BGCOLOR 8 FGCOLOR 15 .

DEFINE RECTANGLE R-maestro1
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 90 BY 9.88
     BGCOLOR 8 FGCOLOR 15 .

DEFINE BUTTON B-Cancel-3 
     LABEL "&Cancelar":L 
     SIZE 9 BY 1
     font 4.

DEFINE BUTTON B-ok-3 AUTO-GO 
     LABEL "&Aceptar":L 
     SIZE 9 BY 1
     font 4.

DEFINE RECTANGLE R-navigaate-4
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 90.57 BY 2
     BGCOLOR 8 FGCOLOR 15 .

DEFINE VARIABLE x-nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 85 BY 1.5
     BGCOLOR 8 FONT 11 NO-UNDO.

DEFINE BUTTON B-Cancel-2 
     LABEL "&Cancelar":L 
     SIZE 9 BY 1
     font 4.

DEFINE BUTTON B-ok-2 
     LABEL "&Aceptar":L 
     SIZE 9 BY 1
     font 4.

DEFINE RECTANGLE R-navigaate-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 90.57 BY 2
     BGCOLOR 8 FGCOLOR 15 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F-maestro FOR 
      integral.cb-tabl SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-add
     B-ok AT ROW 1.5 COL 24.29
     B-Cancel AT ROW 1.5 COL 57.57
     R-navigaate-2 AT ROW 1 COL 1
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 14.61
         SIZE 90.86 BY 2.08
         BGCOLOR 8 FGCOLOR 0 
         CANCEL-BUTTON B-Cancel.

DEFINE FRAME F-update
     B-ok-2 AT ROW 1.5 COL 24.29
     B-Cancel-2 AT ROW 1.5 COL 57.57
     R-navigaate-3 AT ROW 1 COL 1
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 14.61
         SIZE 90.86 BY 2.08
         BGCOLOR 8 FGCOLOR 0 .

DEFINE FRAME F-search
     B-ok-3 AT ROW 1.54 COL 24.29
     B-Cancel-3 AT ROW 1.54 COL 57.57
     R-navigaate-4 AT ROW 1 COL 1
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 14.61
         SIZE 90.86 BY 2.08
         BGCOLOR 8 FGCOLOR 0 .

DEFINE FRAME F-ctrl-frame
     B-query AT ROW 1.5 COL 2
     B-add AT ROW 1.5 COL 13
     B-update AT ROW 1.5 COL 23
     B-delete AT ROW 1.5 COL 33
     B-first AT ROW 1.5 COL 46
     B-prev AT ROW 1.5 COL 52
     B-next AT ROW 1.5 COL 58
     B-last AT ROW 1.5 COL 64
     B-browse AT ROW 1.27 COL 72
     B-exit AT ROW 1.5 COL 82
     R-query AT ROW 1 COL 1
     R-modify AT ROW 1 COL 11
     R-navigaate AT ROW 1 COL 44
     R-consulta AT ROW 1 COL 70
     R-exit AT ROW 1 COL 80
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 14.61
         SIZE 90.86 BY 2.08
         BGCOLOR 8 FGCOLOR 0 font 4.

DEFINE FRAME F-titulo
     x-nombre AT ROW 2 COL 4 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 1.04
         SIZE 90.29 BY 2.92
         BGCOLOR 8 .

DEFINE FRAME F-maestro
     integral.cb-tabl.Codigo AT ROW 4 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .81
          BGCOLOR 15 
     integral.cb-tabl.Nombre AT ROW 5.5 COL 30 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 35.72 BY .81
          BGCOLOR 15 
     R-maestro1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 4.62
         SIZE 90.72 BY 9.92
         BGCOLOR 8 FGCOLOR 0 font 4.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-maestro ASSIGN
         HIDDEN             = YES
         TITLE              = "cb-tabl"
         COLUMN             = 1.14
         ROW                = 1
         HEIGHT             = 15.69
         WIDTH              = 90.86
         MAX-HEIGHT         = 15.69
         MAX-WIDTH          = 90.86
         VIRTUAL-HEIGHT     = 15.69
         VIRTUAL-WIDTH      = 90.86
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = 8
         FGCOLOR            = 0
         THREE-D            = yes
         FONT               = 8
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

IF NOT W-maestro:LOAD-ICON("IMG/valmiesa") THEN
    MESSAGE "Unable to load icon: IMG/valmiesa"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME F-add
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       FRAME F-add:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Cancel IN FRAME F-add
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-ok IN FRAME F-add
   NO-DISPLAY                                                           */
/* SETTINGS FOR FRAME F-ctrl-frame
   UNDERLINE                                                            */
/* SETTINGS FOR BUTTON B-add IN FRAME F-ctrl-frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-delete IN FRAME F-ctrl-frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-exit IN FRAME F-ctrl-frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-first IN FRAME F-ctrl-frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-last IN FRAME F-ctrl-frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-next IN FRAME F-ctrl-frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-prev IN FRAME F-ctrl-frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-query IN FRAME F-ctrl-frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-update IN FRAME F-ctrl-frame
   NO-DISPLAY                                                           */
/* SETTINGS FOR FRAME F-maestro
   UNDERLINE                                                            */
/* SETTINGS FOR FRAME F-search
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       FRAME F-search:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Cancel-3 IN FRAME F-search
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-ok-3 IN FRAME F-search
   NO-DISPLAY                                                           */
/* SETTINGS FOR FRAME F-titulo
   UNDERLINE                                                            */
/* SETTINGS FOR FILL-IN x-nombre IN FRAME F-titulo
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME F-update
   NOT-VISIBLE UNDERLINE                                                */
ASSIGN 
       FRAME F-update:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Cancel-2 IN FRAME F-update
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON B-ok-2 IN FRAME F-update
   NO-DISPLAY                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-maestro)
THEN W-maestro:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-maestro
/* Query rebuild information for FRAME F-maestro
     _TblList          = "integral.cb-tabl"
     _Query            is OPENED
*/  /* FRAME F-maestro */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-titulo
/* Query rebuild information for FRAME F-titulo
     _Query            is NOT OPENED
*/  /* FRAME F-titulo */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define FRAME-NAME F-ctrl-frame
&Scoped-define SELF-NAME B-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-add W-maestro
ON CHOOSE OF B-add IN FRAME F-ctrl-frame /* Crear */
DO:
    FRAME F-ctrl-frame:VISIBLE = FALSE.
    FRAME F-add:VISIBLE = TRUE.
    CLEAR FRAME f-maestro.
    RECID-tmp = 0.
    DO TRANSACTION ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE:
        CREATE {&FIRST-TABLE-IN-QUERY-F-maestro} .
        DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
        ENABLE  {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
        RECID-tmp = RECID ( {&FIRST-TABLE-IN-QUERY-F-maestro} ).
        WAIT-FOR CHOOSE OF b-ok OR CHOOSE OF b-cancel.
    END.
    FRAME f-add:VISIBLE = FALSE.
    FRAME f-ctrl-frame:visible = TRUE.
    DISABLE {&ENABLED-FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-browse W-maestro
ON CHOOSE OF B-browse IN FRAME F-ctrl-frame /* Consulta */
DO:
    RUN {&q-modelo} .
    IF RECID-stack <> 0
    THEN DO:
        FIND {&TABLES-IN-QUERY-F-maestro}
             WHERE RECID( {&TABLES-IN-QUERY-F-maestro} ) = RECID-stack
              NO-LOCK  NO-ERROR.

         IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
             DISPLAY  {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
         END.

         ELSE MESSAGE "No existen Registros." VIEW-AS ALERT-BOX ERROR
         BUTTONS OK.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-add
&Scoped-define SELF-NAME B-Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Cancel W-maestro
ON CHOOSE OF B-Cancel IN FRAME F-add /* Cancelar */
DO:
         IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
              DELETE {&FIRST-TABLE-IN-QUERY-F-maestro}.
         END.
         
         FIND PREV {&FIRST-TABLE-IN-QUERY-F-maestro}
          &IF "{&RECORD-SCOPE}" <> "" &THEN
              WHERE {&RECORD-SCOPE}
          &ENDIF  
          NO-LOCK NO-ERROR.
         IF NOT AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro}
         THEN FIND FIRST {&FIRST-TABLE-IN-QUERY-F-maestro}
                              &IF "{&RECORD-SCOPE}" <> "" &THEN
                                     WHERE {&RECORD-SCOPE}
                              &ENDIF  
                              NO-LOCK NO-ERROR.
         IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
              DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME f-maestro.
         END.
         ELSE CLEAR FRAME F-maestro.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-update
&Scoped-define SELF-NAME B-Cancel-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Cancel-2 W-maestro
ON CHOOSE OF B-Cancel-2 IN FRAME F-update /* Cancelar */
DO:

     IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
                DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME f-maestro.
      END.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-search
&Scoped-define SELF-NAME B-Cancel-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Cancel-3 W-maestro
ON CHOOSE OF B-Cancel-3 IN FRAME F-search /* Cancelar */
DO:
     FIND PREV {&FIRST-TABLE-IN-QUERY-F-maestro} 
     &IF "{&RECORD-SCOPE}" <> "" &THEN
          WHERE {&RECORD-SCOPE}
      &ENDIF  
     NO-LOCK NO-ERROR.
     IF NOT AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN 
     FIND FIRST {&FIRST-TABLE-IN-QUERY-F-maestro}
     &IF "{&RECORD-SCOPE}" <> "" &THEN
          WHERE {&RECORD-SCOPE}
      &ENDIF  
      NO-LOCK NO-ERROR.
     IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN  DO:
              DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME f-maestro.
     END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-ctrl-frame
&Scoped-define SELF-NAME B-delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-delete W-maestro
ON CHOOSE OF B-delete IN FRAME F-ctrl-frame /* Eliminar */
DO:
    FRAME F-ctrl-frame:VISIBLE = FALSE.
    RECID-tmp = 0.
    DO TRANSACTION ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
        IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
            RECID-Tmp = RECID(  {&FIRST-TABLE-IN-QUERY-F-maestro} ).
            FIND {&FIRST-TABLE-IN-QUERY-F-maestro}
                WHERE  RECID-Tmp = RECID(  {&FIRST-TABLE-IN-QUERY-F-maestro} )
                    EXCLUSIVE-LOCK NO-ERROR.
        END.
        IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
            MESSAGE "Eliminar el Registro"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE selection AS LOGICAL.
            IF selection THEN DO:
                   DELETE  {&FIRST-TABLE-IN-QUERY-F-maestro}.
            END.
            ELSE
                FIND {&FIRST-TABLE-IN-QUERY-F-maestro}
                    WHERE  RECID-Tmp = RECID(  {&FIRST-TABLE-IN-QUERY-F-maestro} )
                        NO-LOCK NO-ERROR.
        END.
        ELSE MESSAGE "No existen Registros." VIEW-AS ALERT-BOX ERROR
        BUTTONS OK.  
    END.

    IF NOT AVAIL  {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
        FIND PREV  {&FIRST-TABLE-IN-QUERY-F-maestro}
        &IF "{&RECORD-SCOPE}" <> "" &THEN
            WHERE {&RECORD-SCOPE}
        &ENDIF
        NO-LOCK NO-ERROR.
        IF NOT AVAIL  {&FIRST-TABLE-IN-QUERY-F-maestro}
        THEN FIND FIRST {&FIRST-TABLE-IN-QUERY-F-maestro}
                &IF "{&RECORD-SCOPE}" <> "" &THEN
                    WHERE {&RECORD-SCOPE}
                &ENDIF
                NO-LOCK NO-ERROR.
        IF AVAIL  {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
              DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME f-maestro.
        END.
        ELSE DO:
            CLEAR FRAME F-maestro.
        END.
    END.
    FRAME  F-ctrl-frame:VISIBLE = TRUE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-exit W-maestro
ON CHOOSE OF B-exit IN FRAME F-ctrl-frame /* Salir */
DO:
     APPLY  "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-first
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-first W-maestro
ON CHOOSE OF B-first IN FRAME F-ctrl-frame /* << */
DO:

     FIND FIRST {&TABLES-IN-QUERY-F-maestro}
      &IF "{&RECORD-SCOPE}" <> "" &THEN
          WHERE {&RECORD-SCOPE}
       &ENDIF    
       NO-LOCK  NO-ERROR.

     IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
         DISPLAY  {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
     END.

     ELSE MESSAGE "No existen Registros." VIEW-AS ALERT-BOX ERROR
     BUTTONS OK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-last W-maestro
ON CHOOSE OF B-last IN FRAME F-ctrl-frame /* >> */
DO:

     FIND LAST {&FIRST-TABLE-IN-QUERY-F-maestro}
     &IF "{&RECORD-SCOPE}" <> "" &THEN
          WHERE {&RECORD-SCOPE}
      &ENDIF  
      NO-LOCK  NO-ERROR.
     
     IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
           DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
     END.
     
     ELSE DO:
         CLEAR FRAME f-maestro.    
         MESSAGE "No exiten Registros." VIEW-AS ALERT-BOX ERROR
             BUTTONS OK.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-next W-maestro
ON CHOOSE OF B-next IN FRAME F-ctrl-frame /* > */
DO: 
     FIND NEXT {&FIRST-TABLE-IN-QUERY-F-maestro}  
     &IF "{&RECORD-SCOPE}" <> "" &THEN
          WHERE {&RECORD-SCOPE}
     &ENDIF
     NO-LOCK  NO-ERROR.
     IF NOT AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN 
     FIND FIRST {&FIRST-TABLE-IN-QUERY-F-maestro} 
     &IF "{&RECORD-SCOPE}" <> "" &THEN
          WHERE {&RECORD-SCOPE}
     &ENDIF  
     NO-LOCK NO-ERROR.
     IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
         DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
     END.
     ELSE MESSAGE "No existen Registros." VIEW-AS ALERT-BOX ERROR
     BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-add
&Scoped-define SELF-NAME B-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ok W-maestro
ON CHOOSE OF B-ok IN FRAME F-add /* Aceptar */
DO:
    IF CAN-FIND( {&FIRST-TABLE-IN-QUERY-F-maestro}
        WHERE
        &IF "{&RECORD-SCOPE}" <> "" &THEN
            {&RECORD-SCOPE} AND
        &ENDIF
        {&SEARCH-KEY} ) THEN DO:
        MESSAGE "Registro Existente" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO cb-tabl.Codigo IN FRAME F-maestro.
        RETURN NO-APPLY.
    END.
    ASSIGN FRAME F-Maestro
    {&FIELDS-IN-QUERY-F-maestro}
    {&ASSIGN-ADD}.
    FIND {&FIRST-TABLE-IN-QUERY-F-maestro}
        WHERE RECID-tmp = RECID ( {&FIRST-TABLE-IN-QUERY-F-maestro} )
            NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-update
&Scoped-define SELF-NAME B-ok-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ok-2 W-maestro
ON CHOOSE OF B-ok-2 IN FRAME F-update /* Aceptar */
DO:
  ASSIGN FRAME F-maestro
  {&FIELDS-IN-QUERY-F-maestro}
  {&ASSIGN-ADD}
  .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-search
&Scoped-define SELF-NAME B-ok-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ok-3 W-maestro
ON CHOOSE OF B-ok-3 IN FRAME F-search /* Aceptar */
DO:
    IF cb-tabl.Codigo:SCREEN-VALUE IN FRAME F-maestro <> "" THEN DO:
         FIND FIRST  {&FIRST-TABLE-IN-QUERY-F-maestro}
              WHERE 
                  &IF "{&RECORD-SCOPE}" <> "" &THEN
                      {&RECORD-SCOPE} AND
                  &ENDIF
                  cb-tabl.Codigo = cb-tabl.Codigo:SCREEN-VALUE IN FRAME F-maestro
                      NO-LOCK NO-ERROR.
         ASSIGN cb-tabl.Nombre:SCREEN-VALUE = "".
    END.

    IF cb-tabl.Nombre:SCREEN-VALUE IN FRAME F-maestro <> "" THEN DO:
         FIND FIRST  {&FIRST-TABLE-IN-QUERY-F-maestro}
              WHERE 
                  &IF "{&RECORD-SCOPE}" <> "" &THEN
                      {&RECORD-SCOPE} AND
                  &ENDIF
                  cb-tabl.Nombre BEGINS cb-tabl.Nombre:SCREEN-VALUE IN FRAME F-maestro
                      NO-LOCK NO-ERROR.
         ASSIGN cb-tabl.Codigo:SCREEN-VALUE = "".
   END.
   
   IF NOT AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
        APPLY "CHOOSE" TO B-Browse IN FRAME F-ctrl-frame.
        IF RECID-stack <> 0
        THEN RETURN.
   END.
         
   IF NOT AVAIL  {&FIRST-TABLE-IN-QUERY-F-maestro}  THEN DO:
        FIND FIRST  {&FIRST-TABLE-IN-QUERY-F-maestro}
        &IF "{&RECORD-SCOPE}" <> "" &THEN
             WHERE {&RECORD-SCOPE}
        &ENDIF  
        NO-LOCK NO-ERROR.
   END.
     
   IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN  DO:
        DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME f-maestro.
   END.

   IF NOT AVAIL  {&FIRST-TABLE-IN-QUERY-F-maestro}  THEN DO:
        CLEAR FRAME F-maestro.
        MESSAGE  "Registro no Existente." VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-ctrl-frame
&Scoped-define SELF-NAME B-prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-prev W-maestro
ON CHOOSE OF B-prev IN FRAME F-ctrl-frame /* < */
DO:
     FIND PREV {&FIRST-TABLE-IN-QUERY-F-maestro}
     &IF "{&RECORD-SCOPE}" <> "" &THEN
          WHERE {&RECORD-SCOPE}
     &ENDIF  
     NO-LOCK  NO-ERROR.
     IF NOT AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN 
     FIND LAST {&FIRST-TABLE-IN-QUERY-F-maestro} 
     &IF "{&RECORD-SCOPE}" <> "" &THEN
          WHERE {&RECORD-SCOPE}
     &ENDIF  
     NO-LOCK NO-ERROR.
     IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
         DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
     END.
     ELSE MESSAGE "No Existen Registros" VIEW-AS ALERT-BOX ERROR
     BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-query
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-query W-maestro
ON CHOOSE OF B-query IN FRAME F-ctrl-frame /* Buscar */
DO:    
    FIND FIRST {&FIRST-TABLE-IN-QUERY-F-maestro}
           WHERE 
           &IF "{&RECORD-SCOPE}" <> "" &THEN
                {&RECORD-SCOPE} AND
           &ENDIF  
           {&SEARCH-KEY} EXCLUSIVE-LOCK NO-ERROR.
      
      IF NOT AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
        MESSAGE  "No exiten Registros." VIEW-AS ALERT-BOX ERROR
        BUTTONS OK.
        RETURN.
     END.
    
     CLEAR FRAME F-maestro.     
     FRAME F-ctrl-frame:VISIBLE = FALSE.
     FRAME F-search:VISIBLE = TRUE.
      
     ENABLE {&QUERY-field}  WITH FRAME F-maestro.

     WAIT-FOR  "CHOOSE" OF b-ok-3 IN FRAME f-search
     OR CHOOSE OF b-cancel-3 IN FRAME f-search.
     
     DISABLE {&QUERY-field} WITH FRAME F-maestro.
     FRAME f-search:VISIBLE = FALSE.
     FRAME f-ctrl-frame:VISIBLE = TRUE.

END. /*choose of b-query*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-update W-maestro
ON CHOOSE OF B-update IN FRAME F-ctrl-frame /* Modificar */
DO:
    FRAME f-ctrl-frame:VISIBLE = FALSE.
    FRAME f-update:VISIBLE = TRUE.
    RECID-stack = RECID( {&FIRST-TABLE-IN-QUERY-F-maestro} ).
    DO TRANSACTION ON ENDKEY UNDO, LEAVE ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
        FIND {&FIRST-TABLE-IN-QUERY-F-maestro}
            WHERE RECID-stack = RECID( {&FIRST-TABLE-IN-QUERY-F-maestro} )
                EXCLUSIVE NO-ERROR.
        IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
            ENABLE {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
            DISABLE {&NO-MODIFY} WITH FRAME F-maestro.
            WAIT-FOR CHOOSE OF b-ok-2 OR CHOOSE OF B-CANCEL-2 IN FRAME f-UPDATE.
        END.
        ELSE MESSAGE "No exiten Registros." VIEW-AS ALERT-BOX ERROR
            BUTTONS OK.
    END.  /*transaction*/
    IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN
        FIND {&FIRST-TABLE-IN-QUERY-F-maestro}
            WHERE RECID-stack = RECID( {&FIRST-TABLE-IN-QUERY-F-maestro} )
                NO-LOCK NO-ERROR.
    ELSE
        FIND PREV  {&FIRST-TABLE-IN-QUERY-F-maestro}
            &IF "{&RECORD-SCOPE}" <> "" &THEN
                WHERE {&RECORD-SCOPE}
            &ENDIF
            NO-LOCK NO-ERROR.
    IF NOT AVAIL  {&FIRST-TABLE-IN-QUERY-F-maestro}
    THEN FIND FIRST  {&FIRST-TABLE-IN-QUERY-F-maestro}
        &IF "{&RECORD-SCOPE}" <> "" &THEN
            WHERE {&RECORD-SCOPE}
        &ENDIF
        NO-LOCK NO-ERROR.
    IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN
        DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME f-maestro.
    ELSE
        CLEAR FRAME F-maestro.
    DISABLE {&ENABLED-FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
    FRAME f-update:VISIBLE = FALSE.
    FRAME F-ctrl-frame:VISIBLE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-add
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-maestro 


/* ***************************  Main Block  *************************** */

/* Send messages to alert boxes because there is no message area.       */
ASSIGN CURRENT-WINDOW             = {&WINDOW-NAME}
       SESSION:SYSTEM-ALERT-BOXES = (CURRENT-WINDOW:MESSAGE-AREA = NO).
 
ON CLOSE OF THIS-PROCEDURE
      RUN disable_UI.
        
ON "WINDOW-CLOSE" OF {&WINDOW-NAME}  DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

ON "ENDKEY", END-ERROR OF B-add, B-browse, B-Cancel, B-Cancel-2, B-Cancel-3, 
        B-delete, B-exit, B-first, B-last, B-next, B-ok, B-ok-2, B-ok-3, B-prev, 
        B-query, B-update, FRAME F-maestro
DO: 
    IF FRAME F-add:VISIBLE    = TRUE THEN APPLY "CHOOSE" TO B-cancel.
    IF FRAME F-update:VISIBLE = TRUE THEN APPLY "CHOOSE" TO B-cancel-2.
    IF FRAME F-search:VISIBLE = TRUE THEN APPLY "CHOOSE" TO B-cancel-3.
    IF FRAME F-ctrl-frame:VISIBLE = TRUE 
    THEN APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

ON "GO" OF FRAME F-maestro, FRAME F-add, FRAME F-update, FRAME F-search
DO: 
    IF FRAME F-add:VISIBLE    = TRUE THEN APPLY "CHOOSE" TO B-ok.
    IF FRAME F-update:VISIBLE = TRUE THEN APPLY "CHOOSE" TO B-ok-2.
    IF FRAME F-search:VISIBLE = TRUE THEN APPLY "CHOOSE" TO B-ok-3.
    RETURN NO-APPLY.
END.

ON DELETE-CHARACTER OF  B-query, B-add, B-update, B-delete, 
            B-First, B-prev, B-next, B-last, B-Browse, B-exit
DO:
    APPLY "CHOOSE" TO B-delete.
    RETURN NO-APPLY.    
END.

ON END OF  B-query, B-add, B-update, B-delete, 
            B-First, B-prev, B-next, B-last, B-Browse, B-exit
DO:
    APPLY "CHOOSE" TO B-last.
    RETURN NO-APPLY.    
END.

ON F8 OF  B-query, B-add, B-update, B-delete, 
            B-First, B-prev, B-next, B-last, B-Browse, B-exit
DO:
    APPLY "CHOOSE" TO B-browse.
    RETURN NO-APPLY.    
END.

ON HOME OF  B-query, B-add, B-update, B-delete, 
            B-First, B-prev, B-next, B-last, B-Browse, B-exit
DO:
    APPLY "CHOOSE" TO B-first.
    RETURN NO-APPLY.    
END.

ON INSERT-MODE OF  B-query, B-add, B-update, B-delete, 
            B-First, B-prev, B-next, B-last, B-Browse, B-exit
DO:
    APPLY "CHOOSE" TO B-add.
    RETURN NO-APPLY.    
END.

ON PAGE-DOWN OF  B-query, B-add, B-update, B-delete, 
            B-First, B-prev, B-next, B-last, B-Browse, B-exit
DO:
    APPLY "CHOOSE" TO B-next.
    RETURN NO-APPLY.  
END.

ON PAGE-UP OF  B-query, B-add, B-update, B-delete, 
            B-First, B-prev, B-next, B-last, B-Browse, B-exit
DO:
    APPLY "CHOOSE" TO B-prev.
    RETURN NO-APPLY.  
END.

ON RETURN OF  B-query, B-add, B-update, B-delete, 
            B-First, B-prev, B-next, B-last, B-Browse, B-exit
DO:
    APPLY "CHOOSE" TO B-update.
    RETURN NO-APPLY.    
END.



/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

MAIN-BLOCK:
DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    RUN {&q-modelo1}. 
    ASSIGN
    FRAME f-add:VISIBLE = FALSE
    FRAME f-update:VISIBLE = FALSE
    FRAME f-search:VISIBLE = FALSE
    FRAME F-titulo:VISIBLE = TRUE.
    FRAME F-ctrl-frame:VISIBLE = TRUE.
  
    RUN enable_UI.
    
    IF RECID-stack = 0
    THEN DO:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN.
    END.
    
    RUN user_procedure1.

    FIND LAST {&FIRST-TABLE-IN-QUERY-F-maestro}
    &IF "{&RECORD-SCOPE}" <> "" &THEN
         WHERE {&RECORD-SCOPE}
    &ENDIF
    NO-LOCK  NO-ERROR.
    IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
       DISPLAY {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.
       {&OPEN-BROWSERS-IN-QUERY-F-maestro}
    END.
    ELSE CLEAR FRAME F-maestro.
     
    DISABLE {&FIELDS-IN-QUERY-F-maestro} WITH FRAME F-maestro.     

    IF NOT THIS-PROCEDURE:PERSISTENT THEN    
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-maestro _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-maestro)
  THEN DELETE WIDGET W-maestro.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-maestro _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY x-nombre 
      WITH FRAME F-titulo IN WINDOW W-maestro.
  VIEW FRAME F-titulo IN WINDOW W-maestro.
  {&OPEN-BROWSERS-IN-QUERY-F-titulo}

  {&OPEN-QUERY-F-maestro}
  GET FIRST F-maestro.
  IF AVAILABLE integral.cb-tabl THEN 
    DISPLAY integral.cb-tabl.Codigo integral.cb-tabl.Nombre 
      WITH FRAME F-maestro IN WINDOW W-maestro.
  ENABLE R-maestro1 integral.cb-tabl.Codigo integral.cb-tabl.Nombre 
      WITH FRAME F-maestro IN WINDOW W-maestro.
  {&OPEN-BROWSERS-IN-QUERY-F-maestro}
  ENABLE R-navigaate-2 B-ok B-Cancel 
      WITH FRAME F-add IN WINDOW W-maestro.
  {&OPEN-BROWSERS-IN-QUERY-F-add}
  ENABLE R-query B-query R-modify B-add B-update B-delete R-navigaate B-first 
         B-prev B-next B-last R-consulta B-browse R-exit B-exit 
      WITH FRAME F-ctrl-frame IN WINDOW W-maestro.
  {&OPEN-BROWSERS-IN-QUERY-F-ctrl-frame}
  ENABLE R-navigaate-4 B-ok-3 B-Cancel-3 
      WITH FRAME F-search IN WINDOW W-maestro.
  {&OPEN-BROWSERS-IN-QUERY-F-search}
  ENABLE R-navigaate-3 B-ok-2 B-Cancel-2 
      WITH FRAME F-update IN WINDOW W-maestro.
  {&OPEN-BROWSERS-IN-QUERY-F-update}
  VIEW W-maestro.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE user_procedure1 W-maestro 
PROCEDURE user_procedure1 :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
FIND {&TABLES-IN-QUERY-F-maestro}
    WHERE RECID( {&TABLES-IN-QUERY-F-maestro} ) = RECID-stack
        NO-LOCK  NO-ERROR.
IF AVAIL {&FIRST-TABLE-IN-QUERY-F-maestro} THEN DO:
    x-codigo = cb-tabl.codigo.
    x-nombre = cb-tabl.nombre.
    DISPLAY x-Nombre WITH FRAME F-titulo.
END.
ELSE DO:
    MESSAGE "No existen Registros." VIEW-AS ALERT-BOX ERROR
    BUTTONS OK.
    APPLY "CHOOSE" TO B-exit IN FRAME F-ctrl-frame.
END.
         
APPLY "CHOOSE" TO B-last IN FRAME F-ctrl-frame.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


