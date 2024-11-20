&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r11 GUI
&ANALYZE-RESUME
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
DEFINE VARIABLE FECHA AS DATE.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE DIA     AS INTEGER NO-UNDO.
DEFINE VARIABLE MES     AS INTEGER NO-UNDO.
DEFINE VARIABLE DIA-MAX AS INTEGER NO-UNDO.
DEFINE VARIABLE COL-INI AS DECIMAL INITIAL ? NO-UNDO.
DEFINE VARIABLE ROW-INI AS DECIMAL INITIAL ? NO-UNDO.
DEFINE VARIABLE ancho   AS DECIMAL INITIAL ? NO-UNDO.
DEFINE VARIABLE alto    AS DECIMAL INITIAL ? NO-UNDO.

DEFINE VARIABLE H-DIA   AS WIDGET-HANDLE EXTENT 31.
DEFINE VARIABLE x AS INTEGER NO-UNDO.
DEFINE VARIABLE LISTA-MESES    AS CHARACTER no-undo.

IF FECHA = ? THEN FECHA = TODAY.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MES-STRING ANO BUTTON-UP BUTTON-DOWN 
&Scoped-Define DISPLAYED-OBJECTS MES-STRING ANO 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-DOWN 
     IMAGE-UP FILE "img/b-down":U
     LABEL "" 
     SIZE 2.14 BY .46.

DEFINE BUTTON BUTTON-UP 
     IMAGE-UP FILE "img/b-up":U
     LABEL "" 
     SIZE 2.14 BY .46.

DEFINE VARIABLE MES-STRING AS CHARACTER FORMAT "X(256)":U INITIAL "Enero" 
     VIEW-AS COMBO-BOX INNER-LINES 13
     LIST-ITEMS "Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Setiembre","Octubre","Noviembre","Diciembre" 
     SIZE 13.57 BY 1 NO-UNDO.

DEFINE VARIABLE ANO AS INTEGER FORMAT ">>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.57 BY .88 NO-UNDO.

DEFINE VARIABLE cDia AS INTEGER FORMAT ">9":U INITIAL 0  EXTENT 31
     VIEW-AS FILL-IN 
     SIZE 3 BY .73
     FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     MES-STRING AT ROW 1.08 COL 1 NO-LABEL
     cDia[1] AT ROW 2.88 COL 1.43 NO-LABEL
     cDia[2] AT ROW 2.88 COL 4.43 NO-LABEL
     cDia[8] AT ROW 3.73 COL 1.43 NO-LABEL
     cDia[9] AT ROW 3.73 COL 2.43 COLON-ALIGNED NO-LABEL
     cDia[15] AT ROW 4.58 COL 1.43 NO-LABEL
     cDia[16] AT ROW 4.58 COL 2.43 COLON-ALIGNED NO-LABEL
     cDia[22] AT ROW 5.42 COL 1.43 NO-LABEL
     cDia[23] AT ROW 5.42 COL 2.43 COLON-ALIGNED NO-LABEL
     cDia[29] AT ROW 6.31 COL 1.43 NO-LABEL
     cDia[30] AT ROW 6.31 COL 2.43 COLON-ALIGNED NO-LABEL
     cDia[3] AT ROW 2.88 COL 7.43 NO-LABEL
     cDia[4] AT ROW 2.88 COL 10.43 NO-LABEL
     cDia[10] AT ROW 3.73 COL 5.43 COLON-ALIGNED NO-LABEL
     cDia[11] AT ROW 3.73 COL 8.43 COLON-ALIGNED NO-LABEL
     cDia[17] AT ROW 4.58 COL 5.43 COLON-ALIGNED NO-LABEL
     cDia[18] AT ROW 4.58 COL 8.43 COLON-ALIGNED NO-LABEL
     cDia[24] AT ROW 5.42 COL 5.43 COLON-ALIGNED NO-LABEL
     cDia[25] AT ROW 5.42 COL 8.43 COLON-ALIGNED NO-LABEL
     cDia[31] AT ROW 6.31 COL 5.43 COLON-ALIGNED NO-LABEL
     ANO AT ROW 1.08 COL 15.29 NO-LABEL
     cDia[5] AT ROW 2.88 COL 13.43 NO-LABEL
     cDia[12] AT ROW 3.73 COL 11.43 COLON-ALIGNED NO-LABEL
     cDia[19] AT ROW 4.58 COL 11.43 COLON-ALIGNED NO-LABEL
     cDia[26] AT ROW 5.42 COL 11.43 COLON-ALIGNED NO-LABEL
     BUTTON-UP AT ROW 1.08 COL 20.86
     BUTTON-DOWN AT ROW 1.5 COL 20.86
     cDia[6] AT ROW 2.88 COL 16.43 NO-LABEL
     cDia[7] AT ROW 2.88 COL 19.43 NO-LABEL
     cDia[13] AT ROW 3.73 COL 14.43 COLON-ALIGNED NO-LABEL
     cDia[14] AT ROW 3.73 COL 17.43 COLON-ALIGNED NO-LABEL
     cDia[20] AT ROW 4.58 COL 14.43 COLON-ALIGNED NO-LABEL
     cDia[21] AT ROW 4.58 COL 17.43 COLON-ALIGNED NO-LABEL
     cDia[27] AT ROW 5.42 COL 14.43 COLON-ALIGNED NO-LABEL
     cDia[28] AT ROW 5.42 COL 17.43 COLON-ALIGNED NO-LABEL
     "  L     M    M     J     V    S     D" VIEW-AS TEXT
          SIZE 21 BY .77 AT ROW 2.04 COL 1.57
          BGCOLOR 1 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 6.42
         WIDTH              = 22.29.
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Default                                      */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN ANO IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN cDia[10] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[10]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[11] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[11]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[12] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[12]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[13] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[13]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[14] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[14]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[15] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[15]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[16] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[16]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[17] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[17]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[18] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[18]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[19] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[19]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[1] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[1]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[20] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[20]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[21] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[21]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[22] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[22]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[23] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[23]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[24] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[24]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[25] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[25]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[26] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[26]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[27] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[27]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[28] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[28]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[29] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[29]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[2] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[2]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[30] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[30]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[31] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[31]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[3] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[3]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[4] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[4]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[5] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[5]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[6] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[6]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[7] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[7]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[8] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       cDia[8]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN cDia[9] IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       cDia[9]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR COMBO-BOX MES-STRING IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON BACK-TAB OF FRAME F-Main
ANYWHERE
DO:
  ASSIGN H-DIA[DIA]:FGCOLOR = ?
         H-DIA[DIA]:BGCOLOR = ?.
  DIA = IF DIA = 1 THEN DIA-MAX ELSE DIA - 1.
  ASSIGN H-DIA[DIA]:FGCOLOR = 15
         H-DIA[DIA]:BGCOLOR = 1.
  RUN CAMBIO-FECHA.        
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON CURSOR-DOWN OF FRAME F-Main
ANYWHERE
DO:
  APPLY "CHOOSE":U TO BUTTON-DOWN.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON CURSOR-LEFT OF FRAME F-Main
ANYWHERE
DO:
    MES = IF MES > 1 THEN MES - 1 ELSE 12.
    MES-STRING:SCREEN-VALUE = ENTRY( MES, LISTA-MESES).  
    RUN Calendario.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON CURSOR-RIGHT OF FRAME F-Main
ANYWHERE
DO:
    MES = IF MES < 12 THEN MES + 1 ELSE 1.
    MES-STRING:SCREEN-VALUE = ENTRY( MES, LISTA-MESES).  
    RUN Calendario.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON CURSOR-UP OF FRAME F-Main
ANYWHERE
DO:
  APPLY "CHOOSE":U TO BUTTON-UP.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON TAB OF FRAME F-Main
ANYWHERE
DO:
  ASSIGN H-DIA[DIA]:FGCOLOR = ?
         H-DIA[DIA]:BGCOLOR = ?.
  DIA = IF DIA + 1 > DIA-MAX THEN 1 ELSE DIA + 1.
  RUN CAMBIO-FECHA. 
  ASSIGN H-DIA[DIA]:FGCOLOR = 15
         H-DIA[DIA]:BGCOLOR = 1.
       
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ANO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ANO V-table-Win
ON LEAVE OF ANO IN FRAME F-Main
DO:
  IF INPUT ANO < 1900 AND INPUT ANO > 2099
  THEN DO:
    MESSAGE "Invalido A¤o" VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.
  
  IF INPUT ANO <> ANO
  THEN DO:
    ASSIGN ANO.
    run calendario.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-DOWN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-DOWN V-table-Win
ON CHOOSE OF BUTTON-DOWN IN FRAME F-Main
DO:
  IF ANO < 1099
  THEN DO:
    BELL.
    RETURN NO-APPLY.
  END.
  DISPLAY (INPUT ANO - 1) @ ANO WITH FRAME {&FRAME-NAME}.
  ASSIGN ANO.
  run calendario.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-UP V-table-Win
ON CHOOSE OF BUTTON-UP IN FRAME F-Main
DO:
  IF ANO > 2099
  THEN DO:
    BELL.
    RETURN NO-APPLY.
  END.
  DISPLAY (INPUT ANO + 1) @ ANO WITH FRAME {&FRAME-NAME}.
  ASSIGN ANO.
  RUN Calendario.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cDia[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDia[1] V-table-Win
ON ANY-PRINTABLE OF cDia[1] IN FRAME F-Main
,  cDia[2],  cDia[3],  cDia[4],  cDia[5], 
     cDia[6],  cDia[7],  cDia[8],  cDia[9], cDia[10],
    cDia[11], cDia[12], cDia[13], cDia[14], cDia[15], 
    cDia[16], cDia[17], cDia[18], cDia[19], cDia[20],
    cDia[21], cDia[22], cDia[23], cDia[24], cDia[25], 
    cDia[26], cDia[27], cDia[28], cDia[29], cDia[30],
    cDia[31]
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cDia[1] V-table-Win
ON ENTRY OF cDia[1] IN FRAME F-Main
,  cDia[2],  cDia[3],  cDia[4],  cDia[5], 
     cDia[6],  cDia[7],  cDia[8],  cDia[9], cDia[10],
    cDia[11], cDia[12], cDia[13], cDia[14], cDia[15], 
    cDia[16], cDia[17], cDia[18], cDia[19], cDia[20],
    cDia[21], cDia[22], cDia[23], cDia[24], cDia[25], 
    cDia[26], cDia[27], cDia[28], cDia[29], cDia[30],
    cDia[31]
DO:
  ASSIGN H-Dia[Dia]:BGCOLOR = ?.
  IF H-Dia[Dia]:FGCOLOR = 12
  THEN ASSIGN H-Dia[Dia]:FGCOLOR = 12.
  ELSE ASSIGN H-Dia[Dia]:FGCOLOR = ?.

  DIA = INTEGER(SELF:SCREEN-VALUE).
  RUN CAMBIO-FECHA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MES-STRING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MES-STRING V-table-Win
ON VALUE-CHANGED OF MES-STRING IN FRAME F-Main
DO:
  MES = LOOKUP( SELF:SCREEN-VALUE, LISTA-MESES).
  RUN Calendario.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-fecha V-table-Win 
PROCEDURE adm-fecha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER c-fecha AS DATE.
  ASSIGN H-DIA[DIA]:FGCOLOR = 15
         H-DIA[DIA]:BGCOLOR = 1.
  c-FECHA = DATE(MES, DIA, ANO).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CALENDARIO V-table-Win 
PROCEDURE CALENDARIO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE I  AS INTEGER NO-UNDO.
    DEFINE VARIABLE FECHA-TEMP     AS DATE.
    DEFINE VARIABLE FECHA-FIN-MES  AS DATE.
    DEFINE VARIABLE ROW1  AS DECIMAL.
    DEFINE VARIABLE COL1  AS DECIMAL.
    DEFINE VARIABLE OK    AS LOGICAL.
    
     
    FECHA-TEMP = DATE( MES, 1, ANO).
    IF MES = 12 THEN FECHA-FIN-MES = DATE( MES, 31, ANO).
                   ELSE FECHA-FIN-MES = DATE( MES + 1, 1, ANO) - 1.
        
    DIA-MAX = DAY(FECHA-FIN-MES).                   
    IF DIA > DIA-MAX THEN DO:
        H-Dia[Dia]:FGCOLOR = ?.
        H-Dia[Dia]:BGCOLOR = ?.
        DIA = DIA-MAX.
        H-Dia[Dia]:FGCOLOR = 15.
        H-Dia[Dia]:BGCOLOR = 1.
    END.
    i = WEEKDAY( FECHA-TEMP ).
        
    IF i = 1 THEN i = 7. ELSE i = i - 1.

    ROW1 = ROW-INI.
    COL1 = COL-INI + ( i - 1 ) * ANCHO.
    DO x = 1 TO 31:
        H-Dia[x]:VISIBLE = NO.
    END.        
    DO x = 1 TO 31:
        ASSIGN 
            H-Dia[x]:ROW = ROW1
            H-Dia[x]:COL = COL1.
        COL1 = COL1 + ANCHO.    
        IF (x + i - 1) MODULO 7 = 0 
        THEN ASSIGN COL1 = COL-INI    
                    ROW1 = ROW1 + ALTO
                    H-Dia[x]:FGCOLOR = 12.
        ELSE H-Dia[x]:FGCOLOR = ?.

        IF (x + i) / 7 > 5 
        THEN ROW1 = ROW-INI.
        IF cDia[x] <= DIA-MAX
        THEN DO:
            DISPLAY cDia[x] WITH FRAME {&FRAME-NAME}.
            IF NOT H-Dia[x]:SENSITIVE THEN H-Dia[x]:SENSITIVE = YES.
        END.
    END.
    RUN CAMBIO-FECHA.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CAMBIO-FECHA V-table-Win 
PROCEDURE CAMBIO-FECHA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF Dia > 0 AND Dia < 32
  THEN IF H-Dia[Dia]:FGCOLOR = 12
       THEN ASSIGN H-Dia[Dia]:FGCOLOR = 12
                   H-Dia[Dia]:BGCOLOR = 1.
       ELSE ASSIGN H-Dia[Dia]:FGCOLOR = 15
                   H-Dia[Dia]:BGCOLOR = 1.
  RUN NEW-STATE( "CALENDARIO":U ).        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  LISTA-MESES = MES-STRING:LIST-ITEMS IN FRAME {&FRAME-NAME}.
  MES         = MONTH(FECHA).
  MES-STRING  = ENTRY( MES, LISTA-MESES).
  ANO         = YEAR(FECHA).
  DIA         = DAY(FECHA).
  DISPLAY MES-STRING ANO WITH FRAME {&FRAME-NAME}.
  DO x = 1 TO 31:
     cDia[ x ] = x.
  END.        

  H-DIA[1]  = cDia[1]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[2]  = cDia[2]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[3]  = cDia[3]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[4]  = cDia[4]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[5]  = cDia[5]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[6]  = cDia[6]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[7]  = cDia[7]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[8]  = cDia[8]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[9]  = cDia[9]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[10] = cDia[10]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[11] = cDia[11]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[12] = cDia[12]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[13] = cDia[13]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[14] = cDia[14]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[15] = cDia[15]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[16] = cDia[16]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[17] = cDia[17]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[18] = cDia[18]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[19] = cDia[19]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[20] = cDia[20]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[21] = cDia[21]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[22] = cDia[22]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[23] = cDia[23]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[24] = cDia[24]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[25] = cDia[25]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[26] = cDia[26]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[27] = cDia[27]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[28] = cDia[28]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[29] = cDia[29]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[30] = cDia[30]:HANDLE IN FRAME {&FRAME-NAME}.
  H-DIA[31] = cDia[31]:HANDLE IN FRAME {&FRAME-NAME}.

  ROW-INI = cDia[1]:ROW  IN FRAME {&FRAME-NAME}.
  COL-INI = cDia[1]:COL  IN FRAME {&FRAME-NAME}.
  ANCHO   = cDia[2]:COL  IN FRAME {&FRAME-NAME} - 
            cDia[1]:COL  IN FRAME {&FRAME-NAME}.
  ALTO    = cDia[8]:ROW  IN FRAME {&FRAME-NAME} - 
            cDia[1]:ROW  IN FRAME {&FRAME-NAME}.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN CALENDARIO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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


