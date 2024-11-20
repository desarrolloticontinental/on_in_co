&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"Mantenimiento a las Tablas del Sistema "
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEFINE VARIABLE OK AS LOGICAL NO-UNDO.


DEFINE VARIABLE BASE-DATOS  AS CHARACTER FORMAT "X(50)".
DEFINE VARIABLE TABLA       AS CHARACTER FORMAT "X(80)".
DEFINE VARIABLE IX          AS INTEGER.
DEFINE VARIABLE NUM-CAMPOS  AS INTEGER                  COLUMN-LABEL "Nro.!CampoS"  FORMAT ">,>>>".
DEFINE VARIABLE NUM-INDICES AS INTEGER                  COLUMN-LABEL "Nro.!Idx." FORMAT ">,>>>".
DEFINE VARIABLE FLAGS       AS CHARACTER FORMAT "X(6)"  COLUMN-LABEL "Estado".
DEFINE VARIABLE TIPO-CAMPO  AS CHARACTER FORMAT "X(10)" COLUMN-LABEL "Tipo".
DEFINE VARIABLE CNT         AS INTEGER   FORMAT ">>>"   COLUMN-LABEL "#".

BASE-DATOS = PDBNAME( "DICTDB" ) + "  ( " + DBTYPE( "DICTDB" ) +   " )".

DEFINE FRAME F-REP-TABLA 
    DICTDB._FILE._FILE-LABEL COLUMN-LABEL "Descripci¢n"
    DICTDB._FILE._DUMP-NAME  COLUMN-LABEL "Dump!Name"
    Flags FORMAT "X(12)" COLUMN-LABEL "Estado"
    NUM-CAMPOS
    NUM-INDICES
    DICTDB._FILE._CRC
  HEADER
    "FECHA:"
    TODAY
    STRING(TIME, "HH:MM")
    "ESTRUCTURA DE BASE DE DATOS" AT 30
    "Página :" AT 68
    PAGE-NUMBER FORMAT ">>>"  
    SKIP
    "Base de datos :" BASE-DATOS
    SKIP(1)
    FILL("*", 80) FORMAT "X(80)"
    TABLA 
    SKIP(1) 
    WITH FONT 2 WIDTH 82.

DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.
IF SESSION:DISPLAY-TYPE = "GUI":U THEN DO:
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Exportando ...."
         HEIGHT             = 6
         WIDTH              = 80
         MAX-HEIGHT         = 6
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 6
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = yes
         SENSITIVE          = yes.
   CURRENT-WINDOW = W-WIN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-11 RECT-10 RECT-13 RECT-12 RECT-14 ~
SELECT-1 Btn_exp-def Btn_exp-data Btn_borra-def Btn_borra-data Btn-Listar ~
Btn-Selec Btn-Desel Btn-Salir 
&Scoped-Define DISPLAYED-OBJECTS SELECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Desel 
     LABEL "Deseleccionar" 
     SIZE 13 BY 1.

DEFINE BUTTON Btn-Listar 
     LABEL "Listar" 
     SIZE 13 BY .85.

DEFINE BUTTON Btn-Salir AUTO-END-KEY 
     LABEL "Salir" 
     SIZE 13 BY .85.

DEFINE BUTTON Btn-Selec 
     LABEL "Seleccionar" 
     SIZE 13 BY 1.

DEFINE BUTTON Btn_borra-data 
     LABEL "Datos" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_borra-def 
     LABEL "Definiciones" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_exp-data 
     LABEL "Datos" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_exp-def 
     LABEL "Definiciones" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 2.42.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 2.73.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 2.38.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 1.38.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15 BY 1.19.

DEFINE VARIABLE SELECT-1 AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 39 BY 10.42 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     SELECT-1 AT ROW 1.46 COL 2.29 NO-LABEL
     Btn_exp-def AT ROW 1.77 COL 42.86
     Btn_exp-data AT ROW 2.73 COL 42.86
     Btn_borra-def AT ROW 4.69 COL 42.86
     Btn_borra-data AT ROW 5.65 COL 42.86
     Btn-Listar AT ROW 7.12 COL 42.86
     Btn-Selec AT ROW 8.46 COL 42.86
     Btn-Desel AT ROW 9.46 COL 42.86
     Btn-Salir AT ROW 10.85 COL 42.86
     " Borrar" VIEW-AS TEXT
          SIZE 4.86 BY .5 AT ROW 4.12 COL 47
     "Tablas del sistema" VIEW-AS TEXT
          SIZE 38.57 BY .5 AT ROW 1 COL 2.57
     " Exportar" VIEW-AS TEXT
          SIZE 6.43 BY .5 AT ROW 1.19 COL 45.29
     RECT-11 AT ROW 1.35 COL 41.86
     RECT-10 AT ROW 4.42 COL 41.86
     RECT-13 AT ROW 6.88 COL 41.86
     RECT-12 AT ROW 8.31 COL 41.86
     RECT-14 AT ROW 10.69 COL 41.86
     SPACE(0.00) SKIP(0.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Tablas del Sistema"
         DEFAULT-BUTTON Btn_borra-def CANCEL-BUTTON Btn-Salir.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Tablas del Sistema */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Desel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Desel D-Dialog
ON CHOOSE OF Btn-Desel IN FRAME D-Dialog /* Deseleccionar */
DO:
  DEFINE VARIABLE MATCH-FILE AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.86 BY .81 NO-UNDO.
  DEFINE VARIABLE I AS INTEGER.
  DEFINE VARIABLE NUEVA-LISTA AS CHARACTER NO-UNDO.
  
  MATCH-FILE = "*.*".
  DEFINE FRAME F-MSG-2
    SKIP(1)
    SPACE(2)
    MATCH-FILE  BGCOLOR 15 FGCOLOR 0
    SPACE(2)
    SKIP(1)
    WITH VIEW-AS DIALOG-BOX NO-LABEL TITLE "Deselección de Tablas"
         FONT 4 BGCOLOR 8 FGCOLOR 0.
  UPDATE MATCH-FILE WITH FRAME F-MSG-2.
  NUEVA-LISTA = ?.
  IF NOT MATCH-FILE = "*.*"
  THEN  DO WITH FRAME {&FRAME-NAME}:
      DO I = 1 TO NUM-ENTRIES(SELECT-1:SCREEN-VALUE):
         IF NOT CAN-DO(MATCH-FILE, ENTRY(I,SELECT-1:SCREEN-VALUE))
         THEN IF NUEVA-LISTA = ?
              THEN NUEVA-LISTA = ENTRY(I, SELECT-1:SCREEN-VALUE).
              ELSE NUEVA-LISTA = NUEVA-LISTA + "," + 
                                         ENTRY(I,SELECT-1:SCREEN-VALUE). 
      END.
  END.
  SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ?.
  SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = NUEVA-LISTA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Listar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Listar D-Dialog
ON CHOOSE OF Btn-Listar IN FRAME D-Dialog /* Listar */
DO:
  IF SELECT-1:SCREEN-VALUE = ?
  THEN MESSAGE "Seleccione las tablas a listar" VIEW-AS ALERT-BOX ERROR. 
  ELSE RUN LISTAR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Selec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Selec D-Dialog
ON CHOOSE OF Btn-Selec IN FRAME D-Dialog /* Seleccionar */
DO:
    DEFINE VARIABLE MATCH-FILE AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.86 BY .81 NO-UNDO.
  DEFINE VARIABLE I AS INTEGER.
  
  MATCH-FILE = "*.*".
  DEFINE FRAME F-MSG-1
    SKIP(1)
    SPACE(2)
    MATCH-FILE  BGCOLOR 15 FGCOLOR 0
    SPACE(2)
    SKIP(1)
    WITH VIEW-AS DIALOG-BOX NO-LABEL TITLE "Selección de Tablas"
         FONT 4 BGCOLOR 8 FGCOLOR 0.
  UPDATE MATCH-FILE WITH FRAME F-MSG-1.
  DO WITH FRAME {&FRAME-NAME}:
      DO I = I TO SELECT-1:NUM-ITEMS:
         IF CAN-DO(MATCH-FILE, SELECT-1:ENTRY( I )) 
         THEN IF SELECT-1:SCREEN-VALUE = ? OR SELECT-1:SCREEN-VALUE = ""
              THEN SELECT-1:SCREEN-VALUE = SELECT-1:ENTRY( I ).
              ELSE SELECT-1:SCREEN-VALUE = SELECT-1:SCREEN-VALUE + "," + 
                                                SELECT-1:ENTRY( I ).
      END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_borra-data
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_borra-data D-Dialog
ON CHOOSE OF Btn_borra-data IN FRAME D-Dialog /* Datos */
DO:
  IF SELECT-1:SCREEN-VALUE = ?
  THEN MESSAGE "Seleccione las tablas a listar" VIEW-AS ALERT-BOX ERROR. 
  ELSE RUN BORRA-DAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_borra-def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_borra-def D-Dialog
ON CHOOSE OF Btn_borra-def IN FRAME D-Dialog /* Definiciones */
DO:
  IF SELECT-1:SCREEN-VALUE = ?
  THEN MESSAGE "Seleccione las tablas a listar" VIEW-AS ALERT-BOX ERROR. 
  ELSE RUN BORRA-DEF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_exp-data
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_exp-data D-Dialog
ON CHOOSE OF Btn_exp-data IN FRAME D-Dialog /* Datos */
DO:
  IF SELECT-1:SCREEN-VALUE = ?
  THEN MESSAGE "Seleccione las tablas a exportar" VIEW-AS ALERT-BOX ERROR. 
  ELSE RUN Exporta-Dat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_exp-def
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_exp-def D-Dialog
ON CHOOSE OF Btn_exp-def IN FRAME D-Dialog /* Definiciones */
DO:
  IF SELECT-1:SCREEN-VALUE = ?
  THEN MESSAGE "Seleccione las tablas a exportar" VIEW-AS ALERT-BOX ERROR. 
  ELSE RUN Exporta-DEF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-dat D-Dialog 
PROCEDURE Borra-dat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE I AS INTEGER   NO-UNDO.
    DEFINE VARIABLE X AS CHARACTER NO-UNDO.
    OK = NO.
    MESSAGE "Esta seguro de eliminar las datos de las tablas :" SKIP
            SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
            VIEW-AS ALERT-BOX WARNING 
            BUTTONS YES-NO 
            TITLE "Advertencia"
            UPDATE OK.
    IF OK <> YES THEN RETURN.
       
    DO i = 1 to NUM-ENTRIES( SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} )
        WITH FRAME {&FRAME-NAME}:
        x = ENTRY( i, SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} ).
        RUN bin/_borrdat.i x.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-def D-Dialog 
PROCEDURE Borra-def :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE I AS INTEGER NO-UNDO.
    OK = NO.
    MESSAGE "Esta seguro de eliminar las estructuras de las tablas" SKIP
            SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} 
            VIEW-AS ALERT-BOX WARNING  
            BUTTONS YES-NO 
            TITLE "Advertencia"
            UPDATE OK.

   IF OK <> YES THEN RETURN.

   DO i = 1 to NUM-ENTRIES( SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} )
      WITH FRAME {&FRAME-NAME} TRANSACTION:
        FIND DICTDB._FILE WHERE 
           DICTDB._FILE._FILE-NAME = ENTRY(i, SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} ).
        FOR EACH DICTDB._INDEX WHERE 
            DICTDB._INDEX._FILE-recid = RECID(DICTDB._FILE):
            FOR EACH DICTDB._INDEX-FIELD WHERE 
                DICTDB._INDEX-FIELD._Index-recid = RECID(DICTDB._INDEX):
                DELETE DICTDB._INDEX-FIELD.
             END.
             DELETE DICTDB._INDEX.
        END.
        FOR EACH DICTDB._FIELD-TRIG WHERE 
            DICTDB._FIELD-TRIG._File-recid = RECID(DICTDB._FILE):
            DELETE DICTDB._FIELD-TRIG.
        END.
        FOR EACH DICTDB._FIELD WHERE 
                DICTDB._FIELD._File-recid = RECID(DICTDB._FILE):
            DELETE DICTDB._FIELD.
        END.
        FOR EACH DICTDB._FILE-TRIG WHERE 
                DICTDB._FILE-TRIG._File-recid = RECID(DICTDB._FILE):
            DELETE DICTDB._FILE-TRIG.
        END.
        DELETE DICTDB._FILE.
    END.
    RUN Extrae-Tablas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY SELECT-1 
      WITH FRAME D-Dialog.
  ENABLE RECT-11 RECT-10 RECT-13 RECT-12 RECT-14 SELECT-1 Btn_exp-def 
         Btn_exp-data Btn_borra-def Btn_borra-data Btn-Listar Btn-Selec 
         Btn-Desel Btn-Salir 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exporta-Dat D-Dialog 
PROCEDURE Exporta-Dat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ARCHIVO AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE I       AS INTEGER   NO-UNDO.
    VIEW W-WIN.
    HIDE FRAME {&FRAME-NAME}.
    DO WITH FRAME {&FRAME-NAME}:
      DO I = 1 TO NUM-ENTRIES(SELECT-1:SCREEN-VALUE):
        ARCHIVO = ENTRY( i, SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} ).
        RUN prodict/dump_d.p (archivo, "dbs/", "ibm850").
      END.
    END.
    VIEW FRAME {&FRAME-NAME}.
    MESSAGE "Proceso completado ..." VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Exporta-Def D-Dialog 
PROCEDURE Exporta-Def :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ARCHIVO AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE I       AS INTEGER   NO-UNDO.
    DEFINE FRAME F-MSG-5
        SKIP(1)
        SPACE(2)
        ARCHIVO FORMAT "X(30)"
        SPACE(2)
        SKIP(1)
        WITH VIEW-AS DIALOG-BOX CENTERED NO-LABEL 
            TITLE "Expotando definiciones .." FONT 4.
    VIEW FRAME F-MSG-5.        
    DO WITH FRAME {&FRAME-NAME}:
      DO I = 1 TO NUM-ENTRIES(SELECT-1:SCREEN-VALUE):
        ARCHIVO = ENTRY(I,SELECT-1:SCREEN-VALUE).
        ARCHIVO:SCREEN-VALUE IN FRAME F-MSG-5 = ARCHIVO.
       
        RUN prodict/dump_df.p ("dbs/" + archivo + ".df", archivo, "ibm850").
      END.
    END.
    HIDE FRAME F-MSG-5.
    MESSAGE "Proceso completado ..." VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Extrae-tablas D-Dialog 
PROCEDURE Extrae-tablas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ok = SESSION:SET-WAIT-STATE("GENERAL").
  SELECT-1:LIST-ITEMS IN FRAME {&FRAME-NAME} = ?.
  FOR EACH DICTDB._FILE WHERE NOT DICTDB._FILE._FILE-NAME BEGINS "_":
    OK = SELECT-1:ADD-LAST( DICTDB._FILE._FILE-NAME) IN FRAME {&FRAME-NAME}.
  END.
  ok = SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Listar D-Dialog 
PROCEDURE Listar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE I AS INTEGER NO-UNDO.
  OUTPUT TO "C:\ACTIVO.TXT" PAGED PAGE-SIZE 60.
                     
  DO i = 1 to NUM-ENTRIES( SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} )
      WITH FRAME F-REP-TABLA:
    FIND DICTDB._FILE WHERE 
       DICTDB._FILE._FILE-NAME = ENTRY(i, SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} ).
    TABLA = "  Tabla : " + DICTDB._FILE._FILE-NAME + "  ".
    IX    = 40 - LENGTH( TABLA ) / 2 .
    IF LENGTH( TABLA ) < 80
    THEN TABLA = FILL("=", IX ) + TABLA + FILL("=", 80).
    NUM-CAMPOS = 0.
    FOR EACH DICTDB._FIELD WHERE 
          DICTDB._FIELD._FILE-RECID = RECID( DICTDB._FILE ) :
      NUM-CAMPOS = NUM-CAMPOS + 1.
    END.
    NUM-INDICES = 0.                              
    FOR EACH DICTDB._INDEX WHERE 
          DICTDB._INDEX._FILE-RECID = RECID( DICTDB._FILE ) :   
      NUM-INDICES = NUM-INDICES + 1.
    END.
    Flags = IF DICTDB._FILE._FROZEN THEN "Inmodificable" ELSE "".
  
    DISPLAY DICTDB._FILE._FILE-LABEL 
            DICTDB._FILE._DUMP-NAME  
            Flags 
            NUM-CAMPOS
            NUM-INDICES
            DICTDB._FILE._CRC.
            
    RUN CAMPOS.
    RUN INDICES.
    PAGE.
  END.
  OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE CAMPOS.        
  DEFINE FRAME F-REP-CAMPOS
      DICTDB._FIELD._ORDER                     COLUMN-LABEL "Orden"
      DICTDB._FIELD._FIELD-NAME FORMAT "X(29)" COLUMN-LABEL "Nombre de Campo"
      Tipo-Campo
      Flags
      DICTDB._FIELD._FORMAT     FORMAT "X(20)" COLUMN-LABEL "Formato"
      /*
      DICTDB._FIELD._INITIAL    FORMAT "X(10)" COLUMN-LABEL "Valor Inicial"
    
      DICTDB._FIELD._LABEL      FORMAT "X(25)" COLUMN-LABEL "Etiqueta"
      DICTDB._FIELD._COL-LABEL  FORMAT "X(20)" COLUMN-LABEL "Etiqueta Columna"
      */
    HEADER                     
      "SUMARIO DE CAMPOS" AT 30
      SKIP
       " Estado de Campos:  <c>aso sensitivo may/min, <m>andatorio, <v>ista especial "
       SKIP(1)
    WITH FONT 2 WIDTH 82.
     
  FOR EACH DICTDB._FIELD WHERE 
        DICTDB._FIELD._FILE-RECID = RECID( DICTDB._FILE ) 
        BY DICTDB._FIELD._ORDER  
        WITH FRAME F-REP-CAMPOS:
      TIPO-CAMPO = SUBSTR(DICTDB._FIELD._DATA-TYPE,1,4).
      IF TIPO-CAMPO = "deci"
      THEN TIPO-CAMPO = TIPO-CAMPO + "-" + STRING(DICTDB._FIELD._DECIMAL).
      IF DICTDB._FIELD._EXTENT > 0
      THEN TIPO-CAMPO = TIPO-CAMPO + "[" + STRING(DICTDB._FIELD._EXTENT) + "]".

      Flags =         IF DICTDB._FIELD._Mandatory    THEN "m" ELSE "".
      Flags = Flags + IF DICTDB._FIELD._FLD-CASE     THEN "c" ELSE "".
      Flags = Flags + IF DICTDB._FIELD._View-As <> ? THEN "v" ELSE "".
      LAZO1:
      FOR EACH DICTDB._INDEX WHERE 
               DICTDB._INDEX._FILE-RECID = RECID( DICTDB._FILE ):
          FOR EACH DICTDB._INDEX-FIELD WHERE 
               DICTDB._INDEX-FIELD._INDEX-RECID = RECID( DICTDB._INDEX ):  
            IF DICTDB._INDEX-FIELD._FIELD-RECID = RECID( DICTDB._FIELD )
            THEN DO:
                Flags = Flags + "i".
                LEAVE LAZO1.
            END.
          END.
      END.     
      DISPLAY DICTDB._FIELD._ORDER      
              DICTDB._FIELD._FIELD-NAME 
              Tipo-Campo
              Flags
              DICTDB._FIELD._FORMAT
              /*     
              DICTDB._FIELD._INITIAL    
              DICTDB._FIELD._LABEL      
              DICTDB._FIELD._COL-LABEL */ . 
  END.        

END PROCEDURE.


PROCEDURE INDICES.
  DEFINE VARIABLE CAMPO AS CHARACTER FORMAT "X(31)" COLUMN-LABEL "Nombre de Campo" NO-UNDO.               
  DEFINE FRAME F-REP-INDICES
      Flags                                    
      DICTDB._INDEX._INDEX-NAME                COLUMN-LABEL "Nombre de Indice"
      CNT
      CAMPO
    HEADER                     
      "SUMARIO DE INDICES" AT 29
      SKIP
       " Estado de Indices: <p>rimario, <u>nico, <p>alabra, <i>nactivo, + asc, - desc"    
       SKIP(1)
    WITH FONT 2 WIDTH 132. 


  FOR EACH DICTDB._INDEX WHERE 
        DICTDB._INDEX._FILE-RECID = RECID( DICTDB._FILE ) 
        WITH FRAME F-REP-INDICES:    
      Flags = "".
      IF  DICTDB._FILE._PRIME-INDEX = RECID( DICTDB._INDEX ) 
      THEN Flags = Flags + "p".
      IF NOT DICTDB._INDEX._Active  THEN Flags = Flags + "i".
      IF DICTDB._INDEX._Unique      THEN Flags = Flags + "u".
      IF DICTDB._INDEX._Wordidx > 0 THEN Flags = Flags + "p".
      
      DISPLAY Flags
              DICTDB._INDEX._INDEX-NAME.
      CNT = 0.
      FOR EACH DICTDB._INDEX-FIELD WHERE 
               DICTDB._INDEX-FIELD._INDEX-RECID = RECID( DICTDB._INDEX )
               WITH FRAME F-REP-INDICES:  
          CAMPO = IF DICTDB._INDEX-FIELD._ASCENDING THEN "+ "  ELSE "- ".
          FIND DICTDB._FIELD WHERE  
              DICTDB._INDEX-FIELD._FIELD-RECID = RECID( DICTDB._FIELD )
              NO-LOCK NO-ERROR.
          CNT = CNT + 1.
          IF AVAILABLE DICTDB._FIELD
          THEN CAMPO = CAMPO + DICTDB._FIELD._FIELD-NAME.
          DISPLAY CNT CAMPO.
          DOWN.
     END.
     DOWN.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects D-Dialog 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* Extraer las tablas del sistema */
  RUN Extrae-Tablas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

