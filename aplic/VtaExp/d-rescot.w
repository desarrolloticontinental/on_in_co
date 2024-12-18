&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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

DEF INPUT PARAMETER pParam AS CHAR.

IF LOOKUP(pParam, 'SI,NO,TRUE,FALSE,YES,NO') = 0 THEN DO:
    MESSAGE 'El par�metro de entrada debe tener los siguientes valores:' SKIP
        "SI, NO, TRUE, FALSE, YES o NO" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

DEFINE VAR s-acceso-total AS LOG INIT NO NO-UNDO.

IF LOOKUP(pParam, 'SI,YES,TRUE') > 0 THEN s-acceso-total = YES.
ELSE s-acceso-total = NO.

/* Parameters Definitions ---                                           */
DEFINE NEW SHARED VARIABLE lh_Handle  AS HANDLE.

/* Local Variable Definitions ---                                       */
/*
    @PRINTER2.W    VERSION 1.0
*/
{lib/def-prn.i}    
DEFINE STREAM report.
/*DEFINE VARIABLE x-Raya     AS CHARACTER FORMAT "X(145)".
DEFINE {&NEW} SHARED VARIABLE xTerm     AS CHARACTER INITIAL "".
DEFINE {&NEW} SHARED VARIABLE s-aplic-id  LIKE Modulos.Modulo.
DEFINE {&NEW} SHARED VARIABLE s-user-id  LIKE _user._userid.*/

DEFINE NEW SHARED VARIABLE xTerm     AS CHARACTER INITIAL "".
DEFINE NEW SHARED VARIABLE s-aplic-id  LIKE Modulos.Modulo.
DEFINE NEW SHARED VARIABLE s-user-id  LIKE _user._userid.

  
def var l-immediate-display  AS LOGICAL.
DEFINE        VARIABLE cb-codcia AS INTEGER INITIAL 0.
DEFINE        VARIABLE pv-codcia AS INTEGER INITIAL 0.
DEFINE        VARIABLE cl-codcia AS INTEGER INITIAL 0.
DEFINE        VARIABLE PTO        AS LOGICAL.

DEFINE VARIABLE T-CLIEN AS CHAR INIT "" NO-UNDO.
DEFINE VARIABLE T-VENDE  AS CHAR INIT "".
DEFINE VARIABLE X-MON    AS CHAR FORMAT "X(3)".
DEFINE VARIABLE X-EST    AS CHAR FORMAT "X(30)".

/*VARIABLES GLOBALES */
DEFINE SHARED VAR S-CODCIA AS INTEGER.
DEFINE SHARED VAR S-CODDIV AS CHAR.
DEFINE SHARED VAR S-NOMCIA AS CHAR.
DEFINE SHARED VAR S-CODALM AS CHAR.

/*Variables para el Excel*/
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER init 1.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 2.
DEFINE VARIABLE i-Column                AS INTEGER NO-UNDO.
DEFINE VARIABLE j-Column                AS INTEGER NO-UNDO.

DEFINE VAR W-TOTBRU AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
DEFINE VAR W-TOTDSC AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
DEFINE VAR W-TOTVAL AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.  
DEFINE VAR W-TOTIGV AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
DEFINE VAR W-TOTVEN AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.

DEF VAR FI-MENSAJE AS CHAR FORMAT "X(40)" .

DEFINE FRAME F-Proceso
     IMAGE-1 AT ROW 1.5 COL 5
     "Espere un momento" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1.5 COL 16 FONT 6
     "por favor ...." VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 2.5 COL 19 FONT 6
          SKIP
     Fi-Mensaje NO-LABEL FONT 6
     SKIP     
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 
         TITLE "Procesando ..." FONT 7.

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
&Scoped-Define ENABLED-OBJECTS RECT-5 RECT-41 RECT-43 COMBO-BOX-Lista ~
RADIO-SET-Orden f-desde f-hasta Btn_OK RADIO-SET-1 B-impresoras Btn_Cancel ~
Btn_Excel RB-NUMBER-COPIES RB-BEGIN-PAGE RB-END-PAGE 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-3 COMBO-BOX-Lista RADIO-SET-Orden ~
TOGGLE-Vendedor f-desde f-hasta RADIO-SET-1 RB-NUMBER-COPIES RB-BEGIN-PAGE ~
RB-END-PAGE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON b-archivo 
     IMAGE-UP FILE "IMG/pvstop":U
     LABEL "&Archivos.." 
     SIZE 5 BY 1.

DEFINE BUTTON B-impresoras 
     IMAGE-UP FILE "IMG/pvprint":U
     IMAGE-DOWN FILE "IMG/pvprintd":U
     LABEL "" 
     SIZE 5 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "img\b-cancel":U
     LABEL "Cancelar" 
     SIZE 12 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON Btn_Excel AUTO-GO 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "A&yuda" 
     SIZE 12 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "img\b-ok":U
     LABEL "Aceptar" 
     SIZE 12 BY 1.5
     BGCOLOR 8 .

DEFINE VARIABLE COMBO-BOX-Lista AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lista de Precios" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE f-desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69 NO-UNDO.

DEFINE VARIABLE f-hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Divisi�n" 
     VIEW-AS FILL-IN 
     SIZE 9.72 BY .69
     FONT 6 NO-UNDO.

DEFINE VARIABLE RB-BEGIN-PAGE AS INTEGER FORMAT "ZZZ9":U INITIAL 1 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RB-END-PAGE AS INTEGER FORMAT "ZZZ9":U INITIAL 9999 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RB-NUMBER-COPIES AS INTEGER FORMAT "ZZZ9":U INITIAL 1 
     LABEL "No. Copias" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .69
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RB-OUTPUT-FILE AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26.72 BY .69
     BGCOLOR 15 FGCOLOR 0 FONT 12 NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pantalla", 2,
"Impresora", 1,
"Archivo", 3
     SIZE 12 BY 3
     FONT 6 NO-UNDO.

DEFINE VARIABLE RADIO-SET-Orden AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Por Correlativo", 1,
"Por Vendedor", 2,
"Por Postal", 3,
"Por Fecha Entrega", 4,
"Por Cliente", 5,
"Por Condicion de Venta", 6
     SIZE 23 BY 4.04 NO-UNDO.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 5.19.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.86 BY 6.12.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 51.14 BY 6.12.

DEFINE VARIABLE TOGGLE-Vendedor AS LOGICAL INITIAL no 
     LABEL "Solo totales por vendedor" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.29 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     FILL-IN-3 AT ROW 1.19 COL 2.86
     COMBO-BOX-Lista AT ROW 1.19 COL 44 COLON-ALIGNED WIDGET-ID 2
     RADIO-SET-Orden AT ROW 1.96 COL 9 NO-LABEL
     TOGGLE-Vendedor AT ROW 2.54 COL 33
     f-desde AT ROW 4.38 COL 37 COLON-ALIGNED
     f-hasta AT ROW 4.38 COL 53.57 COLON-ALIGNED
     Btn_OK AT ROW 6.65 COL 55.57
     RADIO-SET-1 AT ROW 7.35 COL 3.14 NO-LABEL
     B-impresoras AT ROW 8.38 COL 16.29
     Btn_Cancel AT ROW 8.5 COL 55.57
     b-archivo AT ROW 9.38 COL 16.43
     RB-OUTPUT-FILE AT ROW 9.54 COL 20.29 COLON-ALIGNED NO-LABEL
     Btn_Excel AT ROW 10.35 COL 55.57
     RB-NUMBER-COPIES AT ROW 11.08 COL 10.86 COLON-ALIGNED
     RB-BEGIN-PAGE AT ROW 11.08 COL 25.57 COLON-ALIGNED
     RB-END-PAGE AT ROW 11.08 COL 39.72 COLON-ALIGNED
     " Configuraci�n de Impresi�n" VIEW-AS TEXT
          SIZE 49.86 BY .62 AT ROW 6.42 COL 1.86
          BGCOLOR 1 FGCOLOR 15 FONT 6
     "P�ginas" VIEW-AS TEXT
          SIZE 7.72 BY .54 AT ROW 10.46 COL 34
          FONT 6
     "Ordenado" VIEW-AS TEXT
          SIZE 7 BY .5 AT ROW 1.96 COL 2
     "Rango de Fechas :" VIEW-AS TEXT
          SIZE 17.14 BY .62 AT ROW 3.5 COL 43.57
          FONT 6
     RECT-5 AT ROW 6.19 COL 1
     RECT-41 AT ROW 1 COL 1
     RECT-43 AT ROW 6.23 COL 54.14
     SPACE(0.71) SKIP(0.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Resumen Pedidos Comerciales".


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

{src/bin/_prns.i}
{src/adm/method/containr.i}
{src/adm-vm/method/vmviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON b-archivo IN FRAME D-Dialog
   NO-ENABLE                                                            */
ASSIGN 
       b-archivo:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME D-Dialog
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN RB-OUTPUT-FILE IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RB-OUTPUT-FILE:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-Vendedor IN FRAME D-Dialog
   NO-ENABLE                                                            */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Resumen Pedidos Comerciales */
DO:  

    ASSIGN f-Desde f-hasta RADIO-SET-Orden TOGGLE-Vendedor.

    IF f-desde = ? then do:
       MESSAGE "Ingrese Fecha Desde ... " VIEW-AS ALERT-BOX.
       APPLY "ENTRY":U to f-desde.
       RETURN NO-APPLY.   
    END.

    IF f-hasta = ? then do:
       MESSAGE "Ingrese Fecha Hasta ... " VIEW-AS ALERT-BOX.
       APPLY "ENTRY":U to f-hasta.
       RETURN NO-APPLY.   
    END.   

    IF f-desde > f-hasta then do:
       MESSAGE "Rango de fechas Mal ingresado" VIEW-AS ALERT-BOX.
       APPLY "ENTRY":U to f-desde.
       RETURN NO-APPLY.
    END.

    CASE RADIO-SET-Orden:
      WHEN 1 THEN RUN Excel-ofi.
      WHEN 2 THEN DO:
          IF TOGGLE-Vendedor = NO
          THEN RUN Excel-ven.
          ELSE RUN Excel-ven-2.
      END.
      WHEN 3 THEN RUN Excel-postal.
      WHEN 4 THEN RUN Excel-ent.
      WHEN 5 THEN RUN Excel-cliente.
      WHEN 6 THEN RUN Excel-cndvta.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-archivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-archivo D-Dialog
ON CHOOSE OF b-archivo IN FRAME D-Dialog /* Archivos.. */
DO:
     SYSTEM-DIALOG GET-FILE RB-OUTPUT-FILE
        TITLE      "Archivo de Impresi�n ..."
        FILTERS    "Archivos Impresi�n (*.txt)"   "*.txt",
                   "Todos (*.*)"   "*.*"
        INITIAL-DIR "./txt"
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
      
    IF OKpressed = TRUE THEN
        RB-OUTPUT-FILE:SCREEN-VALUE = RB-OUTPUT-FILE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-impresoras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-impresoras D-Dialog
ON CHOOSE OF B-impresoras IN FRAME D-Dialog
DO:
    SYSTEM-DIALOG PRINTER-SETUP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Excel D-Dialog
ON CHOOSE OF Btn_Excel IN FRAME D-Dialog /* Ayuda */
DO:
  
    ASSIGN f-Desde f-hasta RADIO-SET-Orden TOGGLE-Vendedor COMBO-BOX-Lista.

    IF f-desde = ? then do:
       MESSAGE "Ingrese Fecha Desde ... " VIEW-AS ALERT-BOX.
       APPLY "ENTRY":U to f-desde.
       RETURN NO-APPLY.   
    END.

    IF f-hasta = ? then do:
       MESSAGE "Ingrese Fecha Hasta ... " VIEW-AS ALERT-BOX.
       APPLY "ENTRY":U to f-hasta.
       RETURN NO-APPLY.   
    END.   

    IF f-desde > f-hasta then do:
       MESSAGE "Rango de fechas Mal ingresado" VIEW-AS ALERT-BOX.
       APPLY "ENTRY":U to f-desde.
       RETURN NO-APPLY.
    END.

    CASE RADIO-SET-Orden:
      WHEN 1 THEN RUN Excel-ofi.
      WHEN 2 THEN DO:
          IF TOGGLE-Vendedor = NO
          THEN RUN Excel-ven.
          ELSE RUN Excel-ven-2.
      END.
      WHEN 3 THEN RUN Excel-postal.
      WHEN 4 THEN RUN Excel-ent.
      WHEN 5 THEN RUN Excel-cliente.
      WHEN 6 THEN RUN Excel-cndvta.
    END CASE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Aceptar */
DO:
  ASSIGN f-Desde f-hasta RADIO-SET-Orden TOGGLE-Vendedor COMBO-BOX-Lista.

  IF f-desde = ? then do:
     MESSAGE "Ingrese Fecha Desde ... " VIEW-AS ALERT-BOX.
     APPLY "ENTRY":U to f-desde.
     RETURN NO-APPLY.   
  END.
   
  IF f-hasta = ? then do:
     MESSAGE "Ingrese Fecha Hasta ... " VIEW-AS ALERT-BOX.
     APPLY "ENTRY":U to f-hasta.
     RETURN NO-APPLY.   
  END.   

  IF f-desde > f-hasta then do:
     MESSAGE "Rango de fechas Mal ingresado" VIEW-AS ALERT-BOX.
     APPLY "ENTRY":U to f-desde.
     RETURN NO-APPLY.
  END.
  /*
  IF f-clien <> "" THEN T-clien = "Cliente :  " + f-clien.
  */

  P-largo   = 62.
  P-Copias  = INPUT FRAME D-DIALOG RB-NUMBER-COPIES.
  P-pagIni  = INPUT FRAME D-DIALOG RB-BEGIN-PAGE.
  P-pagfin  = INPUT FRAME D-DIALOG RB-END-PAGE.
  P-select  = INPUT FRAME D-DIALOG RADIO-SET-1.
  P-archivo = INPUT FRAME D-DIALOG RB-OUTPUT-FILE.
  P-detalle = "Impresora Local (EPSON)".
  P-name    = "Epson E/F/J/RX/LQ".
  P-device  = "PRN".
     
  IF P-select = 2 
     THEN P-archivo = SESSION:TEMP-DIRECTORY + 
          STRING(NEXT-VALUE(sec-arc,integral)) + ".scr".
  ELSE RUN setup-print.      
     IF P-select <> 1 
     THEN P-copias = 1.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 D-Dialog
ON VALUE-CHANGED OF RADIO-SET-1 IN FRAME D-Dialog
DO:
    IF SELF:SCREEN-VALUE = "3"
    THEN ASSIGN b-archivo:VISIBLE = YES
                RB-OUTPUT-FILE:VISIBLE = YES
                b-archivo:SENSITIVE = YES
                RB-OUTPUT-FILE:SENSITIVE = YES.
    ELSE ASSIGN b-archivo:VISIBLE = NO
                RB-OUTPUT-FILE:VISIBLE = NO
                b-archivo:SENSITIVE = NO
                RB-OUTPUT-FILE:SENSITIVE = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-Orden
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-Orden D-Dialog
ON VALUE-CHANGED OF RADIO-SET-Orden IN FRAME D-Dialog
DO:
  IF SELF:SCREEN-VALUE = '2'
  THEN TOGGLE-Vendedor:SENSITIVE = YES.
  ELSE TOGGLE-Vendedor:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */

ASSIGN 
    RB-OUTPUT-FILE       
    FILL-IN-3 = S-CODDIV
    COMBO-BOX-Lista = s-coddiv
    F-DESDE   = TODAY
    F-HASTA   = TODAY.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia WITH FRAME {&FRAME-NAME}:
    COMBO-BOX-Lista:ADD-LAST(gn-divi.coddiv, '999999') NO-ERROR.
END.

IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  PTO                  = SESSION:SET-WAIT-STATE("").    
  l-immediate-display  = SESSION:IMMEDIATE-DISPLAY.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  RUN disable_UI.

  FRAME F-Mensaje:TITLE =  FRAME D-DIALOG:TITLE.
  VIEW FRAME F-Mensaje.  
  PAUSE 0.           
  SESSION:IMMEDIATE-DISPLAY = YES.

  DO c-Copias = 1 to P-copias ON ERROR UNDO, LEAVE
                                ON STOP UNDO, LEAVE:
        OUTPUT STREAM report TO NUL PAGED PAGE-SIZE 1000.
        c-Pagina = 0.
        RUN IMPRIMIR.
    OUTPUT STREAM report CLOSE.        
  END.
  OUTPUT STREAM report CLOSE.        
  SESSION:IMMEDIATE-DISPLAY =   l-immediate-display.
  
  IF NOT LASTKEY = KEYCODE("ESC") AND P-select = 2 THEN DO: 
        RUN bin/_vcat.p ( P-archivo ). 
  END.    
  HIDE FRAME F-Mensaje.  
  RETURN.
END.


RUN disable_UI.

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
  DISPLAY FILL-IN-3 COMBO-BOX-Lista RADIO-SET-Orden TOGGLE-Vendedor f-desde 
          f-hasta RADIO-SET-1 RB-NUMBER-COPIES RB-BEGIN-PAGE RB-END-PAGE 
      WITH FRAME D-Dialog.
  ENABLE RECT-5 RECT-41 RECT-43 COMBO-BOX-Lista RADIO-SET-Orden f-desde f-hasta 
         Btn_OK RADIO-SET-1 B-impresoras Btn_Cancel Btn_Excel RB-NUMBER-COPIES 
         RB-BEGIN-PAGE RB-END-PAGE 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-cliente D-Dialog 
PROCEDURE Excel-cliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

W-TOTBRU = 0.
W-TOTDSC = 0.
W-TOTVAL = 0.
W-TOTIGV = 0.
W-TOTVEN = 0.
t-Column = 2.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/*Header del Excel */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "RESUMEN DE COTIZACIONES DE OFICINA".

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "DESDE:".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-DESDE,"99/99/9999").
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "AL:".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-HASTA,"99/99/9999").

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA: ".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(TODAY,"99/99/9999").

/*Formato*/
chWorkSheet:Columns("A"):NumberFormat = "@".
chWorkSheet:Columns("G"):NumberFormat = "@".

/* set the column names for the Worksheet */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "N� COT".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA EMISION".
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA ENTREGA".
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "CLIENTE".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "RUC".
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "POSTAL".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "VEND".
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = "MON".
cRange = "I" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL BRUTO".
cRange = "J" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL DSCTO".
cRange = "K" + cColumn.
chWorkSheet:Range(cRange):Value = "VALOR VENTA".
cRange = "L" + cColumn.
chWorkSheet:Range(cRange):Value = "IGV".
cRange = "M" + cColumn.
chWorkSheet:Range(cRange):Value = "PRECIO VENTA".
cRange = "N" + cColumn.
chWorkSheet:Range(cRange):Value = "ESTADO".
cRange = "O" + cColumn.
chWorkSheet:Range(cRange):Value = "ACUMULADO".
cRange = "P" + cColumn.
chWorkSheet:Range(cRange):Value = "LINEA DE CREDITO US$".
    
FOR EACH FacCpedi NO-LOCK WHERE
    FacCpedi.CodCia = S-CODCIA 
    AND FacCpedi.CodDiv = S-CODDIV 
    AND FacCpedi.CodDoc = "COT"    
    AND FacCpedi.FchPed >= F-desde 
    AND FacCpedi.FchPed <= F-hasta 
    AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
    AND FacCPedi.FlgEst = 'P',
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
    AND gn-clie.codcli = faccpedi.codcli
    BREAK BY FacCpedi.CodCli:
    
    RUN vta2/p-faccpedi-flgest (Faccpedi.FlgEst, Faccpedi.CodDoc, OUTPUT x-Est).

    IF FacCpedi.Codmon = 1 
    THEN X-MON = "S/.".
    ELSE X-MON = "US$.".
    CASE FacCpedi.FlgEst :
          WHEN "P" THEN DO:
             /*X-EST = "PEN".*/
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             X-EST = "CAN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             /*X-EST = "ANU".       */
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
    END.        
    t-Column = t-Column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NroPed .
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.FchPed .
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCPedi.FchEnt .
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NomCli .
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.RucCli  .
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.CodPos .
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.Codven .
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = X-MON .
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpBrt .
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpDto .
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpVta .
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpIgv  .
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpTot   .
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = X-EST .

    ACCUMULATE FacCPedi.ImpTot (TOTAL BY FacCPedi.CodCli).
    IF LAST-OF(FacCPedi.CodCli) THEN DO:
        cRange = "O" + cColumn.
        chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY FacCPedi.CodCli FacCPedi.ImpTot).
        cRange = "P" + cColumn.
        chWorkSheet:Range(cRange):Value = gn-clie.ImpLC.
    END.
    /*
    IF LAST-OF(FacCPedi.CodCli) THEN DO:
        t-Column = t-Column + 1.
        cColumn = STRING(t-Column).
        IF FacCPedi.CodCli BEGINS '11111111' THEN DO:
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):Value = 'CLIENTES VARIOS'.
        END.
        ELSE DO:
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):Value = FacCpedi.NomCli.
        END.
        IF FacCPedi.RucCli = '' THEN DO:   
            cRange = "E" + cColumn.
            chWorkSheet:Range(cRange):Value = FAcCPedi.CodCli.
        END.
        ELSE DO:
            cRange = "E" + cColumn.
            chWorkSheet:Range(cRange):Value = FacCpedi.RucCli.
        END.
        cRange = "F" + cColumn.
        chWorkSheet:Range(cRange):Value = FacCpedi.CodPos.
        cRange = "G" + cColumn.
        chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY FacCPedi.CodCli FacCPedi.ImpTot).
        cRange = "N" + cColumn.
        chWorkSheet:Range(cRange):Value = 'PEN'.
    END.
    */

    DISPLAY Fi-Mensaje WITH FRAME F-Proceso.
 END.

 /*Resumen*/
 {vtaexp\resumen-detcot.i}    

HIDE FRAME F-Proceso.


/* launch Excel so it is visible to the user */
 chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-cndvta D-Dialog 
PROCEDURE Excel-cndvta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
W-TOTBRU = 0.
W-TOTDSC = 0.
W-TOTVAL = 0.
W-TOTIGV = 0.
W-TOTVEN = 0.
t-Column = 2.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/*Header del Excel */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "RESUMEN DE COTIZACIONES DE OFICINA".

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "DESDE:".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-DESDE,"99/99/9999").
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "AL:".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-HASTA,"99/99/9999").

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA: ".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(TODAY,"99/99/9999").

/*Formato*/
chWorkSheet:Columns("A"):NumberFormat = "@".
chWorkSheet:Columns("G"):NumberFormat = "@".

/* set the column names for the Worksheet */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "N� COT".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA EMISION".
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA ENTREGA".
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "CLIENTE".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "RUC".
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "POSTAL".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "VEND".
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = "MON".
cRange = "I" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL BRUTO".
cRange = "J" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL DSCTO".
cRange = "K" + cColumn.
chWorkSheet:Range(cRange):Value = "VALOR VENTA".
cRange = "L" + cColumn.
chWorkSheet:Range(cRange):Value = "IGV".
cRange = "M" + cColumn.
chWorkSheet:Range(cRange):Value = "PRECIO VENTA".
cRange = "N" + cColumn.
chWorkSheet:Range(cRange):Value = "ESTADO".
    
FOR EACH FacCpedi NO-LOCK WHERE
    FacCpedi.CodCia = S-CODCIA 
    AND FacCpedi.CodDiv = S-CODDIV 
    AND FacCpedi.CodDoc = "COT"    
    AND FacCpedi.FchPed >= F-desde 
    AND FacCpedi.FchPed <= F-hasta 
    AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
    AND LOOKUP(FacCPedi.FlgEst, 'P,A,C,PV,PA,E') > 0
    BREAK BY FacCpedi.FmaPgo:

    IF FIRST-OF(FacCpedi.FmaPgo) THEN DO:
        t-Column = t-Column + 1.
        cColumn = STRING(t-Column).
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = "Condicion: " .
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + FacCpedi.FmaPgo + " ".
        FIND gn-convt WHERE gn-convt.codig = Faccpedi.fmapgo NO-LOCK NO-ERROR.
        IF AVAILABLE gn-convt THEN DO: 
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):Value = Gn-convt.nombr.            
        END.
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = "--------------".            
    END.

    RUN vta2/p-faccpedi-flgest (Faccpedi.FlgEst, Faccpedi.CodDoc, OUTPUT x-Est).

    IF FacCpedi.Codmon = 1 THEN X-MON = "S/.".
    ELSE X-MON = "US$.".
    CASE FacCpedi.FlgEst :
        WHEN "P" OR WHEN "PV" OR WHEN "PA" OR WHEN "E" THEN DO:
         /*X-EST = "PEN".*/
         IF FacCpedi.Codmon = 1 THEN DO:
            w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
            w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
            w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
            w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
            w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
            END.   
         ELSE DO:  
            w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
            w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
            w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
            w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
            w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
         END.   
      END.   
      WHEN "C" THEN DO:
         /*X-EST = "CAN".*/
                 IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             /*X-EST = "ANU".       */
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
    END.        
    t-Column = t-Column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NroPed .
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.FchPed .
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCPedi.FchEnt .
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NomCli .
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.RucCli  .
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.CodPos .
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.Codven .
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = X-MON .
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpBrt .
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpDto .
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpVta .
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpIgv  .
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpTot   .
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = X-EST .

    DISPLAY Fi-Mensaje WITH FRAME F-Proceso.
 END.

 /*Resumen*/
 {vtaexp\resumen-detcot.i}

HIDE FRAME F-Proceso.

/* launch Excel so it is visible to the user */
 chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-ent D-Dialog 
PROCEDURE Excel-ent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
W-TOTBRU = 0.
W-TOTDSC = 0.
W-TOTVAL = 0.
W-TOTIGV = 0.
W-TOTVEN = 0.
t-Column = 2.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/*Header del Excel */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "RESUMEN DE COTIZACIONES DE OFICINA".

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "DESDE:".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-DESDE,"99/99/9999").
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "AL:".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-HASTA,"99/99/9999").

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA: ".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(TODAY,"99/99/9999").

/*Formato*/
chWorkSheet:Columns("A"):NumberFormat = "@".
chWorkSheet:Columns("G"):NumberFormat = "@".

/* set the column names for the Worksheet */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "N� COT".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA EMISION".
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA ENTREGA".
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "CLIENTE".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "RUC".
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "POSTAL".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "VEND".
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = "MON".
cRange = "I" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL BRUTO".
cRange = "J" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL DSCTO".
cRange = "K" + cColumn.
chWorkSheet:Range(cRange):Value = "VALOR VENTA".
cRange = "L" + cColumn.
chWorkSheet:Range(cRange):Value = "IGV".
cRange = "M" + cColumn.
chWorkSheet:Range(cRange):Value = "PRECIO VENTA".
cRange = "N" + cColumn.
chWorkSheet:Range(cRange):Value = "ESTADO".
    
FOR EACH FacCpedi NO-LOCK WHERE
    FacCpedi.CodCia = S-CODCIA 
    AND FacCpedi.CodDiv = S-CODDIV 
    AND FacCpedi.CodDoc = "COT"    
    AND FacCpedi.FchPed >= F-desde 
    AND FacCpedi.FchPed <= F-hasta 
    AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
    AND LOOKUP(FacCPedi.FlgEst, 'P,A,C,PV,PA,E') > 0
    BY FacCpedi.FchEnt:

    RUN vta2/p-faccpedi-flgest (Faccpedi.FlgEst, Faccpedi.CodDoc, OUTPUT x-Est).

    IF FacCpedi.Codmon = 1 THEN X-MON = "S/.".
    ELSE X-MON = "US$.".

    CASE FacCpedi.FlgEst :
        WHEN "P" OR WHEN "PV" OR WHEN "PA" OR WHEN "E" THEN DO:
            /*X-EST = "PEN".*/
            IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
            END.   
            ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
            END.   
        END.   
        WHEN "C" THEN DO:
            /*X-EST = "CAN".*/
            IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
            END.   
            ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
            END.                
        END.   
        WHEN "A" THEN DO:
            /*X-EST = "ANU".       */
            IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
            END.   
            ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
            END.                
        END.   
    END.        
    t-Column = t-Column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NroPed .
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.FchPed .
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCPedi.FchEnt .
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NomCli .
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.RucCli  .
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.CodPos .
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.Codven .
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = X-MON .
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpBrt .
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpDto .
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpVta .
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpIgv  .
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpTot   .
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = X-EST .
    DISPLAY Fi-Mensaje WITH FRAME F-Proceso.
 END.

 /*Resumen*/
 {vtaexp\resumen-detcot.i}

HIDE FRAME F-Proceso.

/* launch Excel so it is visible to the user */
 chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-ofi D-Dialog 
PROCEDURE Excel-ofi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

W-TOTBRU = 0.
W-TOTDSC = 0.
W-TOTVAL = 0.
W-TOTIGV = 0.
W-TOTVEN = 0.
t-Column = 2.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/*Header del Excel */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "RESUMEN DE COTIZACIONES DE OFICINA".

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "DESDE:".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-DESDE,"99/99/9999").
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "AL:".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-HASTA,"99/99/9999").

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA: ".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(TODAY,"99/99/9999").

/*Formato*/
chWorkSheet:Columns("A"):NumberFormat = "@".
chWorkSheet:Columns("G"):NumberFormat = "@".

/* set the column names for the Worksheet */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "N� COT".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA EMISION".
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA ENTREGA".
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "CLIENTE".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "RUC".
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "POSTAL".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "VEND".
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = "MON".
cRange = "I" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL BRUTO".
cRange = "J" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL DSCTO".
cRange = "K" + cColumn.
chWorkSheet:Range(cRange):Value = "VALOR VENTA".
cRange = "L" + cColumn.
chWorkSheet:Range(cRange):Value = "IGV".
cRange = "M" + cColumn.
chWorkSheet:Range(cRange):Value = "PRECIO VENTA".
cRange = "N" + cColumn.
chWorkSheet:Range(cRange):Value = "ESTADO".
    
FOR EACH FacCpedi NO-LOCK WHERE FacCpedi.CodCia = S-CODCIA 
    AND FacCpedi.CodDiv = S-CODDIV 
    AND FacCpedi.CodDoc = "COT"    
    AND FacCpedi.FchPed >= F-desde 
    AND FacCpedi.FchPed <= F-hasta 
    AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
    AND LOOKUP(FacCPedi.FlgEst, 'P,A,C,PV,PA,E') > 0
    BY FacCpedi.NroPed:
    IF FacCpedi.Codmon = 1 THEN X-MON = "S/.".
    ELSE X-MON = "US$.".

    RUN vta2/p-faccpedi-flgest (Faccpedi.FlgEst, Faccpedi.CodDoc, OUTPUT x-Est).

     CASE FacCpedi.FlgEst :
          WHEN "P" OR WHEN "PV" OR WHEN "PA" OR WHEN "E" THEN DO:
             /*X-EST = "PEN".*/
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             /*X-EST = "CAN".*/
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             /*X-EST = "ANU".       */
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
     END.        
        
     t-Column = t-Column + 1.
     cColumn = STRING(t-Column).
     cRange = "A" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.NroPed .
     cRange = "B" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.FchPed .
     cRange = "C" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCPedi.FchEnt .
     cRange = "D" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.NomCli .
     cRange = "E" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.RucCli  .
     cRange = "F" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.CodPos .
     cRange = "G" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.Codven .
     cRange = "H" + cColumn.
     chWorkSheet:Range(cRange):Value = X-MON .
     cRange = "I" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.ImpBrt .
     cRange = "J" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.ImpDto .
     cRange = "K" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.ImpVta .
     cRange = "L" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.ImpIgv  .
     cRange = "M" + cColumn.
     chWorkSheet:Range(cRange):Value = FacCpedi.ImpTot   .
     cRange = "N" + cColumn.
     chWorkSheet:Range(cRange):Value = X-EST .

     DISPLAY Fi-Mensaje WITH FRAME F-Proceso.
 END.
 /*Resumen*/
 {vtaexp\resumen-detcot.i}

HIDE FRAME F-Proceso.

/* launch Excel so it is visible to the user */
 chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-postal D-Dialog 
PROCEDURE Excel-postal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
W-TOTBRU = 0.
W-TOTDSC = 0.
W-TOTVAL = 0.
W-TOTIGV = 0.
W-TOTVEN = 0.
t-Column = 2.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/*Header del Excel */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "RESUMEN DE COTIZACIONES DE OFICINA".

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "DESDE:".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-DESDE,"99/99/9999").
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "AL:".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-HASTA,"99/99/9999").

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA: ".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(TODAY,"99/99/9999").

/*Formato*/
chWorkSheet:Columns("A"):NumberFormat = "@".
chWorkSheet:Columns("G"):NumberFormat = "@".

/* set the column names for the Worksheet */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "N� COT".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA EMISION".
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA ENTREGA".
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "CLIENTE".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "RUC".
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "POSTAL".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "VEND".
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = "MON".
cRange = "I" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL BRUTO".
cRange = "J" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL DSCTO".
cRange = "K" + cColumn.
chWorkSheet:Range(cRange):Value = "VALOR VENTA".
cRange = "L" + cColumn.
chWorkSheet:Range(cRange):Value = "IGV".
cRange = "M" + cColumn.
chWorkSheet:Range(cRange):Value = "PRECIO VENTA".
cRange = "N" + cColumn.
chWorkSheet:Range(cRange):Value = "ESTADO".
    
FOR EACH FacCpedi NO-LOCK WHERE
    FacCpedi.CodCia = S-CODCIA 
    AND FacCpedi.CodDiv = S-CODDIV 
    AND FacCpedi.CodDoc = "COT"    
    AND FacCpedi.FchPed >= F-desde 
    AND FacCpedi.FchPed <= F-hasta 
    AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
    AND LOOKUP(FacCPedi.FlgEst, 'P,A,C,PV,PA,E') > 0
    BREAK BY FacCpedi.CodPos:

    IF FIRST-OF(FacCpedi.CodPos) THEN DO:
        t-Column = t-Column + 1.
        cColumn = STRING(t-Column).
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = "Postal: ".
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + FacCpedi.CodPos + " ".
        FIND almtabla WHERE almtabla.tabla = 'CP'
            AND almtabla.codigo = faccpedi.codpos NO-LOCK NO-ERROR.
        IF AVAILABLE Almtabla THEN DO:
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):Value = almtabla.Nombre .
        END.
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = "--------------".
    END.
    
    RUN vta2/p-faccpedi-flgest (Faccpedi.FlgEst, Faccpedi.CodDoc, OUTPUT x-Est).

    IF FacCpedi.Codmon = 1 THEN X-MON = "S/.".
    ELSE X-MON = "US$.".

    CASE FacCpedi.FlgEst :
        WHEN "P" OR WHEN "PV" OR WHEN "PA" OR WHEN "E" THEN DO:
             /*X-EST = "PEN".*/
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             /*X-EST = "CAN".*/
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             /*X-EST = "ANU".       */
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
    END.        
    t-Column = t-Column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NroPed .
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.FchPed .
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCPedi.FchEnt .
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NomCli .
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.RucCli  .
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.CodPos .
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.Codven .
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = X-MON .
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpBrt .
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpDto .
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpVta .
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpIgv  .
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpTot   .
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = X-EST .
    DISPLAY Fi-Mensaje WITH FRAME F-Proceso.
 END.

 /*Resumen*/
 {vtaexp\resumen-detcot.i}

HIDE FRAME F-Proceso.

/* launch Excel so it is visible to the user */
 chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-ven D-Dialog 
PROCEDURE Excel-ven :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
W-TOTBRU = 0.
W-TOTDSC = 0.
W-TOTVAL = 0.
W-TOTIGV = 0.
W-TOTVEN = 0.
t-Column = 2.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/*Header del Excel */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "RESUMEN DE COTIZACIONES DE OFICINA".

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "DESDE:".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-DESDE,"99/99/9999").
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "AL:".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-HASTA,"99/99/9999").

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA: ".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(TODAY,"99/99/9999").

/*Formato*/
chWorkSheet:Columns("A"):NumberFormat = "@".
chWorkSheet:Columns("G"):NumberFormat = "@".

/* set the column names for the Worksheet */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "N� COT".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA EMISION".
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA ENTREGA".
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "CLIENTE".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "RUC".
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "POSTAL".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "VEND".
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = "MON".
cRange = "I" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL BRUTO".
cRange = "J" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL DSCTO".
cRange = "K" + cColumn.
chWorkSheet:Range(cRange):Value = "VALOR VENTA".
cRange = "L" + cColumn.
chWorkSheet:Range(cRange):Value = "IGV".
cRange = "M" + cColumn.
chWorkSheet:Range(cRange):Value = "PRECIO VENTA".
cRange = "N" + cColumn.
chWorkSheet:Range(cRange):Value = "ESTADO".

FOR EACH FacCpedi NO-LOCK WHERE
    FacCpedi.CodCia = S-CODCIA 
    AND FacCpedi.CodDiv = S-CODDIV 
    AND FacCpedi.CodDoc = "COT"    
    AND FacCpedi.FchPed >= F-desde 
    AND FacCpedi.FchPed <= F-hasta 
    AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
    AND LOOKUP(FacCPedi.FlgEst, 'P,A,C,PV,PA,E') > 0
    BREAK BY FacCpedi.CodVen:

    IF FIRST-OF(FacCpedi.CodVen) THEN DO:
        t-Column = t-Column + 1.
        cColumn = STRING(t-Column).
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = 'Vendedor: '.
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + FacCpedi.CodVen + ' ' .
        FIND Gn-Ven OF FacCpedi NO-LOCK NO-ERROR.
        IF AVAILABLE Gn-Ven THEN DO:
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):Value = gn-ven.NomVen.
        END.
    END.
    IF FacCpedi.Codmon = 1 
    THEN X-MON = "S/.".
    ELSE X-MON = "US$.".

    RUN vta2/p-faccpedi-flgest (Faccpedi.FlgEst, Faccpedi.CodDoc, OUTPUT x-Est).

    CASE FacCpedi.FlgEst :
        WHEN "P" OR WHEN "PV" OR WHEN "PA" OR WHEN "E" THEN DO:
            /*X-EST = "PEN".*/
            IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
            END.   
            ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
            END.   
        END.   
        WHEN "C" THEN DO:
            /*X-EST = "CAN".*/
            IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
            END.   
            ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
            END.                
        END.   
        WHEN "A" THEN DO:
            /*X-EST = "ANU".       */
            IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
            END.   
            ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
            END.                
        END.   
    END.

    t-Column = t-Column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NroPed .
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.FchPed .
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCPedi.FchEnt .
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.NomCli .
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.RucCli  .
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.CodPos .
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.Codven .
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = X-MON .
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpBrt .
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpDto .
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpVta .
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpIgv  .
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = FacCpedi.ImpTot   .
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = X-EST .

    ACCUMULATE FacCPedi.ImpTot (TOTAL BY FacCPedi.CodVen).
    IF LAST-OF(FacCPedi.CodVen) THEN DO:
        t-Column = t-Column + 1.
        cColumn = STRING(t-Column).
        cRange = "M" + cColumn.
        chWorkSheet:Range(cRange):Value = '------------------'.

        t-Column = t-Column + 1.
        cColumn = STRING(t-Column).
        cRange = "D" + cColumn.
        chWorkSheet:Range(cRange):Value = 'SUB-TOTAL'.
        cRange = "M" + cColumn.
        chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY FacCPedi.CodVen FacCPedi.ImpTot).
    END.
    DISPLAY Fi-Mensaje WITH FRAME F-Proceso.
END.

 /*Resumen*/
 {vtaexp\resumen-detcot.i}

HIDE FRAME F-Proceso.

/* launch Excel so it is visible to the user */
 chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-ven-2 D-Dialog 
PROCEDURE Excel-ven-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
W-TOTBRU = 0.
W-TOTDSC = 0.
W-TOTVAL = 0.
W-TOTIGV = 0.
W-TOTVEN = 0.
t-Column = 2.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/*Header del Excel */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "RESUMEN DE COTIZACIONES DE OFICINA".

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "DESDE:".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-DESDE,"99/99/9999").
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "AL:".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(F-HASTA,"99/99/9999").

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA: ".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(TODAY,"99/99/9999").

/*Formato*/
chWorkSheet:Columns("A"):NumberFormat = "@".
chWorkSheet:Columns("G"):NumberFormat = "@".

/*
/* set the column names for the Worksheet */
t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "N� COT".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA EMISION".
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "FECHA ENTREGA".
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "CLIENTE".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "RUC".
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "POSTAL".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "VEND".
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = "MON".
cRange = "I" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL BRUTO".
cRange = "J" + cColumn.
chWorkSheet:Range(cRange):Value = "TOTAL DSCTO".
cRange = "K" + cColumn.
chWorkSheet:Range(cRange):Value = "VALOR VENTA".
cRange = "L" + cColumn.
chWorkSheet:Range(cRange):Value = "IGV".
cRange = "M" + cColumn.
chWorkSheet:Range(cRange):Value = "PRECIO VENTA".
cRange = "N" + cColumn.
chWorkSheet:Range(cRange):Value = "ESTADO".
*/

FOR EACH FacCpedi NO-LOCK WHERE
    FacCpedi.CodCia = S-CODCIA 
    AND FacCpedi.CodDiv = S-CODDIV 
    AND FacCpedi.CodDoc = "COT"    
    AND FacCpedi.FchPed >= F-desde 
    AND FacCpedi.FchPed <= F-hasta 
    AND LOOKUP(FacCPedi.FlgEst, 'P,C,PV,PA,E') > 0
    AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
    BREAK BY FacCpedi.CodVen:

    IF FacCpedi.Codmon = 1 
    THEN X-MON = "S/.".
    ELSE X-MON = "US$.".
         RUN vta2/p-faccpedi-flgest (Faccpedi.FlgEst, Faccpedi.CodDoc, OUTPUT x-Est).
         CASE FacCpedi.FlgEst :
             WHEN "P" OR WHEN "PV" OR WHEN "PA" OR WHEN "E" THEN DO:
                       /*X-EST = "PEN".*/
                       IF FacCpedi.Codmon = 1 THEN DO:
                          w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                          w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                          w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                          w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                          w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                          END.   
                       ELSE DO:  
                          w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                          w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                          w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                          w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                          w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
                       END.   
                    END.   
                    WHEN "C" THEN DO:
                       /*X-EST = "CAN".*/
                       IF FacCpedi.Codmon = 1 THEN DO:
                          w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                          w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                     w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                     w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             /*X-EST = "ANU".       */
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
    END.        

    ACCUMULATE FacCPedi.ImpTot (TOTAL BY FacCPedi.CodVen).
    IF LAST-OF(FacCPedi.CodVen) THEN DO:
        t-Column = t-Column + 1.
        cColumn = STRING(t-Column).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):Value = 'Vendedor '.
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + FacCpedi.CodVen + " ".
        FIND Gn-Ven OF FacCpedi NO-LOCK NO-ERROR.
        IF AVAILABLE Gn-Ven THEN DO:
            cRange = "C" + cColumn.
            chWorkSheet:Range(cRange):Value = gn-ven.NomVen.
        END.
        cRange = "D" + cColumn.
        chWorkSheet:Range(cRange):Value = (ACCUM TOTAL BY FacCPedi.CodVen FacCPedi.ImpTot).
    END.
    DISPLAY Fi-Mensaje WITH FRAME F-Proceso.
END.

 /*Resumen*/
 {vtaexp\resumen-detcot.i}

HIDE FRAME F-Proceso.

/* launch Excel so it is visible to the user */
 chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Formato D-Dialog 
PROCEDURE Formato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR W-TOTBRU AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTDSC AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVAL AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.  
 DEFINE VAR W-TOTIGV AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVEN AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
    
 DEFINE FRAME f-cab
        FacCpedi.NroPed FORMAT "XXX-XXXXXX"
        FacCpedi.FchPed 
        FacCpedi.NomCli FORMAT "X(30)"
        FacCpedi.RucCli FORMAT "X(8)"
        FacCpedi.Codven FORMAT "X(4)"
        X-MON           FORMAT "X(4)"
        FacCpedi.ImpBrt FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpDto FORMAT "->>,>>9.99"
        FacCpedi.ImpVta FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpIgv FORMAT "->>,>>9.99"
        FacCpedi.ImpTot FORMAT "->,>>>,>>9.99"
        X-EST           FORMAT "X(3)"
        HEADER
        {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN3} FORMAT "X(45)" SKIP
        {&PRN2} + {&PRN6A} + "( " + FacCpedi.CodDiv + ")" + {&PRN6B} + {&PRN3} AT 1 FORMAT "X(15)"
        {&PRN6A} + "RESUMEN DE PEDIDOS DE OFICINA"  AT 43 FORMAT "X(35)"
        {&PRN3} + {&PRN6B} + "Pag.  : " AT 96 FORMAT "X(10)" PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        {&PRN2} + {&PRN6A} + "Desde : " AT 50 FORMAT "X(10)" STRING(F-DESDE,"99/99/9999") FORMAT "X(10)" "Al" STRING(F-HASTA,"99/99/9999") + {&PRN6B} + {&PRN3} FORMAT "X(12)"
        {&PRN3} + "Fecha : " AT 109 FORMAT "X(10)" STRING(TODAY,"99/99/9999") + {&PRN6B} + {&PRND} FORMAT "X(12)" 
        T-VENDE AT 1 FORMAT "X(45)" T-CLIEN AT 60 FORMAT "X(20)" "Hora  : " AT 124 STRING(TIME,"HH:MM:SS") SKIP
        "----------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        "    No.      FECHA                                                       T O T A L      TOTAL       VALOR                  P R E C I O        " SKIP
        "  PEDIDO    EMISION   C L I E N T E                   R.U.C.  VEND MON.    BRUTO        DSCTO.      VENTA       I.G.V.      V E N T A   ESTADO" SKIP
        "----------------------------------------------------------------------------------------------------------------------------------------------" SKIP
         
         WITH WIDTH 165 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.
 
 CASE s-salida-impresion:
       WHEN 1 THEN OUTPUT STREAM REPORT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Pantalla */
       WHEN 2 THEN OUTPUT STREAM REPORT TO VALUE(s-port-name)  PAGED PAGE-SIZE 62. /* Impresora */
       WHEN 3 THEN OUTPUT STREAM REPORT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Archivo */
 END CASE.

 PUT CONTROL {&PRN0} + {&PRN5A} + CHR(62) + {&PRN3}.       

 FOR EACH FacCpedi NO-LOCK WHERE
          FacCpedi.CodCia = S-CODCIA 
          AND FacCpedi.CodDiv = S-CODDIV 
          AND FacCpedi.CodDoc = "PED"    
          AND FacCpedi.FchPed >= F-desde 
          AND FacCpedi.FchPed <= F-hasta 
     AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
          /*AND FacCpedi.Codcli BEGINS f-clien */
     BY FacCpedi.NroPed:

     IF FacCpedi.Codmon = 1 THEN X-MON = "S/.".
        ELSE X-MON = "US$.".

     CASE FacCpedi.FlgEst :
          WHEN "P" THEN DO:
             X-EST = "PEN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             X-EST = "CAN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             X-EST = "ANU".       
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
     END.        
               
     DISPLAY STREAM REPORT 
        FacCpedi.NroPed 
        FacCpedi.FchPed 
        FacCpedi.NomCli 
        FacCpedi.RucCli 
        FacCpedi.Codven
        X-MON           
        FacCpedi.ImpVta 
        FacCpedi.ImpDto 
        FacCpedi.ImpBrt 
        FacCpedi.ImpIgv 
        FacCpedi.ImpTot 
        X-EST WITH FRAME F-Cab.

 END.
 DO WHILE LINE-COUNTER(REPORT) < PAGE-SIZE(REPORT) - 8 :
    PUT STREAM REPORT "" skip. 
 END. 
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "TOTAL CANCELADAS       SOLES        DOLARES   " SPACE(4) "TOTAL PENDIENTES       SOLES        DOLARES   " SPACE(4) "TOTAL ANULADAS         SOLES        DOLARES   " SKIP.
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "Total Bruto     :" AT 1 w-totbru[3] AT 19  FORMAT "->,>>>,>>9.99" w-totbru[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[1] AT 69  FORMAT "->,>>>,>>9.99" w-totbru[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[5] AT 119 FORMAT "->,>>>,>>9.99" w-totbru[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Descuento       :" AT 1 w-totdsc[3] AT 19  FORMAT "->,>>>,>>9.99" w-totdsc[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[1] AT 69  FORMAT "->,>>>,>>9.99" w-totdsc[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[5] AT 119 FORMAT "->,>>>,>>9.99" w-totdsc[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Valor de Venta  :" AT 1 w-totval[3] AT 19  FORMAT "->,>>>,>>9.99" w-totval[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[1] AT 69  FORMAT "->,>>>,>>9.99" w-totval[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[5] AT 119 FORMAT "->,>>>,>>9.99" w-totval[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "I.G.V.          :" AT 1 w-totigv[3] AT 19  FORMAT "->,>>>,>>9.99" w-totigv[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[1] AT 69  FORMAT "->,>>>,>>9.99" w-totigv[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[5] AT 119 FORMAT "->,>>>,>>9.99" w-totigv[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Precio de Venta :" AT 1 w-totven[3] AT 19  FORMAT "->,>>>,>>9.99" w-totven[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[1] AT 69  FORMAT "->,>>>,>>9.99" w-totven[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[5] AT 119 FORMAT "->,>>>,>>9.99" w-totven[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.

 CASE s-salida-impresion:
      WHEN 1 OR WHEN 3 THEN RUN LIB/d-README.R(s-print-file). 
 END CASE.                                             
 OUTPUT STREAM REPORT CLOSE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir D-Dialog 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CASE RADIO-SET-Orden:
    WHEN 1 THEN RUN prn-ofi.
    WHEN 2 THEN DO:
        IF TOGGLE-Vendedor = NO
        THEN RUN prn-ven.
        ELSE RUN prn-ven-2.
    END.
    WHEN 3 THEN RUN prn-postal.
    WHEN 4 THEN RUN prn-ent.
    WHEN 5 THEN RUN prn-cliente.
    WHEN 6 THEN RUN prn-cndvta.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      IF s-acceso-total = YES THEN FILL-IN-3:SENSITIVE = YES.
      ELSE ASSIGN FILL-IN-3:SENSITIVE = NO FILL-IN-3:SCREEN-VALUE = s-CodDiv.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NEW-PAGE D-Dialog 
PROCEDURE NEW-PAGE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    c-Pagina = c-Pagina + 1.
    IF c-Pagina > P-pagfin
    THEN RETURN ERROR.
    DISPLAY c-Pagina WITH FRAME f-mensaje.
    IF c-Pagina > 1 THEN PAGE STREAM report.
    IF P-pagini = c-Pagina 
    THEN DO:
        OUTPUT STREAM report CLOSE.
        IF P-select = 1 
        THEN DO:
               OUTPUT STREAM report TO PRINTER NO-MAP NO-CONVERT UNBUFFERED
                    PAGED PAGE-SIZE 1000.
               PUT STREAM report CONTROL P-reset NULL P-flen NULL P-config NULL.
        END.
        ELSE DO:
            OUTPUT STREAM report TO VALUE ( P-archivo ) NO-MAP NO-CONVERT UNBUFFERED
                 PAGED PAGE-SIZE 1000.
            IF P-select = 3 THEN
                PUT STREAM report CONTROL P-reset P-flen P-config.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prn-cliente D-Dialog 
PROCEDURE prn-cliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR W-TOTBRU AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTDSC AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVAL AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.  
 DEFINE VAR W-TOTIGV AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVEN AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.

 DEFINE FRAME f-cab
        FacCpedi.NroPed FORMAT "XXX-XXXXXX"
        FacCpedi.FchPed 
        FacCPedi.FChEnt
        FacCpedi.NomCli FORMAT "X(27)"
        FacCpedi.RucCli FORMAT "X(11)"
        FacCpedi.CodPos FORMAT "X(4)"
        FacCpedi.Codven FORMAT "X(4)"
        X-MON           FORMAT "X(4)"
        FacCpedi.ImpBrt FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpDto FORMAT "->>,>>9.99"
        FacCpedi.ImpVta FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpIgv FORMAT "->>,>>9.99"
        FacCpedi.ImpTot FORMAT "->,>>>,>>9.99"
        X-EST           FORMAT "X(3)"
        HEADER
        {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN4} FORMAT "X(45)" SKIP
        {&PRN2} + {&PRN6A} + "( " + FacCpedi.CodDiv + ")" + {&PRN6B} + {&PRN4} AT 1 FORMAT "X(15)"
        {&PRN6A} + "RESUMEN DE COTIZACIONES DE OFICINA" + {&PRN6B} AT 40 FORMAT "X(40)"
        {&PRN4} + "Pag.  : " AT 92 FORMAT "X(10)" PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        {&PRN6A} + "Desde : " AT 50 FORMAT "X(10)" STRING(F-DESDE,"99/99/9999") FORMAT "X(10)" "Al" STRING(F-HASTA,"99/99/9999") + {&PRN6B} FORMAT "X(12)"
        {&PRN4} + "Fecha : " AT 138 FORMAT "X(10)" STRING(TODAY,"99/99/9999") FORMAT "X(12)" 
        "Hora  : " AT 134 STRING(TIME,"HH:MM:SS") SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        "    No.      FECHA     FECHA                                                             T O T A L      TOTAL       VALOR                  P R E C I O        " SKIP
        "COTIZACION  EMISION   ENTREGA    C L I E N T E                  R.U.C. POSTAL VEND MON.    BRUTO        DSCTO.      VENTA       I.G.V.      V E N T A   ESTADO" SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
/*       xxx-xxxxxx 99/99/9999 99/99/9999 123456789012345678901234567 12345678901 1234 1234 1234 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 123
*/
         WITH WIDTH 165 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.
 

 FOR EACH FacCpedi NO-LOCK WHERE
          FacCpedi.CodCia = S-CODCIA 
          AND FacCpedi.CodDiv = S-CODDIV 
          AND FacCpedi.CodDoc = "COT"    
          AND FacCpedi.FchPed >= F-desde 
          AND FacCpedi.FchPed <= F-hasta 
          AND FacCPedi.FlgEst = 'P'
          AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
        BREAK BY FacCpedi.CodCli:
    {&new-page}.
    /*
    DISPLAY STREAM REPORT WITH FRAME F-CAB.
    IF FIRST-OF(FacCpedi.CodVen)
    THEN DO:
        PUT STREAM REPORT SKIP "Vendedor: " FacCpedi.CodVen " ".
        FIND Gn-Ven OF FacCpedi NO-LOCK NO-ERROR.
        IF AVAILABLE Gn-Ven
        THEN PUT STREAM REPORT gn-ven.NomVen.
        PUT STREAM REPORT SKIP "----------" SKIP.
    END.
    */
    IF FacCpedi.Codmon = 1 
    THEN X-MON = "S/.".
    ELSE X-MON = "US$.".
    CASE FacCpedi.FlgEst :
          WHEN "P" THEN DO:
             X-EST = "PEN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             X-EST = "CAN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             X-EST = "ANU".       
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
    END.        
    /*
    DISPLAY STREAM REPORT 
        FacCpedi.NroPed 
        FacCpedi.FchPed 
        FacCpedi.FchEnt
        FacCpedi.NomCli 
        FacCPedi.RucCli
        FacCpedi.CodPos
        FacCpedi.Codven
        X-MON           
        FacCpedi.ImpVta 
        FacCpedi.ImpDto 
        FacCpedi.ImpBrt 
        FacCpedi.ImpIgv 
        FacCpedi.ImpTot 
        X-EST WITH FRAME F-Cab.
    */
    ACCUMULATE FacCPedi.ImpTot (TOTAL BY FacCPedi.CodCli).
    IF LAST-OF(FacCPedi.CodCli)
    THEN DO:
        DISPLAY STREAM REPORT
            (IF FacCPedi.CodCli BEGINS '11111111' THEN 'CLIENTES VARIOS' ELSE FacCpedi.NomCli) @ FacCPedi.NomCli
            (IF FacCPedi.RucCli = '' THEN FAcCPedi.CodCli ELSE FacCpedi.RucCli) @ FacCPedi.RucCli 
            FacCpedi.CodPos
            (ACCUM TOTAL BY FacCPedi.CodCli FacCPedi.ImpTot) @ faccpedi.imptot
            'PEN' @ x-est
            WITH FRAME F-Cab.
    END.
 END.
 DO WHILE LINE-COUNTER(REPORT) < 62 - 8 :
    PUT STREAM REPORT "" skip. 
 END. 
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "TOTAL CANCELADAS       SOLES        DOLARES   " SPACE(4) "TOTAL PENDIENTES       SOLES        DOLARES   " SPACE(4) "TOTAL ANULADAS         SOLES        DOLARES   " SKIP.
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "Total Bruto     :" AT 1 w-totbru[3] AT 19  FORMAT "->,>>>,>>9.99" w-totbru[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[1] AT 69  FORMAT "->,>>>,>>9.99" w-totbru[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[5] AT 119 FORMAT "->,>>>,>>9.99" w-totbru[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Descuento       :" AT 1 w-totdsc[3] AT 19  FORMAT "->,>>>,>>9.99" w-totdsc[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[1] AT 69  FORMAT "->,>>>,>>9.99" w-totdsc[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[5] AT 119 FORMAT "->,>>>,>>9.99" w-totdsc[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Valor de Venta  :" AT 1 w-totval[3] AT 19  FORMAT "->,>>>,>>9.99" w-totval[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[1] AT 69  FORMAT "->,>>>,>>9.99" w-totval[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[5] AT 119 FORMAT "->,>>>,>>9.99" w-totval[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "I.G.V.          :" AT 1 w-totigv[3] AT 19  FORMAT "->,>>>,>>9.99" w-totigv[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[1] AT 69  FORMAT "->,>>>,>>9.99" w-totigv[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[5] AT 119 FORMAT "->,>>>,>>9.99" w-totigv[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Precio de Venta :" AT 1 w-totven[3] AT 19  FORMAT "->,>>>,>>9.99" w-totven[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[1] AT 69  FORMAT "->,>>>,>>9.99" w-totven[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[5] AT 119 FORMAT "->,>>>,>>9.99" w-totven[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.

 CASE s-salida-impresion:
      WHEN 1 OR WHEN 3 THEN RUN LIB/d-README.R(s-print-file). 
 END CASE.                                             
 OUTPUT STREAM REPORT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prn-cndvta D-Dialog 
PROCEDURE prn-cndvta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR W-TOTBRU AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTDSC AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVAL AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.  
 DEFINE VAR W-TOTIGV AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVEN AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
    
 DEFINE FRAME f-cab
        FacCpedi.NroPed FORMAT "XXX-XXXXXX"
        FacCpedi.FchPed 
        FacCPedi.FChEnt
        FacCpedi.NomCli FORMAT "X(27)"
        FacCpedi.RucCli FORMAT "X(11)"
        FacCpedi.CodPos FORMAT "X(4)"
        FacCpedi.Codven FORMAT "X(4)"
        X-MON           FORMAT "X(4)"
        FacCpedi.ImpBrt FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpDto FORMAT "->>,>>9.99"
        FacCpedi.ImpVta FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpIgv FORMAT "->>,>>9.99"
        FacCpedi.ImpTot FORMAT "->,>>>,>>9.99"
        X-EST           FORMAT "X(3)"
        HEADER
        {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN4} FORMAT "X(45)" SKIP
        {&PRN2} + {&PRN6A} + "( " + FacCpedi.CodDiv + ")" + {&PRN6B} + {&PRN4} AT 1 FORMAT "X(15)"
        {&PRN6A} + "RESUMEN DE COTIZACIONES DE OFICINA" + {&PRN6B} AT 40 FORMAT "X(40)"
        {&PRN4} + "Pag.  : " AT 92 FORMAT "X(10)" PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        {&PRN6A} + "Desde : " AT 50 FORMAT "X(10)" STRING(F-DESDE,"99/99/9999") FORMAT "X(10)" "Al" STRING(F-HASTA,"99/99/9999") + {&PRN6B} FORMAT "X(12)"
        {&PRN4} + "Fecha : " AT 138 FORMAT "X(10)" STRING(TODAY,"99/99/9999") FORMAT "X(12)" 
        "Hora  : " AT 134 STRING(TIME,"HH:MM:SS") SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        "    No.      FECHA     FECHA                                                             T O T A L      TOTAL       VALOR                  P R E C I O        " SKIP
        "COTIZACION  EMISION   ENTREGA    C L I E N T E                  R.U.C. POSTAL VEND MON.    BRUTO        DSCTO.      VENTA       I.G.V.      V E N T A   ESTADO" SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
/*       xxx-xxxxxx 99/99/9999 99/99/9999 123456789012345678901234567 12345678901 1234 1234 1234 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 123
*/
         WITH WIDTH 165 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.
 

 FOR EACH FacCpedi NO-LOCK WHERE
          FacCpedi.CodCia = S-CODCIA 
          AND FacCpedi.CodDiv = S-CODDIV 
          AND FacCpedi.CodDoc = "COT"    
          AND FacCpedi.FchPed >= F-desde 
          AND FacCpedi.FchPed <= F-hasta 
          AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
        BREAK BY FacCpedi.FmaPgo:
    {&new-page}.
    DISPLAY STREAM REPORT WITH FRAME F-CAB.
    IF FIRST-OF(FacCpedi.FmaPgo)
    THEN DO:
        PUT STREAM REPORT SKIP "Condicion: " FacCpedi.FmaPgo " ".
        FIND gn-convt WHERE gn-convt.codig = Faccpedi.fmapgo NO-LOCK NO-ERROR.
        IF AVAILABLE gn-convt
        THEN PUT STREAM REPORT Gn-convt.nombr.
        PUT STREAM REPORT SKIP "--------" SKIP.
    END.
    IF FacCpedi.Codmon = 1 
    THEN X-MON = "S/.".
    ELSE X-MON = "US$.".
    CASE FacCpedi.FlgEst :
          WHEN "P" THEN DO:
             X-EST = "PEN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             X-EST = "CAN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             X-EST = "ANU".       
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
    END.        
    DISPLAY STREAM REPORT 
        FacCpedi.NroPed 
        FacCpedi.FchPed 
        FacCpedi.FchEnt
        FacCpedi.NomCli 
        FacCpedi.RucCli 
        FacCpedi.CodPos
        FacCpedi.Codven
        X-MON           
        FacCpedi.ImpVta 
        FacCpedi.ImpDto 
        FacCpedi.ImpBrt 
        FacCpedi.ImpIgv 
        FacCpedi.ImpTot 
        X-EST WITH FRAME F-Cab.

 END.
 DO WHILE LINE-COUNTER(REPORT) < 62 - 8 :
    PUT STREAM REPORT "" skip. 
 END. 
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "TOTAL CANCELADAS       SOLES        DOLARES   " SPACE(4) "TOTAL PENDIENTES       SOLES        DOLARES   " SPACE(4) "TOTAL ANULADAS         SOLES        DOLARES   " SKIP.
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "Total Bruto     :" AT 1 w-totbru[3] AT 19  FORMAT "->,>>>,>>9.99" w-totbru[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[1] AT 69  FORMAT "->,>>>,>>9.99" w-totbru[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[5] AT 119 FORMAT "->,>>>,>>9.99" w-totbru[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Descuento       :" AT 1 w-totdsc[3] AT 19  FORMAT "->,>>>,>>9.99" w-totdsc[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[1] AT 69  FORMAT "->,>>>,>>9.99" w-totdsc[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[5] AT 119 FORMAT "->,>>>,>>9.99" w-totdsc[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Valor de Venta  :" AT 1 w-totval[3] AT 19  FORMAT "->,>>>,>>9.99" w-totval[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[1] AT 69  FORMAT "->,>>>,>>9.99" w-totval[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[5] AT 119 FORMAT "->,>>>,>>9.99" w-totval[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "I.G.V.          :" AT 1 w-totigv[3] AT 19  FORMAT "->,>>>,>>9.99" w-totigv[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[1] AT 69  FORMAT "->,>>>,>>9.99" w-totigv[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[5] AT 119 FORMAT "->,>>>,>>9.99" w-totigv[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Precio de Venta :" AT 1 w-totven[3] AT 19  FORMAT "->,>>>,>>9.99" w-totven[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[1] AT 69  FORMAT "->,>>>,>>9.99" w-totven[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[5] AT 119 FORMAT "->,>>>,>>9.99" w-totven[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.

 CASE s-salida-impresion:
      WHEN 1 OR WHEN 3 THEN RUN LIB/d-README.R(s-print-file). 
 END CASE.                                             
 OUTPUT STREAM REPORT CLOSE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prn-ent D-Dialog 
PROCEDURE prn-ent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR W-TOTBRU AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTDSC AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVAL AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.  
 DEFINE VAR W-TOTIGV AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVEN AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
    
 DEFINE FRAME f-cab
        FacCpedi.NroPed FORMAT "XXX-XXXXXX"
        FacCpedi.FchPed 
        FacCPedi.FChEnt
        FacCpedi.NomCli FORMAT "X(27)"
        FacCpedi.RucCli FORMAT "X(11)"
        FacCpedi.CodPos FORMAT "X(4)"
        FacCpedi.Codven FORMAT "X(4)"
        X-MON           FORMAT "X(4)"
        FacCpedi.ImpBrt FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpDto FORMAT "->>,>>9.99"
        FacCpedi.ImpVta FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpIgv FORMAT "->>,>>9.99"
        FacCpedi.ImpTot FORMAT "->,>>>,>>9.99"
        X-EST           FORMAT "X(3)"
        HEADER
        {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN4} FORMAT "X(45)" SKIP
        {&PRN2} + {&PRN6A} + "( " + FacCpedi.CodDiv + ")" + {&PRN6B} + {&PRN4} AT 1 FORMAT "X(15)"
        {&PRN6A} + "RESUMEN DE COTIZACIONES DE OFICINA" + {&PRN6B} AT 40 FORMAT "X(40)"
        {&PRN4} + "Pag.  : " AT 92 FORMAT "X(10)" PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        {&PRN6A} + "Desde : " AT 50 FORMAT "X(10)" STRING(F-DESDE,"99/99/9999") FORMAT "X(10)" "Al" STRING(F-HASTA,"99/99/9999") + {&PRN6B} FORMAT "X(12)"
        {&PRN4} + "Fecha : " AT 138 FORMAT "X(10)" STRING(TODAY,"99/99/9999") FORMAT "X(12)" 
        "Hora  : " AT 134 STRING(TIME,"HH:MM:SS") SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        "    No.      FECHA     FECHA                                                             T O T A L      TOTAL       VALOR                  P R E C I O        " SKIP
        "COTIZACION  EMISION   ENTREGA    C L I E N T E                  R.U.C. POSTAL VEND MON.    BRUTO        DSCTO.      VENTA       I.G.V.      V E N T A   ESTADO" SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
/*       xxx-xxxxxx 99/99/9999 99/99/9999 123456789012345678901234567 12345678901 1234 1234 1234 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 123
*/
         WITH WIDTH 165 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.
 

 FOR EACH FacCpedi NO-LOCK WHERE
          FacCpedi.CodCia = S-CODCIA 
          AND FacCpedi.CodDiv = S-CODDIV 
          AND FacCpedi.CodDoc = "COT"    
          AND FacCpedi.FchPed >= F-desde 
          AND FacCpedi.FchPed <= F-hasta 
          AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
     BY FacCpedi.FchEnt:

     {&new-page}.
     IF FacCpedi.Codmon = 1 THEN X-MON = "S/.".
        ELSE X-MON = "US$.".

     CASE FacCpedi.FlgEst :
          WHEN "P" THEN DO:
             X-EST = "PEN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             X-EST = "CAN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             X-EST = "ANU".       
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
     END.        
               
     DISPLAY STREAM REPORT 
        FacCpedi.NroPed 
        FacCpedi.FchPed 
        FacCPedi.FchEnt
        FacCpedi.NomCli 
        FacCpedi.RucCli 
        FacCpedi.CodPos
        FacCpedi.Codven
        X-MON           
        FacCpedi.ImpVta 
        FacCpedi.ImpDto 
        FacCpedi.ImpBrt 
        FacCpedi.ImpIgv 
        FacCpedi.ImpTot 
        X-EST WITH FRAME F-Cab.

 END.
 DO WHILE LINE-COUNTER(REPORT) < 62 - 8 :
    PUT STREAM REPORT "" skip. 
 END. 
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "TOTAL CANCELADAS       SOLES        DOLARES   " SPACE(4) "TOTAL PENDIENTES       SOLES        DOLARES   " SPACE(4) "TOTAL ANULADAS         SOLES        DOLARES   " SKIP.
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "Total Bruto     :" AT 1 w-totbru[3] AT 19  FORMAT "->,>>>,>>9.99" w-totbru[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[1] AT 69  FORMAT "->,>>>,>>9.99" w-totbru[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[5] AT 119 FORMAT "->,>>>,>>9.99" w-totbru[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Descuento       :" AT 1 w-totdsc[3] AT 19  FORMAT "->,>>>,>>9.99" w-totdsc[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[1] AT 69  FORMAT "->,>>>,>>9.99" w-totdsc[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[5] AT 119 FORMAT "->,>>>,>>9.99" w-totdsc[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Valor de Venta  :" AT 1 w-totval[3] AT 19  FORMAT "->,>>>,>>9.99" w-totval[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[1] AT 69  FORMAT "->,>>>,>>9.99" w-totval[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[5] AT 119 FORMAT "->,>>>,>>9.99" w-totval[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "I.G.V.          :" AT 1 w-totigv[3] AT 19  FORMAT "->,>>>,>>9.99" w-totigv[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[1] AT 69  FORMAT "->,>>>,>>9.99" w-totigv[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[5] AT 119 FORMAT "->,>>>,>>9.99" w-totigv[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Precio de Venta :" AT 1 w-totven[3] AT 19  FORMAT "->,>>>,>>9.99" w-totven[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[1] AT 69  FORMAT "->,>>>,>>9.99" w-totven[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[5] AT 119 FORMAT "->,>>>,>>9.99" w-totven[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.

 CASE s-salida-impresion:
      WHEN 1 OR WHEN 3 THEN RUN LIB/d-README.R(s-print-file). 
 END CASE.                                             
 OUTPUT STREAM REPORT CLOSE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prn-ofi D-Dialog 
PROCEDURE prn-ofi :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR W-TOTBRU AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTDSC AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVAL AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.  
 DEFINE VAR W-TOTIGV AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVEN AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
    
 DEFINE FRAME f-cab
        FacCpedi.NroPed FORMAT "XXX-XXXXXX"
        FacCpedi.FchPed 
        FacCPedi.FChEnt
        FacCpedi.NomCli FORMAT "X(27)"
        FacCpedi.RucCli FORMAT "X(11)"
        FacCpedi.CodPos FORMAT "X(4)"
        FacCpedi.Codven FORMAT "X(4)"
        X-MON           FORMAT "X(4)"
        FacCpedi.ImpBrt FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpDto FORMAT "->>,>>9.99"
        FacCpedi.ImpVta FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpIgv FORMAT "->>,>>9.99"
        FacCpedi.ImpTot FORMAT "->,>>>,>>9.99"
        X-EST           FORMAT "X(3)"
        HEADER
        {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN4} FORMAT "X(45)" SKIP
        {&PRN2} + {&PRN6A} + "( " + FacCpedi.CodDiv + ")" + {&PRN6B} + {&PRN4} AT 1 FORMAT "X(15)"
        {&PRN6A} + "RESUMEN DE COTIZACIONES DE OFICINA" + {&PRN6B} AT 40 FORMAT "X(40)"
        {&PRN4} + "Pag.  : " AT 92 FORMAT "X(10)" PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        {&PRN6A} + "Desde : " AT 50 FORMAT "X(10)" STRING(F-DESDE,"99/99/9999") FORMAT "X(10)" "Al" STRING(F-HASTA,"99/99/9999") + {&PRN6B} FORMAT "X(12)"
        {&PRN4} + "Fecha : " AT 138 FORMAT "X(10)" STRING(TODAY,"99/99/9999") FORMAT "X(12)" 
        "Hora  : " AT 134 STRING(TIME,"HH:MM:SS") SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        "    No.      FECHA     FECHA                                                             T O T A L      TOTAL       VALOR                  P R E C I O        " SKIP
        "COTIZACION  EMISION   ENTREGA    C L I E N T E                  R.U.C. POSTAL VEND MON.    BRUTO        DSCTO.      VENTA       I.G.V.      V E N T A   ESTADO" SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
/*       xxx-xxxxxx 99/99/9999 99/99/9999 123456789012345678901234567 12345678901 1234 1234 1234 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 123
*/
         WITH WIDTH 165 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.
 

 FOR EACH FacCpedi NO-LOCK WHERE FacCpedi.CodCia = S-CODCIA AND 
     FacCpedi.CodDiv = S-CODDIV AND 
     FacCpedi.CodDoc = "COT" AND 
     FacCpedi.FchPed >= F-desde AND 
     FacCpedi.FchPed <= F-hasta AND
     FacCPedi.Libre_C01 = COMBO-BOX-Lista
     BY FacCpedi.NroPed:

     {&new-page}.
     IF FacCpedi.Codmon = 1 THEN X-MON = "S/.".
        ELSE X-MON = "US$.".

     CASE FacCpedi.FlgEst :
          WHEN "P" THEN DO:
             X-EST = "PEN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             X-EST = "CAN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             X-EST = "ANU".       
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
     END.        
               
     DISPLAY STREAM REPORT 
        FacCpedi.NroPed 
        FacCpedi.FchPed 
        FacCPedi.FchEnt
        FacCpedi.NomCli 
        FacCpedi.RucCli 
        FacCpedi.CodPos
        FacCpedi.Codven
        X-MON           
        FacCpedi.ImpVta 
        FacCpedi.ImpDto 
        FacCpedi.ImpBrt 
        FacCpedi.ImpIgv 
        FacCpedi.ImpTot 
        X-EST WITH FRAME F-Cab.

 END.
 DO WHILE LINE-COUNTER(REPORT) < 62 - 8 :
    PUT STREAM REPORT "" skip. 
 END. 
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "TOTAL CANCELADAS       SOLES        DOLARES   " SPACE(4) "TOTAL PENDIENTES       SOLES        DOLARES   " SPACE(4) "TOTAL ANULADAS         SOLES        DOLARES   " SKIP.
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "Total Bruto     :" AT 1 w-totbru[3] AT 19  FORMAT "->,>>>,>>9.99" w-totbru[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[1] AT 69  FORMAT "->,>>>,>>9.99" w-totbru[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[5] AT 119 FORMAT "->,>>>,>>9.99" w-totbru[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Descuento       :" AT 1 w-totdsc[3] AT 19  FORMAT "->,>>>,>>9.99" w-totdsc[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[1] AT 69  FORMAT "->,>>>,>>9.99" w-totdsc[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[5] AT 119 FORMAT "->,>>>,>>9.99" w-totdsc[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Valor de Venta  :" AT 1 w-totval[3] AT 19  FORMAT "->,>>>,>>9.99" w-totval[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[1] AT 69  FORMAT "->,>>>,>>9.99" w-totval[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[5] AT 119 FORMAT "->,>>>,>>9.99" w-totval[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "I.G.V.          :" AT 1 w-totigv[3] AT 19  FORMAT "->,>>>,>>9.99" w-totigv[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[1] AT 69  FORMAT "->,>>>,>>9.99" w-totigv[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[5] AT 119 FORMAT "->,>>>,>>9.99" w-totigv[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Precio de Venta :" AT 1 w-totven[3] AT 19  FORMAT "->,>>>,>>9.99" w-totven[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[1] AT 69  FORMAT "->,>>>,>>9.99" w-totven[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[5] AT 119 FORMAT "->,>>>,>>9.99" w-totven[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.

 CASE s-salida-impresion:
      WHEN 1 OR WHEN 3 THEN RUN LIB/d-README.R(s-print-file). 
 END CASE.                                             
 OUTPUT STREAM REPORT CLOSE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prn-postal D-Dialog 
PROCEDURE prn-postal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR W-TOTBRU AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTDSC AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVAL AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.  
 DEFINE VAR W-TOTIGV AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVEN AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
    
 DEFINE FRAME f-cab
        FacCpedi.NroPed FORMAT "XXX-XXXXXX"
        FacCpedi.FchPed 
        FacCPedi.FChEnt
        FacCpedi.NomCli FORMAT "X(27)"
        FacCpedi.RucCli FORMAT "X(11)"
        FacCpedi.CodPos FORMAT "X(4)"
        FacCpedi.Codven FORMAT "X(4)"
        X-MON           FORMAT "X(4)"
        FacCpedi.ImpBrt FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpDto FORMAT "->>,>>9.99"
        FacCpedi.ImpVta FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpIgv FORMAT "->>,>>9.99"
        FacCpedi.ImpTot FORMAT "->,>>>,>>9.99"
        X-EST           FORMAT "X(3)"
        HEADER
        {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN4} FORMAT "X(45)" SKIP
        {&PRN2} + {&PRN6A} + "( " + FacCpedi.CodDiv + ")" + {&PRN6B} + {&PRN4} AT 1 FORMAT "X(15)"
        {&PRN6A} + "RESUMEN DE COTIZACIONES DE OFICINA" + {&PRN6B} AT 40 FORMAT "X(40)"
        {&PRN4} + "Pag.  : " AT 92 FORMAT "X(10)" PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        {&PRN6A} + "Desde : " AT 50 FORMAT "X(10)" STRING(F-DESDE,"99/99/9999") FORMAT "X(10)" "Al" STRING(F-HASTA,"99/99/9999") + {&PRN6B} FORMAT "X(12)"
        {&PRN4} + "Fecha : " AT 138 FORMAT "X(10)" STRING(TODAY,"99/99/9999") FORMAT "X(12)" 
        "Hora  : " AT 134 STRING(TIME,"HH:MM:SS") SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        "    No.      FECHA     FECHA                                                             T O T A L      TOTAL       VALOR                  P R E C I O        " SKIP
        "COTIZACION  EMISION   ENTREGA    C L I E N T E                  R.U.C. POSTAL VEND MON.    BRUTO        DSCTO.      VENTA       I.G.V.      V E N T A   ESTADO" SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
/*       xxx-xxxxxx 99/99/9999 99/99/9999 123456789012345678901234567 12345678901 1234 1234 1234 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 123
*/
         WITH WIDTH 165 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.
 

 FOR EACH FacCpedi NO-LOCK WHERE
          FacCpedi.CodCia = S-CODCIA 
          AND FacCpedi.CodDiv = S-CODDIV 
          AND FacCpedi.CodDoc = "COT"    
          AND FacCpedi.FchPed >= F-desde 
          AND FacCpedi.FchPed <= F-hasta 
          AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
        BREAK BY FacCpedi.CodPos:
    {&new-page}.
    DISPLAY STREAM REPORT WITH FRAME F-CAB.
    IF FIRST-OF(FacCpedi.CodPos)
    THEN DO:
        PUT STREAM REPORT SKIP "Postal: " FacCpedi.CodPos " ".
        FIND almtabla WHERE almtabla.tabla = 'CP'
            AND almtabla.codigo = faccpedi.codpos NO-LOCK NO-ERROR.
        IF AVAILABLE Almtabla
        THEN PUT STREAM REPORT almtabla.Nombre.
        PUT STREAM REPORT SKIP "--------" SKIP.
    END.
    IF FacCpedi.Codmon = 1 
    THEN X-MON = "S/.".
    ELSE X-MON = "US$.".
    CASE FacCpedi.FlgEst :
          WHEN "P" THEN DO:
             X-EST = "PEN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             X-EST = "CAN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             X-EST = "ANU".       
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
    END.        
    DISPLAY STREAM REPORT 
        FacCpedi.NroPed 
        FacCpedi.FchPed 
        FacCpedi.FchEnt
        FacCpedi.NomCli 
        FacCpedi.RucCli 
        FacCpedi.CodPos
        FacCpedi.Codven
        X-MON           
        FacCpedi.ImpVta 
        FacCpedi.ImpDto 
        FacCpedi.ImpBrt 
        FacCpedi.ImpIgv 
        FacCpedi.ImpTot 
        X-EST WITH FRAME F-Cab.

 END.
 DO WHILE LINE-COUNTER(REPORT) < 62 - 8 :
    PUT STREAM REPORT "" skip. 
 END. 
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "TOTAL CANCELADAS       SOLES        DOLARES   " SPACE(4) "TOTAL PENDIENTES       SOLES        DOLARES   " SPACE(4) "TOTAL ANULADAS         SOLES        DOLARES   " SKIP.
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "Total Bruto     :" AT 1 w-totbru[3] AT 19  FORMAT "->,>>>,>>9.99" w-totbru[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[1] AT 69  FORMAT "->,>>>,>>9.99" w-totbru[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[5] AT 119 FORMAT "->,>>>,>>9.99" w-totbru[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Descuento       :" AT 1 w-totdsc[3] AT 19  FORMAT "->,>>>,>>9.99" w-totdsc[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[1] AT 69  FORMAT "->,>>>,>>9.99" w-totdsc[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[5] AT 119 FORMAT "->,>>>,>>9.99" w-totdsc[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Valor de Venta  :" AT 1 w-totval[3] AT 19  FORMAT "->,>>>,>>9.99" w-totval[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[1] AT 69  FORMAT "->,>>>,>>9.99" w-totval[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[5] AT 119 FORMAT "->,>>>,>>9.99" w-totval[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "I.G.V.          :" AT 1 w-totigv[3] AT 19  FORMAT "->,>>>,>>9.99" w-totigv[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[1] AT 69  FORMAT "->,>>>,>>9.99" w-totigv[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[5] AT 119 FORMAT "->,>>>,>>9.99" w-totigv[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Precio de Venta :" AT 1 w-totven[3] AT 19  FORMAT "->,>>>,>>9.99" w-totven[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[1] AT 69  FORMAT "->,>>>,>>9.99" w-totven[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[5] AT 119 FORMAT "->,>>>,>>9.99" w-totven[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.

 CASE s-salida-impresion:
      WHEN 1 OR WHEN 3 THEN RUN LIB/d-README.R(s-print-file). 
 END CASE.                                             
 OUTPUT STREAM REPORT CLOSE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prn-ven D-Dialog 
PROCEDURE prn-ven :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR W-TOTBRU AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTDSC AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVAL AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.  
 DEFINE VAR W-TOTIGV AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVEN AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.

 DEFINE FRAME f-cab
        FacCpedi.NroPed FORMAT "XXX-XXXXXX"
        FacCpedi.FchPed 
        FacCPedi.FChEnt
        FacCpedi.NomCli FORMAT "X(27)"
        FacCpedi.RucCli FORMAT "X(11)"
        FacCpedi.CodPos FORMAT "X(4)"
        FacCpedi.Codven FORMAT "X(4)"
        X-MON           FORMAT "X(4)"
        FacCpedi.ImpBrt FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpDto FORMAT "->>,>>9.99"
        FacCpedi.ImpVta FORMAT "->,>>>,>>9.99"
        FacCpedi.ImpIgv FORMAT "->>,>>9.99"
        FacCpedi.ImpTot FORMAT "->,>>>,>>9.99"
        X-EST           FORMAT "X(3)" SKIP
        HEADER
        {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN4} FORMAT "X(45)" SKIP
        {&PRN2} + {&PRN6A} + "( " + FacCpedi.CodDiv + ")" + {&PRN6B} + {&PRN4} AT 1 FORMAT "X(15)"
        {&PRN6A} + "RESUMEN DE COTIZACIONES DE OFICINA" + {&PRN6B} AT 40 FORMAT "X(40)"
        {&PRN4} + "Pag.  : " AT 92 FORMAT "X(10)" PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        {&PRN6A} + "Desde : " AT 50 FORMAT "X(10)" STRING(F-DESDE,"99/99/9999") FORMAT "X(10)" "Al" STRING(F-HASTA,"99/99/9999") + {&PRN6B} FORMAT "X(12)"
        {&PRN4} + "Fecha : " AT 138 FORMAT "X(10)" STRING(TODAY,"99/99/9999") FORMAT "X(12)" 
        "Hora  : " AT 134 STRING(TIME,"HH:MM:SS") SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        "    No.      FECHA     FECHA                                                             T O T A L      TOTAL       VALOR                  P R E C I O        " SKIP
        "COTIZACION  EMISION   ENTREGA    C L I E N T E                  R.U.C. POSTAL VEND MON.    BRUTO        DSCTO.      VENTA       I.G.V.      V E N T A   ESTADO" SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
/*       xxx-xxxxxx 99/99/9999 99/99/9999 123456789012345678901234567 12345678901 1234 1234 1234 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 123
*/
         WITH WIDTH 165 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.
 

 FOR EACH FacCpedi NO-LOCK WHERE
          FacCpedi.CodCia = S-CODCIA 
          AND FacCpedi.CodDiv = S-CODDIV 
          AND FacCpedi.CodDoc = "COT"    
          AND FacCpedi.FchPed >= F-desde 
          AND FacCpedi.FchPed <= F-hasta 
          AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
        BREAK BY FacCpedi.CodVen:
    {&new-page}.
    DISPLAY STREAM REPORT WITH FRAME F-CAB.
    IF FIRST-OF(FacCpedi.CodVen)
    THEN DO:
        PUT STREAM REPORT SKIP "Vendedor: " FacCpedi.CodVen " ".
        FIND Gn-Ven OF FacCpedi NO-LOCK NO-ERROR.
        IF AVAILABLE Gn-Ven
        THEN PUT STREAM REPORT gn-ven.NomVen.
        PUT STREAM REPORT SKIP "----------" SKIP.
    END.
    IF FacCpedi.Codmon = 1 
    THEN X-MON = "S/.".
    ELSE X-MON = "US$.".
    CASE FacCpedi.FlgEst :
          WHEN "P" THEN DO:
             X-EST = "PEN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             X-EST = "CAN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             X-EST = "ANU".       
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
    END.        
    DISPLAY STREAM REPORT 
        FacCpedi.NroPed 
        FacCpedi.FchPed 
        FacCpedi.FchEnt
        FacCpedi.NomCli 
        FacCpedi.RucCli 
        FacCpedi.CodPos
        FacCpedi.Codven
        X-MON           
        FacCpedi.ImpVta 
        FacCpedi.ImpDto 
        FacCpedi.ImpBrt 
        FacCpedi.ImpIgv 
        FacCpedi.ImpTot 
        X-EST WITH FRAME F-Cab.
    ACCUMULATE FacCPedi.ImpTot (TOTAL BY FacCPedi.CodVen).
    IF LAST-OF(FacCPedi.CodVen)
    THEN DO:
        UNDERLINE STREAM REPORT
            FacCPedi.ImpTot
            WITH FRAME F-Cab.
        DISPLAY STREAM REPORT
            "SUB-TOTAL >>>" @ FacCPedi.NomCli
            (ACCUM TOTAL BY FacCPedi.CodVen FacCPedi.ImpTot) @ faccpedi.imptot
            WITH FRAME F-Cab.
    END.
 END.
 DO WHILE LINE-COUNTER(REPORT) < 62 - 8 :
    PUT STREAM REPORT "" skip. 
 END. 
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "TOTAL CANCELADAS       SOLES        DOLARES   " SPACE(4) "TOTAL PENDIENTES       SOLES        DOLARES   " SPACE(4) "TOTAL ANULADAS         SOLES        DOLARES   " SKIP.
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "Total Bruto     :" AT 1 w-totbru[3] AT 19  FORMAT "->,>>>,>>9.99" w-totbru[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[1] AT 69  FORMAT "->,>>>,>>9.99" w-totbru[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[5] AT 119 FORMAT "->,>>>,>>9.99" w-totbru[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Descuento       :" AT 1 w-totdsc[3] AT 19  FORMAT "->,>>>,>>9.99" w-totdsc[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[1] AT 69  FORMAT "->,>>>,>>9.99" w-totdsc[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[5] AT 119 FORMAT "->,>>>,>>9.99" w-totdsc[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Valor de Venta  :" AT 1 w-totval[3] AT 19  FORMAT "->,>>>,>>9.99" w-totval[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[1] AT 69  FORMAT "->,>>>,>>9.99" w-totval[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[5] AT 119 FORMAT "->,>>>,>>9.99" w-totval[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "I.G.V.          :" AT 1 w-totigv[3] AT 19  FORMAT "->,>>>,>>9.99" w-totigv[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[1] AT 69  FORMAT "->,>>>,>>9.99" w-totigv[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[5] AT 119 FORMAT "->,>>>,>>9.99" w-totigv[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Precio de Venta :" AT 1 w-totven[3] AT 19  FORMAT "->,>>>,>>9.99" w-totven[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[1] AT 69  FORMAT "->,>>>,>>9.99" w-totven[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[5] AT 119 FORMAT "->,>>>,>>9.99" w-totven[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.

 CASE s-salida-impresion:
      WHEN 1 OR WHEN 3 THEN RUN LIB/d-README.R(s-print-file). 
 END CASE.                                             
 OUTPUT STREAM REPORT CLOSE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prn-ven-2 D-Dialog 
PROCEDURE prn-ven-2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR W-TOTBRU AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTDSC AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVAL AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.  
 DEFINE VAR W-TOTIGV AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.
 DEFINE VAR W-TOTVEN AS DECIMAL FORMAT "->,>>>,>>9.99" EXTENT 6 INIT 0.

 DEFINE FRAME f-cab
/*        FacCpedi.NroPed FORMAT "XXX-XXXXXX"
 *         FacCpedi.FchPed 
 *         FacCPedi.FChEnt
 *         FacCpedi.NomCli FORMAT "X(27)"
 *         FacCpedi.RucCli FORMAT "X(11)"
 *         FacCpedi.CodPos FORMAT "X(4)"
 *         FacCpedi.Codven FORMAT "X(4)"
 *         X-MON           FORMAT "X(4)"
 *         FacCpedi.ImpBrt FORMAT "->,>>>,>>9.99"
 *         FacCpedi.ImpDto FORMAT "->>,>>9.99"
 *         FacCpedi.ImpVta FORMAT "->,>>>,>>9.99"
 *         FacCpedi.ImpIgv FORMAT "->>,>>9.99"
 *         FacCpedi.ImpTot FORMAT "->,>>>,>>9.99"
 *         X-EST           FORMAT "X(3)" SKIP*/
        HEADER
        {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN4} FORMAT "X(45)" SKIP
        {&PRN2} + {&PRN6A} + "( " + FacCpedi.CodDiv + ")" + {&PRN6B} + {&PRN4} AT 1 FORMAT "X(15)"
        {&PRN6A} + "RESUMEN DE COTIZACIONES DE OFICINA" + {&PRN6B} AT 40 FORMAT "X(40)"
        {&PRN4} + "Pag.  : " AT 92 FORMAT "X(10)" PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        {&PRN6A} + "Desde : " AT 50 FORMAT "X(10)" STRING(F-DESDE,"99/99/9999") FORMAT "X(10)" "Al" STRING(F-HASTA,"99/99/9999") + {&PRN6B} FORMAT "X(12)"
        {&PRN4} + "Fecha : " AT 138 FORMAT "X(10)" STRING(TODAY,"99/99/9999") FORMAT "X(12)" 
        "Hora  : " AT 134 STRING(TIME,"HH:MM:SS") SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        "    No.      FECHA     FECHA                                                             T O T A L      TOTAL       VALOR                  P R E C I O        " SKIP
        "COTIZACION  EMISION   ENTREGA    C L I E N T E                  R.U.C. POSTAL VEND MON.    BRUTO        DSCTO.      VENTA       I.G.V.      V E N T A   ESTADO" SKIP
        "--------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
/*       xxx-xxxxxx 99/99/9999 99/99/9999 123456789012345678901234567 12345678901 1234 1234 1234 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 ->>,>>9.99 ->,>>>,>>9.99 123
*/
         WITH WIDTH 165 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.
 

 FOR EACH FacCpedi NO-LOCK WHERE
          FacCpedi.CodCia = S-CODCIA 
          AND FacCpedi.CodDiv = S-CODDIV 
          AND FacCpedi.CodDoc = "COT"    
          AND FacCpedi.FchPed >= F-desde 
          AND FacCpedi.FchPed <= F-hasta 
          AND FacCpedi.FlgEst <> 'A'
          AND FacCPedi.Libre_C01 = COMBO-BOX-Lista
        BREAK BY FacCpedi.CodVen:
    {&new-page}.
/*    DISPLAY STREAM REPORT WITH FRAME F-CAB.*/
/*    IF FIRST-OF(FacCpedi.CodVen)
 *     THEN DO:
 *         PUT STREAM REPORT SKIP "Vendedor: " FacCpedi.CodVen " ".
 *         FIND Gn-Ven OF FacCpedi NO-LOCK NO-ERROR.
 *         IF AVAILABLE Gn-Ven
 *         THEN PUT STREAM REPORT gn-ven.NomVen.
 *         PUT STREAM REPORT SKIP "----------" SKIP.
 *     END.*/
    IF FacCpedi.Codmon = 1 
    THEN X-MON = "S/.".
    ELSE X-MON = "US$.".
    CASE FacCpedi.FlgEst :
          WHEN "P" THEN DO:
             X-EST = "PEN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [1] = w-totbru [1] + FacCpedi.ImpBrt.   
                w-totdsc [1] = w-totdsc [1] + FacCpedi.ImpDto.
                w-totval [1] = w-totval [1] + FacCpedi.ImpVta.
                w-totigv [1] = w-totigv [1] + FacCpedi.ImpIgv.
                w-totven [1] = w-totven [1] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [2] = w-totbru [2] + FacCpedi.ImpBrt.   
                w-totdsc [2] = w-totdsc [2] + FacCpedi.ImpDto.
                w-totval [2] = w-totval [2] + FacCpedi.ImpVta.
                w-totigv [2] = w-totigv [2] + FacCpedi.ImpIgv.
                w-totven [2] = w-totven [2] + FacCpedi.ImpTot.                
             END.   
          END.   
          WHEN "C" THEN DO:
             X-EST = "CAN".
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [3] = w-totbru [3] + FacCpedi.ImpBrt.   
                w-totdsc [3] = w-totdsc [3] + FacCpedi.ImpDto.
                w-totval [3] = w-totval [3] + FacCpedi.ImpVta.
                w-totigv [3] = w-totigv [3] + FacCpedi.ImpIgv.
                w-totven [3] = w-totven [3] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [4] = w-totbru [4] + FacCpedi.ImpBrt.   
                w-totdsc [4] = w-totdsc [4] + FacCpedi.ImpDto.
                w-totval [4] = w-totval [4] + FacCpedi.ImpVta.
                w-totigv [4] = w-totigv [4] + FacCpedi.ImpIgv.
                w-totven [4] = w-totven [4] + FacCpedi.ImpTot.                
             END.                
          END.   
          WHEN "A" THEN DO:
             X-EST = "ANU".       
             IF FacCpedi.Codmon = 1 THEN DO:
                w-totbru [5] = w-totbru [5] + FacCpedi.ImpBrt.   
                w-totdsc [5] = w-totdsc [5] + FacCpedi.ImpDto.
                w-totval [5] = w-totval [5] + FacCpedi.ImpVta.
                w-totigv [5] = w-totigv [5] + FacCpedi.ImpIgv.
                w-totven [5] = w-totven [5] + FacCpedi.ImpTot.
                END.   
             ELSE DO:  
                w-totbru [6] = w-totbru [6] + FacCpedi.ImpBrt.   
                w-totdsc [6] = w-totdsc [6] + FacCpedi.ImpDto.
                w-totval [6] = w-totval [6] + FacCpedi.ImpVta.
                w-totigv [6] = w-totigv [6] + FacCpedi.ImpIgv.
                w-totven [6] = w-totven [6] + FacCpedi.ImpTot.                
             END.                
          END.   
    END.        
    ACCUMULATE FacCPedi.ImpTot (TOTAL BY FacCPedi.CodVen).
    IF LAST-OF(FacCPedi.CodVen)
    THEN DO:
         PUT STREAM REPORT SKIP "Vendedor: " FacCpedi.CodVen " ".
         FIND Gn-Ven OF FacCpedi NO-LOCK NO-ERROR.
         IF AVAILABLE Gn-Ven
         THEN PUT STREAM REPORT gn-ven.NomVen.
         PUT STREAM REPORT (ACCUM TOTAL BY FacCPedi.CodVen FacCPedi.ImpTot) FORMAT "->,>>>,>>9.99" SKIP(1).
    END.
 END.
 DO WHILE LINE-COUNTER(REPORT) < 62 - 8 :
    PUT STREAM REPORT "" skip. 
 END. 
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "TOTAL CANCELADAS       SOLES        DOLARES   " SPACE(4) "TOTAL PENDIENTES       SOLES        DOLARES   " SPACE(4) "TOTAL ANULADAS         SOLES        DOLARES   " SKIP.
 PUT STREAM REPORT "----------------------------------------------" SPACE(4) "----------------------------------------------" SPACE(4) "----------------------------------------------" SKIP.
 PUT STREAM REPORT "Total Bruto     :" AT 1 w-totbru[3] AT 19  FORMAT "->,>>>,>>9.99" w-totbru[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[1] AT 69  FORMAT "->,>>>,>>9.99" w-totbru[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Total Bruto     :"
                                            w-totbru[5] AT 119 FORMAT "->,>>>,>>9.99" w-totbru[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Descuento       :" AT 1 w-totdsc[3] AT 19  FORMAT "->,>>>,>>9.99" w-totdsc[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[1] AT 69  FORMAT "->,>>>,>>9.99" w-totdsc[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Descuento       :"
                                            w-totdsc[5] AT 119 FORMAT "->,>>>,>>9.99" w-totdsc[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Valor de Venta  :" AT 1 w-totval[3] AT 19  FORMAT "->,>>>,>>9.99" w-totval[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[1] AT 69  FORMAT "->,>>>,>>9.99" w-totval[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Valor de Venta  :"
                                            w-totval[5] AT 119 FORMAT "->,>>>,>>9.99" w-totval[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "I.G.V.          :" AT 1 w-totigv[3] AT 19  FORMAT "->,>>>,>>9.99" w-totigv[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[1] AT 69  FORMAT "->,>>>,>>9.99" w-totigv[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "I.G.V.          :"
                                            w-totigv[5] AT 119 FORMAT "->,>>>,>>9.99" w-totigv[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.
 PUT STREAM REPORT "Precio de Venta :" AT 1 w-totven[3] AT 19  FORMAT "->,>>>,>>9.99" w-totven[4] AT 34  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[1] AT 69  FORMAT "->,>>>,>>9.99" w-totven[2] AT 84  FORMAT "->,>>>,>>9.99" SPACE(4) "Precio de Venta :"
                                            w-totven[5] AT 119 FORMAT "->,>>>,>>9.99" w-totven[6] AT 134 FORMAT "->,>>>,>>9.99" SKIP.

 CASE s-salida-impresion:
      WHEN 1 OR WHEN 3 THEN RUN LIB/d-README.R(s-print-file). 
 END CASE.                                             
 OUTPUT STREAM REPORT CLOSE.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Parametros D-Dialog 
PROCEDURE Procesa-Parametros :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recoge-Parametros D-Dialog 
PROCEDURE Recoge-Parametros :
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
        WHEN "" THEN ASSIGN input-var-1 = "".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE remvar D-Dialog 
PROCEDURE remvar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER IN-VAR AS CHARACTER.
    DEFINE OUTPUT PARAMETER OU-VAR AS CHARACTER.
    DEFINE VARIABLE P-pos AS INTEGER.
    OU-VAR = IN-VAR.
    IF P-select = 2 THEN DO:
        OU-VAR = "".
        RETURN.
    END.
    P-pos =  INDEX(OU-VAR, "[NULL]" ).
    IF P-pos <> 0
    THEN OU-VAR = SUBSTR(OU-VAR, 1, P-pos - 1) +
                     CHR(0) + SUBSTR(OU-VAR, P-pos + 6).
    P-pos =  INDEX(OU-VAR, "[#B]" ).
    IF P-pos <> 0
    THEN OU-VAR = SUBSTR(OU-VAR, 1, P-pos - 1) +
                     CHR(P-Largo) + SUBSTR(OU-VAR, P-pos + 4).
    P-pos =  INDEX(OU-VAR, "[#]" ).
    IF P-pos <> 0
    THEN OU-VAR = SUBSTR(OU-VAR, 1, P-pos - 1) +
                     STRING(P-Largo, ">>9" ) + SUBSTR(OU-VAR, P-pos + 3).
    RETURN.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Setup-Print D-Dialog 
PROCEDURE Setup-Print :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND integral.P-Codes WHERE integral.P-Codes.Name = P-name NO-LOCK NO-ERROR.
    IF NOT AVAILABLE integral.P-Codes
    THEN DO:
        MESSAGE "Invalido Tabla de Impresora" SKIP
                "configurado al Terminal" XTerm
                VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
    /* Configurando Variables de Impresion */
    RUN RemVar (INPUT integral.P-Codes.Reset,    OUTPUT P-Reset).
    RUN RemVar (INPUT integral.P-Codes.Flen,     OUTPUT P-Flen).
    RUN RemVar (INPUT integral.P-Codes.C6lpi,    OUTPUT P-6lpi).
    RUN RemVar (INPUT integral.P-Codes.C8lpi,    OUTPUT P-8lpi).
    RUN RemVar (INPUT integral.P-Codes.C10cpi,   OUTPUT P-10cpi).
    RUN RemVar (INPUT integral.P-Codes.C12cpi,   OUTPUT P-12cpi).
    RUN RemVar (INPUT integral.P-Codes.C15cpi,   OUTPUT P-15cpi).
    RUN RemVar (INPUT integral.P-Codes.C20cpi,   OUTPUT P-20cpi).
    RUN RemVar (INPUT integral.P-Codes.Landscap, OUTPUT P-Landscap).
    RUN RemVar (INPUT integral.P-Codes.Portrait, OUTPUT P-Portrait).
    RUN RemVar (INPUT integral.P-Codes.DobleOn,  OUTPUT P-DobleOn).
    RUN RemVar (INPUT integral.P-Codes.DobleOff, OUTPUT P-DobleOff).
    RUN RemVar (INPUT integral.P-Codes.BoldOn,   OUTPUT P-BoldOn).
    RUN RemVar (INPUT integral.P-Codes.BoldOff,  OUTPUT P-BoldOff).
    RUN RemVar (INPUT integral.P-Codes.UlineOn,  OUTPUT P-UlineOn).
    RUN RemVar (INPUT integral.P-Codes.UlineOff, OUTPUT P-UlineOff).
    RUN RemVar (INPUT integral.P-Codes.ItalOn,   OUTPUT P-ItalOn).
    RUN RemVar (INPUT integral.P-Codes.ItalOff,  OUTPUT P-ItalOff).
    RUN RemVar (INPUT integral.P-Codes.SuperOn,  OUTPUT P-SuperOn).
    RUN RemVar (INPUT integral.P-Codes.SuperOff, OUTPUT P-SuperOff).
    RUN RemVar (INPUT integral.P-Codes.SubOn,    OUTPUT P-SubOn).
    RUN RemVar (INPUT integral.P-Codes.SubOff,   OUTPUT P-SubOff).
    RUN RemVar (INPUT integral.P-Codes.Proptnal, OUTPUT P-Proptnal).
    RUN RemVar (INPUT integral.P-Codes.Lpi,      OUTPUT P-Lpi).
    
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

