&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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

    DEFINE SHARED VAR s-CodCia  AS INT. /*Compania*/
/*    DEFINE SHARED VAR s-Periodo AS INT.*/
    DEFINE SHARED VAR s-NomCia  AS CHAR.
    {PLN\S-GLOBAL.i}
    DEFINE VARIABLE x-Total AS DECIMAL EXTENT 12.
    DEFINE IMAGE IMAGE-1 FILENAME "IMG\print" SIZE 5 BY 1.5.
    DEF VAR FI-MENSAJE AS CHAR FORMAT "X(40)" .
    
    DEFINE TEMP-TABLE Detalle
        FIELDS CodCia  LIKE s-codcia
        FIELDS TpoBol  LIKE Pl-Bole.TpoBol
        FIELDS CodMov  LIKE Pl-Conc.CodMov   
        FIELDS DesMov  LIKE Pl-Conc.DesMov
        FIELDS CodCta  LIKE Pl-Bole.CodCta
        FIELDS ValCal  AS DECIMAL EXTENT 12
        FIELDS Totales AS DECIMAL
        INDEX LLAVE01 AS PRIMARY
        UNIQUE CodCia TpoBol CodMov.
        
   DEFINE FRAME F-Proceso
    IMAGE-1 AT ROW 1.5 COL 5
    "Espere un momento" VIEW-AS TEXT
        SIZE 18 BY 1 AT ROW 1.5 COL 16 FONT 6
    "por favor ...." VIEW-AS TEXT
        SIZE 10 BY 1 AT ROW 2.5 COL 19 FONT 6
        SKIP
    Fi-Mensaje NO-LABEL FONT 6
    SKIP     
    WITH CENTERED OVERLAY KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 
         TITLE "Procesando ..." FONT 7.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PL-CALC

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 PL-CALC.codcal PL-CALC.descal 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-2
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH PL-CALC ~
      WHERE PL-CALC.codpln = 1 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 PL-CALC
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 PL-CALC


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 BUTTON-3 FILL-IN-Periodo Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Periodo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "img\b-cancel":U
     LABEL "&Done" 
     SIZE 15 BY 1.73
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "img\b-ok":U
     LABEL "&Imprimir" 
     SIZE 15 BY 1.73.

DEFINE VARIABLE FILL-IN-Periodo AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Periodo" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      PL-CALC SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      PL-CALC.codcal
      PL-CALC.descal
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 42 BY 6.15
         FONT 4
         TITLE "Seleccione una planilla".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-2 AT ROW 3.12 COL 6
     BUTTON-3 AT ROW 9.65 COL 9
     FILL-IN-Periodo AT ROW 1.58 COL 10 COLON-ALIGNED
     Btn_Done AT ROW 9.65 COL 29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 49.43 BY 11.04
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Resumen de Planillas Anual Empleados"
         HEIGHT             = 11.04
         WIDTH              = 49.43
         MAX-HEIGHT         = 11.04
         MAX-WIDTH          = 49.43
         VIRTUAL-HEIGHT     = 11.04
         VIRTUAL-WIDTH      = 49.43
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* BROWSE-TAB BROWSE-2 1 F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "INTEGRAL.PL-CALC"
     _Options          = "NO-LOCK"
     _Where[1]         = "PL-CALC.codpln = 1"
     _FldNameList[1]   = INTEGRAL.PL-CALC.codcal
     _FldNameList[2]   = INTEGRAL.PL-CALC.descal
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/bin/_prns.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Resumen de Planillas Anual Empleados */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Resumen de Planillas Anual Empleados */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Win
ON CHOOSE OF Btn_Done IN FRAME F-Main /* Done */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Imprimir */
DO:
  ASSIGN  
    FILL-IN-Periodo.
  RUN Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
/* VM - INCLUDE PARA LA CREACION DEL MENU BAR */
{src/adm/template/cntnrwin.i}

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal W-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i AS INT.

    FOR EACH Detalle:
        DELETE Detalle.
    END.

    DO WITH FRAME {&FRAME-NAME}:
     IF {&BROWSE-NAME}:NUM-SELECTED-ROWS > 0 THEN DO:
      DO i = 1 TO {&BROWSE-NAME}:NUM-SELECTED-ROWS :
       IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(i) THEN DO:
            {Util\Rosa\RPlanilla2.i}   
       END.  
      END.  
     END.   
     ELSE DO:
        FOR EACH Pl-Calc NO-LOCK WHERE 
             Pl-Calc.CodPln = 01:
             {Util\Rosa\RPlanilla.i}
        END.
     END.
    END.
 HIDE FRAME F-Proceso.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-Periodo 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BROWSE-2 BUTTON-3 FILL-IN-Periodo Btn_Done 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Formato W-Win 
PROCEDURE Formato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VARIABLE i AS INTEGER NO-UNDO.
 DEFINE VARIABLE x-Totales AS DECIMAL NO-UNDO.
 DEFINE VARIABLE x-TotalesF AS DECIMAL NO-UNDO.
 

 
 DEFINE FRAME F-Detalle
        Detalle.TpoBol      COLUMN-LABEL "Tipo"                 FORMAT "X(12)"
        Detalle.CodMov      COLUMN-LABEL "Concepto"
        Detalle.DesMov      COLUMN-LABEL "Concepto!Planilla"
        Detalle.CodCta      COLUMN-LABEL "Código!Contable"
        Detalle.ValCal[1]   COLUMN-LABEL "Enero"
        Detalle.ValCal[2]   COLUMN-LABEL "Febrero "
        Detalle.ValCal[3]   COLUMN-LABEL "Marzo"
        Detalle.ValCal[4]   COLUMN-LABEL "Abril"
        Detalle.ValCal[5]   COLUMN-LABEL "Mayo"
        Detalle.ValCal[6]   COLUMN-LABEL "Junio"
        Detalle.ValCal[7]   COLUMN-LABEL "Julio"
        Detalle.ValCal[8]   COLUMN-LABEL "Agosto"
        Detalle.ValCal[9]   COLUMN-LABEL "Setiembre"
        Detalle.ValCal[10]  COLUMN-LABEL "Octubre"
        Detalle.ValCal[11]  COLUMN-LABEL "Noviembre"
        Detalle.ValCal[12]  COLUMN-LABEL "Diciembre"
        Detalle.Totales     COLUMN-LABEL "Totales"              FORMAT "->>>,>>>,>>9.99"
 HEADER
 SKIP
 S-NomCia 
 "Página:" AT 90 PAGE-NUMBER(REPORT) FORMAT ">9" SKIP
 "RESUMEN DE PLANILLA ENERO - DICIEMBRE " FILL-IN-Periodo SKIP(2)
 WITH STREAM-IO NO-BOX DOWN WIDTH 320.
 
 FOR EACH Detalle NO-LOCK
    BREAK BY CodCia
    BY TpoBol DESCENDING
    BY CodMov:
    IF FIRST-OF (TpoBol)
    THEN DISPLAY STREAM REPORT
        Detalle.TpoBol
        WITH STREAM-IO NO-BOX WITH FRAME F-Detalle.
        
        IF FIRST-OF (CodMov) THEN DO:
            x-Totales = 0.
        DO i = 1 TO 12:
            x-Totales = x-Totales + ValCal[i].
        END.        
        ASSIGN
               Detalle.Totales = x-Totales.
            DISPLAY STREAM REPORT
               Totales
            WITH STREAM-IO NO-BOX FRAME F-Detalle.
        END.
        
        IF TpoBol = "Remuneraciones" 
          THEN ASSIGN 
            x-Total[1] = x-Total[1] + ValCal[1]
            x-Total[2] = x-Total[2] + ValCal[2]
            x-Total[3] = x-Total[3] + ValCal[3]
            x-Total[4] = x-Total[4] + ValCal[4]
            x-Total[5] = x-Total[5] + ValCal[5]
            x-Total[6] = x-Total[6] + ValCal[6]
            x-Total[7] = x-Total[7] + ValCal[7]
            x-Total[8] = x-Total[8] + ValCal[8]
            x-Total[9] = x-Total[9] + ValCal[9]
            x-Total[10] = x-Total[10] + ValCal[10]
            x-Total[11] = x-Total[11] + ValCal[11]
            x-Total[12] = x-Total[12] + ValCal[12]
            x-TotalesF = x-TotalesF + x-Totales.
        
        IF TpoBol = "Descuentos" 
          THEN ASSIGN 
            x-Total[1] = x-Total[1] - ValCal[1]
            x-Total[2] = x-Total[2] - ValCal[2]
            x-Total[3] = x-Total[3] - ValCal[3]
            x-Total[4] = x-Total[4] - ValCal[4]
            x-Total[5] = x-Total[5] - ValCal[5]
            x-Total[6] = x-Total[6] - ValCal[6]
            x-Total[7] = x-Total[7] - ValCal[7]
            x-Total[8] = x-Total[8] - ValCal[8]
            x-Total[9] = x-Total[9] - ValCal[9]
            x-Total[10] = x-Total[10] - ValCal[10]
            x-Total[11] = x-Total[11] - ValCal[11]
            x-Total[12] = x-Total[12] - ValCal[12]
            x-TotalesF = x-TotalesF - x-Totales.            
        
        
        ACCUMULATE ValCal[1]  (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[2]  (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[3]  (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[4]  (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[5]  (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[6]  (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[7]  (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[8]  (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[9]  (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[10] (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[11] (SUB-TOTAL BY TpoBol).
        ACCUMULATE ValCal[12] (SUB-TOTAL BY TpoBol).
        ACCUMULATE Totales (SUB-TOTAL BY TpoBol).
        
        DISPLAY STREAM Report
            /*Detalle.TpoBol*/
            Detalle.CodMov
            Detalle.DesMov
            Detalle.CodCta
            Detalle.ValCal[1]
            Detalle.ValCal[2]
            Detalle.ValCal[3]
            Detalle.ValCal[4]
            Detalle.ValCal[5]
            Detalle.ValCal[6]
            Detalle.ValCal[7]
            Detalle.ValCal[8]
            Detalle.ValCal[9]
            Detalle.ValCal[10]
            Detalle.ValCal[11]
            Detalle.ValCal[12]
        WITH STREAM-IO NO-BOX FRAME F-Detalle.
        
        IF LAST-OF (TpoBol) THEN DO:
            UNDERLINE STREAM REPORT
            ValCal[1]
            ValCal[2]
            ValCal[3]
            ValCal[4]
            ValCal[5]
            ValCal[6]
            ValCal[7]
            ValCal[8]
            ValCal[9]
            ValCal[10]
            ValCal[11]
            ValCal[12]
            Totales
            WITH STREAM-IO NO-BOX FRAME F-Detalle.
        
        DISPLAY STREAM REPORT
            "Total " + TpoBol + ":" @ DesMov
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[1]  @ ValCal[1]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[2]  @ ValCal[2]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[3]  @ ValCal[3]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[4]  @ ValCal[4]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[5]  @ ValCal[5]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[6]  @ ValCal[6]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[7]  @ ValCal[7]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[8]  @ ValCal[8]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[9]  @ ValCal[9]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[10] @ ValCal[10]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[11] @ ValCal[11]
            ACCUM SUB-TOTAL BY Detalle.TpoBol ValCal[12] @ ValCal[12]
            ACCUM SUB-TOTAL BY Detalle.TpoBol Totales    @ Totales
            WITH STREAM-IO NO-BOX FRAME F-Detalle.        
            DOWN STREAM REPORT 1 WITH FRAME F-Detalle.
        
        END.
            
        IF LAST-OF (CodCia) THEN DO:
            UNDERLINE STREAM REPORT
            ValCal[1]
            ValCal[2]
            ValCal[3]
            ValCal[4]
            ValCal[5]
            ValCal[6]
            ValCal[7]
            ValCal[8]
            ValCal[9]
            ValCal[10]
            ValCal[11]
            ValCal[12]
            Totales
            WITH STREAM-IO NO-BOX FRAME F-Detalle.
            
            DISPLAY STREAM REPORT
               "Neto a Pagar" @ DesMov
               x-Total[1] @ ValCal[1]
               x-Total[2] @ ValCal[2]
               x-Total[3] @ ValCal[3]
               x-Total[4] @ ValCal[4]
               x-Total[5] @ ValCal[5]
               x-Total[6] @ ValCal[6]
               x-Total[7] @ ValCal[7]
               x-Total[8] @ ValCal[8]
               x-Total[9] @ ValCal[9]
               x-Total[10] @ ValCal[10]
               x-Total[11] @ ValCal[11]
               x-Total[12] @ ValCal[12]
               x-TotalesF  @ Totales
            WITH STREAM-IO NO-BOX FRAME F-Detalle.
        END.
 END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir W-Win 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE c-Copias AS INTEGER NO-UNDO.

    RUN lib/Imprimir2.
    IF s-salida-impresion = 0 THEN RETURN.

    RUN Carga-Temporal.  
    IF not Can-find (First Detalle) then do:
        message "No hay registros a imprimir" VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END.
          
    IF s-salida-impresion = 1 THEN 
        s-print-file = SESSION:TEMP-DIRECTORY +
        STRING(NEXT-VALUE(sec-arc,integral)) + ".prn".
    DO c-Copias = 1 TO s-nro-copias:
        CASE s-salida-impresion:
            WHEN 1 OR WHEN 3 THEN
                OUTPUT STREAM REPORT TO VALUE(s-print-file) PAGED PAGE-SIZE 62.
            WHEN 2 THEN
                OUTPUT STREAM REPORT TO PRINTER PAGED PAGE-SIZE 62. /* Impresora */
        END CASE.
        PUT STREAM REPORT CONTROL {&Prn0} + {&Prn5A} + CHR(66) + {&Prn2}.
        RUN Formato.
        PAGE STREAM REPORT.
        OUTPUT STREAM report CLOSE.
    END.
    OUTPUT STREAM report CLOSE.

    CASE s-salida-impresion:
        WHEN 1 OR WHEN 3 THEN DO:
            FRAME {&FRAME-NAME}:SENSITIVE = FALSE.
            RUN LIB/W-README.R(s-print-file).
            FRAME {&FRAME-NAME}:SENSITIVE = TRUE.
            IF s-salida-impresion = 1 THEN
                OS-DELETE VALUE(s-print-file).
        END.
    END CASE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
  FILL-IN-Periodo = s-Periodo.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "PL-CALC"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
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


