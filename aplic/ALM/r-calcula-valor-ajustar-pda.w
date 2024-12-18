&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
/*{src/bin/_prns.i} */  /* Para la impresion */
{src/adm2/widgetprto.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE SHARED VAR S-CODCIA AS INTEGER.
DEFINE SHARED VAR CL-CODCIA AS INTEGER.
DEFINE SHARED VAR S-NOMCIA AS CHARACTER.
DEFINE SHARED VAR S-CODALM AS CHARACTER.
DEFINE SHARED VAR S-DESALM AS CHARACTER.
DEFINE SHARED VAR S-USER-ID AS CHAR.


DEFINE VAR s-task-no AS INTEGER NO-UNDO.

DEFINE IMAGE IMAGE-1 FILENAME "IMG\print" SIZE 5 BY 1.5.
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

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Ok Btn_Done RECT-71 RECT-73 
&Scoped-Define DISPLAYED-OBJECTS x-codalm txt-desalm x-mensaje 

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
     LABEL "Cancelar" 
     SIZE 12 BY 1.54
     BGCOLOR 8 .

DEFINE BUTTON Btn_Ok 
     IMAGE-UP FILE "img\b-ok":U
     LABEL "Aceptar" 
     SIZE 12 BY 1.54.

DEFINE VARIABLE txt-desalm AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .88
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-codalm AS CHARACTER FORMAT "X(256)":U 
     LABEL "Almacen" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE x-mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 64 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-71
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 70.14 BY 1.88
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-73
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73.29 BY 5.85.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     x-codalm AT ROW 3.15 COL 10.57 COLON-ALIGNED WIDGET-ID 42
     txt-desalm AT ROW 3.15 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     x-mensaje AT ROW 5.15 COL 3.43 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     Btn_Ok AT ROW 6.19 COL 45.86 WIDGET-ID 64
     Btn_Done AT ROW 6.19 COL 59.72
     "  PDA : Calcular el valor del CONTEO FINALpara el AJUSTE de inventarios" VIEW-AS TEXT
          SIZE 70.86 BY .96 AT ROW 1.19 COL 3.43 WIDGET-ID 106
          BGCOLOR 9 FGCOLOR 15 FONT 19
     "Criterios Impresión" VIEW-AS TEXT
          SIZE 12.72 BY .5 AT ROW 2.31 COL 3.86 WIDGET-ID 70
          FGCOLOR 1 
     RECT-71 AT ROW 6 COL 3 WIDGET-ID 24
     RECT-73 AT ROW 2.5 COL 2.29 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.72 BY 7.85
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
         TITLE              = "Calcula valor a ajustar"
         HEIGHT             = 7.85
         WIDTH              = 76.72
         MAX-HEIGHT         = 17.42
         MAX-WIDTH          = 76.72
         VIRTUAL-HEIGHT     = 17.42
         VIRTUAL-WIDTH      = 76.72
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN txt-desalm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-codalm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-mensaje IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Calcula valor a ajustar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Calcula valor a ajustar */
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
ON CHOOSE OF Btn_Done IN FRAME F-Main /* Cancelar */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ok W-Win
ON CHOOSE OF Btn_Ok IN FRAME F-Main /* Aceptar */
DO:
  ASSIGN 
      x-CodAlm.
  RUN ue-procesa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY x-codalm txt-desalm x-mensaje 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE Btn_Ok Btn_Done RECT-71 RECT-73 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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
  
   DO WITH FRAME {&FRAME-NAME}:
       /*x-CodAlm:LIST-ITEMS = s-codalm.*/
       ASSIGN 
           x-CodAlm = s-codalm.
       FIND FIRST Almacen WHERE Almacen.CodCia = s-codcia
           AND Almacen.CodAlm = x-CodAlm NO-LOCK NO-ERROR.
       IF AVAIL Almacen THEN DO:
           ASSIGN txt-desalm = Almacen.Descripcion.
           DISPLAY Almacen.Descripcion @ txt-desalm.
       END.
   END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Parametros W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recoge-Parametros W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ue-procesa W-Win 
PROCEDURE ue-procesa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR cAlmc         AS CHARACTER   NO-UNDO.
    DEFINE VAR lQInventariado AS DECIMAL.
    DEFINE VAR lZona AS CHAR.
    DEFINE VAR lQCont1 AS DEC.
    DEFINE VAR lQCont2 AS DEC.
    DEFINE VAR lQCont3 AS DEC.
    
    DEFINE VAR lRowId AS ROWID.
    
    SESSION:SET-WAIT-STATE('GENERAL').
    
    ASSIGN 
        cAlmc   = x-CodAlm:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

    lqCont1 = ?.
    lqCont2 = ?.
    lqCont3 = ?.
    lZona = "".

    FOR EACH invCPDA WHERE invCPDA.codcia = s-codcia AND
                            invCPDA.CodAlm = cAlmc NO-LOCK
                BREAK BY invCPDA.CodCia BY invCPDA.CodAlm BY invCPDA.CodMat :
        
        DISPLAY "Calculando Valor para el Ajuste " + invCPDA.CodMat @ x-mensaje
            WITH FRAME {&FRAME-NAME}.

        IF FIRST-OF(invCPDA.CodMat) THEN DO:
            lZona = invCPDA.CZona.
        END.        

        IF invCPDA.sConteo = 1 THEN DO:
            IF lqCont1 = ? THEN lqCont1 = 0.
            lQCont1 = lQCont1 + invCPDA.QNeto.
        END.
        IF invCPDA.sConteo = 2 THEN DO:
            IF lqCont2 = ? THEN lqCont2 = 0.
            lQCont2 = lQCont2 + invCPDA.QNeto.
        END.
        IF invCPDA.sConteo = 3 THEN DO:
            IF lqCont3 = ? THEN lqCont3 = 0.
            lQCont3 = lQCont3 + invCPDA.QNeto.
        END.     

        IF LAST-OF(invCPDA.Codmat) THEN DO:

            lQInventariado = ?.
            IF lqCont1 <> ? THEN DO:
                /* Hubo Conteo */
                lQInventariado = lqCont1.
            END.
            IF lqCont2 <> ? THEN DO:
                /* Hubo Re-Conteo */
                lQInventariado = lqCont2.
            END.
            IF lqCont3 <> ? THEN DO:
                /* Hubo Re-Conteo */
                lQInventariado = lqCont3.
            END.

            /* Actualizo la Carga Inicial */
            FIND FIRST invCargaInicial WHERE invCargaInicial.codcia = invCPDA.codcia AND 
                            invCargaInicial.CodAlm = invCPDA.CodAlm AND 
                            invCargaInicial.CodMat = invCPDA.CodMat EXCLUSIVE NO-ERROR.

            IF NOT AVAILABLE invCargaInicial THEN DO:
                /* Si NO existe en caraga inicial ADICIONAR */
                CREATE invCargaInicial.
                    ASSIGN invCargaInicial.codcia = invCPDA.CodCia
                            invCargaInicial.CodAlm = invCPDA.CodAlm
                            invCargaInicial.CodMat = invCPDA.CodMat
                            invCargaInicial.QStkSis = 0
                            invCargaInicial.CZona = lZona
                            invCargaInicial.FechReg = NOW
                            invCargaInicial.CUser = s-user-id.
            END.
            lQInventariado = IF (lQInventariado = ?) THEN 0 ELSE lQInventariado.
            ASSIGN  invCargaInicial.QCont1 = lQCont1
                    invCargaInicial.QCont2 = lQCont2
                    invCargaInicial.QCont3 = lQCont3
                    invCargaInicial.QinvFinal = lQInventariado.          

            RELEASE invCargaInicial.

            /* Reseteo variables */
            lqCont1 = ?.
            lqCont2 = ?.
            lqCont3 = ?.
        END.
    END.
    
    /*
    FOR EACH invCargaInicial WHERE invCargaInicial.codcia = s-codcia AND
            invCargaInicial.CodAlm = cAlmc NO-LOCK:
        lRowId = ROWID(invCargaInicial).

        DISPLAY "Calculando Valor para el Ajuste " + invCargaInicial.CodMat @ x-mensaje
            WITH FRAME {&FRAME-NAME}.

        lqCont1 = ?.
        lqCont2 = ?.
        lqCont3 = ?.

        /**/
        FOR EACH invCPDA WHERE invCPDA.codcia = s-codcia AND
                                invCPDA.CodAlm = invCargaInicial.CodAlm AND
                                invCPDA.codmat = invCargaInicial.CodMat NO-LOCK:
            IF invCPDA.sConteo = 1 THEN DO:
                IF lqCont1 = ? THEN lqCont1 = 0.
                lQCont1 = lQCont1 + invCPDA.QNeto.
            END.
            IF invCPDA.sConteo = 2 THEN DO:
                IF lqCont2 = ? THEN lqCont2 = 0.
                lQCont2 = lQCont2 + invCPDA.QNeto.
            END.
            IF invCPDA.sConteo = 3 THEN DO:
                IF lqCont3 = ? THEN lqCont3 = 0.
                lQCont3 = lQCont3 + invCPDA.QNeto.
            END.            
        END.

        lQInventariado = ?.
        IF lqCont1 <> ? THEN DO:
            /* Hubo Conteo */
            lQInventariado = lqCont1.
        END.
        IF lqCont2 <> ? THEN DO:
            /* Hubo Re-Conteo */
            lQInventariado = lqCont2.
        END.
        IF lqCont3 <> ? THEN DO:
            /* Hubo Re-Conteo */
            lQInventariado = lqCont3.
        END.

        /**/
        FIND FIRST b-invCargaInicial WHERE ROWID(b-invCargaInicial) = lRowId 
            EXCLUSIVE NO-ERROR.
        IF AVAILABLE b-invCargaInicial THEN DO:
            lQInventariado = IF (lQInventariado = ?) THEN 0 ELSE lQInventariado.
            ASSIGN  b-invCargaInicial.QCont1 = lQCont1
                    b-invCargaInicial.QCont2 = lQCont2
                    b-invCargaInicial.QCont3 = lQCont3
                    b-invCargaInicial.QinvFinal = lQInventariado.
        END.
    END.
    */
    SESSION:SET-WAIT-STATE('').

    DISPLAY "" @ x-mensaje WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

