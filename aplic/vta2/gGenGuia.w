&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-ADOCU FOR CcbADocu.
DEFINE BUFFER B-CDocu FOR CcbCDocu.
DEFINE SHARED TEMP-TABLE PEDI LIKE FacDPedi.
DEFINE SHARED TEMP-TABLE PEDI-1 LIKE FacDPedi.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

DEFINE INPUT PARAMETER rwParaRowID AS ROWID.
DEFINE INPUT PARAMETER pTipoGuia   AS CHAR.
DEFINE INPUT PARAMETER pCodDoc     AS CHAR.
/* pTipoGuia
    A: autom�tica
    M: manual
*/
DEFINE SHARED VARIABLE s-CodCia AS INTEGER.
DEFINE SHARED VARIABLE s-NomCia AS CHARACTER.
DEFINE SHARED VARIABLE s-CodDiv AS CHARACTER.
/*DEFINE SHARED VARIABLE s-CodAlm AS CHARACTER.*/
DEFINE SHARED VARIABLE s-User-Id AS CHARACTER.
DEFINE SHARED VARIABLE cl-codcia AS INTEGER.

DEFINE VARIABLE cCodDoc AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE cCodAlm AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE cCodMov AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountGuide AS INTEGER NO-UNDO.
DEFINE VARIABLE cObser  AS CHARACTER   NO-UNDO.

FIND FacCPedi WHERE ROWID(FacCPedi) = rwParaRowID NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCPedi THEN DO:
    MESSAGE
        "Registro de O/D no disponible"
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
IF NOT (FacCPedi.FlgEst = "P" AND FacCPedi.FlgSit = 'C') THEN DO:
    MESSAGE
        "Registro de O/D ya no est� 'PENDIENTE'"
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
/* cCodAlm = FacCPedi.Codalm.      /* <<< OJO <<< */ */
cObser  = FacCPedi.Glosa.

/* Consistencia del tipo de guia */
IF pTipoGuia = 'A' THEN DO:     /* AUTOMATICA */
    FIND FIRST facdpedi OF faccpedi WHERE facdpedi.canate > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE facdpedi THEN DO:
        MESSAGE 'Esta Orden tiene atenciones parciales' SKIP
            'Solo se pueden generar guias manuales'
            VIEW-AS ALERT-BOX WARNING.
        RETURN.
    END.
END.

cCodDoc = pCodDoc.
FIND FacDocum WHERE FacDocum.CodCia = s-CodCia 
    AND FacDocum.CodDoc = cCodDoc 
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacDocum OR FacDocum.CodMov = 0 THEN DO:
    MESSAGE
        "Codigo de Documento" cCodDoc "no configurado"
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
cCodMov = FacDocum.CodMov.

FIND FIRST FacCorre WHERE FacCorre.CodCia = s-CodCia 
    AND FacCorre.CodDiv = s-CodDiv 
    AND FacCorre.CodDoc = cCodDoc 
    AND FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    MESSAGE
        "Codigo de Documento" cCodDoc "no tiene series activas"
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

DEFINE TEMP-TABLE Reporte NO-UNDO
    FIELDS CodCia LIKE CcbCDocu.CodCia
    FIELDS CodDoc LIKE CcbCDocu.CodDoc
    FIELDS NroDoc LIKE CcbCDocu.Nrodoc.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-NroSer Btn_OK Btn_Cancel FILL-IN-Glosa ~
FILL-IN-LugEnt RECT-1 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NroPed FILL-IN-Cliente ~
FILL-IN-DirClie COMBO-NroSer FILL-IN-NroDoc FILL-IN-items FILL-IN-Glosa ~
FILL-IN-LugEnt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "img/b-cancel.bmp":U
     LABEL "&Cancelar" 
     SIZE 12 BY 1.54.

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "img/b-ok.bmp":U
     LABEL "&Aceptar" 
     SIZE 12 BY 1.54.

DEFINE VARIABLE COMBO-NroSer AS CHARACTER FORMAT "X(3)":U 
     LABEL "Serie G/R" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 6.72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Cliente AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81
     BGCOLOR 9 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE FILL-IN-DirClie AS CHARACTER FORMAT "X(256)":U 
     LABEL "Direcci�n" 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81
     BGCOLOR 9 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE FILL-IN-Glosa AS CHARACTER FORMAT "X(60)":U 
     LABEL "Glosa" 
     VIEW-AS FILL-IN 
     SIZE 56 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-items AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Items por Gu�a" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 9 FGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE FILL-IN-LugEnt AS CHARACTER FORMAT "X(60)":U 
     LABEL "Entregar en" 
     VIEW-AS FILL-IN 
     SIZE 56 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-NroDoc AS CHARACTER FORMAT "XXX-XXXXXX":U 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 9 FGCOLOR 15 FONT 0 NO-UNDO.

DEFINE VARIABLE FILL-IN-NroPed AS CHARACTER FORMAT "X(9)":U 
     LABEL "O/D" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 9 FGCOLOR 15 FONT 0 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 5.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     FILL-IN-NroPed AT ROW 1.81 COL 11 COLON-ALIGNED WIDGET-ID 20
     FILL-IN-Cliente AT ROW 1.81 COL 28 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-DirClie AT ROW 2.62 COL 28 COLON-ALIGNED WIDGET-ID 24
     COMBO-NroSer AT ROW 3.69 COL 5.28 WIDGET-ID 2
     FILL-IN-NroDoc AT ROW 3.69 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     FILL-IN-items AT ROW 6.12 COL 63 COLON-ALIGNED WIDGET-ID 18
     Btn_OK AT ROW 7.19 COL 3
     Btn_Cancel AT ROW 7.19 COL 15
     FILL-IN-Glosa AT ROW 5.31 COL 11 COLON-ALIGNED WIDGET-ID 28
     FILL-IN-LugEnt AT ROW 4.5 COL 11 COLON-ALIGNED WIDGET-ID 30
     RECT-1 AT ROW 1.27 COL 2 WIDGET-ID 4
     SPACE(1.28) SKIP(2.34)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Generando Guia(s) de Remisi�n por Orden de Despacho..." WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: B-ADOCU B "?" ? INTEGRAL CcbADocu
      TABLE: B-CDocu B "?" ? INTEGRAL CcbCDocu
      TABLE: PEDI T "SHARED" ? INTEGRAL FacDPedi
      TABLE: PEDI-1 T "SHARED" ? INTEGRAL FacDPedi
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/bin/_prns.i}
{src/adm/method/containr.i}
{src/adm-vm/method/vmviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX COMBO-NroSer IN FRAME gDialog
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-Cliente IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DirClie IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-items IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NroDoc IN FRAME gDialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NroPed IN FRAME gDialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Generando Guia(s) de Remisi�n por Orden de Despacho... */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* Aceptar */
DO:

    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

    FOR EACH FacDPedi OF FacCPedi NO-LOCK:
        iCount = iCount + 1.
    END.
    IF iCount = 0 THEN DO:
        MESSAGE "No hay items por despachar"
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.

    MESSAGE
        "�Todos los datos son correctos?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE rpta AS LOGICAL.
    IF rpta <> TRUE THEN RETURN NO-APPLY.

    ASSIGN
        COMBO-NroSer
        FILL-IN-LugEnt
        FILL-IN-Glosa.

    CASE pTipoGuia:
        WHEN 'A' THEN DO:       /* AUTOMATICA */
            RUN proc_CreaGuia.
            IF RETURN-VALUE = 'ADM-ERROR' THEN
                MESSAGE
                    "Ninguna gu�a fu� generada"
                    VIEW-AS ALERT-BOX WARNING.
            ELSE DO:
                MESSAGE
                    "Se ha(n) generado" iCountGuide "gu�a(s)"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RUN Papel-Blanco.
            END.
        END.
        WHEN 'M' THEN DO:       /* MANUAL */
            RUN proc_CreaGuiaManual.
            IF RETURN-VALUE = 'ADM-ERROR' THEN
                MESSAGE
                    "Ninguna gu�a fu� generada"
                    VIEW-AS ALERT-BOX WARNING.
            ELSE DO:
                RUN Papel-Blanco.
            END.
        END.
    END CASE.
    

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-NroSer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-NroSer gDialog
ON RETURN OF COMBO-NroSer IN FRAME gDialog /* Serie G/R */
DO:
    APPLY 'Tab':U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-NroSer gDialog
ON VALUE-CHANGED OF COMBO-NroSer IN FRAME gDialog /* Serie G/R */
DO:

    /* Correlativo */
    FIND FacCorre WHERE
        FacCorre.CodCia = s-CodCia AND
        FacCorre.CodDoc = cCodDoc AND
        FacCorre.CodDiv = s-CodDiv AND
        FacCorre.NroSer = INTEGER(SELF:SCREEN-VALUE)
        NO-LOCK NO-ERROR.
    IF AVAILABLE FacCorre THEN
        FILL-IN-NroDoc =
            STRING(FacCorre.NroSer,"999") +
            STRING(FacCorre.Correlativo,"999999").
    ELSE FILL-IN-NroDoc = "".
    DISPLAY FILL-IN-NroDoc WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cierra-Orden gDialog 
PROCEDURE Cierra-Orden :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Derramos O/D */
  FacCPedi.FlgEst = "C".
  FOR EACH FacDPedi NO-LOCK WHERE FacDPedi.CodCia = S-CODCIA 
      AND FacDPedi.CodDoc = CcbCDocu.Libre_c01
      AND FacDPedi.NroPed = CcbCDocu.Libre_c02:
      IF (FacDPedi.CanPed - FacDPedi.CanAte) > 0 THEN DO:
          FacCPedi.FlgEst = "P".
          LEAVE.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
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
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-NroPed FILL-IN-Cliente FILL-IN-DirClie COMBO-NroSer 
          FILL-IN-NroDoc FILL-IN-items FILL-IN-Glosa FILL-IN-LugEnt 
      WITH FRAME gDialog.
  ENABLE COMBO-NroSer Btn_OK Btn_Cancel FILL-IN-Glosa FILL-IN-LugEnt RECT-1 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Formato gDialog 
PROCEDURE Formato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE x-vend  AS CHARACTER NO-UNDO.

    DEFINE FRAME f-det
        CcbDDocu.CodMat  FORMAT 'X(7)'
        Almmmatg.DesMat  FORMAT 'x(60)'
        Almmmatg.DesMar  FORMAT 'x(24)'
        Almmmatg.UndBas
        CcbDDocu.CanDes  FORMAT ">>,>>>,>>9.9999"
        WITH WIDTH 400 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.

    FOR EACH Reporte,
        FIRST CCbCDocu NO-LOCK WHERE 
            CcbCDocu.CodCia = Reporte.CodCia AND
            CcbCDocu.CodDoc = Reporte.CodDoc AND
            CcbCDocu.NroDoc = Reporte.NroDoc:
        FOR FIRST gn-ven WHERE
            gn-ven.codcia = CcbCDocu.CodCia AND
            gn-ven.CodVen = CcbCDocu.CodVen NO-LOCK:
            x-vend = gn-ven.NomVen.
        END.
        DEFINE FRAME f-cab
            HEADER
            {&PRN2} + {&PRN7A} + {&PRN6A} + s-NOMCIA + {&PRN6B} + {&PRN7B} + {&PRN3} FORMAT "X(45)" SKIP(2)
            {&PRN4} + {&PRN6A} + "Cliente : " + CcbCDocu.NomCli  AT 1 FORMAT "X(60)" 
            {&PRN4} + {&PRN6B} + "Pagina: " AT 100 FORMAT "X(15)" PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
            {&PRN4} + {&PRN6A} + "Direcci�n: " + CcbCDocu.DirCli  AT 1 FORMAT "X(60)" SKIP
            {&PRN4} + {&PRN6A} + "RUC: " + CcbCDocu.RucCli  AT 1 FORMAT "X(40)"
            {&PRN4} + {&PRN6B} + "Hora  : " AT 100 FORMAT "X(15)" STRING(TIME,"HH:MM") FORMAT "X(12)" SKIP
            {&PRN4} + {&PRN6A} + "Vendedor : " + x-vend  AT 1 FORMAT "X(15)" SKIP
            {&PRN4} + {&PRN6B} + "Forma Pago : " + CcbCDocu.FmaPgo  AT 1 FORMAT "X(40)" SKIP
/*RD01*/    {&PRN4} + {&PRN6B} + "Glosa : " + cObser  AT 1 FORMAT "X(60)" SKIP
            {&PRN4} + {&PRN7A} + {&PRN6B} + "N� Gu�a de Remisi�n : " + CcbCDocu.NroDoc + {&PRN6B} + {&PRN7B} + {&PRN3} AT 1 FORMAT "X(50)" SKIP   
            {&PRN4} + {&PRN7A} + {&PRN6B} + "N� Orden de Despacho: " + CcbCDocu.Libre_c02 + {&PRN6B} + {&PRN7B} + {&PRN3} AT 1 FORMAT "X(50)" SKIP   
            "------------------------------------------------------------------------------------------------------------------------------------" SKIP
            " C�digo  Descripci�n                                                    Marca                  Unidad      Cantidad           " SKIP
            "------------------------------------------------------------------------------------------------------------------------------------" SKIP 
    /***      999999  123456789012345678901234567890123456789012345678901234567890 12345678901234567890 123456789  >>,>>>,>>9.9999 */
            WITH WIDTH 400 NO-BOX NO-LABELS NO-UNDERLINE STREAM-IO DOWN.
        VIEW STREAM Report FRAME f-cab.
        FOR EACH CcbDDocu OF CcbCDocu NO-LOCK,
            EACH Almmmatg NO-LOCK WHERE Almmmatg.CodCia = CcbDDocu.CodCia
                AND Almmmatg.CodMat = CcbDDocu.CodMat,
                FIRST Almmmate WHERE
                    Almmmate.CodCia = CcbDDocu.CodCia AND
                    Almmmate.CodAlm = CcbCDocu.CodAlm AND
                    Almmmate.CodMat = CcbDDocu.CodMat 
                    BREAK BY Almmmate.CodUbi
                          BY CcbDDocu.CodMat:
                          DISPLAY STREAM Report
                                CcbDDocu.CodMat  
                                Almmmatg.DesMat
                                Almmmatg.DesMar
                                Almmmatg.UndBas
                                CcbDDocu.CanDes
                                WITH FRAME f-det.
        END.
        PAGE STREAM Report.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir gDialog 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE c-Copias AS INTEGER NO-UNDO.
 
    RUN lib/Imprimir2.
    IF s-salida-impresion = 0 THEN RETURN.

    IF s-salida-impresion = 1 THEN 
        s-print-file = SESSION:TEMP-DIRECTORY +
        STRING(NEXT-VALUE(sec-arc,integral)) + ".prn".

    DO c-Copias = 1 TO s-nro-copias ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
        CASE s-salida-impresion:
            WHEN 1 OR WHEN 3 THEN
                OUTPUT STREAM REPORT TO VALUE(s-print-file) PAGED PAGE-SIZE 30.
            WHEN 2 THEN
                OUTPUT STREAM REPORT TO PRINTER PAGED PAGE-SIZE 30. /* Impresora */
        END CASE.
        PUT STREAM REPORT CONTROL {&Prn0} + {&Prn5A} + CHR(33) + {&Prn4} .
        RUN Formato.
        PAGE STREAM REPORT.
        OUTPUT STREAM REPORT CLOSE.
    END.
    OUTPUT CLOSE.

    CASE s-salida-impresion:
        WHEN 1 OR WHEN 3 THEN DO:
            RUN LIB/W-README.R(s-print-file).
            IF s-salida-impresion = 1 THEN OS-DELETE VALUE(s-print-file).
        END.
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize gDialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

    DEFINE VARIABLE cListItems AS CHARACTER NO-UNDO.

    FIND FacCfgGn WHERE FacCfgGn.CodCia = s-CodCia NO-LOCK NO-ERROR.
    FOR EACH FacCorre NO-LOCK WHERE 
        FacCorre.CodCia = s-CodCia AND
        FacCorre.CodDiv = s-CodDiv AND 
        FacCorre.CodDoc = cCodDoc AND
        FacCorre.FlgEst = YES:
        IF cListItems = "" THEN cListItems = STRING(FacCorre.NroSer,"999").
        ELSE cListItems = cListItems + "," + STRING(FacCorre.NroSer,"999").
    END.
    DO WITH FRAME {&FRAME-NAME}:
        COMBO-NroSer:LIST-ITEMS = cListItems.
        COMBO-NroSer = ENTRY(1,COMBO-NroSer:LIST-ITEMS).
        FILL-IN-items = FacCfgGn.Items_Guias.
        FILL-IN-NroPed = FacCPedi.NroPed.
        FILL-IN-Glosa = FacCPedi.Glosa.
        FILL-IN-LugEnt = FacCPedi.LugEnt.
        FILL-IN-Cliente = FacCPedi.CodCli + " - " + FacCPedi.NomCli.
        FILL-IN-DirClie = FacCPedi.DirCli.
        /* Correlativo */
        FIND FacCorre WHERE
            FacCorre.CodCia = s-CodCia AND
            FacCorre.CodDoc = cCodDoc AND
            FacCorre.CodDiv = s-CodDiv AND
            FacCorre.NroSer = INTEGER(COMBO-NroSer)
            NO-LOCK NO-ERROR.
        IF AVAILABLE FacCorre THEN
            FILL-IN-NroDoc =
                STRING(FacCorre.NroSer,"999") +
                STRING(FacCorre.Correlativo,"999999").
    END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Papel-Blanco gDialog 
PROCEDURE Papel-Blanco :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RETURN.
    IF CcbCDocu.FlgEst <> "A" THEN DO:
        IF CcbCDocu.FlgEnv  = YES    /* Es G/R manual */
            OR Ccbcdocu.FlgEst = 'X' THEN DO:
            MESSAGE 'Esta GUIA SOLO debe imprimirse en PAPEL BLANCO' SKIP
                'NO usar formatos preimpresos' SKIP
                'Continuamos la impresion?'
                VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO
                UPDATE rpta-1 AS LOG.
            IF rpta-1 = NO THEN RETURN.
            RUN Imprimir.
        END.
        ELSE DO:
            IF Ccbcdocu.flgest <> 'F' THEN DO:
                MESSAGE 'NO se puede imprimir la gu�a' SKIP
                    'A�n no se ha Facturado' VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END.
            RUN vta/d-fmtgui (ROWID(CcbCDocu)).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Parametros gDialog 
PROCEDURE Procesa-Parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    CASE HANDLE-CAMPO:NAME:
        WHEN "" THEN .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_CreaCabecera gDialog 
PROCEDURE proc_CreaCabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Correlativo */
    {vtagn/i-faccorre-01.i &Codigo = cCodDoc &Serie = INTEGER(COMBO-NroSer) }
    /* *********** */
    CREATE CcbCDocu.
    ASSIGN
        CcbCDocu.CodCia = s-CodCia
        CcbCDocu.CodDiv = s-CodDiv   
        CcbCDocu.DivOri = FacCPedi.CodDiv    /* OJO: division de estadisticas */
        CcbCDocu.CodAlm = FacCPedi.CodAlm   /* OJO: Almac�n despacho */
        /*CcbCDocu.CodAlm = cCodAlm*/
        CcbCDocu.CodDoc = cCodDoc
        CcbCDocu.NroDoc = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999") 
        CcbCDocu.FchDoc = TODAY
        CcbCDocu.CodMov = cCodMov
        CcbCDocu.CodRef = FacCPedi.CodDoc           /* CONTROL POR DEFECTO */
        CcbCDocu.NroRef = FacCPedi.NroPed
        CcbCDocu.Libre_c01 = FacCPedi.CodDoc        /* CONTROL ADICIONAL */
        CcbCDocu.Libre_c02 = FacCPedi.NroPed
        Ccbcdocu.CodPed = FacCPedi.CodRef           /* NUMERO DE PEDIDO */
        Ccbcdocu.NroPed = FacCPedi.NroRef
        CcbCDocu.Tipo   = "OFICINA"
        CcbCDocu.FchVto = TODAY
        CcbCDocu.CodCli = FacCPedi.CodCli
        Ccbcdocu.NomCli = FacCPedi.NomCli
        Ccbcdocu.RucCli = FacCPedi.RucCli
        CcbCDocu.CodAnt = FacCPedi.Atencion     /* DNI */
        Ccbcdocu.DirCli = FacCPedi.DirCli
        CcbCDocu.CodVen = FacCPedi.CodVen
        CcbCDocu.TipVta = FacCPedi.TipVta
        CcbCDocu.TpoFac = "R"       /* GUIA VENTA AUTOMATICA */
        CcbCDocu.FmaPgo = FacCPedi.FmaPgo
        CcbCDocu.CodMon = FacCPedi.CodMon
        CcbCDocu.TpoCmb = FacCfgGn.TpoCmb[1]
        CcbCDocu.PorIgv = FacCPedi.PorIgv
        CcbCDocu.NroOrd = FacCPedi.ordcmp
        CcbCDocu.FlgEst = "P"       /* POR FACTURAR */
        CcbCDocu.FlgSit = "P"
        CcbCDocu.usuario = S-USER-ID
        CcbCDocu.HorCie = STRING(TIME,'hh:mm')
        CcbCDocu.FlgEnv = (FacCPedi.TpoPed = 'M')
        CcbCDocu.LugEnt = FILL-IN-LugEnt
        CcbCDocu.Glosa = FILL-IN-Glosa.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    /*RDP*/    
    CREATE Reporte.
        ASSIGN 
            Reporte.CodCia = CcbCDocu.CodCia
            Reporte.CodDoc = CcbCDocu.CodDoc
            Reporte.NroDoc = CcbCDocu.NroDoc.
    FIND gn-convt WHERE gn-convt.Codig = CcbCDocu.FmaPgo NO-LOCK NO-ERROR.
    IF AVAILABLE gn-convt THEN DO:
        CcbCDocu.TipVta = IF gn-ConVt.TotDias = 0 THEN "1" ELSE "2".
        CcbCDocu.FchVto = CcbCDocu.FchDoc + INTEGER(ENTRY(NUM-ENTRIES(gn-ConVt.Vencmtos),gn-ConVt.Vencmtos)).
    END.
    FIND gn-clie WHERE gn-clie.CodCia = cl-codcia 
        AND gn-clie.CodCli = CcbCDocu.CodCli 
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie  THEN DO:
        ASSIGN
            CcbCDocu.CodDpto = gn-clie.CodDept 
            CcbCDocu.CodProv = gn-clie.CodProv 
            CcbCDocu.CodDist = gn-clie.CodDist.
    END.
    /* COPIAMOS DATOS DEL TRANSPORTISTA */
    FIND Ccbadocu WHERE Ccbadocu.codcia = Faccpedi.codcia
        AND Ccbadocu.coddiv = Faccpedi.coddiv
        AND Ccbadocu.coddoc = Faccpedi.coddoc
        AND Ccbadocu.nrodoc = Faccpedi.nroped
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbadocu THEN DO:
        CREATE B-ADOCU.
        BUFFER-COPY Ccbadocu TO B-ADOCU
            ASSIGN
                B-ADOCU.CodDiv = Ccbcdocu.CodDiv
                B-ADOCU.CodDoc = Ccbcdocu.CodDoc
                B-ADOCU.NroDoc = Ccbcdocu.NroDoc.
    END.
    /* ******************************** */
    /* TRACKING GUIAS */
    RUN vtagn/pTracking-04 (s-CodCia,
                            s-CodDiv,
                            Ccbcdocu.CodPed,
                            Ccbcdocu.NroPed,
                            s-User-Id,
                            'EGUI',
                            'P',
                            DATETIME(TODAY, MTIME),
                            DATETIME(TODAY, MTIME),
                            Ccbcdocu.coddoc,
                            Ccbcdocu.nrodoc,
                            CcbCDocu.Libre_c01,
                            CcbCDocu.Libre_c02).
END.
RETURN 'OK'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_CreaGuia gDialog 
PROCEDURE proc_CreaGuia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE iCountItem AS INTEGER INITIAL 1 NO-UNDO.
    DEFINE VARIABLE lCreaHeader AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lItemOk AS LOGICAL NO-UNDO.
    
    /*Borrando Temporal*/
    EMPTY TEMP-TABLE Reporte.

    trloop:
    DO TRANSACTION ON ERROR UNDO trloop, RETURN 'ADM-ERROR' ON STOP UNDO trloop, RETURN 'ADM-ERROR':
        FIND FacCPedi WHERE ROWID(FacCPedi) = rwParaRowID EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE FacCPedi THEN DO:
            MESSAGE
                "Registro de O/D no disponible"
                VIEW-AS ALERT-BOX ERROR.
            RETURN 'ADM-ERROR'.
        END.
        IF NOT (FacCPedi.FlgEst = "P" AND FacCPedi.FlgSit = "C") THEN DO:
            MESSAGE
                "Registro de O/D ya no est� 'PENDIENTE'"
                VIEW-AS ALERT-BOX ERROR.
            RETURN 'ADM-ERROR'.
        END.

        iCountGuide = 0.
        lCreaHeader = TRUE.
        lItemOk = FALSE.

        /* GENERACION DE GUIAS DE REMISION */
        FOR EACH FacDPedi OF FacCPedi NO-LOCK, FIRST Almmmatg OF FacDPedi NO-LOCK 
            BREAK BY FacDPedi.CodCia BY FacDPedi.CodMat:
            /* Crea Cabecera */
            IF lCreaHeader THEN DO:
                /* Cabecera de Gu�a */
                RUN proc_CreaCabecera.
                IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO trloop, RETURN 'ADM-ERROR'.
                ASSIGN
                    iCountGuide = iCountGuide + 1
                    lCreaHeader = FALSE.
            END.
            /* Crea Detalle */
            CREATE CcbDDocu.
            BUFFER-COPY FacDPedi
                TO CcbDDocu
                ASSIGN
                    CcbDDocu.NroItm = iCountItem
                    CcbDDocu.CodCia = CcbCDocu.CodCia
                    CcbDDocu.Coddoc = CcbCDocu.Coddoc
                    CcbDDocu.NroDoc = CcbCDocu.NroDoc 
                    CcbDDocu.FchDoc = CcbCDocu.FchDoc
                    CcbDDocu.CodDiv = CcbcDocu.CodDiv
                    CcbDDocu.CanDes = FacDPedi.CanPed.
            lItemOk = TRUE.
            iCountItem = iCountItem + 1.
            IF iCountItem > FILL-IN-items OR LAST-OF (FacDPedi.CodCia) THEN DO:
                RUN proc_GrabaTotales.
                /* Descarga de Almacen SOLO para pedidos al CREDITO */
                RUN vta2/act_alm (ROWID(CcbCDocu)).
                IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO trloop, RETURN 'ADM-ERROR'.
                /* ************************************************ */
            END.
            IF iCountItem > FILL-IN-items THEN DO:
                iCountItem = 1.
                lCreaHeader = TRUE.
                lItemOk = FALSE.
            END.
        END.
        /* Cierra la O/D */
        ASSIGN 
            FacCPedi.FlgEst = "C".
        FOR EACH Facdpedi OF Faccpedi:
            ASSIGN
                Facdpedi.canate = Facdpedi.canped
                Facdpedi.flgest = Faccpedi.flgest.
        END.
        IF AVAILABLE(Faccpedi) THEN RELEASE FacCPedi.
        IF AVAILABLE(FacCorre) THEN RELEASE FacCorre.
        IF AVAILABLE(Ccbcdocu) THEN RELEASE Ccbcdocu.
        IF AVAILABLE(Ccbddocu) THEN RELEASE Ccbddocu.
    END. /* DO TRANSACTION... */
    RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_CreaGuiaManual gDialog 
PROCEDURE proc_CreaGuiaManual :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-NroItm AS INT NO-UNDO.

    /*Borrando Temporal*/
    EMPTY TEMP-TABLE Reporte.

    trloop:
    DO TRANSACTION ON ERROR UNDO trloop, RETURN 'ADM-ERROR' ON STOP UNDO trloop, RETURN 'ADM-ERROR':
        FIND FacCPedi WHERE ROWID(FacCPedi) = rwParaRowID EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE FacCPedi THEN DO:
            MESSAGE
                "Registro de O/D no disponible"
                VIEW-AS ALERT-BOX ERROR.
            RETURN 'ADM-ERROR'.
        END.
        IF NOT (FacCPedi.FlgEst = "P" AND FacCPedi.FlgSit = "C") THEN DO:
            MESSAGE
                "Registro de O/D ya no est� 'PENDIENTE'"
                VIEW-AS ALERT-BOX ERROR.
            RETURN 'ADM-ERROR'.
        END.
        /* Correlativo */
        /* Crea Cabecera */
        RUN proc_CreaCabecera.
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO trloop, RETURN 'ADM-ERROR'.

        x-NroItm = 1.
        FOR EACH PEDI, FIRST Almmmatg OF PEDI NO-LOCK BY PEDI.CodMat:
            /* RECALCULAMOS IMPORTES */
            ASSIGN
                PEDI.CanPed = PEDI.CanAte.      /* << OJO CON ESTO << */
            ASSIGN
                PEDI.ImpLin = ROUND ( PEDI.CanPed * PEDI.PreUni * 
                    ( 1 - PEDI.Por_Dsctos[1] / 100 ) *
                    ( 1 - PEDI.Por_Dsctos[2] / 100 ) *
                    ( 1 - PEDI.Por_Dsctos[3] / 100 ), 2 ).
            IF PEDI.Por_Dsctos[1] = 0 AND PEDI.Por_Dsctos[2] = 0 AND PEDI.Por_Dsctos[3] = 0 
                THEN PEDI.ImpDto = 0.
            ELSE PEDI.ImpDto = PEDI.CanPed * PEDI.PreUni - PEDI.ImpLin.
            ASSIGN
                PEDI.ImpLin = ROUND(PEDI.ImpLin, 2)
                PEDI.ImpDto = ROUND(PEDI.ImpDto, 2).
            IF PEDI.AftIsc 
                THEN PEDI.ImpIsc = ROUND(PEDI.PreBas * PEDI.CanPed * (Almmmatg.PorIsc / 100),4).
            ELSE PEDI.ImpIsc = 0.
            IF PEDI.AftIgv 
                THEN PEDI.ImpIgv = PEDI.ImpLin - ROUND(PEDI.ImpLin  / (1 + (Faccpedi.PorIgv / 100)),4).
            ELSE PEDI.ImpIgv = 0.
            /* Crea Detalle */
            CREATE CcbDDocu.
            BUFFER-COPY PEDI TO CcbDDocu
                ASSIGN
                    CcbDDocu.CodCia = CcbCDocu.CodCia
                    CcbDDocu.Coddoc = CcbCDocu.Coddoc
                    CcbDDocu.NroDoc = CcbCDocu.NroDoc 
                    CcbDDocu.FchDoc = CcbCDocu.FchDoc
                    CcbDDocu.CodDiv = CcbcDocu.CodDiv
                    CcbDDocu.NroItm = x-NroItm
                    CcbDDocu.CanDes = PEDI.CanAte.       /* OJO */
            ASSIGN
                CcbDDocu.Pesmat = Almmmatg.Pesmat * (CcbDDocu.Candes * CcbDDocu.Factor)
                x-NroItm = x-NroItm + 1.
            /* Actualiza Detalle de la Orden de Despacho */
            FIND FacDPedi OF FacCPedi WHERE FacDPedi.CodMat = CcbDDocu.CodMat EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE FacDPedi THEN FacDPedi.CanAte = FacDPedi.CanAte + CcbDDocu.CanDes.
        END. /* FOR EACH PEDI... */

        RUN proc_GrabaTotales.

        /* Descarga de Almacen */
        RUN vta2\act_alm (ROWID(CcbCDocu)).
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO trloop, RETURN 'ADM-ERROR'.

        /* Cierra la O/D */
        RUN Cierra-Orden.
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        
        IF AVAILABLE(Faccpedi) THEN RELEASE FacCPedi.
        IF AVAILABLE(Facdpedi) THEN RELEASE FacDPedi.
        IF AVAILABLE(FacCorre) THEN RELEASE FacCorre.
        IF AVAILABLE(Ccbcdocu) THEN RELEASE Ccbcdocu.
        IF AVAILABLE(Ccbddocu) THEN RELEASE Ccbddocu.
    END. /* DO TRANSACTION... */
    RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_GrabaTotales gDialog 
PROCEDURE proc_GrabaTotales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{vta2/graba-totales-fac.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recoge-Parametros gDialog 
PROCEDURE Recoge-Parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    CASE HANDLE-CAMPO:NAME:
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
