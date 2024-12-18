&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE DETA LIKE CcbDDocu.
DEFINE TEMP-TABLE DOCU LIKE CcbCDocu.
DEFINE TEMP-TABLE T-FELogErrores NO-UNDO LIKE FELogErrores.



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

/* Shared Variable Definitions ---                                       */

DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.
DEFINE SHARED VARIABLE S-CODCIA   AS INTEGER.
DEFINE SHARED VARIABLE S-NOMCIA   AS CHAR.
DEFINE SHARED VARIABLE S-USER-ID  AS CHAR.
DEFINE SHARED VARIABLE S-CODDOC   AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV   AS CHAR.
DEFINE SHARED VARIABLE cl-codcia  AS INT.
DEFINE SHARED VARIABLE s-codalm   AS CHAR.
DEFINE SHARED VARIABLE s-CodTer   AS CHAR.
DEFINE SHARED VARIABLE s-TpoFac   AS CHAR.

DEFINE SHARED VARIABLE S-NROSER   AS INT.

DEFINE SHARED VARIABLE S-PORDTO AS DEC.
DEFINE SHARED VARIABLE S-PORIGV AS DEC.

DEFINE SHARED VARIABLE s-Sunat-Activo AS LOG.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE I-NROSER       AS INTEGER   NO-UNDO.
DEFINE VARIABLE I-NRODOC       AS INTEGER   NO-UNDO.
DEFINE VARIABLE R-NRODEV       AS ROWID     NO-UNDO.
DEFINE VARIABLE S-PRINTER-NAME AS CHARACTER NO-UNDO.
DEFINE VARIABLE S-CODCLI       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE R-ROWID        AS ROWID     NO-UNDO. 


FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
     FacCorre.CodDoc = S-CODDOC AND
     FacCorre.CodDiv = S-CODDIV AND
     FacCorre.CodAlm = s-CodAlm AND 
     FacCorre.FlgEst = YES NO-LOCK NO-ERROR.
IF AVAILABLE FacCorre THEN 
   ASSIGN I-NroSer = FacCorre.NroSer.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

/* FORMATO DEL COMPROBANTE: XXX-XXXXXXXX    (3-8) */
DEF VAR x-Formato AS CHAR INIT '999-999999' NO-UNDO.
RUN sunat\p-formato-doc (INPUT s-CodDoc, OUTPUT x-Formato).

DEF VAR pMensaje AS CHAR NO-UNDO.

/* ICBPER */
DEFINE VAR x-articulo-ICBPER AS CHAR.

x-articulo-ICBPER = '099268'.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES CcbCDocu
&Scoped-define FIRST-EXTERNAL-TABLE CcbCDocu


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR CcbCDocu.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS CcbCDocu.CodRef CcbCDocu.NroRef ~
CcbCDocu.CodAnt CcbCDocu.CodMon CcbCDocu.CodCli CcbCDocu.NomCli ~
CcbCDocu.RucCli CcbCDocu.DirCli CcbCDocu.TpoCmb CcbCDocu.Glosa 
&Scoped-define ENABLED-TABLES CcbCDocu
&Scoped-define FIRST-ENABLED-TABLE CcbCDocu
&Scoped-Define DISPLAYED-FIELDS CcbCDocu.NroDoc CcbCDocu.FchDoc ~
CcbCDocu.CodRef CcbCDocu.NroRef CcbCDocu.CodAnt CcbCDocu.CodMon ~
CcbCDocu.CodCli CcbCDocu.NomCli CcbCDocu.RucCli CcbCDocu.DirCli ~
CcbCDocu.TpoCmb CcbCDocu.Glosa CcbCDocu.NroOrd 
&Scoped-define DISPLAYED-TABLES CcbCDocu
&Scoped-define FIRST-DISPLAYED-TABLE CcbCDocu
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Estado 

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
DEFINE VARIABLE FILL-IN-Estado AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 12 FGCOLOR 15 FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CcbCDocu.NroDoc AT ROW 1 COL 12 COLON-ALIGNED
          LABEL "N�mero" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          FONT 0
     FILL-IN-Estado AT ROW 1 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     CcbCDocu.FchDoc AT ROW 1 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.CodRef AT ROW 1.81 COL 12 COLON-ALIGNED WIDGET-ID 16
          LABEL "Referencia" FORMAT "x(3)"
          VIEW-AS COMBO-BOX INNER-LINES 2
          LIST-ITEMS "BOL","FAC" 
          DROP-DOWN-LIST
          SIZE 7 BY 1
     CcbCDocu.NroRef AT ROW 1.81 COL 20 COLON-ALIGNED NO-LABEL FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .81
          FONT 0
     CcbCDocu.CodAnt AT ROW 1.81 COL 38 COLON-ALIGNED WIDGET-ID 14
          LABEL "DNI" FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     CcbCDocu.CodMon AT ROW 1.81 COL 88 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Soles", 1,
"D�lares", 2
          SIZE 10 BY 1.54
     CcbCDocu.CodCli AT ROW 2.62 COL 12 COLON-ALIGNED FORMAT "x(11)"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     CcbCDocu.NomCli AT ROW 2.62 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 48 BY .81
     CcbCDocu.RucCli AT ROW 3.42 COL 12 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     CcbCDocu.DirCli AT ROW 4.23 COL 12 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
     CcbCDocu.TpoCmb AT ROW 4.23 COL 86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
     CcbCDocu.Glosa AT ROW 5.04 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
     CcbCDocu.NroOrd AT ROW 5.04 COL 86 COLON-ALIGNED
          LABEL "No.Devoluci�n" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     "Moneda:" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 2.08 COL 81 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.CcbCDocu
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: DETA T "SHARED" ? INTEGRAL CcbDDocu
      TABLE: DOCU T "?" ? INTEGRAL CcbCDocu
      TABLE: T-FELogErrores T "?" NO-UNDO INTEGRAL FELogErrores
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 6.96
         WIDTH              = 105.29.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN CcbCDocu.CodAnt IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.CodCli IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR COMBO-BOX CcbCDocu.CodRef IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.FchDoc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.NroDoc IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN CcbCDocu.NroOrd IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN CcbCDocu.NroRef IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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

&Scoped-define SELF-NAME CcbCDocu.CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CcbCDocu.CodCli V-table-Win
ON LEAVE OF CcbCDocu.CodCli IN FRAME F-Main /* Cliente */
DO:
    IF INPUT {&self-name} = '' THEN RETURN.
    IF INPUT {&self-name} = FacCfgGn.CliVar 
        OR CcbCDocu.CodRef:SCREEN-VALUE = "BOL"
        THEN
        ASSIGN
        CcbCDocu.CodAnt:SENSITIVE = YES
        CcbCDocu.DirCli:SENSITIVE = YES
        CcbCDocu.NomCli:SENSITIVE = YES.
    ELSE 
        ASSIGN
        CcbCDocu.CodAnt:SENSITIVE = NO
        CcbCDocu.DirCli:SENSITIVE = NO
        CcbCDocu.NomCli:SENSITIVE = NO.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CcbCDocu.NroRef
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CcbCDocu.NroRef V-table-Win
ON LEAVE OF CcbCDocu.NroRef IN FRAME F-Main /* Numero */
DO:
  IF SELF:SCREEN-VALUE = '' THEN RETURN.
  DEF VAR pError AS CHAR NO-UNDO.
  RUN vta2/p-inicio-le (s-codcia,
                   CcbCDocu.CodRef:SCREEN-VALUE,
                   CcbCDocu.NroRef:SCREEN-VALUE,
                   INPUT-OUTPUT TABLE DOCU,
                   INPUT-OUTPUT TABLE DETA,
                   OUTPUT pError).
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      MESSAGE pError VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  FIND FIRST DOCU NO-LOCK NO-ERROR.
  DISPLAY 
      DOCU.CodCli @ CcbCDocu.CodCli
      DOCU.NomCli @ Ccbcdocu.NomCli
      DOCU.RucCli @ Ccbcdocu.RucCli
      DOCU.DirCli @ Ccbcdocu.DirCli
      DOCU.CodAnt @ Ccbcdocu.CodAnt
      WITH FRAME {&FRAME-NAME}.
  ASSIGN
      S-PORDTO = DOCU.PorDto
      S-PORIGV = DOCU.PorIgv.
  RUN Procesa-Handle IN lh_Handle ('Browse').
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
  {src/adm/template/row-list.i "CcbCDocu"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "CcbCDocu"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Detalle V-table-Win 
PROCEDURE Genera-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       NO se puede modificar un documento
------------------------------------------------------------------------------*/

DEF VAR i AS INT NO-UNDO.
i = 1.
FOR EACH DETA ON STOP UNDO, RETURN ERROR ON ERROR UNDO, RETURN ERROR BY DETA.NroItm:
    CREATE CcbDDocu.
    ASSIGN 
        CcbDDocu.NroItm = i
        CcbDDocu.CodCia = CcbCDocu.CodCia 
        CcbDDocu.Coddiv = CcbCDocu.Coddiv 
        CcbDDocu.CodDoc = CcbCDocu.CodDoc 
        CcbDDocu.NroDoc = CcbCDocu.NroDoc
        CcbDDocu.CodMat = DETA.codmat 
        CcbDDocu.PreUni = DETA.PreUni 
        CcbDDocu.CanDes = DETA.CanDes 
        CcbDDocu.Factor = DETA.Factor 
        CcbDDocu.ImpIsc = DETA.ImpIsc
        CcbDDocu.ImpIgv = DETA.ImpIgv 
        CcbDDocu.ImpLin = DETA.ImpLin
        CcbDDocu.AftIgv = DETA.AftIgv
        CcbDDocu.AftIsc = DETA.AftIsc
        CcbDDocu.UndVta = DETA.UndVta
        CcbDDocu.ImpCto = DETA.ImpCto.
    i = i + 1.
END.
DEF VAR f-Des AS DEC NO-UNDO.
DEF VAR f-Dev AS DEC NO-UNDO.
DEF VAR c-Sit AS CHAR NO-UNDO.

FOR EACH CcbDDocu OF CcbCDocu NO-LOCK:
    F-Des = F-Des + CcbDDocu.CanDes.
    F-Dev = F-Dev + CcbDDocu.CanDev. 
END.
IF F-Dev > 0 THEN C-SIT = "P".
IF F-Des = F-Dev THEN C-SIT = "D".
ASSIGN 
    CcbCDocu.FlgCon = C-SIT.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Totales V-table-Win 
PROCEDURE Graba-Totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*     {vta2/graba-totales-factura-cred.i} */

    {vta/graba-totales-abono.i}

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
FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA 
     AND FacCorre.CodDoc = S-CODDOC 
     AND FacCorre.NroSer = s-NroSer
     NO-LOCK NO-ERROR.
IF FacCorre.FlgEst = NO THEN DO:
     MESSAGE 'Esta serie est� bloqueada para hacer movimientos' VIEW-AS ALERT-BOX WARNING.
     RETURN 'ADM-ERROR'.
END.
  
/* Dispatch standard ADM method.                             */
RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  
/* Code placed here will execute AFTER standard behavior.    */
RUN Numero-de-Documento(NO).
DO WITH FRAME {&FRAME-NAME}:
    DISPLAY 
        TODAY @ CcbCDocu.FchDoc
        FacCfgGn.Tpocmb[1] @ CcbCDocu.TpoCmb
        STRING(I-NroSer, ENTRY(1, x-Formato, '-')) +
        STRING(I-NroDoc, ENTRY(2, x-Formato, '-')) @ CcbCDocu.NroDoc.
END.
RUN Procesa-Handle IN lh_Handle ('Pagina2').
RUN Procesa-Handle IN lh_Handle ('Browse').

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {vtagn/i-faccorre-01.i &Codigo = s-coddoc &Serie = s-nroser}

  FIND FIRST DOCU NO-LOCK.
  BUFFER-COPY DOCU
      EXCEPT DOCU.CodRef DOCU.NroRef DOCU.Glosa DOCU.NroOrd DOCU.CodAnt
      TO CcbCDocu
      ASSIGN 
      CcbCDocu.CodCia = S-CODCIA
      CcbCDocu.CodDiv = S-CODDIV
      CcbCDocu.CodDoc = S-CODDOC
      CcbCDocu.NroDoc = STRING(FacCorre.NroSer, ENTRY(1, x-Formato, '-')) +
                        STRING(FacCorre.Correlativo, ENTRY(2, x-Formato, '-'))
      CcbCDocu.FchDoc = TODAY
      CcbCDocu.FchVto = ADD-INTERVAL (TODAY, 1, 'years')
      CcbCDocu.FlgEst = "P"
      CcbCDocu.TpoCmb = FacCfgGn.TpoCmb[1]
      CcbCDocu.CndCre = 'D'
      CcbCDocu.Tipo   = "MOSTRADOR"
      CcbCDocu.CodCaja= s-CodTer
      CcbCDocu.usuario = S-USER-ID
      CcbCDocu.SdoAct = DOCU.ImpTot
      CcbCDocu.ImpTot2 = 0
      CcbCDocu.ImpDto2 = 0
      CcbCDocu.CodMov = 09     /* INGRESO POR DEVOLUCION DEL CLIENTE */
      CcbCDocu.CodAlm = s-CodAlm       /* OJO */
      CcbCDocu.acubon[10] = 0      /* Impuesto Bolsas Plasticas */
      CcbCDocu.dcto_otros_mot = ""
      CcbCDocu.dcto_otros_factor = 0
      CcbCDocu.dcto_otros_vv = 0
      CcbCDocu.dcto_otros_pv = 0
      .
  ASSIGN
      Ccbcdocu.TpoFac = s-TpoFac.      /* LF */
      
  ASSIGN
      FacCorre.Correlativo = FacCorre.Correlativo + 1.
  /* ACTUALIZAR EL CENTRO DE COSTO 22.07.04 CY */
  FIND GN-VEN WHERE gn-ven.codcia = s-codcia
      AND gn-ven.codven = DOCU.codven
      NO-LOCK NO-ERROR.
  IF AVAILABLE GN-VEN THEN ccbcdocu.cco = gn-ven.cco.
   
  RUN Genera-Detalle NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE 'NO se pudo generar el detalle' VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN 'ADM-ERROR'.
  END.

  RUN vta2/ing-devo-utilex (ROWID(Ccbcdocu)).
  IF RETURN-VALUE = "ADM-ERROR" THEN UNDO, RETURN "ADM-ERROR".
  
  RUN Graba-Totales.

  /* ****************************** */
  /* Ic - 16Nov2021 - Importes Arimetica de SUNAT */
  /* ****************************** */
  DEF VAR hProc AS HANDLE NO-UNDO.
  RUN sunat/sunat-calculo-importes PERSISTENT SET hProc.
  RUN tabla-ccbcdocu IN hProc (INPUT Ccbcdocu.CodDiv,
                               INPUT Ccbcdocu.CodDoc,
                               INPUT Ccbcdocu.NroDoc,
                               OUTPUT pMensaje).
  DELETE PROCEDURE hProc.

  IF RETURN-VALUE = "ADM-ERROR" THEN UNDO, RETURN 'ADM-ERROR'.
  /* ****************************** */

  /* RHC SUNAT: Generaci�n del Archivo FELogComprobantes s� o s� */
/*   RUN sunat\progress-to-ppll-v2 ( INPUT ROWID(Ccbcdocu), OUTPUT pMensaje ). */
  RUN sunat\progress-to-ppll-v3( INPUT Ccbcdocu.coddiv,
                                 INPUT Ccbcdocu.coddoc,
                                 INPUT Ccbcdocu.nrodoc,
                                 INPUT-OUTPUT TABLE T-FELogErrores,
                                 OUTPUT pMensaje ).
  IF RETURN-VALUE = "ADM-ERROR" THEN UNDO, RETURN 'ADM-ERROR'.
  IF RETURN-VALUE = 'ERROR-EPOS' THEN DO:
      /* NO se pudo confirmar el comprobante en el e-pos */
      /* Se procede a ANULAR el comprobante              */
      pMensaje = pMensaje + CHR(10) +
          "Se procede a anular el comprobante: " + Ccbcdocu.coddoc + " " + Ccbcdocu.nrodoc + CHR(10) +
          "Salga del sistema, vuelva a entra y vuelva a intentarlo".
      ASSIGN
          CcbCDocu.FchAnu = TODAY
          CcbCDocu.FlgEst = "A"
          CcbCDocu.SdoAct = 0
          CcbCDocu.UsuAnu = s-user-id.
      /* DESCARGA ALMACENES */
      RUN vta2/anula-ing-devo-utilex (ROWID(CcbCDocu)).
      /* ****************************************** */
      RETURN.
  END.
  /* *********************************************************** */
  RUN Procesa-Handle IN lh_Handle ('Pagina1'). 
  RUN Procesa-Handle IN lh_Handle ('browse'). 

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
  RUN Procesa-Handle IN lh_Handle ('Pagina1').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF CcbCDocu.FlgEst = "A" THEN DO:
      MESSAGE 'El documento se encuentra anulado...' VIEW-AS ALERT-BOX.
      RETURN 'ADM-ERROR'.
  END.
  IF CcbCDocu.SdoAct < CcbCDocu.ImpTot  THEN DO:
      MESSAGE 'El documento registra amortizaciones...' VIEW-AS ALERT-BOX.
      RETURN 'ADM-ERROR'.
  END.
  IF CcbCDocu.CndCre <> 'D' THEN DO:
      MESSAGE 'El documento no corresponde a devolucion de mercaderia' VIEW-AS ALERT-BOX.
      RETURN 'ADM-ERROR'.
  END.
  /* consistencia de la fecha del cierre del sistema */
  IF s-user-id <> "ADMIN" THEN DO:
      /* ********************************************* */
      /* Inicio de actividades facturaci�n electr�nica */
      /* ********************************************* */
      IF s-Sunat-Activo = YES THEN DO:
          MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
          RETURN 'ADM-ERROR'.
      END.
      /* ********************************************* */
      DEF VAR dFchCie AS DATE.
      RUN gn/fecha-de-cierre (OUTPUT dFchCie).
      IF ccbcdocu.fchdoc <= dFchCie THEN DO:
          MESSAGE 'NO se puede anular/modificar ningun documento antes del' (dFchCie + 1)
              VIEW-AS ALERT-BOX WARNING.
          RETURN 'ADM-ERROR'.
      END.
      /* RHC CONSISTENCIA SOLO PARA TIENDAS UTILEX */
      FIND gn-divi WHERE gn-divi.codcia = s-codcia
          AND gn-divi.coddiv = s-coddiv NO-LOCK NO-ERROR.
      IF AVAILABLE gn-divi 
          AND GN-DIVI.CanalVenta = "MIN" 
          AND Ccbcdocu.fchdoc < TODAY THEN DO:
          MESSAGE 'Solo se pueden anular documentos del d�a' VIEW-AS ALERT-BOX ERROR.
          RETURN 'ADM-ERROR'.
      END.
      /* fin de consistencia */
      {adm/i-DocPssw.i ccbcdocu.CodCia ccbcdocu.CodDoc ""DEL""}
  END.

  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
      /* Motivo de anulacion */
      DEF VAR cReturnValue AS CHAR NO-UNDO.
      RUN ccb/d-motanu (ccbcdocu.codcia, ccbcdocu.coddoc, ccbcdocu.nrodoc, s-user-id, OUTPUT cReturnValue).
      IF cReturnValue = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      /* ******************* */
      FIND CURRENT Ccbcdocu EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE Ccbcdocu THEN UNDO, RETURN 'ADM-ERROR'.
      ASSIGN 
          CcbCDocu.FlgEst = "A"
          CcbCDocu.SdoAct = 0 
          CcbCDocu.UsuAnu = S-USER-ID
          CcbCDocu.FchAnu = TODAY.

       /* DESCARGA ALMACENES */
       RUN vta2/anula-ing-devo-utilex (ROWID(CcbCDocu)).
       IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

       FIND CURRENT Ccbcdocu NO-LOCK NO-ERROR.
       IF AVAILABLE(Almcmov) THEN RELEASE Almcmov.
       IF AVAILABLE(Almdmov) THEN RELEASE Almdmov.
  END.
  RUN Procesa-Handle IN lh_Handle ('Browse').
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

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
  IF NOT AVAILABLE Ccbcdocu THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      FILL-IN-Estado = Ccbcdocu.flgest.
      CASE Ccbcdocu.flgest:
          WHEN "P" THEN FILL-IN-Estado = "PENDIENTE".
          WHEN "C" THEN FILL-IN-Estado = "CERRADO".
          WHEN "A" THEN FILL-IN-Estado = "ANULADO".
      END CASE.
      DISPLAY FILL-IN-Estado.
  END.

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
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          CcbCDocu.CodCli:SENSITIVE = NO
          CcbCDocu.CodMon:SENSITIVE = NO
          CcbCDocu.FchDoc:SENSITIVE = NO
          CcbCDocu.NomCli:SENSITIVE = NO
          CcbCDocu.DirCli:SENSITIVE = NO
          CcbCDocu.NroDoc:SENSITIVE = NO
          CcbCDocu.NroOrd:SENSITIVE = NO
          CcbCDocu.RucCli:SENSITIVE = NO
          /*CcbCDocu.CodAnt:SENSITIVE = NO*/
          CcbCDocu.TpoCmb:SENSITIVE = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-imprime V-table-Win 
PROCEDURE local-imprime :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'imprime':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF Ccbcdocu.FlgEst = "A" THEN RETURN.

  DEFINE VAR x-version AS CHAR.
  DEFINE VAR x-formato-tck AS LOG.
  DEFINE VAR x-Imprime-directo AS LOG.
  DEFINE VAR x-nombre-impresora AS CHAR.
  DEFINE VAR hPrinter AS HANDLE NO-UNDO.

  RUN sunat\r-print-electronic-doc-sunat PERSISTENT SET hPrinter.

  x-formato-tck = NO.        /* YES : Formato Ticket,  NO : Formato A4 */
  x-imprime-directo = NO.
  x-nombre-impresora = "".

  /* 1-8-23 Reimpresi�n: L�mite de reimpresiones */
  DEF VAR iImpresionesExistentes AS INTE INIT 0 NO-UNDO.

  CASE TRUE:
      WHEN CAN-FIND(FIRST Invoices_Printed WHERE Invoices_Printed.CodCia = s-codcia AND
                    Invoices_Printed.CodDoc = Ccbcdocu.coddoc AND
                    Invoices_Printed.NroDoc = Ccbcdocu.nrodoc AND 
                    LOOKUP(Invoices_Printed.Version_Printed,"L,A") > 0 NO-LOCK) 
          THEN DO:
          iImpresionesExistentes = DYNAMIC-FUNCTION('PRINT_fget-count-invoice-printed' IN hPrinter, 
                                                    Ccbcdocu.coddoc, 
                                                    Ccbcdocu.nrodoc).
          IF iImpresionesExistentes > 0 THEN DO:
              FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia AND
                  VtaTabla.Tabla = 'CFG_PRINT_INVOICE' AND
                  VtaTabla.Llave_c1 = 'PARAMETER' AND
                  VtaTabla.Llave_c2 = 'RE-IMPRESION'
                  NO-LOCK NO-ERROR.
              IF AVAILABLE VtaTabla AND iImpresionesExistentes >= VtaTabla.Valor[1] THEN DO:
                  MESSAGE 'No se pueden hacer m�s reimpresiones' SKIP(1)
                      'El l�mite de reimpresiones es:' STRING(VtaTabla.Valor[1], '>9')
                      VIEW-AS ALERT-BOX WARNING.
                  DELETE PROCEDURE hPrinter.
                  RETURN.
              END.
          END.
          x-version = 'R'.
          {gn/i-print-electronic-doc-sunat.i}
      END.
      OTHERWISE DO:
          x-version = 'L'.
          {gn/i-print-electronic-doc-sunat.i}
      END.
  END CASE.

  DELETE PROCEDURE hPrinter.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
  
  /* Dispatch standard ADM method.                             */
  pMensaje = "".
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  IF pMensaje <> "" THEN MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE(FacCorre) THEN RELEASE FacCorre.
  IF AVAILABLE(Ccbddocu) THEN RELEASE Ccbddocu.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Numero-de-Documento V-table-Win 
PROCEDURE Numero-de-Documento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER L-INCREMENTA AS LOGICAL.
  
  IF L-INCREMENTA THEN 
     FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
                    AND  FacCorre.CodDoc = S-CODDOC 
                    AND  FacCorre.CodDiv = S-CODDIV 
                    AND  FacCorre.NroSer = s-NroSer
                   EXCLUSIVE-LOCK NO-ERROR.
  ELSE
     FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA 
                    AND  FacCorre.CodDoc = S-CODDOC 
                    AND  FacCorre.CodDiv = S-CODDIV 
                    AND  FacCorre.NroSer = s-NroSer
                   NO-LOCK NO-ERROR.
  IF AVAILABLE FacCorre THEN DO:      
     ASSIGN I-NroDoc = FacCorre.Correlativo.
     IF L-INCREMENTA THEN ASSIGN FacCorre.Correlativo = FacCorre.Correlativo + 1.
  END.
  RELEASE FacCorre.

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
  {src/adm/template/snd-list.i "CcbCDocu"}

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

  IF p-state = 'update-begin':U THEN DO:
     RUN valida-update.
     IF RETURN-VALUE = "ADM-ERROR" THEN RETURN.
  END.

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

DO WITH FRAME {&FRAME-NAME} :
    IF CcbCDocu.NroRef:SCREEN-VALUE = '' THEN DO:
        MESSAGE 'Deebe ingresar el n�mero de referencia' VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO CcbCDocu.NroRef.
        RETURN 'ADM-ERROR'.
    END.
    IF CcbCDocu.CodAnt:SENSITIVE = YES AND LENGTH(INPUT CcbCDocu.CodAnt) <> 8
        THEN DO:
        MESSAGE 'Debe ingresar el DNI' VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO CcbCDocu.CodAnt.
        RETURN 'ADM-ERROR'.
    END.

END.

RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update V-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Rutina de validacion en caso de modificacion
  Parameters:  Regresar "ADM-ERROR" si no se quiere modificar
  Notes:       
------------------------------------------------------------------------------*/

MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX WARNING.
RETURN 'ADM-ERROR'. /* No se puede realizar modificaciones */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

