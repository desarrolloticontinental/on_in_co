&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-CPEDI FOR FacCPedi.
DEFINE BUFFER GUIAS FOR CcbCDocu.



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
DEF SHARED VAR s-permiso-anulacion AS LOG.

/* Shared Variable Definitions ---                                       */

DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.
DEFINE SHARED VARIABLE S-CODCIA   AS INTEGER.
DEFINE SHARED VARIABLE S-TPOFAC   AS CHAR.
DEFINE SHARED VARIABLE CL-CODCIA  AS INTEGER.
DEFINE SHARED VARIABLE S-NOMCIA   AS CHAR.
DEFINE SHARED VARIABLE S-USER-ID  AS CHAR.
DEFINE SHARED VARIABLE S-CODDOC   AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV   AS CHAR.
DEFINE SHARED VARIABLE S-NROSER  AS INTEGER.
DEFINE SHARED VARIABLE S-IMPFLE  AS DECIMAL.

DEFINE SHARED VARIABLE s-Sunat-Activo AS LOG.

DEFINE SHARED TEMP-TABLE DETA LIKE CcbDDocu.
DEFINE TEMP-TABLE T-DETA LIKE Ccbddocu.     /* PARA EL CONTROL DE PRECIOS AL SELCCIONAR LAS G/R */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE s-aplic-fact-ade AS LOG NO-UNDO.    /* Aplicacion de la factura adelantada */

DEFINE VARIABLE I              AS INTEGER   NO-UNDO.
DEFINE VARIABLE C-NRODOC       AS CHAR      NO-UNDO.
DEFINE VARIABLE C-CODPED       AS CHAR      NO-UNDO.
DEFINE VARIABLE C-NROPED       AS CHAR      NO-UNDO.
DEFINE VARIABLE C-NROGUI       AS CHAR      NO-UNDO.
DEFINE VARIABLE I-NROSER       AS INTEGER   NO-UNDO.
DEFINE VARIABLE I-NRODOC       AS INTEGER   NO-UNDO.
DEFINE VARIABLE I-ListPr       AS INTEGER   NO-UNDO.
DEFINE VARIABLE S-PRINTER-NAME AS CHARACTER NO-UNDO.
DEFINE VARIABLE S-CODCLI       AS CHARACTER NO-UNDO. 
/*DEFINE VARIABLE S-CODALM       AS CHAR      NO-UNDO.*/
DEFINE VARIABLE S-CODMOV       AS INTEGER   NO-UNDO. 
DEFINE VARIABLE I-CODMON       AS INTEGER   NO-UNDO. 
DEFINE VARIABLE R-ROWID        AS ROWID     NO-UNDO. 
DEFINE VARIABLE C-CODVEN       AS CHAR      NO-UNDO.
DEFINE VARIABLE D-FCHVTO       AS DATE      NO-UNDO.

DEFINE BUFFER B-CDOCU FOR CcbCDocu.
DEFINE BUFFER B-CCDOCU FOR CcbCDocu.

FIND FacDocum WHERE FacDocum.CodCia = S-CODCIA AND
     FacDocum.CodDoc = S-CODDOC NO-LOCK NO-ERROR.
IF AVAILABLE FacDocum THEN S-CODMOV = FacDocum.CodMov.

FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
     FacCorre.CodDoc = S-CODDOC AND
     FacCorre.CodDiv = S-CODDIV AND
     FacCorre.NroSer = S-NROSER NO-LOCK NO-ERROR.
IF AVAILABLE FacCorre THEN 
   ASSIGN /*S-CodAlm = FacCorre.CodAlm */
          I-NroSer = FacCorre.NroSer.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DEFINE TEMP-TABLE T-CPEDM LIKE FacCPedi.
DEFINE TEMP-TABLE T-DPEDM LIKE FacDPedi.

DEFINE VARIABLE RPTA        AS CHARACTER NO-UNDO.

DEFINE VARIABLE x-cto1 AS DECI INIT 0.
DEFINE VARIABLE x-cto2 AS DECI INIT 0.

DEFINE BUFFER F-CDOCU FOR CcbCDocu.
DEFINE BUFFER F-DDOCU FOR CcbDDocu.

DEF VAR s-FechaI AS DATETIME NO-UNDO.
DEF VAR s-FechaT AS DATETIME NO-UNDO.

s-FechaI = DATETIME(TODAY, MTIME).

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
&Scoped-Define ENABLED-FIELDS CcbCDocu.usuario CcbCDocu.NroCard ~
CcbCDocu.Glosa 
&Scoped-define ENABLED-TABLES CcbCDocu
&Scoped-define FIRST-ENABLED-TABLE CcbCDocu
&Scoped-Define DISPLAYED-FIELDS CcbCDocu.NroDoc CcbCDocu.FchDoc ~
CcbCDocu.CodCli CcbCDocu.RucCli CcbCDocu.CodAnt CcbCDocu.FchVto ~
CcbCDocu.NomCli CcbCDocu.usuario CcbCDocu.DirCli CcbCDocu.NroOrd ~
CcbCDocu.CodRef CcbCDocu.NroRef CcbCDocu.CodMon CcbCDocu.NroCard ~
CcbCDocu.TpoCmb CcbCDocu.FmaPgo CcbCDocu.CodCta CcbCDocu.Glosa ~
CcbCDocu.DivOri 
&Scoped-define DISPLAYED-TABLES CcbCDocu
&Scoped-define FIRST-DISPLAYED-TABLE CcbCDocu
&Scoped-Define DISPLAYED-OBJECTS F-Estado F-Nomtar F-CndVta ~
FILL-IN-Concepto FILL-IN-DivOri 

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
DEFINE VARIABLE F-CndVta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE F-Estado AS CHARACTER FORMAT "X(256)":U 
     LABEL "ESTADO" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 15 FGCOLOR 12 FONT 0 NO-UNDO.

DEFINE VARIABLE F-Nomtar AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-Concepto AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-DivOri AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .81
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CcbCDocu.NroDoc AT ROW 1 COL 16 COLON-ALIGNED FORMAT "XXX-XXXXXXXXX"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          FONT 1
     F-Estado AT ROW 1 COL 40 COLON-ALIGNED
     CcbCDocu.FchDoc AT ROW 1 COL 99 COLON-ALIGNED
          LABEL "Fecha de Emisi�n"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.CodCli AT ROW 1.81 COL 16 COLON-ALIGNED FORMAT "x(11)"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     CcbCDocu.RucCli AT ROW 1.81 COL 40 COLON-ALIGNED FORMAT "x(11)"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     CcbCDocu.CodAnt AT ROW 1.81 COL 61 COLON-ALIGNED WIDGET-ID 16
          LABEL "DNI" FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
     CcbCDocu.FchVto AT ROW 1.81 COL 99 COLON-ALIGNED
          LABEL "Fecha Vencimiento"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.NomCli AT ROW 2.62 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
     CcbCDocu.usuario AT ROW 2.62 COL 99 COLON-ALIGNED WIDGET-ID 18
          LABEL "Generado por"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.DirCli AT ROW 3.42 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
     CcbCDocu.NroOrd AT ROW 3.42 COL 99 COLON-ALIGNED
          LABEL "No.Devoluci�n" FORMAT "XXX-XXXXXX"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
     CcbCDocu.CodRef AT ROW 4.23 COL 16 COLON-ALIGNED WIDGET-ID 102
          LABEL "Referencia"
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     CcbCDocu.NroRef AT ROW 4.23 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 104
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     CcbCDocu.CodMon AT ROW 4.23 COL 101.14 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "S/.", 1,
"US$", 2
          SIZE 10.72 BY .81
     CcbCDocu.NroCard AT ROW 5.04 COL 16 COLON-ALIGNED WIDGET-ID 28
          LABEL "Tarjeta"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     F-Nomtar AT ROW 5.04 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     CcbCDocu.TpoCmb AT ROW 5.04 COL 99 COLON-ALIGNED
          LABEL "T/ Cambio"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.FmaPgo AT ROW 5.85 COL 16 COLON-ALIGNED
          LABEL "Condici�n de Venta"
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
     F-CndVta AT ROW 5.85 COL 22 COLON-ALIGNED NO-LABEL
     CcbCDocu.CodCta AT ROW 6.65 COL 16 COLON-ALIGNED WIDGET-ID 50
          LABEL "Concepto"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     FILL-IN-Concepto AT ROW 6.65 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     CcbCDocu.Glosa AT ROW 7.46 COL 16 COLON-ALIGNED
          LABEL "Glosa"
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
     CcbCDocu.DivOri AT ROW 8.27 COL 16 COLON-ALIGNED WIDGET-ID 94
          LABEL "Divisi�n Origen"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 8 FGCOLOR 0 
     FILL-IN-DivOri AT ROW 8.27 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 96
     "Moneda:" VIEW-AS TEXT
          SIZE 6.43 BY .5 AT ROW 4.5 COL 94
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
      TABLE: B-CPEDI B "?" ? INTEGRAL FacCPedi
      TABLE: GUIAS B "?" ? INTEGRAL CcbCDocu
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
         HEIGHT             = 9.96
         WIDTH              = 118.72.
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
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:PRIVATE-DATA     = 
                "sdfsdfsdfsdfsdf".

/* SETTINGS FOR FILL-IN CcbCDocu.CodAnt IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN CcbCDocu.CodCli IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.CodCta IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR RADIO-SET CcbCDocu.CodMon IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.CodRef IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN CcbCDocu.DirCli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.DivOri IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN F-CndVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Nomtar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.FchDoc IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN CcbCDocu.FchVto IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FILL-IN-Concepto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DivOri IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.FmaPgo IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN CcbCDocu.Glosa IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.NomCli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.NroCard IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.NroDoc IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.NroOrd IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN CcbCDocu.NroRef IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.RucCli IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.TpoCmb IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN CcbCDocu.usuario IN FRAME F-Main
   EXP-LABEL                                                            */
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
   IF SELF:SCREEN-VALUE = "" THEN RETURN.
   FIND gn-clie WHERE gn-clie.CodCia = cl-codcia
                 AND  gn-clie.CodCli = CcbCDocu.CodCli:SCREEN-VALUE
                NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gn-clie  THEN DO:
      MESSAGE "Codigo de cliente no existe" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   IF gn-clie.FlgSit = "I" THEN DO:
      MESSAGE "Cliente esta Inactivo" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO CcbCDocu.CodCli.
      RETURN NO-APPLY.
   END.

   DISPLAY gn-clie.NomCli @ CcbCDocu.NomCli
           gn-clie.Ruc    @ CcbCDocu.RucCli
           gn-clie.DirCli @ CcbCDocu.DirCli WITH FRAME {&FRAME-NAME}.
    /* RHC agregamos el distrito */
    FIND Tabdistr WHERE Tabdistr.CodDepto = gn-clie.CodDept
      AND Tabdistr.Codprovi = gn-clie.codprov 
      AND Tabdistr.Coddistr = gn-clie.CodDist NO-LOCK NO-ERROR.
    IF AVAILABLE Tabdistr 
    THEN CcbCDocu.DirCli:SCREEN-VALUE = TRIM(CcbCDocu.DirCli:SCREEN-VALUE) + ' - ' +
                                      TabDistr.NomDistr.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CcbCDocu.CodCta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CcbCDocu.CodCta V-table-Win
ON LEAVE OF CcbCDocu.CodCta IN FRAME F-Main /* Concepto */
DO:
    IF SELF:SCREEN-VALUE = '' THEN RETURN.
    FIND CcbTabla WHERE CcbTabla.CodCia = s-codcia 
        AND CcbTabla.Tabla  = s-CodDoc
        AND CcbTabla.Codigo = INPUT {&self-name}
        NO-LOCK NO-ERROR.
    IF AVAILABLE CcbTabla THEN FILL-IN-Concepto:SCREEN-VALUE = CcbTabla.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CcbCDocu.CodCta V-table-Win
ON LEFT-MOUSE-DBLCLICK OF CcbCDocu.CodCta IN FRAME F-Main /* Concepto */
DO:
  ASSIGN
      input-var-1 = s-CodDoc
      input-var-2 = ''
      input-var-3 = ''
      output-var-1 = ?.
  RUN LKUP\C-ABOCAR-3 ("Conceptos").
  IF output-var-1 <> ? THEN SELF:SCREEN-VALUE = output-var-2.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Deta V-table-Win 
PROCEDURE Actualiza-Deta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       POR AHORA NO SE PUEDE MODIFICAR, SOLO CREAR
------------------------------------------------------------------------------*/
FOR EACH DETA:
    DELETE DETA.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Guias V-table-Win 
PROCEDURE Actualiza-Guias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
    FIND B-CDOCU WHERE B-CDOCU.CodCia = Ccbcdocu.codcia
        AND  B-CDOCU.CodDoc = Ccbcdocu.codref
        AND  B-CDOCU.NroDoc = Ccbcdocu.nroref
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE B-CDOCU THEN DO :
        ASSIGN 
            B-CDOCU.FlgEst = "F"
            B-CDOCU.CodRef = CcbCDocu.CodDoc
            B-CDOCU.NroRef = CcbCDocu.NroDoc
            B-CDOCU.FchCan = CcbCDocu.FchDoc
            B-CDOCU.SdoAct = 0.
    END.
    RELEASE B-CDOCU.
END.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Lista-Escolar V-table-Win 
PROCEDURE Actualiza-Lista-Escolar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pTipo AS INT.
  
  FIND PedidoC WHERE PedidoC.NroPed = CcbCDocu.NroPed
    EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE PedidoC
  THEN IF pTipo = 1
        THEN ASSIGN
                    PedidoC.Estado = 'F'
                    PedidoC.FecFac = TODAY.
        ELSE ASSIGN
                    PedidoC.Estado = ''
                    PedidoC.FecFac = ?.
  RELEASE PedidoC.
                      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Anexo-Kits V-table-Win 
PROCEDURE Anexo-Kits :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN vta2/ranexofackits (ROWID(CcbCDocu)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Detalle V-table-Win 
PROCEDURE Borra-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH CcbDDocu WHERE CcbDDocu.CodCia = CcbCDocu.CodCia 
    AND CcbDDocu.CodDoc = CcbCDocu.CodDoc 
    AND CcbDDocu.NroDoc = CcbCDocu.NroDoc 
    ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
         
    DELETE CcbDDocu.
         
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crea-temporal V-table-Win 
PROCEDURE Crea-temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
  DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
  FOR EACH T-CPEDM:
      DELETE T-CPEDM.
  END.
  FOR EACH T-DPEDM:
      DELETE T-DPEDM.
  END.
  DEFINE VARIABLE L-NewPed AS LOGICAL INIT YES NO-UNDO.
  DEFINE VARIABLE I-NPED   AS INTEGER INIT 0.
  DEFINE VARIABLE I-NItem  AS INTEGER INIT 0.
/*  FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.*/
  FOR EACH FacDPedi OF FacCPedi 
           BREAK BY FacDPedi.CodCia
                 BY FacDPedi.AlmDes : 
      IF FIRST-OF(FacDPedi.AlmDes) OR L-NewPed THEN DO:
         I-NPED = I-NPED + 1.
         CREATE T-CPEDM.
         ASSIGN T-CPEDM.CodCia   = FacCPedi.CodCia 
                T-CPEDM.CodDiv   = FacCPedi.CodDiv
                T-CPEDM.CodAlm   = FacDPedi.AlmDes
                T-CPEDM.CodDoc   = FacCPedi.CodDoc
                T-CPEDM.NroPed   = SUBSTRING(FacCPedi.NroPed,1,3) + STRING(I-NPED,"999999").
/*                T-CPEDM.Cmpbnte  = FacCPedi.Cmpbnte 
 *                 T-CPEDM.CodCli   = FacCPedi.CodCli 
 *                 T-CPEDM.CodMon   = FacCPedi.CodMon 
 *                 T-CPEDM.CodTrans = FacCPedi.CodTrans 
 *                 T-CPEDM.CodVen   = FacCPedi.CodVen 
 *                 T-CPEDM.DirCli   = FacCPedi.DirCli 
 *                 T-CPEDM.FchPed   = FacCPedi.FchPed 
 *                 T-CPEDM.FlgEst   = FacCPedi.FlgEst 
 *                 T-CPEDM.FmaPgo   = FacCPedi.FmaPgo 
 *                 T-CPEDM.Glosa    = FacCPedi.Glosa 
 *                 T-CPEDM.Hora     = FacCPedi.Hora 
 *                 T-CPEDM.LugEnt   = FacCPedi.LugEnt
 *                 T-CPEDM.NomCli   = FacCPedi.NomCli 
 *                 T-CPEDM.PorDto   = FacCPedi.PorDto 
 *                 T-CPEDM.PorIgv   = FacCPedi.PorIgv
 *                 T-CPEDM.RucCli   = FacCPedi.RucCli 
 *                 T-CPEDM.TipVta   = FacCPedi.TipVta 
 *                 T-CPEDM.TpoCmb   = FacCPedi.TpoCmb 
 *                 T-CPEDM.UsrDscto = FacCPedi.UsrDscto 
 *                 T-CPEDM.usuario  = FacCPedi.usuario
 *                 T-CPEDM.ImpBrt   = 0
 *                 T-CPEDM.ImpDto   = 0
 *                 T-CPEDM.ImpExo   = 0
 *                 T-CPEDM.ImpIgv   = 0
 *                 T-CPEDM.ImpIsc   = 0
 *                 T-CPEDM.ImpTot   = 0
 *                 T-CPEDM.ImpVta   = 0.
 *          F-IGV = 0.
 *          F-ISC = 0.*/
         L-NewPed = NO.
         I-NItem = 0.
      END.
/*      T-CPEDM.ImpDto = T-CPEDM.ImpDto + FacDPedi.ImpDto.
 *                F-IGV = F-IGV + FacDPedi.ImpIgv.
 *                F-ISC = F-ISC + FacDPedi.ImpIsc.
 *       T-CPEDM.ImpTot = T-CPEDM.ImpTot + FacDPedi.ImpLin.
 *       IF NOT FacDPedi.AftIgv THEN T-CPEDM.ImpExo = T-CPEDM.ImpExo + FacDPedi.ImpLin.
 *       CREATE T-DPEDM.
 *       RAW-TRANSFER FacDPedi TO T-DPEDM.
 *       ASSIGN T-DPEDM.NroPed = T-CPEDM.NroPed.
 *       I-NItem = I-NItem + 1.
 *       IF ( T-CPEDM.Cmpbnte = "BOL" AND I-NItem >= FacCfgGn.Items_Boleta ) OR 
 *          ( T-CPEDM.Cmpbnte = "FAC" AND I-NItem >= FacCfgGn.Items_Factura ) THEN DO:
 *          L-NewPed = YES.
 *       END.*/
      IF LAST-OF(FacDPedi.AlmDes) OR L-NewPed THEN DO:
         L-NewPed = YES.    /****   ADD BY C.Q. 03/03/2000  ****/
/*         ASSIGN T-CPEDM.ImpIgv = ROUND(F-IGV,2).
 *                 T-CPEDM.ImpIsc = ROUND(F-ISC,2).
 *                 T-CPEDM.ImpBrt = T-CPEDM.ImpTot - T-CPEDM.ImpIgv - T-CPEDM.ImpIsc + 
 *                                  T-CPEDM.ImpDto - T-CPEDM.ImpExo.
 *                 T-CPEDM.ImpVta = T-CPEDM.ImpBrt - T-CPEDM.ImpDto.
 * 
 *         /****    ADD BY C.Q. 03/03/2000  ****/
 *         /*** DESCUENTO GLOBAL ****/
 *         IF T-CPEDM.PorDto > 0 THEN DO:
 *            T-CPEDM.ImpDto = T-CPEDM.ImpDto + ROUND(T-CPEDM.ImpTot * T-CPEDM.PorDto / 100,2).
 *            T-CPEDM.ImpTot = ROUND(T-CPEDM.ImpTot * (1 - T-CPEDM.PorDto / 100),2).
 *            T-CPEDM.ImpVta = ROUND(T-CPEDM.ImpTot / (1 + T-CPEDM.PorIgv / 100),2).
 *            T-CPEDM.ImpIgv = T-CPEDM.ImpTot - T-CPEDM.ImpVta.
 *            T-CPEDM.ImpBrt = T-CPEDM.ImpTot - T-CPEDM.ImpIgv - T-CPEDM.ImpIsc + 
 *                             T-CPEDM.ImpDto - T-CPEDM.ImpExo.
 *         END.*/
      END.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Elimina-Comprobante V-table-Win 
PROCEDURE Elimina-Comprobante :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DISABLE TRIGGERS FOR LOAD OF Ccbcdocu.

  ELIMINACION:          
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
      /* TRACKING FACTURAS */
      RUN vtagn/pTracking-04 (s-CodCia,
                              Ccbcdocu.CodDiv,
                              Ccbcdocu.CodPed,
                              Ccbcdocu.NroPed,
                              s-User-Id,
                              'EFAC',
                              'A',
                              DATETIME(TODAY, MTIME),
                              DATETIME(TODAY, MTIME),
                              Ccbcdocu.coddoc,
                              Ccbcdocu.nrodoc,
                              Ccbcdocu.codref,
                              Ccbcdocu.nroref).

      /* EXTORNAMOS CONTROL DE PERCEPCIONES POR CARGOS */
      FOR EACH B-CDOCU EXCLUSIVE-LOCK WHERE B-CDOCU.codcia = Ccbcdocu.codcia
          AND B-CDOCU.coddiv = Ccbcdocu.coddiv
          AND B-CDOCU.coddoc = "PRC"
          AND B-CDOCU.codref = Ccbcdocu.coddoc
          AND B-CDOCU.nroref = Ccbcdocu.nrodoc:
          DELETE B-CDOCU.
      END.
      /* RHC 12.07.2012 Anulamos el control de impresion en caja */
      FIND w-report WHERE w-report.Llave-I = Ccbcdocu.codcia
          AND w-report.Campo-C[1] = Ccbcdocu.coddoc
          AND w-report.Campo-C[2] = Ccbcdocu.nrodoc      
          AND w-report.Llave-C = "IMPCAJA"
          AND w-report.Llave-D = Ccbcdocu.fchdoc
          AND w-report.Task-No = INTEGER(Ccbcdocu.coddiv)
          EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE w-report THEN DELETE w-report.

      /* ANULAMOS LA FACTURA EN PROGRESS */
      FIND B-CDOCU WHERE ROWID(B-CDOCU) = ROWID(CcbCDocu) EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABL B-CDOCU THEN UNDO, RETURN 'ADM-ERROR'.
      ASSIGN 
          B-CDOCU.FlgEst = "A"
          B-CDOCU.SdoAct = 0
          B-CDOCU.UsuAnu = S-USER-ID
          B-CDOCU.FchAnu = TODAY
          B-CDOCU.Glosa  = "A N U L A D O".
     /* RHC 05.10.05 Reversion de puntos bonus */
     FIND GN-CARD WHERE GN-CARD.nrocard = Ccbcdocu.nrocard NO-ERROR.
     IF AVAILABLE GN-CARD THEN GN-CARD.AcuBon[10] = GN-CARD.AcuBon[10] - Ccbcdocu.puntos.

     /* *************** ANULAMOS LA FACTURA EN EL SPEED **************** */
/*      RUN sypsa/anular-comprobante (ROWID(Ccbcdocu)).              */
/*      IF RETURN-VALUE = "ADM-ERROR" THEN UNDO, RETURN "ADM-ERROR". */
     /* ************************************************************** */
      /* HAY RUTINAS QUE NO SE DEBEN HACER SI ES UNA FACTURA POR FAI */
     /* ************************************************************** */
      IF Ccbcdocu.CodRef = "FAI" THEN LEAVE.
     /* ************************************************************** */

      /* RHC 04/2/2016 solo para TCK de Lista Express */
      IF Ccbcdocu.coddoc = "TCK" THEN DO:
          FOR EACH Ccbdcaja WHERE Ccbdcaja.codcia = Ccbcdocu.codcia
              AND Ccbdcaja.coddoc = Ccbcdocu.coddoc
              AND Ccbdcaja.nrodoc = Ccbcdocu.nrodoc:
              ASSIGN Ccbcdocu.sdoact = Ccbcdocu.sdoact + Ccbdcaja.imptot.
              DELETE Ccbdcaja.
          END.
      END.
      
      /* DESCARGA ALMACENES */
      RUN vta2/des_alm (ROWID(CcbCDocu)).
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

      
      /* RHC 14/12/2015 FACTURAS SUPERMERCADOS -> PLAZA VEA */
      IF CcbCDocu.CndCre = "PLAZA VEA" THEN DO:
          /* RHC 12/12/2015 Actualizamos saldos */
          DEF VAR x-CanDes AS DEC NO-UNDO.
          DEF VAR x-CanAte AS DEC NO-UNDO.
          /* Barremos el control */
          FOR EACH ControlOD WHERE ControlOD.CodCia = s-codcia
              AND ControlOD.CodDiv = s-coddiv
              AND ControlOD.NroFac = Ccbcdocu.nrodoc:
              /*MESSAGE controlod.coddoc controlod.nrodoc controlod.nroetq.*/
              FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
                  /*MESSAGE ccbddocu.codmat.*/
                  x-CanDes = Ccbddocu.candes.
                  FOR EACH Vtaddocu WHERE VtaDDocu.CodCia = ControlOD.codcia
                      AND VtaDDocu.CodDiv = ControlOD.coddiv
                      AND VtaDDocu.CodPed = ControlOD.coddoc
                      AND VtaDDocu.NroPed = ControlOD.nrodoc
                      AND VtaDDocu.Libre_c01 = ControlOD.nroetq
                      AND VtaDDocu.CodMat = Ccbddocu.codmat,
                      FIRST Facdpedi WHERE Facdpedi.codcia = s-codcia
                      AND Facdpedi.coddoc = Vtaddocu.codped
                      AND Facdpedi.nroped = Vtaddocu.nroped
                      AND Facdpedi.codmat = Vtaddocu.codmat,
                      FIRST Faccpedi WHERE Faccpedi.codcia = Facdpedi.codcia
                      AND Faccpedi.coddiv = Facdpedi.coddiv
                      AND Faccpedi.coddoc = Facdpedi.coddoc
                      AND Faccpedi.nroped = Facdpedi.nroped:
                      x-CanAte = MINIMUM(VtaDDocu.CanAte, x-CanDes).
                      VtaDDocu.CanAte = VtaDDocu.CanAte - x-CanAte.
                      Facdpedi.CanAte = Facdpedi.CanAte - x-CanAte.
                      x-CanDes = x-CanDes - x-CanAte.
                      Faccpedi.FlgEst = "P".
                      IF x-CanDes <= 0 THEN LEAVE.
                  END.  /* Vtaddocu */
              END.  /* Ccbddocu */
              ASSIGN
                  ControlOD.NroFac = ''
                  ControlOD.FchFac = ?
                  ControlOD.UsrFac = ''.
          END.      /* ControlOD */
      END.
      ELSE DO:
          /* ACTUALIZAMOS ORDEN DE DESPACHO */
          FIND Faccpedi WHERE Faccpedi.codcia = Ccbcdocu.codcia
              AND Faccpedi.coddiv = Ccbcdocu.DivOri
              AND Faccpedi.coddoc = Ccbcdocu.Libre_c01
              AND Faccpedi.nroped = Ccbcdocu.Libre_c02
              EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAILABLE Faccpedi THEN DO:
              MESSAGE 'NO se pudo actualizar la' Ccbcdocu.Libre_c01 Ccbcdocu.Libre_c02
                  VIEW-AS ALERT-BOX ERROR.
              UNDO, RETURN "ADM-ERROR".
          END.
          FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
              FOR EACH Facdpedi OF Faccpedi WHERE Facdpedi.codmat = Ccbddocu.codmat:
                  ASSIGN
                      Facdpedi.CanAte = Facdpedi.CanAte -  Ccbddocu.CanDes.
                  IF Facdpedi.CanAte < 0  THEN Facdpedi.CanAte = 0.
              END.
          END.
          ASSIGN 
              Faccpedi.FlgEst = "P".
      END.
      /* RHC 10.12.2010 ACTUALIZAMOS FACTURA ADELANTADA */
      RUN vtagn/p-extorna-factura-adelantada ( ROWID(Ccbcdocu) ).
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      /* ANULAMOS LA GUIA DE REMISION */
      FOR EACH B-CDOCU WHERE B-CDOCU.CodCia = s-CodCia
          AND B-CDOCU.CodDoc = "G/R"
          AND B-CDOCU.CodRef = Ccbcdocu.CodDoc
          AND B-CDOCU.NroRef = Ccbcdocu.NroDoc
          AND B-CDOCU.FlgEst = "F":
          ASSIGN 
              B-CDOCU.FlgEst = "A"
              B-CDOCU.SdoAct = 0
              B-CDOCU.UsuAnu = S-USER-ID
              B-CDOCU.FchAnu = TODAY
              B-CDOCU.Glosa  = "A N U L A D O".
      END.
  END.
  RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel V-table-Win 
PROCEDURE Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER init 1.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 1.
DEFINE VARIABLE F-PreUni                LIKE CcbDDocu.Preuni.
DEFINE VARIABLE F-ImpLin                LIKE CcbDDocu.ImpLin.
DEFINE VARIABLE F-ImpTot                LIKE CcbCDocu.ImpTot.

DEF VAR can as integer.
DEF        VAR C-NomVen  AS CHAR FORMAT "X(30)".
DEF        VAR C-Descli  AS CHAR FORMAT "X(60)".
DEF        VAR C-Moneda  AS CHAR FORMAT "X(7)".
DEF        VAR C-NomCon  AS CHAR FORMAT "X(30)".
DEF        VAR W-DIRALM  AS CHAR FORMAT "X(50)".
DEF        VAR W-TLFALM  AS CHAR FORMAT "X(65)".
DEF        VAR X-ORDCOM AS CHARACTER FORMAT "X(18)".
DEF        VAR X-EnLetras AS CHAR FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE C-OBS AS CHAR EXTENT 2.
DEFINE VARIABLE K AS INTEGER.

IF NUM-ENTRIES(CcbCDocu.glosa,"-") - 1 > 6 THEN DO:
   DO K = 2 TO 7:
      IF ENTRY(K,CcbCDocu.glosa,"-") <> "" THEN 
         C-OBS[1] = C-OBS[1] + "- " + ENTRY(K,CcbCDocu.glosa,"-").
   END.
   DO K = 8 TO NUM-ENTRIES(CcbCDocu.glosa,"-"):
      IF ENTRY(K,CcbCDocu.glosa,"-") <> "" THEN 
         C-OBS[2] = C-OBS[2] + "- " + ENTRY(K,CcbCDocu.glosa,"-").
   END.
END.
ELSE DO: 
   C-OBS[1] = CcbCDocu.glosa.
   C-OBS[2] = "".
END.
/* ************************ cargamos variables ********************* */
FIND gn-ven WHERE 
     gn-ven.CodCia = CcbCDocu.CodCia AND  
     gn-ven.CodVen = CcbCDocu.CodVen 
     NO-LOCK NO-ERROR.
C-NomVen = CcbCDocu.CodVen.
IF AVAILABLE gn-ven THEN C-NomVen = C-NomVen + " - " + gn-ven.NomVen.
FIND gn-clie WHERE 
     gn-clie.codcia = cl-codcia AND  
     gn-clie.codcli = CcbCDocu.codcli NO-LOCK NO-ERROR.
     
C-DESCLI  = Gn-clie.codcli + ' - ' + Gn-clie.Nomcli.
C-DESCLI  = CcbCDocu.codcli + ' - ' + CcbCDocu.Nomcli.

FIND gn-ConVt WHERE gn-ConVt.Codig = CcbCDocu.FmaPgo NO-LOCK NO-ERROR.
C-NomCon = CcbCDocu.FmaPgo.
IF AVAILABLE gn-ConVt THEN C-NomCon = gn-ConVt.Nombr.
IF CcbCDocu.Codmon = 2 THEN C-Moneda = "DOLARES".
ELSE C-Moneda = "SOLES".

/* ******************************************************************** */

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
chWorkSheet:Columns("A"):ColumnWidth = 4.
chWorkSheet:Columns("B"):ColumnWidth = 7.
chWorkSheet:Columns("C"):ColumnWidth = 10.
chWorkSheet:Columns("D"):ColumnWidth = 4.
chWorkSheet:Columns("E"):ColumnWidth = 45.
chWorkSheet:Columns("F"):ColumnWidth = 11.
chWorkSheet:Columns("G"):ColumnWidth = 15.
chWorkSheet:Columns("H"):ColumnWidth = 10.
chWorkSheet:Columns("I"):ColumnWidth = 15.

t-Column = t-Column + 2.
cColumn = STRING(t-Column).
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = CcbCDocu.NomCli.
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = CcbCDocu.CodCli.

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = CcbCDocu.dircli. 
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = CcbCDocu.ruccli.


t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = c-nomcon.

t-Column = t-Column + 3.
cColumn = STRING(t-Column).
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.fchdoc, '99/99/9999').
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "'" + CcbCDocu.NroPed. 
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "'" + CcbCDocu.NroRef.

t-Column = t-Column + 3.
cColumn = STRING(t-Column).
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = c-moneda. 
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "OFICINA".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.fchVto, '99/99/9999').
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = CcbCDocu.CodVen.
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "'" +  STRING(TIME,"HH:MM:SS") + "  " + S-USER-ID .
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = CcbCDocu.usuario.
cRange = "I" + cColumn.
chWorkSheet:Range(cRange):Value = "'" + CcbCDocu.nroDoc.

t-Column = t-Column + 2.
cColumn = STRING(t-Column).
FOR EACH CcbDDocu OF CcbCDocu NO-LOCK,
        FIRST almmmatg OF CcbDDocu NO-LOCK
        BREAK BY CcbCDocu.NroPed BY CcbDDocu.NroItm:
    IF CcbDDocu.aftIgv THEN DO:
       F-PreUni = CcbDDocu.PreUni.
       F-ImpLin = CcbDDocu.ImpLin. 
    END.
    ELSE DO:
       F-PreUni = ROUND(CcbDDocu.PreUni / (1 + CcbCDocu.PorIgv / 100),2).
       F-ImpLin = ROUND(CcbDDocu.ImpLin / (1 + CcbCDocu.PorIgv / 100),2). 
    END.  
    
    t-column = t-column + 1.
    cColumn = STRING(t-Column).
    
    can = can + 1.
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value =  can.
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + CcbDDocu.codmat.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = almmmatg.desmat.
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = almmmatg.desmar.
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = CcbDDocu.UndVta.
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = CcbDDocu.candes.
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = F-PreUni.
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = f-ImpLin.
END.

/* PIE DE PAGINA */
RUN bin/_numero(CcbCDocu.imptot, 2, 1, OUTPUT X-EnLetras).
X-EnLetras = "SON : " + X-EnLetras + (IF CcbCDocu.codmon = 1 THEN " NUEVOS SOLES" ELSE " DOLARES AMERICANOS").

t-column = t-column + 9.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = x-EnLetras.

t-column = t-column + 4.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "O.Compra:  " + CcbCDocu.NroOrd.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.impbrt,'->>,>>>,>>9.99').
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.impdto,'->>,>>>,>>9.99').
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.impvta,'->>,>>>,>>9.99').

t-column = t-column + 1.
cColumn = STRING(t-Column).
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.PorIgv,'->>9.99') + " %".

t-column = t-column + 2.
cColumn = STRING(t-Column).
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.impigv,'->>,>>>,>>9.99').
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.imptot,'->>,>>>,>>9.99').
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "S/.        " + STRING(CcbCDocu.imptot,'->>,>>>,>>9.99').

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel2 V-table-Win 
PROCEDURE Excel2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER init 1.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 1.
DEFINE VARIABLE F-PreUni                LIKE CcbDDocu.Preuni.
DEFINE VARIABLE F-ImpLin                LIKE CcbDDocu.ImpLin.
DEFINE VARIABLE F-ImpTot                LIKE CcbCDocu.ImpTot.

DEF VAR C-NomVen   AS CHAR FORMAT "X(30)"       NO-UNDO.
DEF VAR C-Moneda   AS CHAR FORMAT "X(7)"        NO-UNDO.
DEF VAR C-NomCon   AS CHAR FORMAT "X(30)"       NO-UNDO.
DEF VAR X-ORDCOM   AS CHARACTER FORMAT "X(18)"  NO-UNDO.
DEF VAR X-EnLetras AS CHAR FORMAT "x(100)"      NO-UNDO.
DEF VAR C-OBS      AS CHAR EXTENT 2             NO-UNDO.
DEF VAR K          AS INTEGER              NO-UNDO.
DEF VAR iItm       AS INTEGER              NO-UNDO.

IF NUM-ENTRIES(CcbCDocu.glosa,"-") - 1 > 6 THEN DO:
   DO K = 2 TO 7:
      IF ENTRY(K,CcbCDocu.glosa,"-") <> "" THEN 
         C-OBS[1] = C-OBS[1] + "- " + ENTRY(K,CcbCDocu.glosa,"-").
   END.
   DO K = 8 TO NUM-ENTRIES(CcbCDocu.glosa,"-"):
      IF ENTRY(K,CcbCDocu.glosa,"-") <> "" THEN 
         C-OBS[2] = C-OBS[2] + "- " + ENTRY(K,CcbCDocu.glosa,"-").
   END.
END.
ELSE DO: 
   C-OBS[1] = CcbCDocu.glosa.
   C-OBS[2] = "".
END.
/* ************************ cargamos variables ********************* */
FIND gn-ven WHERE 
     gn-ven.CodCia = CcbCDocu.CodCia AND  
     gn-ven.CodVen = CcbCDocu.CodVen 
     NO-LOCK NO-ERROR.
C-NomVen = CcbCDocu.CodVen.
IF AVAILABLE gn-ven THEN C-NomVen = C-NomVen + " - " + gn-ven.NomVen.
FIND gn-clie WHERE 
     gn-clie.codcia = cl-codcia AND  
     gn-clie.codcli = CcbCDocu.codcli NO-LOCK NO-ERROR.
     
FIND gn-ConVt WHERE gn-ConVt.Codig = CcbCDocu.FmaPgo NO-LOCK NO-ERROR.
C-NomCon = CcbCDocu.FmaPgo.
IF AVAILABLE gn-ConVt THEN C-NomCon = gn-ConVt.Nombr.
IF CcbCDocu.Codmon = 2 THEN C-Moneda = "DOLARES".
ELSE C-Moneda = "SOLES".

/* ******************************************************************** */

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.

DEF var x-Plantilla AS CHAR NO-UNDO.
GET-KEY-VALUE SECTION 'Plantillas' KEY 'Carpeta' VALUE x-Plantilla .
x-Plantilla = x-Plantilla + "FacturaTienda.xlt".

chWorkbook = chExcelApplication:Workbooks:Add(x-Plantilla).

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
chWorkSheet:Columns("A"):ColumnWidth = 6.
chWorkSheet:Columns("B"):ColumnWidth = 7.
chWorkSheet:Columns("C"):ColumnWidth = 50.
chWorkSheet:Columns("D"):ColumnWidth = 15.
chWorkSheet:Columns("E"):ColumnWidth = 10.
chWorkSheet:Columns("F"):ColumnWidth = 12.
chWorkSheet:Columns("G"):ColumnWidth = 12.
chWorkSheet:Columns("H"):ColumnWidth = 12.

t-Column = 9.
cColumn = STRING(t-Column).
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = CcbCDocu.CodDoc + '-' + CcbCDocu.nroDoc. 


t-Column = t-Column + 2.
cColumn = STRING(t-Column).

cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = 'Se�or: ' + CcbCDocu.NomCli.
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = 'Codigo: ' + CcbCDocu.CodCli.

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = 'Direcci�n: ' + CcbCDocu.dircli. 
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = 'RUC: ' + CcbCDocu.ruccli.

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = c-nomcon.

t-Column = t-Column + 1.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Pedido: " + CcbCDocu.NroPed. 
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = 'Fecha Emision: ' + STRING(CcbCDocu.fchdoc, '99/99/9999').

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = 'Moneda: ' + c-moneda. 
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "'" + CcbCDocu.NroRef.
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = 'Fecha Vcto: ' + STRING(CcbCDocu.fchVto, '99/99/9999').

t-Column = t-Column + 1.
cColumn = STRING(t-Column).
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "Vendedor: " + CcbCDocu.CodVen + '-' + c-NomVen.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "OFICINA".
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = "Hora: " +  STRING(TIME,"HH:MM:SS") + "  " + S-USER-ID .
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = CcbCDocu.usuario.

t-Column = t-Column + 6.
cColumn = STRING(t-Column).

FOR EACH CcbDDocu OF CcbCDocu NO-LOCK,
        FIRST almmmatg OF CcbDDocu NO-LOCK
        BREAK BY CcbCDocu.NroPed BY CcbDDocu.NroItm DESC:
    IF CcbDDocu.aftIgv THEN DO:
       F-PreUni = CcbDDocu.PreUni.
       F-ImpLin = CcbDDocu.ImpLin. 
    END.
    ELSE DO:
       F-PreUni = ROUND(CcbDDocu.PreUni / (1 + CcbCDocu.PorIgv / 100),2).
       F-ImpLin = ROUND(CcbDDocu.ImpLin / (1 + CcbCDocu.PorIgv / 100),2). 
    END.  

        /*Agrega Row*/
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):EntireRow:INSERT.

    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = CcbDDocu.NroItm.
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + CcbDDocu.codmat.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = almmmatg.desmat.
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = almmmatg.desmar.
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = CcbDDocu.UndVta.
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = CcbDDocu.candes.
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = F-PreUni.
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = f-ImpLin.
    iItm = iItm + 1.

END.

/* PIE DE PAGINA */
RUN bin/_numero(CcbCDocu.imptot, 2, 1, OUTPUT X-EnLetras).
X-EnLetras = "SON : " + X-EnLetras + (IF CcbCDocu.codmon = 1 THEN " NUEVOS SOLES" ELSE " DOLARES AMERICANOS").

t-column = t-column + iItm + 4.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = x-EnLetras.

t-column = t-column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "O.Compra:  " + CcbCDocu.NroOrd.
/****************
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.impbrt,'->>,>>>,>>9.99').
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.impdto,'->>,>>>,>>9.99').
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.impvta,'->>,>>>,>>9.99').

t-column = t-column + 1.
cColumn = STRING(t-Column).
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.PorIgv,'->>9.99') + " %".

t-column = t-column + 2.
cColumn = STRING(t-Column).
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.impigv,'->>,>>>,>>9.99').
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(CcbCDocu.imptot,'->>,>>>,>>9.99').
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):Value = "S/.        " + STRING(CcbCDocu.imptot,'->>,>>>,>>9.99').

*******/

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Detalle V-table-Win 
PROCEDURE Genera-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
   FOR EACH DETA WHERE DETA.CanDes > 0 BY DETA.NroItm:
       CREATE CcbDDocu.
       BUFFER-COPY DETA TO CcbDDocu
           ASSIGN 
                CcbDDocu.CodCia = CcbCDocu.CodCia 
                CcbDDocu.CodDiv = CcbCDocu.CodDiv
                CcbDDocu.CodDoc = CcbCDocu.CodDoc 
                CcbDDocu.NroDoc = CcbCDocu.NroDoc
                CcbDDocu.FchDoc = TODAY
                CcbDDOcu.CanDev = 0.                /* Control de Devoluciones */
   END.
END.
RETURN 'OK'.

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

DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':

    {vta2/graba-totales-fac.i}

    /* RHC 30-11-2006 Transferencia Gratuita */
    IF Ccbcdocu.FmaPgo = '900' THEN Ccbcdocu.sdoact = 0.
    IF Ccbcdocu.sdoact <= 0 
    THEN ASSIGN
            Ccbcdocu.fchcan = TODAY
            Ccbcdocu.flgest = 'C'.
END.
RETURN "OK".

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
  FIND FacCorre WHERE FacCorre.codcia = s-codcia
      AND FacCorre.coddoc = s-coddoc
      AND FacCorre.nroser = s-nroser
      NO-LOCK NO-ERROR.
  IF FacCorre.FlgEst = NO THEN DO:
      MESSAGE 'Esta serie est� desactivada' VIEW-AS ALERT-BOX WARNING.
      RETURN "ADM-ERROR".
  END.

  {adm/i-DocPssw.i s-CodCia s-CodDoc ""ADD""}

  RUN Actualiza-Deta.
  ASSIGN
      input-var-1 = ""
      output-var-2 = "".
  RUN lkup/c-guias-4 ("Guias Pendientes x Facturar").
  IF output-var-1 = ? THEN RETURN "ADM-ERROR".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Numero-de-Documento(NO).
  DO WITH FRAME {&FRAME-NAME}:
     DISPLAY TODAY @ CcbCDocu.FchDoc
             TODAY @ CcbCDocu.FchVto
             FacCfgGn.CliVar @ CcbCDocu.CodCli
             FacCfgGn.Tpocmb[1] @ CcbCDocu.TpoCmb
             STRING(I-NroSer,"999") + STRING(I-NroDoc,"999999") @ CcbCDocu.NroDoc.
     RUN Asigna-Guias.
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
  Notes:       SOLO SE PUEDEN CREAR FACTURAS
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {vtagn/i-faccorre-01.i &Codigo = s-CodDoc &Serie = s-NroSer }
  ASSIGN 
      I-NroSer = FacCorre.NroSer
      I-NroDoc = FacCorre.Correlativo
      FacCorre.Correlativo = FacCorre.Correlativo + 1.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Buscamos la Guia de Remision */
  FIND B-CDOCU WHERE B-CDOCU.codcia = s-codcia
      AND B-CDOCU.coddoc = 'G/R'
      AND B-CDOCU.nrodoc = c-NroGui
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE B-CDOCU THEN DO:
      MESSAGE 'NO pudo bloquear la guia de remisi�n' c-NroGui
          VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN 'ADM-ERROR'.
  END.
  BUFFER-COPY B-CDOCU 
      TO CcbCDocu
      ASSIGN 
          CcbCDocu.CodCia = S-CODCIA
          CcbCDocu.CodDoc = S-CODDOC
          CcbCDocu.CodMov = S-CODMOV 
          CcbCDocu.CodDiv = S-CODDIV
          CcbCDocu.NroDoc = STRING(I-NroSer,"999") + STRING(I-NroDoc,"999999")
          CcbCDocu.CodRef = B-CDOCU.CodDoc
          CcbCDocu.NroRef = B-CDOCU.NroDoc.
  ASSIGN 
      CcbCDocu.FchDoc = TODAY
      CcbCDocu.FchVto = D-FCHVTO
      CcbCDocu.FchAte = TODAY
      CcbCDocu.FlgEst = "P"
      CcbCDocu.FlgAte = "P"
      CcbCDocu.Tipo   = "OFICINA"
      CcbCDocu.TipVta = "2"
      CcbCDocu.TpoFac = S-TPOFAC
      CcbCDocu.TpoCmb = FacCfgGn.TpoCmb[1]
      CcbCDocu.FlgAte = 'D'
      CcbCDocu.usuario = S-USER-ID
      CcbCDocu.HorCie = STRING(TIME, 'HH:MM').

  FIND gn-clie WHERE gn-clie.CodCia = cl-codcia 
      AND gn-clie.CodCli = CcbCDocu.CodCli NO-LOCK NO-ERROR.
  IF AVAILABLE gn-clie  THEN DO:
      ASSIGN 
          CcbCDocu.CodDpto = gn-clie.CodDept 
          CcbCDocu.CodProv = gn-clie.CodProv 
          CcbCDocu.CodDist = gn-clie.CodDist.
  END.

  /* TRACKING FACTURAS */
  RUN vtagn/pTracking-04 (s-CodCia,
                       s-CodDiv,
                       Ccbcdocu.CodPed,
                       Ccbcdocu.NroPed,
                       s-User-Id,
                       'EFAC',
                       'P',
                       DATETIME(TODAY, MTIME),
                       DATETIME(TODAY, MTIME),
                       Ccbcdocu.coddoc,
                       Ccbcdocu.nrodoc,
                       Ccbcdocu.codref,
                       Ccbcdocu.nroref).

  RUN Genera-Detalle.    /* Detalle de la Factura/Boleta */ 
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      MESSAGE 'NO pudo actualizar el detalle del comprobante'
          VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN "ADM-ERROR".
  END.
  RUN Graba-Totales.
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      MESSAGE 'NO pudo actualizar el total de la factura'
          VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN "ADM-ERROR".
  END.
  RUN Actualiza-Guias.
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      MESSAGE 'NO pudo actualizar las guias de remisi�n'
          VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN "ADM-ERROR".
  END.
  
  /* RHC 22.10.08 APLICACION DE FACTURAS ADELANTADAS */
  IF s-aplic-fact-ade = YES THEN DO:
/*       RUN Carga-Factura-Adelantada.                                */
/*       IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'. */

      RUN vta2/p-aplica-factura-adelantada ( ROWID(Ccbcdocu) ).
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      RUN Graba-Totales.
  END.

  IF AVAILABLE(FacCorre) THEN RELEASE FacCorre.
  
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
  RUN Procesa-Handle IN lh_Handle ('Browse').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  IF CcbCDocu.FlgEst = "A" THEN DO:
     MESSAGE 'El documento se encuentra Anulado...' VIEW-AS ALERT-BOX.
     RETURN 'ADM-ERROR'.
  END.
  IF s-permiso-anulacion = NO THEN DO:
      MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.
  /* ********************************************* */
  /* Inicio de actividades facturaci�n electr�nica */
  /* ********************************************* */
  IF s-Sunat-Activo = YES THEN DO:
      /* Verificamos si fue rechazada por SUNAT */
      IF NOT CAN-FIND(FeLogComprobantes WHERE FELogComprobantes.CodCia = Ccbcdocu.codcia
                      AND FELogComprobantes.CodDiv = Ccbcdocu.coddiv
                      AND FELogComprobantes.CodDoc = Ccbcdocu.coddoc
                      AND FELogComprobantes.NroDoc = Ccbcdocu.nrodoc
                      AND FELogComprobantes.FlagSunat = 2
                      NO-LOCK)
          THEN DO:
          MESSAGE 'No est� rechazado por SUNAT' SKIP
              'Verificar el Log de Comprobantes (FeLogComprobantes)' SKIP
              'Continuamos con la anulaci�n?' VIEW-AS ALERT-BOX ERROR
              BUTTONS YES-NO UPDATE rpta4 AS LOG.
          IF rpta4 = NO THEN RETURN 'ADM-ERROR'.
      END.
      MESSAGE 'Se va a proceder a anular un comprobante SUNAT' SKIP
          'Continuamos?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          UPDATE rpta AS LOG.
      IF rpta = NO THEN RETURN 'ADM-ERROR'.
  END.
  /* ********************************************* */
  DEF VAR x-Rowid AS ROWID NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  /* CONSISTENCIA DE AMORTIZACIONES AL DOCUMENTO */
  IF CcbCDocu.SdoAct < CcbCDocu.ImpTot  THEN DO:
      MESSAGE 'El documento registra amortizaciones...' VIEW-AS ALERT-BOX.
      RETURN 'ADM-ERROR'.
  END.
  IF CcbCDocu.CndCre <> 'N' THEN DO:
      MESSAGE 'El documento no corresponde a otros conceptos' VIEW-AS ALERT-BOX.
      RETURN 'ADM-ERROR'.
  END.
  /* CONSISTENCIA DE CANJE POR LETRA EN TRAMITE */
  IF Ccbcdocu.FlgSit = "X" THEN DO:
      MESSAGE "El Comprobante tiene un CANJE por LETRA en tr�mite" VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  x-Rowid = ROWID(Ccbcdocu).
  /* ******************************************************************** */
  /* consistencia de la fecha del cierre del sistema */
  DEF VAR dFchCie AS DATE.
  RUN gn/fecha-de-cierre (OUTPUT dFchCie).
  IF ccbcdocu.fchdoc <= dFchCie THEN DO:
      MESSAGE 'NO se puede anular/modificar ningun documento antes del' (dFchCie + 1) SKIP
          'Desea continuar con la anulaci�n?' VIEW-AS ALERT-BOX WARNING
          BUTTONS YES-NO UPDATE rpta3 AS LOG.
      IF rpta3 = NO THEN RETURN 'ADM-ERROR'.
  END.
  /* fin de consistencia */

  PRINCIPAL:
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
      /* Motivo de anulacion */
      DEF VAR cReturnValue AS CHAR NO-UNDO.
      RUN ccb/d-motanu (ccbcdocu.codcia, ccbcdocu.coddoc, ccbcdocu.nrodoc, s-user-id, OUTPUT cReturnValue).
      IF cReturnValue = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      /* ******************* */
      {lib/lock-genericov3.i
          &Tabla="B-CDOCU"
          &Condicion="ROWID(B-CDOCU) = ROWID(CcbCDocu)"
          &Bloqueo="EXCLUSIVE-LOCK"
          &Accion="RETRY"
          &Mensaje="YES"
          &TipoError="UNDO, RETURN 'ADM-ERROR'"
          }
        
      ASSIGN 
          B-CDOCU.FlgEst = "A"
          B-CDOCU.SdoAct = 0 
          B-CDOCU.Glosa  = '**** Documento Anulado ****'
          B-CDOCU.UsuAnu = S-USER-ID
          B-CDOCU.FchAnu = TODAY.
      
  END.
  IF AVAILABLE(B-CDOCU) THEN RELEASE B-CDOCU.
  FIND CURRENT Ccbcdocu NO-LOCK NO-ERROR.
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
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE CcbCDocu THEN DO WITH FRAME {&FRAME-NAME}:
     IF CcbCDocu.FlgEst = "P" THEN F-Estado:SCREEN-VALUE = "PENDIENTE".
     IF CcbCDocu.FlgEst = "A" THEN F-Estado:SCREEN-VALUE = "ANULADO".
     IF CcbCDocu.FlgEst = "C" THEN F-Estado:SCREEN-VALUE = "CANCELADO".
     F-CndVta:SCREEN-VALUE = "".
     FIND gn-convt WHERE gn-convt.Codig = CcbCDocu.FmaPgo NO-LOCK NO-ERROR.
     IF AVAILABLE gn-convt THEN F-CndVta:SCREEN-VALUE = gn-convt.Nombr.
      FILL-IN-DivOri:SCREEN-VALUE = "".
      FIND gn-divi WHERE gn-divi.codcia = s-codcia
          AND gn-divi.coddiv = Ccbcdocu.divori
          NO-LOCK NO-ERROR.
      IF AVAILABLE gn-divi THEN FILL-IN-DivOri:SCREEN-VALUE = GN-DIVI.DesDiv.
      FIND CcbTabla WHERE CcbTabla.CodCia = s-codcia 
        AND CcbTabla.Tabla  = Ccbcdocu.CodDoc
        AND CcbTabla.Codigo = CcbCDocu.CodCta
        NO-LOCK NO-ERROR.
      IF AVAILABLE CcbTabla THEN DISPLAY CcbTabla.Nombre @ FILL-IN-Concepto.
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
  /* Solicitamos formato de impresi�n */
  DEF VAR pFormato AS INT NO-UNDO.

  RUN sunat\d-selecciona-formato (OUTPUT pFormato).
  IF pFormato = 0 THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'imprime':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  CASE pFormato:
      WHEN 1 THEN DO:       /* Formato Pre-Impreso  ANTIGUO */
          CASE s-CodDoc:
              WHEN "BOL" THEN DO:
                  RUN VTA\R-IMPBOL2-1 (ROWID(CcbCDocu)).
              END.
              WHEN "FAC" THEN DO:
                  RUN VTA\R-IMPFAC2-1 (ROWID(CcbCDocu)).
              END.
              WHEN "FAI" THEN DO:
                  MESSAGE "Cambie el formato por PAPEL BLANCO" VIEW-AS ALERT-BOX WARNING
                      BUTTONS OK-CANCEL UPDATE rpta AS LOG.
                  IF rpta = NO THEN RETURN.
                  RUN VTA\R-IMPFAC2-1 (ROWID(CcbCDocu)).
              END.
              WHEN "TCK" THEN DO:
                  RUN vta2/r-ticket-listaexpress (ROWID(ccbcdocu), "O").
              END.
          END CASE.
      END.
      WHEN 2 THEN DO:       /* Formato SUNAT */
          CASE s-CodDoc:
              WHEN "BOL" OR WHEN "FAC" OR WHEN "N/C" THEN DO:

                    /* Ic - 23Dic2017, Impresion QR */

                    DEFINE BUFFER x-gn-divi FOR gn-divi.

                    FIND FIRST x-gn-divi WHERE x-gn-divi.codcia = s-codcia AND 
                                                x-gn-divi.coddiv = ccbcdocu.coddiv NO-LOCK NO-ERROR.
        
                    IF AVAILABLE x-gn-divi THEN DO:
                        IF x-gn-divi.campo-log[7] = YES THEN DO:
                            /* Configurado para QR */
                            DEFINE VAR x-version AS CHAR.
                            DEFINE VAR x-formato-tck AS LOG.
                            DEFINE VAR x-imprime-directo AS LOG.
                            DEFINE VAR x-nombre-impresora AS CHAR.

                            DEF VAR answer AS LOGICAL NO-UNDO.
                            SYSTEM-DIALOG PRINTER-SETUP UPDATE answer.
                            IF NOT answer THEN RETURN.

                            /* pVersion: "O": ORIGINAL "C": COPIA "R": RE-IMPRESION "L" : CLiente "A" : Control Administrativo */

                            x-version = 'L'.
                            x-formato-tck = NO.        /* YES : Formato Ticket,  NO : Formato A4 */
                            x-imprime-directo = YES.
                            x-nombre-impresora = SESSION:PRINTER-NAME.
    
                            /* Lista Express imprime Ticket */
                            IF CcbCDocu.CodDiv = "00506" THEN x-formato-tck = YES.
                            IF s-CodDoc = 'N/C' THEN x-formato-tck = NO.
    
                            /* CLIENTE */
                            RUN sunat\r-impresion-doc-electronico(INPUT CcbCDocu.CodDiv, 
                                                                  INPUT CcbCDocu.coddoc, 
                                                                  INPUT CcbCDocu.nrodoc,
                                                                  INPUT x-version,
                                                                  INPUT x-formato-tck,
                                                                  INPUT x-imprime-directo,
                                                                  INPUT x-nombre-impresora).
                            /* CONTROL ADMINISTRATIVO */
                            x-version = 'A'.
                            RUN sunat\r-impresion-doc-electronico(INPUT CcbCDocu.CodDiv, 
                                                                  INPUT CcbCDocu.coddoc, 
                                                                  INPUT CcbCDocu.nrodoc,
                                                                  INPUT x-version,
                                                                  INPUT x-formato-tck,
                                                                  INPUT x-imprime-directo,
                                                                  INPUT x-nombre-impresora).
        
                    END.
                    ELSE DO:
                        /* Impresion MATRICIAL sin QR */
                        RUN sunat\r-impresion-documentos-sunat ( ROWID(Ccbcdocu), "O", NO ).                        
                    END.
                END.
                ELSE DO:
                    MESSAGE "La division(" + ccbcdocu.coddiv + ") a la que pertenece el documento NO EXISTE".
                END.

                RELEASE x-gn-divi.

              END.

              WHEN "TCK" THEN DO:
                  RUN sunat\r-tickets-sunat ( ROWID(Ccbcdocu), "O", YES ).
              END.
          END CASE.
      END.
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR x-Moneda AS CHAR FORMAT 'x(3)'.  
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
  /* Control de Facturas Adelantadas */
  DEFINE VARIABLE x-saldo-mn AS DEC NO-UNDO.
  DEFINE VARIABLE x-saldo-me AS DEC NO-UNDO.
  ASSIGN
      s-aplic-fact-ade = NO
      x-saldo-mn = 0
      x-saldo-me = 0.
  FOR EACH F-CDOCU USE-INDEX Llave06 NO-LOCK WHERE F-CDOCU.codcia = s-codcia
      AND F-CDOCU.codcli = CcbCDocu.CodCli:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      AND F-CDOCU.flgest = 'P'
      AND F-CDOCU.coddoc = "A/C":
      IF F-CDOCU.CodMon = 1 THEN x-saldo-mn = x-saldo-mn + F-CDOCU.SdoAct.
      ELSE x-saldo-me = x-saldo-me + F-CDOCU.SdoAct.
  END.
  IF x-saldo-mn > 0 OR x-saldo-me > 0 THEN DO:
      MESSAGE 'Hay un SALDO de Factura(s) Adelantada(s) por aplicar' SKIP
          'Por aplicar NUEVOS SOLES:' x-saldo-mn SKIP
          'Por aplicar DOLARES:' x-saldo-me SKIP
          'APLICAMOS el(los) Adelanto(s)?' VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO-CANCEL
          UPDATE s-aplic-fact-ade.
      IF s-aplic-fact-ade = ? THEN RETURN 'ADM-ERROR'.
  END.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

  MESSAGE "Imprimir Documentos"  SKIP(1)
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      TITLE "" UPDATE choice AS LOGICAL.
  IF choice THEN RUN dispatch IN THIS-PROCEDURE ('imprime':U).
  
  RUN Procesa-Handle IN lh_Handle ('Pagina1'). 
  RUN Procesa-Handle IN lh_Handle ('browse'). 
  
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
  IF L-INCREMENTA  THEN
     FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
          FacCorre.CodDoc = S-CODDOC AND
          FacCorre.NroSer = S-NROSER EXCLUSIVE-LOCK NO-ERROR.
  ELSE
     FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
          FacCorre.CodDoc = S-CODDOC AND
          FacCorre.NroSer = S-NROSER NO-LOCK NO-ERROR.
  
  IF AVAILABLE FacCorre THEN DO:
     ASSIGN I-NroDoc = FacCorre.Correlativo.
     IF L-INCREMENTA THEN ASSIGN FacCorre.Correlativo = FacCorre.Correlativo + 1.
     I-NroSer = FacCorre.NroSer.
     /*S-CodAlm = FacCorre.CodAlm.*/
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
        WHEN "codcli" THEN
            ASSIGN
                input-var-1 = s-coddiv
                input-var-2 = "G/R"
                input-var-3 = "P".
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
DEFINE VARIABLE X-ITMS AS DECIMAL NO-UNDO.
DO WITH FRAME {&FRAME-NAME} :
   IF CcbCDocu.CodCli:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Codigo de cliente no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO CcbCDocu.CodCli.
      RETURN "ADM-ERROR".   
   END.
   FOR EACH DETA:
       X-ITMS = X-ITMS + DETA.CanDes.
   END.
   IF X-ITMS = 0 THEN DO:
      MESSAGE "No existen items a generar" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO CcbCDocu.Glosa.
      RETURN "ADM-ERROR".   
   END.
/*    IF s-asigna-guia = 'YES' THEN DO:                                   */
/*        /* rhc 18.12.09 Buscar guias por chequear */                    */
/*        FIND FIRST F-CDOCU WHERE F-CDOCU.codcia = s-codcia              */
/*            AND F-CDOCU.coddoc = 'G/R'                                  */
/*            AND F-CDOCU.codcli = Ccbcdocu.CodCli:SCREEN-VALUE           */
/*            AND F-CDOCU.flgest = 'X'                                    */
/*            AND F-CDOCU.nroped = Ccbcdocu.NroPed:SCREEN-VALUE           */
/*            NO-LOCK NO-ERROR.                                           */
/*        IF AVAILABLE F-CDOCU THEN DO:                                   */
/*            MESSAGE 'Falta chequear la Guia de Remision' F-CDOCU.NroDoc */
/*                VIEW-AS ALERT-BOX ERROR.                                */
/*            RETURN 'ADM-ERROR'.                                         */
/*        END.                                                            */
/*        /* 11.01.10 buscamos si la O/D aun no se ha cerrado */          */
/*        FIND FIRST B-CPEDI WHERE B-CPEDI.codcia = s-codcia              */
/*            AND B-CPEDI.coddoc = 'O/D'                                  */
/*            AND B-CPEDI.codcli = Ccbcdocu.CodCli:SCREEN-VALUE           */
/*            AND B-CPEDI.flgest = 'P'                                    */
/*            AND B-CPEDI.nroref = Ccbcdocu.NroPed:SCREEN-VALUE           */
/*            NO-LOCK NO-ERROR.                                           */
/*        IF AVAILABLE B-CPEDI THEN DO:                                   */
/*            MESSAGE 'Falta cerrar la Orden de Despacho' B-CPEDI.NroPed  */
/*                VIEW-AS ALERT-BOX ERROR.                                */
/*            RETURN 'ADM-ERROR'.                                         */
/*        END.                                                            */
/*    END.                                                                */
END.

RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Valida-Asigna-Guias V-table-Win 
PROCEDURE Valida-Asigna-Guias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pGuias AS CHAR.
  
  DEF VAR i AS INT NO-UNDO.
  DEF VAR cNroDoc AS CHAR NO-UNDO.  /* Guia de Remision */
  DEF VAR cNroPed AS CHAR NO-UNDO.  /* Pedido(s) */
  
  /* Verificamos los Pedidos */
  FOR EACH T-DETA:
    DELETE T-DETA.
  END.
  DO i = 1 TO NUM-ENTRIES(pGuias):
    cNroDoc = ENTRY(i, pGuias).
    /* Verificamos si tiene mas de un pedido */
    FIND Ccbcdocu WHERE Ccbcdocu.codcia = s-codcia
        AND Ccbcdocu.coddoc = 'G/R'
        AND Ccbcdocu.nrodoc = cNroDoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbcdocu THEN DO:
        FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
            CREATE T-DETA.
            BUFFER-COPY Ccbddocu TO T-DETA.
        END.
        /* Buscamos la O/D */
        FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
            AND Faccpedi.coddoc = Ccbcdocu.codped
            AND Faccpedi.nroped = Ccbcdocu.nroped
            NO-LOCK NO-ERROR.
        IF AVAILABLE Faccpedi THEN DO:
            /* Capturamos los pedidos */
            IF cNroPed = ''
            THEN cNroPed = Faccpedi.nroref.
            ELSE DO:
                IF LOOKUP(TRIM(Faccpedi.nroref), cNroPed) = 0 
                THEN cNroPed = cNroPed + ',' + Faccpedi.nroref.
            END.
        END.            
    END.
  END.
  IF NUM-ENTRIES(cNroPed) > 1 THEN DO:
        MESSAGE 'Las Gu�as de Remisi�n pertenecen a mas de un pedido:' SKIP
            cNroPed SKIP 'Continuamos?' VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO
            UPDATE rpta-1 AS LOG.
        IF rpta-1 = NO THEN RETURN 'ADM-ERROR'.
  END.
  /* Verificamos los precios */
  DO i = 1 TO NUM-ENTRIES(pGuias):
    cNroDoc = ENTRY(i, pGuias).
    FIND Ccbcdocu WHERE Ccbcdocu.codcia = s-codcia
        AND Ccbcdocu.coddoc = 'G/R'
        AND Ccbcdocu.nrodoc = cNroDoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Ccbcdocu THEN NEXT.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK:
        FOR EACH T-DETA NO-LOCK WHERE T-DETA.CodMat = Ccbddocu.CodMat
                AND T-DETA.NroDoc <> Ccbddocu.NroDoc:
            IF T-DETA.PreUni <> Ccbddocu.PreUni THEN DO:
                MESSAGE 'Los precios unitarios no coinciden en el producto' Ccbddocu.codmat SKIP
                    'Entre las Guias de Remision' T-DETA.nrodoc 'y' Ccbddocu.nrodoc
                    VIEW-AS ALERT-BOX ERROR.
                RETURN 'ADM-ERROR'.
            END.
        END.
    END.
  END.
    
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
/* LAS FACTURAS Y BOLETAS NO SE PUEDEN MODIFICAR */
RETURN "ADM-ERROR".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

