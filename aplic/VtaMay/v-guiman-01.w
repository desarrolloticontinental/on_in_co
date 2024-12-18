&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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

/* Shared Variable Definitions ---                                       */

DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.
DEFINE SHARED VARIABLE S-CODCIA   AS INTEGER.
DEFINE SHARED VARIABLE CL-CODCIA   AS INTEGER.
DEFINE SHARED VARIABLE PV-CODCIA   AS INTEGER.
DEFINE SHARED VARIABLE S-NOMCIA   AS CHAR.
DEFINE SHARED VARIABLE S-USER-ID  AS CHAR.
DEFINE SHARED VARIABLE S-CODDOC   AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV   AS CHAR.
DEFINE SHARED VARIABLE S-CODALM   AS CHAR.
DEFINE SHARED VARIABLE S-CODMOV   AS INTEGER.
DEFINE SHARED VARIABLE S-NROSER   AS INTEGER.
DEFINE NEW SHARED VARIABLE X-CTRANS  AS CHAR initial "100038146".
DEFINE NEW SHARED VARIABLE X-RTRANS  AS CHAR.
DEFINE NEW SHARED VARIABLE X-DTRANS  AS CHAR.
DEFINE NEW SHARED VARIABLE X-NUMORD  AS CHAR.
DEFINE NEW SHARED VARIABLE X-obser   AS CHAR.
DEF VAR X-Nombre LIKE gn-prov.NomPro.
DEF VAR X-ruc    LIKE gn-prov.Ruc.
DEF VAR X-TRANS  LIKE FACCPEDI.Libre_c01.


DEFINE SHARED TEMP-TABLE DETA LIKE CcbDDocu.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE L-CREA         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE C-CODPED       AS CHAR      NO-UNDO.
DEFINE VARIABLE C-NROPED       AS CHAR      NO-UNDO.
DEFINE VARIABLE I-NROSER       AS INTEGER   NO-UNDO.
DEFINE VARIABLE I-NRODOC       AS INTEGER   NO-UNDO.
DEFINE VARIABLE I-ListPr       AS INTEGER   NO-UNDO.
DEFINE VARIABLE S-PRINTER-NAME AS CHARACTER NO-UNDO.
DEFINE VARIABLE S-CODCLI       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE S-CODVEN       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE R-ROWID        AS ROWID     NO-UNDO. 
DEFINE VARIABLE x-codalm AS CHARACTER NO-UNDO.

DEFINE BUFFER B-CDOCU FOR CcbCDocu.

FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
     FacCorre.CodDoc = S-CODDOC AND
     FacCorre.CodDiv = S-CODDIV AND
     FacCorre.NroSer = S-NROSER NO-LOCK NO-ERROR.
IF AVAILABLE FacCorre THEN 
   ASSIGN I-NroSer = FacCorre.NroSer.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

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
&Scoped-Define ENABLED-FIELDS CcbCDocu.TpoCmb 
&Scoped-define ENABLED-TABLES CcbCDocu
&Scoped-define FIRST-ENABLED-TABLE CcbCDocu
&Scoped-Define ENABLED-OBJECTS RECT-21 
&Scoped-Define DISPLAYED-FIELDS CcbCDocu.NroDoc CcbCDocu.CodRef ~
CcbCDocu.NroRef CcbCDocu.CodCli CcbCDocu.TpoCmb CcbCDocu.HorCie ~
CcbCDocu.FchDoc CcbCDocu.CodVen CcbCDocu.CodPed CcbCDocu.NroPed ~
CcbCDocu.FmaPgo 
&Scoped-define DISPLAYED-TABLES CcbCDocu
&Scoped-define FIRST-DISPLAYED-TABLE CcbCDocu
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-pedido F-Estado FILL-IN_NomCli ~
FILL-IN_DirCli FILL-IN_RucCli F-nOMvEN F-CndVta 

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
     SIZE 46 BY .69 NO-UNDO.

DEFINE VARIABLE F-Estado AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .69
     BGCOLOR 15 FGCOLOR 12 FONT 0 NO-UNDO.

DEFINE VARIABLE F-nOMvEN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-pedido AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pedido" 
     VIEW-AS FILL-IN 
     SIZE 9.14 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN_DirCli AS CHARACTER FORMAT "x(60)" 
     LABEL "Direccion" 
     VIEW-AS FILL-IN 
     SIZE 35 BY .69.

DEFINE VARIABLE FILL-IN_NomCli AS CHARACTER FORMAT "x(50)" 
     VIEW-AS FILL-IN 
     SIZE 41 BY .69.

DEFINE VARIABLE FILL-IN_RucCli AS CHARACTER FORMAT "x(11)" 
     LABEL "Ruc" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 3.77.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CcbCDocu.NroDoc AT ROW 1.12 COL 9 COLON-ALIGNED FORMAT "XXX-XXXXXX"
          VIEW-AS FILL-IN 
          SIZE 17.29 BY .69
          FONT 1
     FILL-IN-pedido AT ROW 1.12 COL 33.14 COLON-ALIGNED
     CcbCDocu.CodRef AT ROW 1.12 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.57 BY .69
     CcbCDocu.NroRef AT ROW 1.12 COL 53.72 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .69
     F-Estado AT ROW 1.12 COL 72.43 COLON-ALIGNED NO-LABEL
     CcbCDocu.CodCli AT ROW 1.81 COL 9 COLON-ALIGNED FORMAT "x(11)"
          VIEW-AS FILL-IN 
          SIZE 11 BY .69
     FILL-IN_NomCli AT ROW 1.81 COL 20 COLON-ALIGNED NO-LABEL
     CcbCDocu.TpoCmb AT ROW 1.81 COL 74 COLON-ALIGNED
          LABEL "T/  Cambio"
          VIEW-AS FILL-IN 
          SIZE 10.43 BY .69
     FILL-IN_DirCli AT ROW 2.5 COL 9 COLON-ALIGNED
     FILL-IN_RucCli AT ROW 2.5 COL 50 COLON-ALIGNED
     CcbCDocu.HorCie AT ROW 2.5 COL 78.72 COLON-ALIGNED HELP
          "" NO-LABEL FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 7.72 BY .69
          FGCOLOR 9 
     CcbCDocu.FchDoc AT ROW 2.54 COL 68.29 COLON-ALIGNED
          LABEL "Emisi�n"
          VIEW-AS FILL-IN 
          SIZE 9.14 BY .69
     CcbCDocu.CodVen AT ROW 3.19 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .69
     F-nOMvEN AT ROW 3.19 COL 15 COLON-ALIGNED NO-LABEL
     CcbCDocu.CodPed AT ROW 3.35 COL 64.29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .69
     CcbCDocu.NroPed AT ROW 3.35 COL 74 COLON-ALIGNED
          LABEL "Nro" FORMAT "XXX-XXXXXX"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .69
     CcbCDocu.FmaPgo AT ROW 3.88 COL 9 COLON-ALIGNED
          LABEL "Cond.Vta"
          VIEW-AS FILL-IN 
          SIZE 6 BY .69
     F-CndVta AT ROW 3.88 COL 15 COLON-ALIGNED NO-LABEL
     RECT-21 AT ROW 1 COL 1
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
         HEIGHT             = 3.77
         WIDTH              = 88.29.
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

/* SETTINGS FOR FILL-IN CcbCDocu.CodCli IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.CodPed IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN CcbCDocu.CodRef IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.CodVen IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-CndVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-nOMvEN IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.FchDoc IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FILL-IN-pedido IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DirCli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_NomCli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_RucCli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.FmaPgo IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN CcbCDocu.HorCie IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN CcbCDocu.NroDoc IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.NroPed IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN CcbCDocu.NroRef IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN CcbCDocu.TpoCmb IN FRAME F-Main
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
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH DETA:
    DELETE DETA.
END.
IF NOT L-CREA THEN DO:
   FOR EACH CcbDDocu NO-LOCK WHERE 
            CcbDDocu.CodCia = CcbCDocu.CodCia AND  
            CcbDDocu.coddoc = CcbCDocu.coddoc AND
            CcbDDocu.NroDoc = CcbCDocu.NroDoc :
       CREATE DETA.
       BUFFER-COPY Ccbddocu TO DETA.
/*        ASSIGN DETA.CodCia = CcbDDocu.CodCia  */
/*               DETA.codmat = CcbDDocu.codmat  */
/*               DETA.PreUni = CcbDDocu.PreUni  */
/*               DETA.CanDes = CcbDDocu.CanDes  */
/*               DETA.Pesmat = CcbDDocu.Pesmat  */
/*               DETA.Factor = CcbDDocu.Factor  */
/*               DETA.UndVta = CcbDDocu.UndVta  */
/*               DETA.PreBas = CcbDDocu.PreBas  */
/*               DETA.PorDto = CcbDDocu.PorDto  */
/*               DETA.ImpLin = CcbDDocu.ImpLin  */
/*               DETA.ImpIsc = CcbDDocu.ImpIsc  */
/*               DETA.ImpIgv = CcbDDocu.ImpIgv  */
/*               DETA.ImpDto = CcbDDocu.ImpDto  */
/*               DETA.AftIsc = CcbDDocu.AftIsc  */
/*               DETA.AftIgv = CcbDDocu.AftIgv  */
/*               DETA.NroItm = CcbDDocu.NroItm. */
   END.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna_datos V-table-Win 
PROCEDURE Asigna_datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT AVAILABLE Ccbcdocu THEN RETURN "ADM-ERROR".                                                    
    RUN vtamay\w-agtrans-03(ROWID(Ccbcdocu)).

    /* IF AVAILABLE B-CDOCU THEN DO:
         FIND FacCPedi WHERE 
              FacCPedi.CODCIA = B-CDOCU.CODCIA AND
              FacCPedi.CODDOC = B-CDOCU.CODPED AND  
              FacCPedi.NROPED = B-CDOCU.NROPED NO-LOCK NO-ERROR. 
        IF AVAILABLE FacCPedi THEN RUN vta\w-agtrans-01(ROWID(FacCPedi), ROWID(Ccbcdocu)).
        ELSE DO:
            FIND FacCPedm WHERE 
                 FacCPedm.CODCIA = B-CDOCU.CODCIA AND
                 FacCPedm.CODDOC = B-CDOCU.CODPED AND  
                 FacCPedm.NROPED = B-CDOCU.NROPED NO-LOCK NO-ERROR. 
            IF AVAILABLE FacCPedm THEN RUN vtamay\w-agtrans-02(ROWID(FacCPedm), ROWID(Ccbcdocu)).
        END.
    END.
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Guia V-table-Win 
PROCEDURE Borra-Guia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH CcbDDocu EXCLUSIVE-LOCK WHERE 
           CcbDDocu.CodCia = CcbCDocu.CodCia AND  
           CcbDDocu.CodDoc = CcbCDocu.CodDoc AND  
           CcbDDocu.Nrodoc = CcbCDocu.NroDoc
           ON ERROR UNDO, RETURN "ADM-ERROR":
      DELETE CcbDDocu.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cierra-Pedido V-table-Win 
PROCEDURE Cierra-Pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE I-NRO AS INTEGER INIT 0 NO-UNDO.
FOR EACH FacDPedi NO-LOCK WHERE 
         FacDPedi.CodCia = S-CODCIA AND
         FacDPedi.CodDoc = CcbCDocu.Codped AND
         FacDPedi.NroPed = CcbCDocu.NroPed:
    IF (FacDPedi.CanPed - FacDPedi.CanAte) > 0 THEN DO:
       I-NRO = 1.
       LEAVE.
    END.
END.
IF I-NRO = 0 THEN DO ON ERROR UNDO, RETURN "ADM-ERROR": 
   FIND FacCPedi WHERE 
        FacCPedi.CodCia = S-CODCIA AND
        FacCPedi.CodDoc = CcbCDocu.Codped AND
        FacCPedi.NroPed = C-NROPED EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE FacCPedi THEN ASSIGN FacCPedi.FlgEst = "C".
   RELEASE FacCPedi.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Guia V-table-Win 
PROCEDURE Genera-Guia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  ------------------------------------------------------------------------------*/
   FOR EACH DETA NO-LOCK WHERE DETA.CodMat <> "" 
        ON ERROR UNDO, RETURN "ADM-ERROR"
        ON STOP  UNDO, RETURN 'ADM-ERROR': 
       CREATE CcbDDocu. 
       BUFFER-COPY DETA TO Ccbddocu
           ASSIGN 
                CcbDDocu.CodCia = CcbCDocu.CodCia 
                CcbDDocu.Coddoc = CcbCDocu.Coddoc
                CcbDDocu.NroDoc = CcbCDocu.NroDoc 
                CcbDDocu.AlmDes = X-CODALM
                CcbDDocu.FchDoc = TODAY
                CcbDDocu.CodDiv = CcbcDocu.CodDiv.
   END.
   
END PROCEDURE.


/*
   FOR EACH DETA NO-LOCK WHERE DETA.CodMat <> "" 
        ON ERROR UNDO, RETURN "ADM-ERROR"
        ON STOP  UNDO, RETURN 'ADM-ERROR': 
       CREATE CcbDDocu. 
       ASSIGN CcbDDocu.CodCia = CcbCDocu.CodCia 
              CcbDDocu.Coddoc = CcbCDocu.Coddoc
              CcbDDocu.NroDoc = CcbCDocu.NroDoc 
              CcbDDocu.AlmDes = X-CODALM
              CcbDDocu.codmat = DETA.codmat 
              CcbDDocu.CanDes = DETA.CanDes 
              CcbDDocu.UndVta = DETA.UndVta
              CcbDDocu.Factor = DETA.Factor 
              CcbDDocu.Pesmat = DETA.Pesmat
              CcbDDocu.PreUni = DETA.PreUni 
              CcbDDocu.PreBas = DETA.PreBas 
              CcbDDocu.PorDto = DETA.PorDto 
              CcbDDocu.ImpLin = DETA.ImpLin 
              CcbDDocu.ImpIsc = DETA.ImpIsc 
              CcbDDocu.ImpIgv = DETA.ImpIgv 
              CcbDDocu.ImpDto = DETA.ImpDto 
              CcbDDocu.AftIsc = DETA.AftIsc 
              CcbDDocu.AftIgv = DETA.AftIgv
              CcbDDocu.FchDoc = TODAY
              CcbDDocu.CodDiv = CcbcDocu.CodDiv.
   END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Totales V-table-Win 
PROCEDURE Graba-Totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.

/*DO ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':*/
   CcbCdocu.ImpDto = 0.
   CcbCdocu.ImpIgv = 0.
   CcbCdocu.ImpIsc = 0.
   CcbCdocu.ImpTot = 0.
   CcbCdocu.ImpExo = 0.
   FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK: 
       CcbCdocu.ImpDto = CcbCdocu.ImpDto + Ccbddocu.ImpDto.
       F-Igv = F-Igv + Ccbddocu.ImpIgv.
       F-Isc = F-Isc + Ccbddocu.ImpIsc.
       CcbCdocu.ImpTot = CcbCdocu.ImpTot + Ccbddocu.ImpLin.
       IF NOT Ccbddocu.AftIgv THEN CcbCdocu.ImpExo = CcbCdocu.ImpExo + Ccbddocu.ImpLin.
   END.
   CcbCdocu.ImpIgv = ROUND(F-IGV,2).
   CcbCdocu.ImpIsc = ROUND(F-ISC,2).
   CcbCdocu.ImpBrt = CcbCdocu.ImpTot - CcbCdocu.ImpIgv - CcbCdocu.ImpIsc + 
                    CcbCdocu.ImpDto - CcbCdocu.ImpExo.
   CcbCdocu.ImpVta = CcbCdocu.ImpBrt - CcbCdocu.ImpDto.
   /*CcbCdocu.SdoAct = CcbCdocu.ImpTot.*/  /* Cero x no ser para cta cte */
   /*RELEASE CcbCdocu.*/
/*END.*/

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
  FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
       FacCorre.CodDoc = S-CODDOC AND
       FacCorre.NroSer = S-NROSER  NO-LOCK NO-ERROR.
  IF NOT AVAILABLE FacCorre OR FacCorre.FlgEst = NO THEN DO:
      MESSAGE 'Serie NO autorizada para hacer movimientos' VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  
  
  input-var-1 = "".
  /*RUN lkup\C-Guiman.r("Facturas Para Guias").*/
  RUN vtamay/B-FACTU.r.
  IF output-var-1 = ? THEN RETURN ERROR.
  S-CODCLI = output-var-3.
  L-CREA = YES.
  
  RUN Actualiza-Deta.
  RUN Procesa-Factura.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN ERROR.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */

  RUN Numero-de-Documento(NO).
  DO WITH FRAME {&FRAME-NAME}:
     FIND b-cdocu WHERE rowid(B-Cdocu) = output-var-1.
     DISPLAY TODAY @ CcbCDocu.FchDoc
             S-CODCLI @ CcbCDocu.CodCli
             STRING(I-NroSer,"999") + STRING(I-NroDoc,"999999") @ CcbCDocu.NroDoc 
             b-cdocu.CodVen @ CcbCDocu.CodVen
             FacCfgGn.Tpocmb[1] @ CcbCDocu.TpoCmb.
     FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA AND
          gn-clie.CodCli = S-CODCLI NO-LOCK NO-ERROR.
     F-NomVen = "".
     F-CndVta = "".
     /* RHC 01.09.09 */
     DISPLAY b-cdocu.NomCli @ FILL-IN_NomCli 
             b-cdocu.RucCli @ FILL-IN_RucCli 
             b-cdocu.DirCli @ FILL-IN_DirCli.
/*      IF gn-clie.CodCli = FacCfgGn.CliVar THEN DO: */
/*          DISPLAY b-cdocu.NomCli @ FILL-IN_NomCli  */
/*                  b-cdocu.RucCli @ FILL-IN_RucCli  */
/*                  b-cdocu.DirCli @ FILL-IN_DirCli. */
/*      END.                                         */
    
     FIND gn-ven WHERE 
          gn-ven.CodCia = S-CODCIA AND  
          gn-ven.CodVen = b-cdocu.CodVen 
          NO-LOCK NO-ERROR.
     IF AVAILABLE gn-ven THEN F-NomVen = gn-ven.NomVen.
     FIND gn-convt WHERE gn-convt.Codig = b-cdocu.FmaPgo NO-LOCK NO-ERROR.
     IF AVAILABLE gn-convt THEN F-CndVta = gn-convt.Nombr.                                                      
     DISPLAY F-NomVen F-CndVta.
/*      IF AVAILABLE gn-clie THEN DO:                */
/*         DISPLAY gn-clie.NomCli @ FILL-IN_NomCli   */
/*                 gn-clie.Ruc    @ FILL-IN_RucCli   */
/*                 gn-clie.DirCli @ FILL-IN_DirCli.  */
/*      END.                                         */
/*      IF gn-clie.CodCli = FacCfgGn.CliVar THEN DO: */
/*          DISPLAY b-cdocu.NomCli @ FILL-IN_NomCli  */
/*                  b-cdocu.RucCli @ FILL-IN_RucCli  */
/*                  b-cdocu.DirCli @ FILL-IN_DirCli. */
/*      END.                                         */
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
  
  IF L-CREA THEN DO WITH FRAME {&FRAME-NAME}:
     RUN Numero-de-Documento(YES).
     IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

     FIND CURRENT b-cdocu EXCLUSIVE-LOCK NO-ERROR.
     IF NOT AVAILABLE b-cdocu THEN UNDO, RETURN 'ADM-ERROR'.

     
     S-CODALM = B-Cdocu.CodAlm.
     ASSIGN CcbCDocu.CodCia = S-CODCIA
/*             CcbCDocu.CodAlm = X-CODALM */
            CcbCDocu.CodAlm = s-CodAlm
            CcbCDocu.CodDiv = S-CODDIV
            CcbCDocu.CodDoc = S-CODDOC
            CcbCDocu.Nomcli = FILL-IN_NomCli:screen-value
            CcbCDocu.Dircli = FILL-IN_DirCli:screen-value
            CcbCDocu.Ruccli = FILL-IN_RucCli:screen-value
            CcbCDocu.NroDoc = STRING(I-NroSer,"999") + STRING(I-NroDoc,"999999") 
            CcbCDocu.FchDoc = TODAY 
            CcbCDocu.CodMov = S-CODMOV 
            /*CcbCDocu.CodPed = C-CODPED */
            CcbCDocu.CodPed = b-cdocu.codped
            CcbCDocu.NroPed = b-cdocu.nroped 
            CcbCDocu.Nroord = b-cdocu.nroord 
            CcbCDocu.Tipo   = "OFICINA"
            CcbCDocu.FchVto = TODAY 
            CcbCDocu.CodCli = b-cdocu.CodCli
            CcbCDocu.CodVen = b-cdocu.CodVen
            CcbCDocu.CodRef = b-cdocu.CodDoc
            CcbCDocu.Nroref = b-cdocu.Nrodoc
            CcbCDocu.TipVta = "2"
            CcbCDocu.TpoFac = "M"       /*  Guia Manual */
            CcbCDocu.FmaPgo = b-cdocu.FmaPgo
            CcbCDocu.CodMon = b-cdocu.CodMon
            CcbCDocu.TpoCmb = b-cdocu.TpoCmb 
            CcbCDocu.PorIgv = b-cdocu.PorIgv
            CcbCDocu.FlgEst = "F"
            CcbCDocu.FlgSit = "P"
         /* CcbCDocu.FlgAte = "P" */
            CcbCDocu.usuario = S-USER-ID
            CcbCDocu.HorCie = string(time,'hh:mm:ss').
     FIND gn-convt WHERE gn-convt.Codig = CcbCDocu.FmaPgo NO-LOCK NO-ERROR.
     IF AVAILABLE gn-convt THEN 
        CcbCDocu.TipVta = IF gn-ConVt.TotDias = 0 THEN "1" ELSE "2".
        CcbCDocu.FchVto = CcbCDocu.FchDoc + INTEGER(ENTRY(NUM-ENTRIES(gn-ConVt.Vencmtos),gn-ConVt.Vencmtos)).
     FIND gn-clie WHERE 
          gn-clie.CodCia = CL-CODCIA AND
          gn-clie.CodCli = CcbCDocu.CodCli 
          NO-LOCK NO-ERROR.
     IF AVAILABLE gn-clie  THEN DO:
        ASSIGN CcbCDocu.CodDpto = gn-clie.CodDept 
               CcbCDocu.CodProv = gn-clie.CodProv 
               CcbCDocu.CodDist = gn-clie.CodDist.
     END.
     ASSIGN
         b-cdocu.codref = Ccbcdocu.coddoc
         b-cdocu.nroref = Ccbcdocu.nrodoc.
  END.
  RUN Genera-Guia.    /* Detalle de la Guia */ 
  /*RUN Actualiza-Pedido(1).*/
  
  RUN Graba-Totales.
  IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
  
  /*RUN Cierra-Pedido.*/
  
  /* descargamos de almacen */
  /*RUN VTA\act_alm.r(ROWID(CcbCDocu)).*/

  RELEASE b-cdocu.
  
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
  /* Code placed here will execute PRIOR to standard behavior. */
   DEFINE VAR RPTA AS CHAR.
   
   IF CcbCDocu.FlgEst = "A" THEN DO:
      MESSAGE 'El documento se enuentra anulado...' VIEW-AS ALERT-BOX.
      RETURN 'ADM-ERROR'.
   END.
   FIND Almacen WHERE 
        Almacen.CodCia = S-CODCIA AND
        Almacen.CodAlm = S-CODALM NO-LOCK NO-ERROR.
   RUN vtamay/g-CLAVE.R(Almacen.Clave,OUTPUT RPTA).
   IF RPTA = "ERROR" THEN RETURN "ADM-ERROR".
   
   /* consistencia de la fecha del cierre del sistema */
   DEF VAR dFchCie AS DATE.
   RUN gn/fecha-de-cierre (OUTPUT dFchCie).
   IF ccbcdocu.fchdoc <= dFchCie THEN DO:
       MESSAGE 'NO se puede anular ningun documento antes del' (dFchCie + 1)
           VIEW-AS ALERT-BOX WARNING.
       RETURN 'ADM-ERROR'.
   END.
   /* fin de consistencia */
   
   DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
       /* Motivo de anulacion */
       DEF VAR cReturnValue AS CHAR NO-UNDO.
       RUN ccb/d-motanu (ccbcdocu.codcia, ccbcdocu.coddoc, ccbcdocu.nrodoc, s-user-id, OUTPUT cReturnValue).
       IF cReturnValue = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
       /* ******************* */
      /*RUN Actualiza-Pedido(-1).*/
      
      RUN Borra-Guia.
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      
      /* ACTUALIZAMOS STOCK DE ALMACEN */
      /*RUN VTA\des_alm.r(ROWID(CcbCDocu)).*/
      
      FIND B-CDOCU WHERE ROWID(B-CDOCU) = ROWID(CcbCDocu) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF NOT AVAILABLE B-CDOCU
      THEN DO:
        RUN dispatch IN THIS-PROCEDURE ('show-errors':U).
        UNDO, RETURN 'ADM-ERROR'.
      END.
      ASSIGN B-CDOCU.FlgEst = "A"
                B-CDOCU.SdoAct = 0
                B-CDOCU.Glosa  = "A N U L A D O"
                B-CDOCU.FchAnu = TODAY
                B-CDOCU.Usuanu = S-USER-ID. 
      RELEASE B-CDOCU.
      /*FIND FacCPedi WHERE 
           FacCPedi.CodCia = CcbCDocu.CodCia AND
           FacCPedi.CodDoc = CcbCDocu.Codped AND
           FacCPedi.NroPed = CcbCDocu.NroPed EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FacCPedi THEN ASSIGN FacCPedi.FlgEst = "P".
      RELEASE FacCPedi.*/
   END.
   RUN Procesa-Handle IN lh_Handle ('browse'). 
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

  /* Dispatch standard ADM method.     
                                  */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
    
  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE CcbCDocu THEN DO WITH FRAME {&FRAME-NAME}:
     FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA AND
          gn-clie.CodCli = CcbCDocu.CodCli NO-LOCK NO-ERROR.
     DISPLAY
         ccbcdocu.nomcli @ FILL-IN_NomCli
         ccbcdocu.ruccli @ FILL-IN_RucCli
         ccbcdocu.dircli @ FILL-IN_DirCli.
    
    DISPLAY  ccbcdocu.nroped @ FILL-IN-pedido.
       
     CASE CcbCDocu.FlgEst:
         WHEN "A" THEN DISPLAY "ANULADO" @ F-Estado WITH FRAME {&FRAME-NAME}.
         WHEN "F" THEN DISPLAY "FACTURADO" @ F-Estado WITH FRAME {&FRAME-NAME}. 
         WHEN "X" THEN DISPLAY "POR CHEQUEAR" @ F-Estado WITH FRAME {&FRAME-NAME}.
         WHEN "P" THEN DISPLAY "PENDIENTE" @ F-Estado WITH FRAME {&FRAME-NAME}. 
     END CASE.         

     FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA AND 
          gn-ven.CodVen = CcbCDocu.CodVen NO-LOCK NO-ERROR.
     IF AVAILABLE gn-ven THEN F-NomVen:screen-value = gn-ven.NomVen.
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
  IF Ccbcdocu.FlgEst = 'A' THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'imprime':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF Ccbcdocu.flgest = 'X' THEN DO:
      MESSAGE 'NO se puede imprimir la gu�a' SKIP
              'A�n no se ha chequeado' VIEW-AS ALERT-BOX ERROR.
      RETURN.
  END.
  IF Ccbcdocu.flgest = 'F' THEN DO:
      CASE Ccbcdocu.coddiv:
          WHEN '00026' THEN RUN vtamay/r-impgma-00026 (ROWID(CcbCDocu)).
          OTHERWISE RUN vtamay/r-impgma-03 (ROWID(CcbCDocu)).
      END CASE.
  END.
      

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
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  
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
  IF L-INCREMENTA THEN
          
     FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
          FacCorre.CodDoc = S-CODDOC AND
          /*FacCorre.CodDiv = S-CODDIV AND*/
          FacCorre.NroSer = S-NROSER  EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
  
  ELSE
     FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
          FacCorre.CodDoc = S-CODDOC AND
          /*FacCorre.CodDiv = S-CODDIV AND*/
          FacCorre.NroSer = S-NROSER  NO-LOCK NO-ERROR.
  IF NOT AVAILABLE FacCorre
  THEN DO:
    RUN dispatch IN THIS-PROCEDURE ('show-errors':U).
    RETURN 'ADM-ERROR'.
  END.
  ASSIGN I-NroDoc = FacCorre.Correlativo.
  IF L-INCREMENTA THEN ASSIGN FacCorre.Correlativo = FacCorre.Correlativo + 1.
  I-NROSER = FacCorre.NroSer.
  S-CODALM = FacCorre.CodAlm.
  RELEASE FacCorre.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Factura V-table-Win 
PROCEDURE Procesa-Factura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE C-RETORNO AS CHAR INIT "OK" NO-UNDO.


FIND B-CDOCU WHERE ROWID(B-CDOCU) = output-var-1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CDOCU THEN RETURN "ADM-ERROR".

FOR EACH CcbDdocu OF B-CDOCU NO-LOCK:   /* FAC o BOL */
    CREATE DETA.
    BUFFER-COPY Ccbddocu TO DETA.
END.
RETURN C-RETORNO.

END PROCEDURE.

/*
FIND B-CDOCU WHERE ROWID(B-CDOCU) = output-var-1 NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CDOCU THEN RETURN "ADM-ERROR".

FOR EACH CcbDdocu OF B-CDOCU NO-LOCK :
    CREATE DETA.
    ASSIGN DETA.CodCia = S-CODCIA
           DETA.NroItm = CcbDdocu.NroItm
           DETA.codmat = CcbDdocu.CodMat
           DETA.CanDes = CcbDdocu.CanDes
           DETA.CanDev = CcbDdocu.CanDes
           DETA.UndVta = CcbDdocu.UndVta
           DETA.Factor = CcbDdocu.Factor
           DETA.PreUni = CcbDdocu.PreUni 
           DETA.PreBas = CcbDdocu.PreBas 
           DETA.PorDto = CcbDdocu.PorDto 
           DETA.ImpDto = CcbDdocu.ImpDto 
           DETA.AftIsc = CcbDdocu.AftIsc 
           DETA.AftIgv = CcbDdocu.AftIgv
           DETA.ImpDto = ROUND( DETA.PreBas * (DETA.PorDto / 100) * DETA.CanDes , 2 )
           DETA.ImpLin = ROUND( DETA.PreUni * DETA.CanDes , 2 ).
    IF DETA.AftIgv THEN 
       DETA.ImpIgv = DETA.ImpLin - ROUND(DETA.ImpLin / (1 + (FacCfgGn.PorIgv / 100)),4).
    FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
                   AND  Almmmatg.codmat = CcbDdocu.CodMat 
                  NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmatg AND DETA.AftIsc THEN 
       ASSIGN
          DETA.ImpIsc = ROUND(DETA.PreBas * DETA.CanDes * Almmmatg.PorIsc / 100,2).
    IF AVAILABLE Almmmatg THEN
       ASSIGN 
          DETA.Pesmat = Almmmatg.Pesmat * (DETA.Candes * DETA.Factor).
END.
RETURN C-RETORNO.
*/

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
  IF p-state = 'update-begin':U THEN DO:
     L-CREA = NO.
     RUN Actualiza-Deta.
     RUN Procesa-Handle IN lh_Handle ('Pagina2').
     RUN Procesa-Handle IN lh_Handle ('browse').
  END.  
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
DEFINE VARIABLE X-ITEMS AS INTEGER INIT 0.
DEFINE VARIABLE I-ITEMS AS DECIMAL NO-UNDO.
DO WITH FRAME {&FRAME-NAME} :
/*    FIND GN-PROV WHERE GN-PROV.CodPro = CcbcDocu.CodAge:screen-value NO-LOCK NO-ERROR. */
/*    IF NOT AVAILABLE GN-PROV THEN DO:                                                  */
/*       MESSAGE "Codigo de transportista no existe" VIEW-AS ALERT-BOX ERROR.            */
/*       APPLY "ENTRY" TO CcbCDocu.CodAge.                                               */
/*       RETURN "ADM-ERROR".                                                             */
/*    END.                                                                               */
   X-ITEMS = 0.
   FOR EACH DETA NO-LOCK:
       I-ITEMS = I-ITEMS + DETA.CanDes.
       X-ITEMS = X-ITEMS + 1.
   END.
   IF I-ITEMS = 0 THEN DO:
      MESSAGE "No hay items por despachar" VIEW-AS ALERT-BOX ERROR.
/*       APPLY "ENTRY" TO CcbCDocu.CodAge. */
      RETURN "ADM-ERROR".   
   END.
   
 

    
    FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.
    IF X-ITEMS >  FacCfgGn.Items_Guias THEN DO:
     MESSAGE "Numero de Items Mayor al Configurado para el Tipode Documento " VIEW-AS ALERT-BOX INFORMATION.
     RETURN "ADM-ERROR".
    END. 

/*   FIND AlmCierr WHERE 
 *         AlmCierr.CodCia = S-CODCIA AND 
 *         AlmCierr.FchCie = INPUT CcbCDocu.FchDoc 
 *         NO-LOCK NO-ERROR.
 *    IF AVAILABLE AlmCierr AND
 *       AlmCierr.FlgCie THEN DO:
 *       MESSAGE "Este dia " AlmCierr.FchCie " se encuentra cerrado" SKIP 
 *               "Consulte con sistemas " VIEW-AS ALERT-BOX INFORMATION.
 *       RETURN "ADM-ERROR".
 *    END.*/

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
  /* consistencia de la fecha del cierre del sistema */
  DEF VAR dFchCie AS DATE.
  RUN gn/fecha-de-cierre (OUTPUT dFchCie).
  IF ccbcdocu.fchdoc <= dFchCie THEN DO:
      MESSAGE 'NO se puede anular/modificar ningun documento antes del' (dFchCie + 1)
          VIEW-AS ALERT-BOX WARNING.
      RETURN 'ADM-ERROR'.
  END.
  /* fin de consistencia */

  RETURN "ADM-ERROR".
IF NOT AVAILABLE CcbCDocu THEN RETURN "ADM-ERROR".
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

