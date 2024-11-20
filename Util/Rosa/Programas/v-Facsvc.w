&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE Deta LIKE INTEGRAL.CcbDDocu.


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

/* Local Variable Definitions ---                                       */

DEFINE SHARED VAR s-codcia AS INT.
DEFINE SHARED VAR s-CodDiv AS CHAR.
DEFINE SHARED VAR s-CodDoc AS CHAR.
DEFINE SHARED VAR s-NroSer AS INT.
DEFINE SHARED VAR s-CodRef AS CHAR.
DEFINE BUFFER B-Docu FOR CcbCDocu. /*Buffer de esta tabla*/
DEFINE SHARED VAR Lh_Handle AS Handle.
DEFINE SHARED VAR s-user-id AS CHAR.
DEFINE SHARED VAR s-TpoFac AS CHAR.

FIND FacCfgGn WHERE
    FacCfgGn.CodCia = s-codcia NO-LOCK.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES CcbCDocu
&Scoped-define FIRST-EXTERNAL-TABLE CcbCDocu


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR CcbCDocu.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS CcbCDocu.NroDoc CcbCDocu.CodCli ~
CcbCDocu.NomCli CcbCDocu.DirCli CcbCDocu.RucCli CcbCDocu.CodVen ~
CcbCDocu.FmaPgo CcbCDocu.NroRef CcbCDocu.Glosa CcbCDocu.FchDoc ~
CcbCDocu.TpoCmb CcbCDocu.FchVto CcbCDocu.FchAnu CcbCDocu.CodMon ~
CcbCDocu.usuario CcbCDocu.UsuAnu 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}NroDoc ~{&FP2}NroDoc ~{&FP3}~
 ~{&FP1}CodCli ~{&FP2}CodCli ~{&FP3}~
 ~{&FP1}NomCli ~{&FP2}NomCli ~{&FP3}~
 ~{&FP1}DirCli ~{&FP2}DirCli ~{&FP3}~
 ~{&FP1}RucCli ~{&FP2}RucCli ~{&FP3}~
 ~{&FP1}CodVen ~{&FP2}CodVen ~{&FP3}~
 ~{&FP1}FmaPgo ~{&FP2}FmaPgo ~{&FP3}~
 ~{&FP1}NroRef ~{&FP2}NroRef ~{&FP3}~
 ~{&FP1}Glosa ~{&FP2}Glosa ~{&FP3}~
 ~{&FP1}FchDoc ~{&FP2}FchDoc ~{&FP3}~
 ~{&FP1}TpoCmb ~{&FP2}TpoCmb ~{&FP3}~
 ~{&FP1}FchVto ~{&FP2}FchVto ~{&FP3}~
 ~{&FP1}FchAnu ~{&FP2}FchAnu ~{&FP3}~
 ~{&FP1}usuario ~{&FP2}usuario ~{&FP3}~
 ~{&FP1}UsuAnu ~{&FP2}UsuAnu ~{&FP3}
&Scoped-define ENABLED-TABLES CcbCDocu
&Scoped-define FIRST-ENABLED-TABLE CcbCDocu
&Scoped-Define DISPLAYED-FIELDS CcbCDocu.NroDoc CcbCDocu.CodCli ~
CcbCDocu.NomCli CcbCDocu.DirCli CcbCDocu.RucCli CcbCDocu.CodVen ~
CcbCDocu.FmaPgo CcbCDocu.NroRef CcbCDocu.Glosa CcbCDocu.FchDoc ~
CcbCDocu.TpoCmb CcbCDocu.FchVto CcbCDocu.FchAnu CcbCDocu.CodMon ~
CcbCDocu.usuario CcbCDocu.UsuAnu 
&Scoped-Define DISPLAYED-OBJECTS x-estado x-NomVen x-despgo 

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
DEFINE VARIABLE x-despgo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE x-estado AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 15 FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE x-NomVen AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CcbCDocu.NroDoc AT ROW 1.19 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     CcbCDocu.CodCli AT ROW 1.96 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     CcbCDocu.NomCli AT ROW 2.73 COL 16 COLON-ALIGNED
          LABEL "Nombre Cliente"
          VIEW-AS FILL-IN 
          SIZE 33 BY .81
     CcbCDocu.DirCli AT ROW 3.5 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 48 BY .81
     CcbCDocu.RucCli AT ROW 4.27 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     CcbCDocu.CodVen AT ROW 5.04 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
     CcbCDocu.FmaPgo AT ROW 5.81 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
     CcbCDocu.NroRef AT ROW 6.58 COL 16 COLON-ALIGNED
          LABEL "Numero Ref."
          VIEW-AS FILL-IN 
          SIZE 9 BY .81
     CcbCDocu.Glosa AT ROW 7.35 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 48 BY .81
     x-estado AT ROW 1.19 COL 27 COLON-ALIGNED NO-LABEL
     x-NomVen AT ROW 5.04 COL 25 COLON-ALIGNED NO-LABEL
     x-despgo AT ROW 5.81 COL 25 COLON-ALIGNED NO-LABEL
     CcbCDocu.FchDoc AT ROW 1.19 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.TpoCmb AT ROW 1.96 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.FchVto AT ROW 2.73 COL 91 COLON-ALIGNED
          LABEL "Fecha de Vto."
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.FchAnu AT ROW 3.5 COL 91 COLON-ALIGNED
          LABEL "Fecha Anulación"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.CodMon AT ROW 4.27 COL 93 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "S/.", 1,
"US$", 2
          SIZE 12 BY .81
     CcbCDocu.usuario AT ROW 5.04 COL 91 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     CcbCDocu.UsuAnu AT ROW 6 COL 91 COLON-ALIGNED
          LABEL "Usuario Anulación"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     "Moneda:" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 4.46 COL 85
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: INTEGRAL.CcbCDocu
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: Deta T "SHARED" ? INTEGRAL CcbDDocu
   END-TABLES.
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
         HEIGHT             = 7.58
         WIDTH              = 107.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN CcbCDocu.FchAnu IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.FchVto IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.NomCli IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.NroRef IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.UsuAnu IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN x-despgo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-NomVen IN FRAME F-Main
   NO-ENABLE                                                            */
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

{src/adm-vm/method/vmviewer.i}
{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Temporal V-table-Win 
PROCEDURE Borra-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Deta:
    DELETE Deta.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargar-Temporal V-table-Win 
PROCEDURE Cargar-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN Borra-Temporal.

FOR EACH CcbDDocu OF B-Docu NO-LOCK:
    CREATE Deta.
    BUFFER-COPY CcbDDocu TO Deta.
END.


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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Detalle V-table-Win 
PROCEDURE Graba-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*Grabando Detalle*/
    FOR EACH DETA:
        CREATE CcbDDocu.
        BUFFER-COPY Deta TO CcbDDocu
            ASSIGN 
              CcbDDocu.CodCia = CcbCDocu.CodCia
              CcbDDocu.CodDoc = CcbCDocu.CodDoc
              CcbDDocu.NroDoc = CcbCDocu.NroDoc.
    END.
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
  ASSIGN
    CcbCDocu.ImpBrt = 0
    CcbCDocu.ImpDto = 0
    CcbCDocu.ImpExo = 0
    CcbCDocu.ImpFle = 0
    CcbCDocu.ImpIgv = 0
    CcbCDocu.ImpIsc = 0
    CcbCDocu.ImpTot = 0
    CcbCDocu.ImpVta = 0.
  FOR EACH CcbDDocu OF CcbCDocu NO-LOCK:
    CcbCDocu.impigv = CcbCDocu.impigv + CcbDDocu.impigv.
    CcbCDocu.impisc = CcbCDocu.impisc + CcbDDocu.impisc.
    CcbCDocu.ImpTot = CcbCDocu.ImpTot + CcbDDocu.ImpLin.
    IF NOT CcbDDocu.AftIgv THEN CcbCDocu.ImpExo = CcbCDocu.ImpExo + CcbDDocu.ImpLin.
    IF CcbDDocu.AftIgv = YES
    THEN CcbCDocu.ImpDto = CcbCDocu.ImpDto + ROUND(CcbDDocu.ImpDto / (1 + CcbCDocu.PorIgv / 100), 2).
        ELSE CcbCDocu.ImpDto = CcbCDocu.ImpDto + CcbDDocu.ImpDto.
  END.
  CcbCDocu.impvta = CcbCDocu.imptot - CcbCDocu.impexo - CcbCDocu.impigv.
  IF CcbCDocu.PorDto > 0 THEN DO:
    ASSIGN
        CcbCDocu.ImpDto = CcbCDocu.ImpDto + ROUND((CcbCDocu.ImpVta + CcbCDocu.ImpExo) * CcbCDocu.PorDto / 100, 2)
        CcbCDocu.ImpTot = ROUND(CcbCDocu.ImpTot * (1 - CcbCDocu.PorDto / 100),2)
        CcbCDocu.ImpVta = ROUND(CcbCDocu.ImpVta * (1 - CcbCDocu.PorDto / 100),2)
        CcbCDocu.ImpExo = ROUND(CcbCDocu.ImpExo * (1 - CcbCDocu.PorDto / 100),2)
        CcbCDocu.ImpIgv = CcbCDocu.ImpTot - CcbCDocu.ImpExo - CcbCDocu.ImpVta.
  END.
  CcbCDocu.ImpBrt = CcbCDocu.ImpVta + CcbCDocu.ImpIsc + CcbCDocu.ImpDto + CcbCDocu.ImpExo.
  
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
    DEFINE VARIABLE p-NroRef LIKE CcbCDocu.NroRef.

    RUN Util/Rosa/d-grpend(OUTPUT p-NroRef).
    IF p-NroRef = "ADM-ERROR"
    THEN RETURN "ADM-ERROR".
    
    FIND FacCorre WHERE 
        FacCorre.CodCia = s-codcia AND
        FacCorre.CodDiv = s-CodDiv AND
        FacCorre.CodDoc = s-CodDoc AND
        FacCorre.NroSer = s-NroSer
        NO-LOCK NO-ERROR.
    IF FacCorre.FlgEst = NO THEN DO:
        MESSAGE "Serie Inactiva" VIEW-AS ALERT-BOX INFORMATION.
        RETURN "ADM-ERROR".
    END.   
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  
  DO WITH FRAME {&FRAME-NAME}:
    FIND B-Docu WHERE
        B-Docu.CodCia = s-codcia AND
        B-Docu.CodDiv = s-CodDiv AND
        B-Docu.CodDoc = s-CodRef AND
        B-Docu.NroDoc = p-NroRef
        NO-LOCK.
        FIND Gn-ConVt WHERE
            Gn-ConVt.Codig = B-Docu.FmaPgo
            NO-LOCK.
        FIND Gn-Ven WHERE
            Gn-Ven.CodCia = s-codcia AND
            Gn-Ven.CodVen = B-Docu.CodVen
            NO-LOCK.
            
    DISPLAY
    STRING (FacCorre.NroSer,'999') + STRING (FacCorre.Correlativo, '999999') @ CcbCDocu.NroDoc
        Today @ CcbCDocu.FchDoc
        Today @ CcbCDocu.FchVto
        B-Docu.CodCli @ CcbcDocu.CodCli
        B-Docu.NomCli @ CcbcDocu.NomCli
        B-Docu.DirCli @ CcbcDocu.DirCli
        B-Docu.RucCli @ CcbcDocu.RucCli
        B-Docu.CodVen @ CcbcDocu.CodVen
        Gn-Ven.NomVen @ x-NomVen
        B-Docu.FmaPgo @ CcbcDocu.FmaPgo
        B-Docu.NroDoc @ CcbcDocu.NroRef 
        B-Docu.usuario @ CcbCDocu.usuario
        Today + Gn-ConVt.TotDias @ CcbcDocu.FchVto 
        Gn-ConVt.Nombr @ x-despgo
        FacCfgGn.TpoCmb[1] @ CcbCDocu.TpoCmb
    WITH FRAME {&FRAME-NAME}.
    RUN Cargar-Temporal.
    ASSIGN
    CcbCDocu.CodMon:SCREEN-VALUE = STRING (B-Docu.CodMon, '9').
    
  END.  
  
  RUN Procesa-Handle IN Lh_Handle("Pagina2").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  /*Control de correlativo*/

   FIND FacCorre WHERE 
    FacCorre.CodCia = s-codcia AND
    FacCorre.CodDiv = s-CodDiv AND
    FacCorre.CodDoc = s-CodDoc AND
    FacCorre.NroSer = s-NroSer
    EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE FacCorre THEN UNDO, RETURN "ADM-ERROR".
   
   FIND B-Docu WHERE B-Docu.CodCia = s-CodCia AND
        B-Docu.CodDoc = s-CodRef AND
        B-Docu.NroDoc = CcbCDocu.NroRef:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE B-Docu OR B-Docu.FlgEst <> 'P'
   THEN DO:
      MESSAGE "G/R no válida" VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN "ADM-ERROR".
   END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
    CcbCDocu.CodCia = s-CodCia
    CcbCDocu.CodDiv = s-CodDiv
    CcbCDocu.CodDoc = s-CodDoc
    CcbCDocu.TpoFac = s-TpoFac
    CcbCDocu.CodRef = s-CodRef
    CcbCDocu.NroDoc = STRING (FacCorre.NroSer,'999') + STRING (FacCorre.Correlativo,'999999')
    FacCorre.Correlativo = FacCorre.Correlativo + 1
    B-Docu.FlgEst = 'F'
    CcbCDocu.usuario = s-user-id
    B-Docu.CodRef = s-coddoc
    CcbCDocu.FlgEst = 'P'
    CcbCDocu.CodRef = B-Docu.CodDoc.

    RUN Graba-Detalle.
    RUN Graba-Totales.
    ASSIGN 
        CcbCDocu.SdoAct = CcbCDocu.ImpTot.
    RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

    RELEASE B-Docu.
    RELEASE CcbDDocu.
    RELEASE FacCorre.

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

    RUN Procesa-Handle IN Lh_Handle("Pagina1").

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

  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .*/

  /* Code placed here will execute AFTER standard behavior.    */
  
  IF CcbCDocu.FlgEst <> 'P' THEN DO:
    MESSAGE "Acceso Denegado" VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
  END.
  IF CcbCDocu.ImpTot <> CcbCDocu.SdoAct THEN DO:
    MESSAGE "Documento presenta amortizaciones" VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
  END.
  
  DO TRANSACTION ON STOP UNDO, RETURN "ADM-ERROR" 
                 ON ERROR UNDO, RETURN "ADM-ERROR":
      FIND CURRENT CcbCDocu EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE CcbCDocu THEN UNDO, RETURN "ADM-ERROR".
         ASSIGN 
           CcbCDocu.FlgEst = 'A'
           CcbCDocu.UsuAnu = s-user-id
           CcbCDocu.FchAnu = TODAY.
         FIND B-Docu WHERE 
           B-Docu.CodCia = CcbCDocu.CodCia AND
           B-Docu.CodDoc = CcbCDocu.CodRef AND
           B-Docu.NroDoc = CcbCDocu.NroRef
           EXCLUSIVE-LOCK NO-ERROR.  
      ASSIGN
        B-Docu.FlgEst = 'P'
        B-Docu.CodRef = ''
        B-Docu.NroRef = ''.
      RELEASE B-Docu.
      FIND CURRENT CcbCDocu NO-LOCK.
  END.
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
  
  IF AVAILABLE CcbCDocu THEN /*Para asegurar que el puntero este en la bd*/
  DO WITH FRAME {&FRAME-NAME}:
    Case CcbCDocu.FlgEst:
        WHEN 'P' THEN x-estado:SCREEN-VALUE = "Pendiente".
        WHEN 'A' THEN x-estado:SCREEN-VALUE = "Anulado".
        WHEN 'C' THEN x-estado:SCREEN-VALUE = "Cancelado".
        OTHERWISE x-estado:SCREEN-VALUE = "????".
    End Case.    
    FIND Gn-Ven NO-LOCK WHERE 
         Gn-Ven.CodCia = s-codcia AND 
         Gn-Ven.CodVen = CcbCDocu.CodVen.
  
    IF AVAILABLE Gn-Ven THEN
        x-NomVen:SCREEN-VALUE = Gn-Ven.NomVen.
     ELSE x-NomVen:SCREEN-VALUE = "".
    
    FIND Gn-ConVt WHERE
       Gn-ConVt.Codig = CcbcDocu.FmaPgo
       NO-LOCK.
    x-despgo:SCREEN-VALUE = IF AVAILABLE Gn-ConVt THEN Gn-ConVt.Nombr ELSE "".  
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
        CcbcDocu.NroDoc:SENSITIVE = NO
        CcbcDocu.CodCli:SENSITIVE = NO
        CcbcDocu.NomCli:SENSITIVE = NO
        CcbcDocu.DirCli:SENSITIVE = NO
        CcbcDocu.RucCli:SENSITIVE = NO
        CcbcDocu.CodVen:SENSITIVE = NO
        x-NomVen:SENSITIVE = NO
        CcbcDocu.FmaPgo:SENSITIVE = NO
        x-despgo:SENSITIVE = NO
        CcbcDocu.NroRef:SENSITIVE = NO
        CcbcDocu.FchDoc:SENSITIVE = NO
        CcbcDocu.TpoCmb:SENSITIVE = NO
        CcbcDocu.FchAnu:SENSITIVE = NO
        CcbcDocu.CodMon:SENSITIVE = NO
        CcbcDocu.usuario:SENSITIVE = NO
        CcbcDocu.UsuAnu:SENSITIVE = NO.

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
    
   RUN Procesa-Handle IN Lh_Handle("Pagina1").
 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
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
   /* IF CAMPO:SCREEN-VALUE = "" THEN DO:
         MESSAGE "Campo no debe ser blanco"
         VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO CAMPO.
         RETURN "ADM-ERROR".   
   
      END.
   */

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

RETURN "ADM-ERROR".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


