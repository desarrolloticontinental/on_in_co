&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
DEFINE SHARED VARIABLE S-NOMCIA   AS CHAR.
DEFINE SHARED VARIABLE S-USER-ID  AS CHAR.
DEFINE SHARED VARIABLE S-CODDOC   AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV   AS CHAR.
DEFINE SHARED TEMP-TABLE DETA LIKE CcbDDocu.
DEFINE SHARED VARIABLE S-CODMON   AS INTEGER.

DEFINE SHARED VARIABLE S-PORDTO AS DEC.
DEFINE SHARED VARIABLE S-PORIGV AS INT.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE L-CREA         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE I-NROSER       AS INTEGER   NO-UNDO.
DEFINE VARIABLE I-NRODOC       AS INTEGER   NO-UNDO.
DEFINE VARIABLE R-NRODEV       AS ROWID     NO-UNDO.
DEFINE VARIABLE S-PRINTER-NAME AS CHARACTER NO-UNDO.
DEFINE VARIABLE S-CODCLI       AS CHARACTER NO-UNDO. 
DEFINE VARIABLE R-ROWID        AS ROWID     NO-UNDO. 


DEFINE BUFFER B-CDOCU FOR CcbCDocu.

FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
     FacCorre.CodDoc = S-CODDOC AND
     FacCorre.CodDiv = S-CODDIV NO-LOCK NO-ERROR.
IF AVAILABLE FacCorre THEN 
   ASSIGN I-NroSer = FacCorre.NroSer.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

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
&Scoped-Define ENABLED-FIELDS CcbCDocu.NroDoc CcbCDocu.CodRef ~
CcbCDocu.NroRef CcbCDocu.NroOrd CcbCDocu.FchDoc CcbCDocu.TpoCmb ~
CcbCDocu.CodMon CcbCDocu.ImpTot CcbCDocu.ImpDto CcbCDocu.Glosa 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}NroDoc ~{&FP2}NroDoc ~{&FP3}~
 ~{&FP1}CodRef ~{&FP2}CodRef ~{&FP3}~
 ~{&FP1}NroRef ~{&FP2}NroRef ~{&FP3}~
 ~{&FP1}NroOrd ~{&FP2}NroOrd ~{&FP3}~
 ~{&FP1}FchDoc ~{&FP2}FchDoc ~{&FP3}~
 ~{&FP1}TpoCmb ~{&FP2}TpoCmb ~{&FP3}~
 ~{&FP1}ImpTot ~{&FP2}ImpTot ~{&FP3}~
 ~{&FP1}ImpDto ~{&FP2}ImpDto ~{&FP3}~
 ~{&FP1}Glosa ~{&FP2}Glosa ~{&FP3}
&Scoped-define ENABLED-TABLES CcbCDocu
&Scoped-define FIRST-ENABLED-TABLE CcbCDocu
&Scoped-Define ENABLED-OBJECTS RECT-19 RECT-29 
&Scoped-Define DISPLAYED-FIELDS CcbCDocu.NroDoc CcbCDocu.CodRef ~
CcbCDocu.NroRef CcbCDocu.NroOrd CcbCDocu.FchDoc CcbCDocu.TpoCmb ~
CcbCDocu.CodMon CcbCDocu.ImpTot CcbCDocu.ImpDto CcbCDocu.CodCli ~
CcbCDocu.ImpBrt CcbCDocu.Glosa CcbCDocu.ImpExo CcbCDocu.ImpVta ~
CcbCDocu.ImpIsc CcbCDocu.ImpIgv 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_DirCli FILL-IN_NomCli ~
FILL-IN_RucCli 

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
DEFINE VARIABLE FILL-IN_DirCli AS CHARACTER FORMAT "x(60)" 
     LABEL "Direccion" 
     VIEW-AS FILL-IN 
     SIZE 44.72 BY .69.

DEFINE VARIABLE FILL-IN_NomCli AS CHARACTER FORMAT "x(50)" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .69.

DEFINE VARIABLE FILL-IN_RucCli AS CHARACTER FORMAT "x(11)" 
     LABEL "Ruc" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 80.43 BY 5.96.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18.43 BY .92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CcbCDocu.NroDoc AT ROW 1.12 COL 12 COLON-ALIGNED FORMAT "XXXXXXXXX"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .69
          FONT 0
     CcbCDocu.CodRef AT ROW 3.85 COL 6.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.29 BY .69
     CcbCDocu.NroRef AT ROW 3.85 COL 12 COLON-ALIGNED NO-LABEL FORMAT "XXXXXXXXX"
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .69
          FONT 0
     CcbCDocu.NroOrd AT ROW 3.85 COL 44.72 COLON-ALIGNED
          LABEL "No.Devolucion" FORMAT "XXXXXXXXX"
          VIEW-AS FILL-IN 
          SIZE 12 BY .69
     CcbCDocu.FchDoc AT ROW 1.19 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     CcbCDocu.TpoCmb AT ROW 2.81 COL 69.43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .69
     CcbCDocu.CodMon AT ROW 4.23 COL 62.43 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Soles", 1,
"Dolares", 2
          SIZE 16.86 BY .69
     CcbCDocu.ImpTot AT ROW 5.85 COL 66.72 COLON-ALIGNED NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.ImpDto AT ROW 5.85 COL 22.57 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.CodCli AT ROW 1.77 COL 12 COLON-ALIGNED FORMAT "x(11)"
          VIEW-AS FILL-IN 
          SIZE 11 BY .69
     CcbCDocu.ImpBrt AT ROW 5.85 COL 3.43 NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     FILL-IN_DirCli AT ROW 2.46 COL 12 COLON-ALIGNED
     CcbCDocu.Glosa AT ROW 3.15 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 44.72 BY .69
     CcbCDocu.ImpExo AT ROW 5.85 COL 12.14 COLON-ALIGNED NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     FILL-IN_NomCli AT ROW 1.77 COL 23.72 COLON-ALIGNED NO-LABEL
     CcbCDocu.ImpVta AT ROW 5.85 COL 33.29 COLON-ALIGNED NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.ImpIsc AT ROW 5.85 COL 44.29 COLON-ALIGNED NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     CcbCDocu.ImpIgv AT ROW 5.85 COL 54.57 COLON-ALIGNED NO-LABEL FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
          BGCOLOR 15 
     FILL-IN_RucCli AT ROW 1.96 COL 68 COLON-ALIGNED
     RECT-19 AT ROW 1 COL 1
     RECT-29 AT ROW 4.04 COL 61.72
     "Imp.Bruto" VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 5.19 COL 3.43
     "Exonerado" VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 5.19 COL 14.14
     "Imp.Dcto." VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 5.19 COL 24.57
     "Valor Venta" VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 5.19 COL 35.29
     "I.G.V." VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 5.23 COL 56.29
     "I.S.C." VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 5.23 COL 46.57
     " Moneda" VIEW-AS TEXT
          SIZE 6.72 BY .5 AT ROW 3.73 COL 67.29
     "Imp.Total" VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 5.19 COL 68.72
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
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 6.04
         WIDTH              = 80.43.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN CcbCDocu.CodCli IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.CodRef IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DirCli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_NomCli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_RucCli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpBrt IN FRAME F-Main
   NO-ENABLE ALIGN-L EXP-FORMAT                                         */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpExo IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpIgv IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpIsc IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpTot IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN CcbCDocu.ImpVta IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCDocu.NroDoc IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN CcbCDocu.NroOrd IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CcbCDocu.CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CcbCDocu.CodCli V-table-Win
ON LEAVE OF CcbCDocu.CodCli IN FRAME F-Main /* Cliente */
DO:
  FIND gn-clie WHERE gn-clie.CodCia = 0 AND 
       gn-clie.CodCli = CcbCDocu.CodCli:SCREEN-VALUE 
       NO-LOCK NO-ERROR.
  IF NOT AVAILABLE gn-clie THEN DO:
     MESSAGE 'Codigo de Cliente no existe' VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.
  DO WITH FRAME {&FRAME-NAME}:
     DISPLAY gn-clie.NomCli @ FILL-IN_NomCli
             gn-clie.DirCli @ FILL-IN_DirCli
             gn-clie.Ruc @ FILL-IN_RucCli.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CcbCDocu.NroDoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CcbCDocu.NroDoc V-table-Win
ON LEAVE OF CcbCDocu.NroDoc IN FRAME F-Main /* Numero */
DO:
  IF SELF:SCREEN-VALUE = "" THEN RETURN NO-APPLY.
  FIND B-CDOCU WHERE B-CDOCU.Codcia = S-CODCIA AND
                   B-CDOCU.CodDoc = S-CODDOC AND
                   B-CDOCU.NroDoc = SELF:SCREEN-VALUE NO-ERROR. 
  IF AVAILABLE B-CDOCU THEN DO:
     MESSAGE "Numero Documento Ya se Encuentra Registrado" 
     VIEW-AS ALERT-BOX.
     RETURN NO-APPLY.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CcbCDocu.NroRef
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CcbCDocu.NroRef V-table-Win
ON LEAVE OF CcbCDocu.NroRef IN FRAME F-Main /* Numero */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     FIND B-CDOCU WHERE B-CDOCU.Codcia = S-CODCIA AND
                        B-CDOCU.Coddoc = CcbCDocu.CodRef:SCREEN-VALUE AND
                        B-CDOCU.NroDoc = SELF:SCREEN-VALUE NO-ERROR.
     IF NOT AVAILABLE B-CDOCU THEN DO:
        MESSAGE "Documento de Referencia No Existe " 
                VIEW-AS ALERT-BOX .
                RETURN NO-APPLY.
                
     END.                   
     DISPLAY TODAY @ CcbCDocu.FchDoc
             B-CDOCU.CodCli @ CcbCDocu.CodCli
             FacCfgGn.Tpocmb[1] @ CcbCDocu.TpoCmb
             B-CDOCU.NomCli @ FILL-IN_NomCli 
             B-CDOCU.RucCli @ FILL-IN_RucCli  
             B-CDOCU.DirCli @ FILL-IN_DirCli.
     S-PORDTO = CcbCDocu.PorDto.
     S-PORIGV = CcbCDocu.PorIgv.
             
     FIND gn-clie WHERE gn-clie.CodCia = 0 
                   AND  gn-clie.CodCli = B-CDOCU.CodCli 
                  NO-LOCK NO-ERROR.
     CcbCDocu.CodMon:SCREEN-VALUE = STRING(B-CDOCU.CodMon).
  END.
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
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH DETA:
    DELETE DETA.
END.
IF NOT L-CREA THEN DO:
   FOR EACH CcbDDocu WHERE CcbDDocu.CodCia = CcbCDocu.CodCia
                      AND  CcbDDocu.CodDoc = CcbCDocu.CodDoc
                      AND  CcbDDocu.NroDoc = CcbCDocu.NroDoc
                     NO-LOCK:
       CREATE DETA.
       ASSIGN DETA.CodCia = CcbDDocu.CodCia
              DETA.codmat = CcbDDocu.codmat 
              DETA.PreUni = CcbDDocu.PreUni 
              DETA.CanDes = CcbDDocu.CanDes 
              DETA.Factor = CcbDDocu.Factor 
              DETA.ImpIsc = CcbDDocu.ImpIsc
              DETA.ImpIgv = CcbDDocu.ImpIgv 
              DETA.ImpLin = CcbDDocu.ImpLin
              DETA.PorDto = CcbDDocu.PorDto 
              DETA.PreBas = CcbDDocu.PreBas 
              DETA.ImpDto = CcbDDocu.ImpDto
              DETA.AftIgv = CcbDDocu.AftIgv
              DETA.AftIsc = CcbDDocu.AftIsc
              DETA.UndVta = CcbDDocu.UndVta.
   END.
END.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Documento V-table-Win 
PROCEDURE Borra-Documento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DELETE FROM CcbDDocu WHERE CcbDDocu.CodCia = CcbCDocu.CodCia
                      AND  CcbDDocu.CodDoc = CcbCDocu.CodDoc 
                      AND  CcbDDocu.NroDoc = CcbCDocu.NroDoc.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Detalle V-table-Win 
PROCEDURE Genera-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ON ERROR UNDO, RETURN "ADM-ERROR":
   RUN Borra-Documento. 
   FOR EACH DETA ON ERROR UNDO, RETURN "ADM-ERROR":
       CREATE CcbDDocu.
       ASSIGN CcbDDocu.CodCia = CcbCDocu.CodCia 
              CcbDDocu.Coddiv = CcbCDocu.Coddiv 
              CcbDDocu.CodDoc = CcbCDocu.CodDoc 
              CcbDDocu.NroDoc = CcbCDocu.NroDoc
              CcbDDocu.codmat = DETA.codmat 
              CcbDDocu.PreUni = DETA.PreUni 
              CcbDDocu.CanDes = DETA.CanDes 
              CcbDDocu.Factor = DETA.Factor 
              CcbDDocu.ImpIsc = DETA.ImpIsc
              CcbDDocu.ImpIgv = DETA.ImpIgv 
              CcbDDocu.ImpLin = DETA.ImpLin
              CcbDDocu.PorDto = DETA.PorDto 
              CcbDDocu.PreBas = DETA.PreBas 
              CcbDDocu.ImpDto = DETA.ImpDto
              CcbDDocu.AftIgv = DETA.AftIgv
              CcbDDocu.AftIsc = DETA.AftIsc
              CcbDDocu.UndVta = DETA.UndVta
              CcbDDocu.Por_Dsctos[1] = DETA.Por_Dsctos[1]
              CcbDDocu.Por_Dsctos[2] = DETA.Por_Dsctos[2]
              CcbDDocu.Por_Dsctos[3] = DETA.Por_Dsctos[3]
              CcbDDocu.Flg_factor = DETA.Flg_factor
              CcbDDocu.ImpCto     = DETA.ImpCto.
   END.
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
DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
DO ON ERROR UNDO, RETURN "ADM-ERROR":
   FIND B-CDOCU WHERE ROWID(B-CDOCU) = ROWID(CcbCDocu) EXCLUSIVE-LOCK NO-ERROR.
   B-CDOCU.ImpIgv = 0.
   B-CDOCU.ImpIsc = 0.
   B-CDOCU.ImpExo = 0.
   B-CDOCU.ImpVta = ROUND(B-CDOCU.ImpTot / 1.18 , 2).
   B-CDOCU.ImpIgv = B-CDOCU.ImpTot - B-CDOCU.ImpVta.

   B-CDOCU.ImpBrt = B-CDOCU.ImpTot - B-CDOCU.ImpIgv - B-CDOCU.ImpIsc + 
                    B-CDOCU.ImpDto - B-CDOCU.ImpExo.

   B-CDOCU.SdoAct  = B-CDOCU.ImpTot.
   B-CDOCU.Imptot2 = B-CDOCU.ImpTot.
   RELEASE B-CDOCU.
END.
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
  
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
   
  L-CREA = YES.
  RUN Actualiza-Deta.
  RUN Numero-de-Documento(NO).

  DO WITH FRAME {&FRAME-NAME}:
  END.
/*
  DO WITH FRAME {&FRAME-NAME}:
     FIND Almcmov WHERE ROWID(Almcmov) = output-var-1 NO-LOCK NO-ERROR.
     R-NRODEV = ROWID(Almcmov).
     CcbCDocu.CodMon:SCREEN-VALUE = STRING(Almcmov.CodMon).
     DISPLAY TODAY @ CcbCDocu.FchDoc
             Almcmov.CodCli @ CcbCDocu.CodCli
             FacCfgGn.Tpocmb[1] @ CcbCDocu.TpoCmb
             Almcmov.CodRef @ CcbCDocu.CodRef
             Almcmov.NroRf1 @ CcbCDocu.NroRef
             Almcmov.NroRf2 @ CcbCDocu.NroOrd
             STRING(I-NroSer,"999") + STRING(I-NroDoc,"999999") @ CcbCDocu.NroDoc.
     FIND gn-clie WHERE gn-clie.CodCia = 0 
                   AND  gn-clie.CodCli = Almcmov.CodCli 
                  NO-LOCK NO-ERROR.
     IF AVAILABLE gn-clie THEN 
        DISPLAY gn-clie.NomCli @ FILL-IN_NomCli 
                gn-clie.Ruc    @ FILL-IN_RucCli  
                gn-clie.DirCli @ FILL-IN_DirCli.
        FIND CcbCDocu WHERE CcbCDocu.codcia = s-codcia 
                       AND  CcbCDocu.CodDoc = Almcmov.CodRef 
                       AND  CcbCDocu.NroDoc = Almcmov.NroRf1 
                      NO-LOCK NO-ERROR.
        IF AVAILABLE CcbCDocu THEN DO:
           DISPLAY CcbCDocu.NomCli @ FILL-IN_NomCli 
                   CcbCDocu.RucCli @ FILL-IN_RucCli  
                   CcbCDocu.DirCli @ FILL-IN_DirCli.
           S-PORDTO = CcbCDocu.PorDto.
           S-PORIGV = CcbCDocu.PorIgv.
        END.
        
     FOR EACH Almdmov NO-LOCK WHERE Almdmov.CodCia = Almcmov.CodCia 
                               AND  Almdmov.CodAlm = Almcmov.CodAlm 
                               AND  Almdmov.TipMov = Almcmov.TipMov 
                               AND  Almdmov.CodMov = Almcmov.CodMov 
                               AND  Almdmov.NroDoc = Almcmov.NroDoc:
         CREATE DETA.
         ASSIGN DETA.CodCia = AlmDMov.CodCia
                DETA.codmat = AlmDMov.codmat
                DETA.PreUni = AlmDMov.PreUni
                DETA.CanDes = AlmDMov.CanDes
                DETA.Factor = AlmDMov.Factor
                DETA.ImpIsc = AlmDMov.ImpIsc
                DETA.ImpIgv = AlmDMov.ImpIgv
                DETA.ImpLin = AlmDMov.ImpLin
                DETA.PorDto = AlmDMov.PorDto
                DETA.PreBas = AlmDMov.PreBas
                DETA.ImpDto = AlmDMov.ImpDto
                DETA.AftIgv = AlmDMov.AftIgv
                DETA.AftIsc = AlmDMov.AftIsc
                DETA.UndVta = AlmDMov.CodUnd
                DETA.Por_Dsctos[1] = Almdmov.Por_Dsctos[1]
                DETA.Por_Dsctos[2] = Almdmov.Por_Dsctos[2]
                DETA.Por_Dsctos[3] = Almdmov.Por_Dsctos[3]
                DETA.Flg_factor = Almdmov.Flg_factor.
        /*************Grabando Costos ********************/
        FIND CcbDDocu WHERE CcbDDocu.codcia = s-codcia 
                       AND  CcbDDocu.CodDiv = S-CODDIV
                       AND  CcbDDocu.CodDoc = Almcmov.CodRef 
                       AND  CcbDDocu.NroDoc = Almcmov.NroRf1 
                       AND  CcbDDocu.CodMat = Almdmov.codMat
                       USE-INDEX LLAVE04 NO-LOCK NO-ERROR.
        IF AVAILABLE CcbDDocu THEN DO: 
           DETA.ImpCto = CcbDDocu.ImpCto * ( ( DETA.Candes * DETA.Factor ) / (CcbDDocu.Candes * CcbDDocu.Factor) ). 
        END.
        /****************************************************/
                
     END.
  END.
  */
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
  IF L-CREA THEN DO:
    DO WITH FRAME {&FRAME-NAME}:
     FIND B-CDOCU WHERE B-CDOCU.Codcia = S-CODCIA AND
                        B-CDOCU.Coddoc = CcbCDocu.CodRef:SCREEN-VALUE AND
                        B-CDOCU.NroDoc = CcbCDocu.Nroref:SCREEN-VALUE NO-ERROR.

     ASSIGN CcbCDocu.CodCia = S-CODCIA
            CcbCDocu.FchDoc = DATE(CcbCDocu.FchDoc:SCREEN-VALUE)
            CcbCDocu.FchVto = DATE(CcbCDocu.FchDoc:SCREEN-VALUE)
            CcbCDocu.FlgEst = "P"
            CcbCDocu.PorIgv = FacCfgGn.PorIgv
            CcbCDocu.PorDto = S-PORDTO
            CcbCDocu.CodDoc = S-CODDOC
            CcbCDocu.NroDoc = CcbCDocu.NroDoc:SCREEN-VALUE
            CcbCDocu.CodDiv = S-CODDIV
            CcbCDocu.TpoCmb = FacCfgGn.TpoCmb[1]
            CcbCDocu.CndCre = 'D'
            CcbCDocu.Tipo   = "OFICINA"
            CcbCDocu.usuario = S-USER-ID
            CcbCDocu.SdoAct = CcbCDocu.Imptot
            CcbCDocu.NomCli = FILL-IN_NomCli:SCREEN-VALUE
            CcbCDocu.DirCli = FILL-IN_DirCli:SCREEN-VALUE
            CcbCDocu.RucCli = FILL-IN_RucCli:SCREEN-VALUE.
              
     ASSIGN CcbCDocu.CodCli = CcbCDocu.CodCli:SCREEN-VALUE 
            CcbCDocu.CodMon = INTEGER(CcbCDocu.CodMon:SCREEN-VALUE)
            CcbCDocu.CodAlm = B-CDOCU.CodAlm
            CcbCDocu.CodMov = 09
            CcbCDocu.CodVen = B-CDOCU.CodVen
            CcbCDocu.CodRef = CcbCDocu.CodRef:SCREEN-VALUE
            /*CcbCDocu.NroPed = STRING(Almcmov.NroDoc,"999999")*/
            CcbCDocu.NroOrd = CcbCDocu.NroOrd:SCREEN-VALUE
            CcbCDocu.NroRef = CcbCDocu.NroRef:SCREEN-VALUE.

     IF CcbCDocu.Codcli = FacCfgGn.CliVar THEN 
        ASSIGN
            CcbCDocu.RucCli = FILL-IN_RucCli:SCREEN-VALUE
            CcbCDocu.DirCli = FILL-IN_DirCli:SCREEN-VALUE.

     DISPLAY CcbCDocu.NroDoc.

    END.
    RUN Graba-Totales.

  END.
  
  RUN Genera-Detalle.   
  
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  
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
   FIND B-CDOCU WHERE ROWID(B-CDOCU) = ROWID(CcbCDocu) NO-ERROR.
   IF CcbCDocu.FlgEst = "A" THEN DO:
      MESSAGE 'El documento se enuentra anulado...' VIEW-AS ALERT-BOX.
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
   DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
      RUN Borra-Documento.   
      FIND B-CDOCU WHERE ROWID(B-CDOCU) = ROWID(CcbCDocu) NO-ERROR.
      IF AVAILABLE B-CDOCU THEN 
         ASSIGN CcbCDocu.FlgEst = "A"
                CcbCDocu.SdoAct = 0 
                CcbCDocu.Glosa  = '**** Documento Anulado ****'
                CcbCDocu.UsuAnu = S-USER-ID.
      RELEASE B-CDOCU.
/*
      FIND Almcmov WHERE Almcmov.CodCia = CcbCDocu.CodCia
                    AND  Almcmov.CodAlm = CcbCDocu.CodAlm 
                    AND  Almcmov.TipMov = "I"
                    AND  Almcmov.CodMov = CcbCDocu.CodMov 
                    AND  Almcmov.NroDoc = INTEGER(CcbCDocu.NroPed)
                   NO-ERROR.
      ASSIGN Almcmov.FlgEst = "P".
      RELEASE Almcmov.
*/  
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  IF AVAILABLE CcbCDocu THEN DO WITH FRAME {&FRAME-NAME}:
     FIND gn-clie WHERE gn-clie.CodCia = 0 
                   AND  gn-clie.CodCli = CcbCDocu.CodCli 
                  NO-LOCK NO-ERROR.
     IF AVAILABLE gn-clie  THEN 
        DISPLAY gn-clie.NomCli @ FILL-IN_NomCli 
                gn-clie.Ruc    @ FILL-IN_RucCli  
                gn-clie.DirCli @ FILL-IN_DirCli.
     IF CcbCDocu.Codcli = FacCfgGn.CliVar THEN 
        DISPLAY CcbCDocu.NomCli @ FILL-IN_NomCli 
                CcbCDocu.Ruc    @ FILL-IN_RucCli  
                CcbCDocu.DirCli @ FILL-IN_DirCli.
  END.
  /* Code placed here will execute AFTER standard behavior.    */

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
  IF CCBCDOCU.FLGEST <> "A" THEN
     RUN CCB\R-NOTCRE2.R (ROWID(CCBCDOCU)).
  
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
     FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA 
                    AND  FacCorre.CodDoc = S-CODDOC 
                    AND  FacCorre.CodDiv = S-CODDIV 
                   EXCLUSIVE-LOCK NO-ERROR.
  ELSE
     FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA 
                    AND  FacCorre.CodDoc = S-CODDOC 
                    AND  FacCorre.CodDiv = S-CODDIV 
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
  IF p-state = 'update-begin':U THEN DO:
     L-CREA = NO.
     DO WITH FRAME {&FRAME-NAME}:
        DISABLE ALL.
        /*
        Ccbcdocu.NroDoc:SENSITIVE  = FALSE.
        */
     END.
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
IF NOT AVAILABLE CcbCDocu THEN RETURN "ADM-ERROR".
S-CODMON = Ccbcdocu.CodMon.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


