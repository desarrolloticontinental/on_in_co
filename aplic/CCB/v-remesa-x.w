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

/* Local Variable Definitions ---                                       */

DEF SHARED VAR s-codcia  AS INT.
DEF SHARED VAR s-coddiv  AS CHAR.
DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR s-coddoc  AS CHAR.
DEF SHARED VAR s-ptovta  AS INTE.
DEF SHARED VAR s-tipo    AS CHAR.
DEF SHARED VAR s-codter  LIKE ccbcterm.codter.
DEF SHARED VAR s-codcli  LIKE gn-clie.codcli.
DEF SHARED VAR s-tpocmb  AS DEC.
DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.

DEF VAR s-ok AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR X-ImpNac AS DECIMAL INITIAL 0 NO-UNDO.
DEF VAR X-ImpUsa AS DECIMAL INITIAL 0 NO-UNDO.

DEF BUFFER b-ccbccaja FOR ccbccaja.
DEF BUFFER b-ccbdcaja FOR ccbdcaja.
DEF BUFFER b-CDocu    FOR ccbcdocu.

DEF VAR x-tabla AS CHAR.

IF s-coddoc = "I/C" THEN x-tabla = "IJ" .
IF s-coddoc = "E/C" THEN x-tabla = "EJ" .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES CcbCCaja
&Scoped-define FIRST-EXTERNAL-TABLE CcbCCaja


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR CcbCCaja.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS CcbCCaja.Codcta[1] CcbCCaja.CodCli ~
CcbCCaja.Glosa 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}Codcta[1] ~{&FP2}Codcta[1] ~{&FP3}~
 ~{&FP1}CodCli ~{&FP2}CodCli ~{&FP3}
&Scoped-define ENABLED-TABLES CcbCCaja
&Scoped-define FIRST-ENABLED-TABLE CcbCCaja
&Scoped-Define ENABLED-OBJECTS RECT-1 F-CodDoc F-NroDoc 
&Scoped-Define DISPLAYED-FIELDS CcbCCaja.NroDoc CcbCCaja.CodDiv ~
CcbCCaja.Codcta[1] CcbCCaja.CodCli CcbCCaja.Glosa CcbCCaja.TpoCmb ~
CcbCCaja.FchDoc CcbCCaja.usuario 
&Scoped-Define DISPLAYED-OBJECTS F-Descta F-Descli F-CodDoc F-NroDoc ~
F-CodRef F-NroRef X-Status F-Moneda F-Importe 

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
DEFINE VARIABLE F-CodDoc AS CHARACTER FORMAT "X(4)" 
     LABEL "No.Documento" 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .69
     BGCOLOR 15 FGCOLOR 0 .

DEFINE VARIABLE F-CodRef AS CHARACTER FORMAT "X(4)" 
     LABEL "Nro.Referencia" 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .69.

DEFINE VARIABLE F-Descli AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE F-Descta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.72 BY .81 NO-UNDO.

DEFINE VARIABLE F-Importe AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Importe" 
     VIEW-AS FILL-IN 
     SIZE 9.29 BY .81.

DEFINE VARIABLE F-Moneda AS CHARACTER FORMAT "X(256)":U 
     LABEL "Moneda" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81 NO-UNDO.

DEFINE VARIABLE F-NroDoc AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 14.72 BY .69
     BGCOLOR 15 .

DEFINE VARIABLE F-NroRef AS CHARACTER FORMAT "X(10)" 
     VIEW-AS FILL-IN 
     SIZE 14.43 BY .69.

DEFINE VARIABLE X-Status AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76.57 BY 7.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CcbCCaja.NroDoc AT ROW 1.19 COL 7 COLON-ALIGNED FORMAT "XXX-XXXXXXXX"
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .69
          FONT 6
     CcbCCaja.CodDiv AT ROW 1.96 COL 7 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.57 BY .81
     CcbCCaja.Codcta[1] AT ROW 2.81 COL 7.14 COLON-ALIGNED
          LABEL "Concepto"
          VIEW-AS FILL-IN 
          SIZE 12.29 BY .81
     F-Descta AT ROW 2.81 COL 20.72 COLON-ALIGNED NO-LABEL
     CcbCCaja.CodCli AT ROW 3.69 COL 7.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.29 BY .81
     F-Descli AT ROW 3.73 COL 17.29 COLON-ALIGNED NO-LABEL
     F-CodDoc AT ROW 4.69 COL 29.29 COLON-ALIGNED
     F-NroDoc AT ROW 4.73 COL 35.43 COLON-ALIGNED NO-LABEL
     F-CodRef AT ROW 5.5 COL 29.29 COLON-ALIGNED
     F-NroRef AT ROW 5.5 COL 35.43 COLON-ALIGNED NO-LABEL
     CcbCCaja.Glosa AT ROW 6.85 COL 2.29 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP
          SIZE 72.14 BY 1.35
     X-Status AT ROW 1.19 COL 62 COLON-ALIGNED
     CcbCCaja.TpoCmb AT ROW 1.96 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .69
     CcbCCaja.FchDoc AT ROW 2.73 COL 62 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     CcbCCaja.usuario AT ROW 3.5 COL 62 COLON-ALIGNED
          LABEL "User"
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     F-Moneda AT ROW 4.42 COL 62.14 COLON-ALIGNED
     F-Importe AT ROW 5.23 COL 61.86 COLON-ALIGNED
     "Detalle" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 6.19 COL 2.86
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.CcbCCaja
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
         HEIGHT             = 7.73
         WIDTH              = 76.86.
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

/* SETTINGS FOR FILL-IN CcbCCaja.Codcta[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CcbCCaja.CodDiv IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       F-CodDoc:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-CodRef IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Descli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Descta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Importe IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Moneda IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       F-NroDoc:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN F-NroRef IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCCaja.FchDoc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR CcbCCaja.Glosa IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN CcbCCaja.NroDoc IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN CcbCCaja.TpoCmb IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN CcbCCaja.usuario IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN X-Status IN FRAME F-Main
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




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CcbCCaja.CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CcbCCaja.CodCli V-table-Win
ON LEAVE OF CcbCCaja.CodCli IN FRAME F-Main /* Cliente */
DO:
   IF SELF:SCREEN-VALUE = "" THEN RETURN.
   FIND gn-clie WHERE gn-clie.CodCia = 0
                 AND  gn-clie.CodCli = CcbCCaja.CodCli:SCREEN-VALUE
                NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gn-clie  THEN DO:
      MESSAGE "Codigo de cliente no existe" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.


   DISPLAY gn-clie.NomCli @ F-Descli.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CcbCCaja.Codcta[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CcbCCaja.Codcta[1] V-table-Win
ON LEAVE OF CcbCCaja.Codcta[1] IN FRAME F-Main /* Concepto */
DO:
    IF SELF:SCREEN-VALUE = "" THEN RETURN.
    FIND almtabla WHERE almtabla.Tabla = x-tabla AND
                        almtabla.Codigo = SELF:SCREEN-VALUE 
                        NO-LOCK NO-ERROR.
     IF AVAILABLE almtabla THEN 
        DISPLAY almtabla.Nombre @ F-Descta.
     ELSE DO:
        MESSAGE "Concepto de Caja No existe" 
        VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.
  
  
    FIND cb-ctas WHERE cb-ctas.CodCia = 0 AND
                       cb-ctas.CodCta = SELF:SCREEN-VALUE
                       NO-LOCK NO-ERROR.
    
    IF  AVAILABLE cb-ctas THEN DO:
        IF cb-ctas.PidAux THEN DO:
            ASSIGN CcbCCaja.CodCli:VISIBLE = YES               
                   F-Descli:VISIBLE = YES
                   CcbCCaja.Codcli:SENSITIVE = TRUE.

        END.
        ELSE DO:
            ASSIGN CcbCCaja.CodCli:HIDDEN = YES               
                   F-Descli:HIDDEN = YES
                   CcbCCaja.CodCli:SCREEN-VALUE = "".

        END.
        IF cb-ctas.PidDoc THEN DO:
            ASSIGN F-CODDOC:VISIBLE = YES               
                   F-NroDoc:VISIBLE = YES
                   F-CodDoc:SENSITIVE = TRUE
                   F-NroDoc:SENSITIVE = TRUE.
        END.
        ELSE DO:
            ASSIGN F-CodDoc:HIDDEN = YES               
                   F-NroDoc:HIDDEN = YES
                   F-CodDoc:SCREEN-VALUE = ""
                   F-NroDoc:SCREEN-VALUE = "".
        END.
        IF cb-ctas.PidRef THEN DO:
        END.

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
  {src/adm/template/row-list.i "CcbCCaja"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "CcbCCaja"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cancelar-Documento V-table-Win 
PROCEDURE Cancelar-Documento :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER X-suma AS LOGICAL.

 FIND B-CDocu WHERE B-CDocu.CodCia = s-codcia 
               AND  B-CDocu.CodDiv = s-coddiv
               AND  B-CDocu.CodDoc = CcbCCaja.Voucher[09]
               AND  B-CDocu.NroDoc = CcbCCaja.Voucher[10]
                NO-LOCK NO-ERROR.
  IF NOT AVAILABLE B-CDocu THEN DO:
     MESSAGE 'Nota de Credito no se encuentra registrada' VIEW-AS ALERT-BOX ERROR.
     RETURN "ADM-ERROR".
  END.
/*
  CREATE CcbDCaja.
  ASSIGN 
     CcbDCaja.CodCia = s-CodCia
     CcbDCaja.CodDoc = c-tipdoc
     CcbDCaja.NroDoc = Ccbccaja.Voucher[10]
     CcbDCaja.CodMon = B-CDocu.CodMon
     CcbDCaja.TpoCmb = CcbCDocu.tpocmb
     CcbDCaja.CodCli = CcbCDocu.CodCli
     CcbDCaja.CodRef = B-CDocu.CodDoc
     CcbDCaja.NroRef = B-CDocu.NroDoc
     CcbDCaja.FchDoc = CcbCDocu.FchDoc.
  IF B-CDocu.CodMon = 1 THEN
     ASSIGN 
        CcbDCaja.ImpTot = CcbCCaja.ImpNac[1].
  ELSE
     ASSIGN 
        CcbDCaja.ImpTot = CcbCCaja.ImpUsa[1].
*/
  DEFINE VAR x-imptot AS DECI.
  DEFINE VAR x-mon    AS INTEGER.
  
  IF CcbCCaja.ImpNac[1] <> 0 THEN DO:
    x-imptot = CcbCCaja.ImpNac[1].
    x-mon    = 1.
  END.
  
  IF CcbCCaja.ImpUsa[1] <> 0 THEN DO:
    x-imptot = CcbCCaja.ImpUsa[1].
    x-mon    = 2.
  END.
  
  RUN Cancelar_Nota_Credito ( s-CodCia, B-CDocu.CodDoc, B-CDocu.NroDoc, x-mon,
                              CcbCCaja.TpoCmb, x-ImpTot, x-suma ).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cancelar_Nota_Credito V-table-Win 
PROCEDURE Cancelar_Nota_Credito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER iCodCia AS INTEGER.
DEFINE INPUT PARAMETER cCodDoc AS CHAR.
DEFINE INPUT PARAMETER cNroDoc AS CHAR.
DEFINE INPUT PARAMETER iCodMon AS INTEGER.
DEFINE INPUT PARAMETER fTpoCmb AS DECIMAL.
DEFINE INPUT PARAMETER fImpTot AS DECIMAL.
DEFINE INPUT PARAMETER LSumRes AS LOGICAL.

DEFINE VAR XfImport AS DECIMAL INITIAL 0.

FIND FIRST B-CDocu WHERE B-CDocu.CodCia = iCodCia 
                    AND  B-CDocu.CodDoc = cCodDoc 
                    AND  B-CDocu.NroDoc = cNroDoc 
                   EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE B-CDocu THEN DO :
   XfImport = fImpTot.
   IF B-CDocu.CodMon <> iCodMon THEN DO:
      IF B-CDocu.CodMon = 1 THEN 
         ASSIGN XfImport = ROUND( fImpTot * fTpoCmb , 2 ).
      ELSE ASSIGN XfImport = ROUND( fImpTot / fTpoCmb , 2 ).
   END.
   
   IF LSumRes THEN ASSIGN B-CDocu.SdoAct = B-CDocu.SdoAct + XfImport.
   ELSE ASSIGN B-CDocu.SdoAct = B-CDocu.SdoAct - XfImport.
   
   IF B-CDocu.SdoAct <= 0 THEN ASSIGN B-CDocu.FlgEst = "C".
   ELSE ASSIGN B-CDocu.FlgEst = "P".
   RELEASE B-CDocu.
END.                          
ELSE MESSAGE "Nota de Credito no registrada " VIEW-AS ALERT-BOX.

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
  FIND FacCorre WHERE 
       FacCorre.CodCia = s-codcia AND
       FacCorre.CodDoc = s-coddoc AND 
       FacCorre.NroSer = s-ptovta
       NO-LOCK NO-ERROR.
  DO WITH FRAME {&FRAME-NAME}:
     CcbCCaja.NroDoc:SCREEN-VALUE = STRING(faccorre.nroser, "999") + 
                                    STRING(faccorre.correlativo, "999999").
     FIND LAST gn-tcmb WHERE
               gn-tcmb.Fecha <= TODAY NO-LOCK NO-ERROR.
     IF AVAILABLE gn-tcmb THEN DISPLAY gn-tcmb.Venta @ CcbCCaja.TpoCmb.
     DISPLAY TODAY @ CcbCCaja.FchDoc
          S-CODDIV @ CcbCCaja.CodDiv.
     
     CcbCCaja.Codcli:SENSITIVE = FALSE.
     
  END.
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
 DO WITH FRAME {&FRAME-NAME}:
  FIND FacCorre WHERE 
       FacCorre.CodCia = s-codcia AND
       FacCorre.CodDoc = s-coddoc AND 
       FacCorre.NroSer = s-ptovta
       EXCLUSIVE-LOCK.

  ASSIGN CcbCCaja.CodCia  = s-codcia
         CcbCCaja.CodDoc  = s-coddoc
         CcbCCaja.NroDoc  = STRING(faccorre.nroser, "999") + 
                            STRING(faccorre.correlativo, "999999")
         CcbCCaja.CodDiv  = S-CODDIV
         CcbCCaja.Tipo    = s-tipo
         CcbCCaja.usuario = s-user-id
         CcbCCaja.CodCaja = S-CODTER
         CcbCCaja.FchDoc  = TODAY
         CcbcCaja.Flgest  = "C"
         CcbCCaja.Voucher[10] = F-Nrodoc:SCREEN-VALUE
         CcbCCaja.Voucher[09] = F-CodDoc:SCREEN-VALUE
         FacCorre.Correlativo = FacCorre.Correlativo + 1.
  
  FIND LAST gn-tcmb WHERE
            gn-tcmb.Fecha <= TODAY NO-LOCK NO-ERROR.
  IF AVAILABLE gn-tcmb THEN CcbCCaja.TpoCmb = gn-tcmb.Venta.
         
  RELEASE faccorre.

  IF Cb-Ctas.CodMon = 1 THEN DO:
     CcbCCaja.ImpNac[1] = F-Importe.
     CREATE CcbDCaja.
     ASSIGN CcbDCaja.CodCia = CcbCCaja.CodCia   
         CcbDCaja.CodDoc = CcbCCaja.CodDoc
         CcbDCaja.NroDoc = CcbCCaja.NroDoc
         CcbDCaja.CodMon = 1
         CcbDCaja.CodRef = CcbCCaja.CodDoc
         CcbDCaja.NroRef = CcbCCaja.NroDoc
         CcbDCaja.FchDoc = CcbCCaja.FchDoc
         CcbDCaja.ImpTot = CcbCCaja.ImpNac[1]
         CcbDCaja.TpoCmb = CcbCCaja.TpoCmb.

  END.
  
  IF Cb-Ctas.CodMon = 2 THEN DO:
     CcbCCaja.ImpUsa[1] = F-Importe.
     CREATE CcbDCaja.
     ASSIGN CcbDCaja.CodCia = CcbCCaja.CodCia   
         CcbDCaja.CodDoc = CcbCCaja.CodDoc
         CcbDCaja.NroDoc = CcbCCaja.NroDoc
         CcbDCaja.CodMon = 2
         CcbDCaja.CodRef = CcbCCaja.CodDoc
         CcbDCaja.NroRef = CcbCCaja.NroDoc
         CcbDCaja.FchDoc = CcbCCaja.FchDoc
         CcbDCaja.ImpTot = CcbCCaja.ImpUsa[1]
         CcbDCaja.TpoCmb = CcbCCaja.TpoCmb.
  END.
  IF Ccbccaja.Voucher[10] <> ' ' THEN RUN Cancelar-Documento(FALSE).

 END.
  
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
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  IF ccbccaja.flgcie = "C" THEN DO:
     MESSAGE "Ya se hizo el cierre de caja" VIEW-AS ALERT-BOX ERROR.
     RETURN "ADM-ERROR".
  END.
  /* actualizamos la cuenta corriente */

  IF Ccbccaja.Voucher[10] <> ' ' THEN RUN Cancelar-Documento(TRUE).
  
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
     FOR EACH ccbdcaja OF ccbccaja:
         DELETE ccbdcaja.
     END.
     FIND b-ccbccaja WHERE ROWID(b-ccbccaja) = ROWID(ccbccaja) EXCLUSIVE-LOCK.
     ASSIGN  b-ccbccaja.flgest = "A"
             .
     RELEASE b-ccbccaja.
  END.

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
  IF AVAILABLE CcbCCaja THEN DO WITH FRAME {&FRAME-NAME}:
     IF CcbCCaja.FlgEst  = "A" THEN X-Status:SCREEN-VALUE = " ANULADO ".  
     IF CcbCCaja.FlgEst  = "C" THEN X-Status:SCREEN-VALUE = " PENDIENTE".  
     IF CcbCCaja.FlgEst  = "P" THEN X-Status:SCREEN-VALUE = " PENDIENTE".  
     IF CcbCCaja.FlgEst  = " " THEN X-Status:SCREEN-VALUE = " PENDIENTE".  
     IF CcbCCaja.FlgCie  = "C" THEN X-Status:SCREEN-VALUE = " CERRADO ".  
     FIND gn-clie WHERE gn-clie.CodCia = 0
                   AND  gn-clie.CodCli = CcbCCaja.CodCli
                   NO-LOCK NO-ERROR.
     IF AVAILABLE gn-clie THEN 
        DISPLAY gn-clie.NomCli @ F-Descli.
   
     FIND almtabla WHERE almtabla.Tabla = x-tabla AND
                         almtabla.Codigo = CcbCCaja.Codcta[1] 
                         NO-LOCK NO-ERROR.
      IF AVAILABLE almtabla THEN 
         DISPLAY almtabla.Nombre @ F-Descta.
      
 
      DISPLAY CcbCCaja.Voucher[09] @ F-Coddoc
              CcbCCaja.Voucher[10] @ F-NroDoc.
 
      IF CcbCCaja.ImpNac[1] <> 0 THEN DO:
         DISPLAY CcbCCaja.ImpNac[1] @ F-Importe
                 "S/."              @ F-Moneda.
      END.
        
      IF CcbCCaja.ImpUsa[1] <> 0 THEN DO:
         DISPLAY CcbCCaja.ImpUsa[1] @ F-Importe
                 "US$/."            @ F-Moneda.
      END.

     
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
  
  RUN CCB\r-recibo (ROWID(CcbcCaja)).
  
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
        WHEN "Voucher[1]" THEN ASSIGN input-var-1 = x-tabla.
        
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
  {src/adm/template/snd-list.i "CcbCCaja"}

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
   IF CcbCCaja.CodCta[1]:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Concepto no debe ser blanco"
      VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO CcbCCaja.CodCta[1].
      RETURN "ADM-ERROR".      
    END.  
    IF cb-ctas.PidAux AND CcbCCaja.Codcli:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Concepto no debe ser blanco"
       VIEW-AS ALERT-BOX ERROR.
       APPLY "ENTRY" TO CcbCCaja.CodCta[1].
       RETURN "ADM-ERROR".      
    END.
    IF cb-ctas.PidDoc AND F-Coddoc:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Codigo De Documento no debe ser blanco"
       VIEW-AS ALERT-BOX ERROR.
       APPLY "ENTRY" TO F-CodDoc.
       RETURN "ADM-ERROR".      
    END.
    IF cb-ctas.PidDoc AND F-NroDoc:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Numero de Documento no debe ser blanco"
       VIEW-AS ALERT-BOX ERROR.
       APPLY "ENTRY" TO F-NroDoc.
       RETURN "ADM-ERROR".      
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

RETURN "ADM-ERROR".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

