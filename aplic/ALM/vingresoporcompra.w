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

/* Local Variable Definitions ---                                       */
DEFINE SHARED VAR lh_Handle AS HANDLE.
DEFINE SHARED VAR S-CODCIA AS INTEGER.
DEFINE SHARED VAR pv-codcia AS INT.
DEFINE SHARED VAR cl-codcia AS INT.
DEFINE SHARED VAR S-CODDIV AS CHAR.
DEFINE SHARED VAR S-USER-ID AS CHAR. 
DEFINE SHARED VAR s-seguridad AS CHAR.
DEFINE SHARED VAR S-CODALM AS CHAR.
DEFINE SHARED VAR S-NROSER  AS INTEGER.
DEFINE        VAR I-CODMON AS INTEGER NO-UNDO.
DEFINE        VAR R-ROWID  AS ROWID   NO-UNDO.
DEFINE        VAR L-CREA   AS LOGICAL NO-UNDO.
DEFINE        VAR F-ESTADO AS CHAR NO-UNDO.
DEFINE BUFFER TDOCM FOR Almtdocm.
DEFINE BUFFER CMOV  FOR Almcmov.
DEFINE SHARED TEMP-TABLE ITEM LIKE almdmov.
DEFINE SHARED VAR S-STATUS-ALMACEN AS LOG.

/* SOLO LOS USUARIOS DEL GRUPOS IMPORTACIONES Y ADMIN TENDRAN ACCESO TOTAL */
DEFINE VAR s-acceso-total AS LOG NO-UNDO.

IF s-user-id = 'ADMIN' OR LOOKUP('Importaciones', s-Seguridad) > 0 
    THEN s-acceso-total = YES.
ELSE s-acceso-total = NO.
s-acceso-total = YES.   /* Por ahora */

/* Variables para los mensajes de error */
DEF VAR pMensaje AS CHAR NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES Almcmov Almtdocm
&Scoped-define FIRST-EXTERNAL-TABLE Almcmov


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR Almcmov, Almtdocm.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Almcmov.NroRf1 Almcmov.CodPro Almcmov.CodMon ~
Almcmov.Observ Almcmov.TpoCmb Almcmov.CodCli Almcmov.NroRf3 
&Scoped-define ENABLED-TABLES Almcmov
&Scoped-define FIRST-ENABLED-TABLE Almcmov
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 
&Scoped-Define DISPLAYED-FIELDS Almcmov.NroDoc Almcmov.ModAdq ~
Almcmov.NroRf1 Almcmov.NroRf2 Almcmov.FchDoc Almcmov.CodPro Almcmov.CodMon ~
Almcmov.Observ Almcmov.TpoCmb Almcmov.CodCli Almcmov.NroRf3 ~
Almcmov.Libre_c03 
&Scoped-define DISPLAYED-TABLES Almcmov
&Scoped-define FIRST-DISPLAYED-TABLE Almcmov
&Scoped-Define DISPLAYED-OBJECTS F-modalidad F-STATUS FILL-IN-NomPro ~
FILL-IN-NomCli FILL-IN-NomChq 

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
DEFINE VARIABLE F-modalidad AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28.43 BY .69 NO-UNDO.

DEFINE VARIABLE F-STATUS AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14.29 BY .69
     FONT 0 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomChq AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomCli AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .69 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomPro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.14 BY .69 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 13.29 BY .77.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 4.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Almcmov.NroDoc AT ROW 1.27 COL 13 COLON-ALIGNED
          LABEL "No. Ingreso"
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
          FONT 0
     Almcmov.ModAdq AT ROW 1.27 COL 34.43 COLON-ALIGNED FORMAT "X(2)"
          VIEW-AS FILL-IN 
          SIZE 3.29 BY .69
     F-modalidad AT ROW 1.27 COL 38.57 COLON-ALIGNED NO-LABEL
     F-STATUS AT ROW 1.27 COL 71.57 COLON-ALIGNED NO-LABEL
     Almcmov.NroRf1 AT ROW 1.92 COL 13 COLON-ALIGNED
          LABEL "Referencia 1"
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     Almcmov.NroRf2 AT ROW 1.92 COL 39 COLON-ALIGNED
          LABEL "Referencia 2"
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     Almcmov.FchDoc AT ROW 1.92 COL 73.86 COLON-ALIGNED FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .69
     Almcmov.CodPro AT ROW 2.65 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .69
     FILL-IN-NomPro AT ROW 2.65 COL 23.86 COLON-ALIGNED NO-LABEL
     Almcmov.CodMon AT ROW 2.73 COL 76.57 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "S/.", 1,
"US$", 2
          SIZE 12 BY .58
     Almcmov.Observ AT ROW 3.35 COL 13 COLON-ALIGNED FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 47 BY .69
     Almcmov.TpoCmb AT ROW 3.35 COL 73.86 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .69
     Almcmov.CodCli AT ROW 4.04 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .69
     FILL-IN-NomCli AT ROW 4.04 COL 24 COLON-ALIGNED NO-LABEL
     Almcmov.NroRf3 AT ROW 4.04 COL 73.86 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     Almcmov.Libre_c03 AT ROW 4.85 COL 13 COLON-ALIGNED WIDGET-ID 4
          LABEL "Chequeador"
          VIEW-AS FILL-IN 
          SIZE 11 BY .69
     FILL-IN-NomChq AT ROW 4.85 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     "Moneda :" VIEW-AS TEXT
          SIZE 6.57 BY .69 AT ROW 2.73 COL 69
     RECT-4 AT ROW 2.65 COL 76
     RECT-5 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.Almcmov,integral.Almtdocm
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
         HEIGHT             = 5.31
         WIDTH              = 89.
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

/* SETTINGS FOR FILL-IN F-modalidad IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-STATUS IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Almcmov.FchDoc IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-NomChq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NomCli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NomPro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Almcmov.Libre_c03 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Almcmov.ModAdq IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Almcmov.NroDoc IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Almcmov.NroRf1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Almcmov.NroRf2 IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Almcmov.Observ IN FRAME F-Main
   EXP-FORMAT                                                           */
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

&Scoped-define SELF-NAME Almcmov.CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Almcmov.CodCli V-table-Win
ON LEAVE OF Almcmov.CodCli IN FRAME F-Main /* Cliente */
DO:
  IF Almcmov.CodCli:VISIBLE THEN DO:
        FIND gn-clie WHERE gn-clie.CodCia = cl-CodCia AND 
             gn-clie.CodCli = INPUT Almcmov.CodCli NO-LOCK NO-ERROR.
     IF AVAILABLE gn-clie THEN DISPLAY gn-clie.NomCli @ FILL-IN-NomCli WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Almcmov.CodMon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Almcmov.CodMon V-table-Win
ON ENTRY OF Almcmov.CodMon IN FRAME F-Main /* Cod!mon */
DO:
  IF I-CODMON <> 3 THEN DO:
     ASSIGN Almcmov.CodMon:SCREEN-VALUE = STRING(I-CODMON,'9').
     APPLY "ENTRY" TO Almcmov.TpoCmb.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Almcmov.CodPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Almcmov.CodPro V-table-Win
ON LEAVE OF Almcmov.CodPro IN FRAME F-Main /* Proveedor */
DO:
  IF Almcmov.CodPro:VISIBLE THEN DO:
          FIND gn-prov WHERE gn-prov.CodCia = pv-codcia AND 
               gn-prov.CodPro = INPUT Almcmov.CodPro NO-LOCK NO-ERROR.
          IF AVAILABLE gn-prov THEN DISPLAY gn-prov.NomPro @ FILL-IN-NomPro WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Almcmov.FchDoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Almcmov.FchDoc V-table-Win
ON LEAVE OF Almcmov.FchDoc IN FRAME F-Main /* Fecha */
DO:
  FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= INPUT Almcmov.FchDoc NO-LOCK NO-ERROR.
  IF AVAILABLE gn-tcmb THEN DISPLAY gn-tcmb.venta @ Almcmov.TpoCmb WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Almcmov.NroRf2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Almcmov.NroRf2 V-table-Win
ON LEAVE OF Almcmov.NroRf2 IN FRAME F-Main /* Referencia 2 */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     IF SELF:SCREEN-VALUE <> ' ' THEN DO:
        FIND FIRST CMov WHERE CMov.codcia = s-codcia AND
             CMov.Codpro = integral.Almcmov.CodPro:SCREEN-VALUE AND 
             CMov.NroRf2 = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE CMov THEN DO:
           IF L-CREA = No THEN 
              IF CMov.Nrodoc = Almcmov.Nrodoc THEN DO:
                 MESSAGE 'Numero de Guia se encuentra registrada' VIEW-AS ALERT-BOX ERROR.
                 APPLY 'ENTRY':U TO SELF.
                 RETURN NO-APPLY.
              END.
           ELSE DO:
              MESSAGE 'Numero de Guia se encuentra registrada' VIEW-AS ALERT-BOX ERROR.
              APPLY 'ENTRY':U TO SELF.
              RETURN NO-APPLY.
           END.
        END.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Almcmov.NroRf3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Almcmov.NroRf3 V-table-Win
ON LEAVE OF Almcmov.NroRf3 IN FRAME F-Main /* Referencia 3 */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     IF SELF:SCREEN-VALUE <> ' ' THEN DO:
        FIND FIRST CMov WHERE CMov.codcia = s-codcia AND
             CMov.Codpro = integral.Almcmov.CodPro:SCREEN-VALUE AND 
             CMov.NroRf3 = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE CMov THEN DO:
           IF L-CREA = No THEN 
              IF CMov.Nrodoc = Almcmov.Nrodoc THEN DO:
                 MESSAGE 'Numero de Guia se encuentra registrada' VIEW-AS ALERT-BOX ERROR.
                 APPLY 'ENTRY':U TO SELF.
                 RETURN NO-APPLY.
              END.
           ELSE DO:
              MESSAGE 'Numero de Guia se encuentra registrada' VIEW-AS ALERT-BOX ERROR.
              APPLY 'ENTRY':U TO SELF.
              RETURN NO-APPLY.
           END.
        END.
     END.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Almcmov.TpoCmb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Almcmov.TpoCmb V-table-Win
ON ENTRY OF Almcmov.TpoCmb IN FRAME F-Main /* Tipo de cambio */
DO:
  Almcmov.TpoCmb:SENSITIVE = NO.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Detalle-Orden-Compra V-table-Win 
PROCEDURE Actualiza-Detalle-Orden-Compra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER I-Factor AS INTEGER.

  FOR EACH Almdmov OF Almcmov NO-LOCK ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP  UNDO, RETURN "ADM-ERROR":
      {lib/lock-nowait.i &Tabla=Lg-docmp &Condicion="LG-DOCmp.CodCia = Almdmov.CodCia AND ~
          LOOKUP(LG-DOCmp.TpoDoc, 'N,C') > 0 AND ~
          LG-DOCmp.NroDoc = INTEGER(Almcmov.NroRf1) AND ~
          LG-DOCmp.Codmat = Almdmov.CodMat"}
/*       FIND LG-DOCmp WHERE LG-DOCmp.CodCia = Almdmov.CodCia AND            */
/*           LOOKUP(LG-DOCmp.TpoDoc, "N,C") > 0 AND                          */
/*           LG-DOCmp.NroDoc = INTEGER(Almcmov.NroRf1) AND                   */
/*           LG-DOCmp.Codmat = Almdmov.CodMat EXCLUSIVE-LOCK NO-ERROR.       */
/*       IF NOT AVAILABLE lg-docmp THEN DO:                                  */
/*           MESSAGE 'No se pudo actualizar el producto' Almdmov.codmat SKIP */
/*               'Grabaci�n abortada'                                        */
/*               VIEW-AS ALERT-BOX ERROR.                                    */
/*           UNDO, RETURN 'ADM-ERROR'.                                       */
/*       END.                                                                */
      LG-DOCmp.CanAten = LG-DOCmp.CanAten + (Almdmov.CanDes * I-Factor).
  END.
  RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-ITEM V-table-Win 
PROCEDURE Actualiza-ITEM :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ITEM:
    DELETE ITEM.
END.
IF NOT L-CREA THEN DO:
    /* RHC 03.12.2010 carga el saldo correcto */
    FIND LG-COCmp WHERE LG-COCmp.CodCia = Almcmov.CodCia 
        AND LOOKUP(LG-COCmp.TpoDoc, "N,C") > 0
        AND LG-COCmp.NroDoc = INTEGER(Almcmov.NroRf1) 
        NO-LOCK NO-ERROR.
   FOR EACH Almdmov OF Almcmov NO-LOCK :
       CREATE ITEM.
       ASSIGN ITEM.CodCia    = Almdmov.CodCia 
              ITEM.CodAlm    = Almdmov.CodAlm 
              ITEM.codmat    = Almdmov.codmat 
              ITEM.PreUni    = Almdmov.PreUni 
              ITEM.CanDes    = Almdmov.CanDes 
              ITEM.CanDev    = Almdmov.CanDes 
              ITEM.Factor    = Almdmov.Factor 
              ITEM.CodUnd    = Almdmov.CodUnd 
              ITEM.Pesmat    = Almdmov.Pesmat
              ITEM.ImpCto    = Almdmov.ImpCto 
              ITEM.PreLis    = Almdmov.PreLis 
              ITEM.Dsctos[1] = Almdmov.Dsctos[1] 
              ITEM.Dsctos[2] = Almdmov.Dsctos[2] 
              ITEM.Dsctos[3] = Almdmov.Dsctos[3] 
              ITEM.IgvMat    = Almdmov.IgvMat.
       IF AVAILABLE Lg-cocmp THEN DO:
           FIND Lg-docmp OF Lg-cocmp WHERE Lg-docmp.codmat = Almdmov.codmat NO-LOCK NO-ERROR.
           IF AVAILABLE Lg-docmp THEN DO:
               ASSIGN
                   ITEM.CanDev = ( Lg-docmp.CanPedi - Lg-docmp.CanAte) + Almdmov.CanDes.
           END.
       END.
   END.
END.
END PROCEDURE.

/*
FIND LG-COCmp WHERE 
            LG-COCmp.CodCia = Almcmov.CodCia AND
            LG-COCmp.TpoDoc = "N" AND
            LG-COCmp.NroDoc = INTEGER(Almcmov.NroRf1) EXCLUSIVE-LOCK NO-ERROR.
            */

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
  {src/adm/template/row-list.i "Almcmov"}
  {src/adm/template/row-list.i "Almtdocm"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "Almcmov"}
  {src/adm/template/row-find.i "Almtdocm"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna-Orden-Compra V-table-Win 
PROCEDURE Asigna-Orden-Compra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE ITEM.
FIND LG-COCmp WHERE ROWID(LG-COCmp) = output-var-1 NO-LOCK NO-ERROR.
IF AVAILABLE LG-COCmp THEN DO WITH FRAME {&FRAME-NAME}:
    DISPLAY 
        STRING(LG-COCmp.NroDoc, '999999') @ Almcmov.NroRf1 
        LG-COCmp.CodPro @ Almcmov.CodPro 
        LG-COCmp.NomPro @ FILL-IN-NomPro
        LG-COCmp.Observaciones @ Almcmov.Observ
        LG-COCmp.ModAdq @ Almcmov.ModAdq.
    Almcmov.CodMon:SCREEN-VALUE = STRING(LG-COCmp.Codmon).
    FOR EACH LG-DOCmp OF LG-COCmp NO-LOCK WHERE LG-DOCmp.CanPedi - LG-DOCmp.CanAten > 0:
        CREATE ITEM.
        ASSIGN
            ITEM.CodCia = S-CODCIA
            ITEM.CodAlm = S-CODALM
            ITEM.Codmat = LG-DOCmp.Codmat 
            ITEM.CodUnd = LG-DOCmp.UndCmp
            ITEM.CanDes = LG-DOCmp.CanPedi - LG-DOCmp.CanAten
            ITEM.CanDev = LG-DOCmp.CanPedi - LG-DOCmp.CanAten
            ITEM.PreLis = LG-DOCmp.PreUni
            ITEM.Dsctos[1] = LG-DOCmp.Dsctos[1]
            ITEM.Dsctos[2] = LG-DOCmp.Dsctos[2]
            ITEM.Dsctos[3] = LG-DOCmp.Dsctos[3]
            ITEM.IgvMat = LG-DOCmp.IgvMat
            ITEM.PreUni = ROUND(LG-DOCmp.PreUni * (1 - (LG-DOCmp.Dsctos[1] / 100)) * 
                                (1 - (LG-DOCmp.Dsctos[2] / 100)) * 
                                (1 - (LG-DOCmp.Dsctos[3] / 100)),4)
            ITEM.ImpCto = ROUND(ITEM.CanDes * ITEM.PreUni,2).
        FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
            AND  Almmmatg.codmat = ITEM.codmat  
            NO-LOCK NO-ERROR.
        FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
            AND  Almtconv.Codalter = ITEM.CodUnd 
            NO-LOCK NO-ERROR.
        ITEM.Factor = Almtconv.Equival / Almmmatg.FacEqu.
    END.
/*     RUN Procesa-Handle IN lh_Handle ('browse'). */
END.

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

  FOR EACH Almdmov OF Almcmov EXCLUSIVE-LOCK ON ERROR UNDO, RETURN "ADM-ERROR"
      ON STOP UNDO, RETURN 'ADM-ERROR':
      ASSIGN R-ROWID = ROWID(Almdmov).
      RUN ALM\ALMDCSTK (R-ROWID). /* Descarga del Almacen */
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      RUN ALM\ALMACPR1 (R-ROWID,"D").
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      DELETE Almdmov.
  END.
  RETURN 'OK'.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cerrar-Orden-Compra V-table-Win 
PROCEDURE Cerrar-Orden-Compra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR I-NRO AS INTEGER INIT 0 NO-UNDO.

  FOR EACH LG-DOCmp NO-LOCK WHERE LG-DOCmp.CodCia = s-CodCia 
        AND LOOKUP(LG-DOCmp.TpoDoc, "N,C") > 0
        AND LG-DOCmp.NroDoc = INTEGER(Almcmov.NroRf1):
    IF (LG-DOCmp.CanPedi - LG-DOCmp.CanAten) > 0 THEN DO:
        I-NRO = 1.
        LEAVE.
    END.
  END.

  DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      {lib/lock-nowait.i &Tabla=Lg-cocmp &Condicion="LG-COCmp.CodCia = Almcmov.CodCia AND ~
          LOOKUP(LG-COCmp.TpoDoc, 'N,C') > 0 AND ~
          LG-COCmp.NroDoc = INTEGER(Almcmov.NroRf1)"}

/*       FIND LG-COCmp WHERE LG-COCmp.CodCia = Almcmov.CodCia AND                   */
/*           LOOKUP(LG-COCmp.TpoDoc, "N,C") > 0 AND                                 */
/*           LG-COCmp.NroDoc = INTEGER(Almcmov.NroRf1) EXCLUSIVE-LOCK NO-ERROR.     */
/*       IF NOT AVAILABLE LG-cocmp THEN DO:                                         */
/*           MESSAGE 'NO se pudo actualizar la Orden de Compra' Almcmov.NroRf1 SKIP */
/*               'Grabaci�n abortada'                                               */
/*               VIEW-AS ALERT-BOX ERROR.                                           */
/*           RETURN 'ADM-ERROR'.                                                    */
/*       END.                                                                       */

      IF I-NRO = 0 THEN LG-COCmp.FlgSit = "T".
      ELSE              LG-COCmp.FlgSit = "P".
      LG-COCmp.FchAte = Almcmov.FchDoc.
      RELEASE LG-COCmp.
  END.
  RETURN 'OK'.
  
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
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR F-PesUnd AS DECIMAL NO-UNDO.

FOR EACH ITEM WHERE ITEM.codmat <> "" ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP  UNDO, RETURN "ADM-ERROR":
    CREATE almdmov.
    ASSIGN 
        Almdmov.CodCia = Almcmov.CodCia 
        Almdmov.CodAlm = Almcmov.CodAlm 
        Almdmov.TipMov = Almcmov.TipMov 
        Almdmov.CodMov = Almcmov.CodMov 
        Almdmov.NroSer = Almcmov.NroSer 
        Almdmov.NroDoc = Almcmov.NroDoc 
        Almdmov.CodMon = Almcmov.CodMon 
        Almdmov.FchDoc = Almcmov.FchDoc 
        Almdmov.TpoCmb = Almcmov.TpoCmb
        Almdmov.codmat = ITEM.codmat
        Almdmov.CanDes = ITEM.CanDes
        Almdmov.CodUnd = ITEM.CodUnd
        Almdmov.Factor = ITEM.Factor
        Almdmov.Pesmat = ITEM.Pesmat
        Almdmov.ImpCto = ROUND(ITEM.CanDes * ITEM.PreUni,2)
        Almdmov.PreLis = ITEM.PreLis
        Almdmov.PreUni = ITEM.PreUni
        Almdmov.Dsctos[1] = ITEM.Dsctos[1]
        Almdmov.Dsctos[2] = ITEM.Dsctos[2]
        Almdmov.Dsctos[3] = ITEM.Dsctos[3]
        Almdmov.IgvMat = ITEM.IgvMat
        Almdmov.CodAjt = 'A'
        Almdmov.HraDoc = Almcmov.HorRcp
               R-ROWID = ROWID(Almdmov).

    FIND Almmmatg WHERE Almmmatg.CodCia = Almdmov.CodCia 
        AND Almmmatg.CodMat = Almdmov.codmat NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmatg AND NOT Almmmatg.AftIgv THEN  Almdmov.IgvMat = 0.
    RUN ALM\ALMACSTK (R-ROWID).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    RUN ALM\ALMACPR1 (R-ROWID,"U").
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    /* RHC 03.04.04 BLOQUEADO, SE ACTUALIZA EN LA NOCHE
    RUN ALM\ALMACPR2 (R-ROWID,"U").
    *************************************************** */
     
    IF Almcmov.codmon = 1 
    THEN Almcmov.ImpMn1 = Almcmov.ImpMn1 + Almdmov.ImpMn1.
    ELSE Almcmov.ImpMn2 = Almcmov.ImpMn2 + Almdmov.ImpMn2.

    /* Actualiza el Peso del Material  */
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia AND
         Almmmatg.codmat = ITEM.Codmat NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmatg AND Almmmatg.Pesmat > 0 
    THEN DO:
         FIND CURRENT Almmmatg EXCLUSIVE-LOCK NO-ERROR.
         IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
         F-PesUnd = ROUND(ITEM.Pesmat / (ITEM.Candes * ITEM.Factor) * 1000, 3).
         IF ABS((F-PesUnd * 100 / Almmmatg.Pesmat) - 100) <= 4 
         THEN ASSIGN Almmmatg.Pesmat = F-PesUnd.
         RELEASE Almmmatg.
    END.
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
  IF s-acceso-total = NO THEN RETURN 'ADM-ERROR'.
  IF s-status-almacen = NO THEN DO:
      MESSAGE 'Almac�n INACTIVO' VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.

  /* CONTROL DE SERIES */
  FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
        AND Almacen.CodAlm = Almtdocm.CodAlm 
        NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Almacen THEN DO:
        MESSAGE 'Error en la serie' s-NroSer
            VIEW-AS ALERT-BOX ERROR.
        RETURN 'ADM-ERROR'.
  END.
  /* Solicitamos la Orden de Compra */
  ASSIGN
      input-var-1 = "P"   /* Solo aprobadas */
      input-var-2 = ''.
  RUN LKUP\C-OCPEN("Ordenes de Compra Pendientes").
  IF output-var-1 = ? THEN RETURN 'ADM-ERROR'.
  FIND LG-COCmp WHERE ROWID(LG-COCmp) = output-var-1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE LG-COCmp THEN RETURN 'ADM-ERROR'.
  IF Lg-cocmp.fchvto < TODAY THEN DO:
      MESSAGE 'La orden de compra ya venci� el' Lg-cocmp.fchvto VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR 'ADM-ERROR'.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  L-CREA = YES.
  DO WITH FRAME {&FRAME-NAME}:
     DISPLAY TODAY @ Almcmov.FchDoc Almacen.CorrIng @ Almcmov.NroDoc.
     /*FIND LAST gn-tcmb NO-LOCK NO-ERROR.*/
     FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= INPUT Almcmov.FchDoc NO-LOCK NO-ERROR.
     IF AVAILABLE gn-tcmb THEN DISPLAY gn-tcmb.venta @ Almcmov.TpoCmb.
     Almcmov.NroRf1:SENSITIVE = NO.
     Almcmov.CodMon:SENSITIVE = NO.
     RUN Asigna-Orden-Compra.
  END.
  RUN Procesa-Handle IN lh_Handle ('Pagina2').
  RUN Procesa-Handle IN lh_Handle ('browse').

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
      {lib/lock-generico.i &Bloqueo="EXCLUSIVE-LOCK" ~
          &Tabla=Almacen ~
          &Condicion="Almacen.CodCia = S-CODCIA AND Almacen.CodAlm = Almtdocm.CodAlm"~
          &Accion="RETRY" ~
          &Mensaje="YES" ~
          &TipoError=""ADM-ERROR""}
/*       {lib/lock-wait.i &Tabla=Almace &Condicion="Almacen.CodCia = S-CODCIA ~ */
/*           AND Almacen.CodAlm = Almtdocm.CodAlm"}                             */
      ASSIGN 
          Almcmov.CodCia  = Almtdocm.CodCia 
          Almcmov.CodAlm  = Almtdocm.CodAlm 
          Almcmov.TipMov  = Almtdocm.TipMov
          Almcmov.CodMov  = Almtdocm.CodMov
          Almcmov.NroSer  = s-NroSer
          Almcmov.HorRcp  = STRING(TIME,"HH:MM:SS")
          Almcmov.NomRef  = Fill-in-nompro:screen-value in frame {&FRAME-NAME}
          Almcmov.NroDoc = Almacen.CorrIng
          Almacen.CorrIng = Almacen.CorrIng + 1.
      DISPLAY Almcmov.NroDoc @ Almcmov.NroDoc WITH FRAME {&FRAME-NAME}.
  END.
  ASSIGN 
    Almcmov.usuario = S-USER-ID
    Almcmov.ImpIgv  = 0
    Almcmov.ImpMn1  = 0
    Almcmov.ImpMn2  = 0.

  /* ELIMINAMOS EL DETALLE ANTERIOR */
  IF NOT L-CREA THEN DO:
     RUN Actualiza-Detalle-Orden-Compra(-1).
     IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
         pMensaje = "NO se pudo actualizar la Orden de Compra.".
         UNDO, RETURN 'ADM-ERROR'.
     END.
     RUN Borra-Detalle.
     IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
         pMensaje = "NO se pudo borrar el detalle.".
         UNDO, RETURN 'ADM-ERROR'.
     END.
END.

/* GENERAMOS NUEVO DETALLE */
RUN Genera-Detalle.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    pMensaje = "NO se pudo generar el detalle.".
    UNDO, RETURN 'ADM-ERROR'.
END.

IF Almcmov.codmon = 1 
    THEN Almcmov.ImpMn2 = ROUND(Almcmov.ImpMn1 / Almcmov.tpocmb, 2).
ELSE Almcmov.ImpMn1 = ROUND(Almcmov.ImpMn2 * Almcmov.tpocmb, 2).
  
RUN Actualiza-Detalle-Orden-Compra(1).
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    pMensaje = "NO se pudo actualizar la Orden de Compra.".
    UNDO, RETURN 'ADM-ERROR'.
END.

RUN Cerrar-Orden-Compra.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    pMensaje = "NO se pudo actualizar la Orden de Compra." + CHR(10) +
        "Registro en uso por otro usuario.".
    UNDO, RETURN 'ADM-ERROR'.
END.
IF AVAILABLE(Almacen) THEN RELEASE Almacen.
IF AVAILABLE(Almmmatg) THEN RELEASE Almmmatg.
IF AVAILABLE(Lg-cocmp) THEN RELEASE Lg-cocmp.
IF AVAILABLE(Lg-docmp) THEN RELEASE Lg-docmp.
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
  RUN valida-update.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN ERROR.

  /* consistencia de la fecha del cierre del sistema */
/*   DEF VAR dFchCie AS DATE.                                                  */
/*   RUN gn/fecha-de-cierre (OUTPUT dFchCie).                                  */
/*   IF almcmov.fchdoc <= dFchCie THEN DO:                                     */
/*       MESSAGE 'NO se puede anular ningun documento antes del' (dFchCie + 1) */
/*           VIEW-AS ALERT-BOX WARNING.                                        */
/*       RETURN 'ADM-ERROR'.                                                   */
/*   END.                                                                      */
  /* fin de consistencia */

  /* Eliminamos el detalle */
  DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN 'ADM-ERROR':
     RUN Actualiza-Detalle-Orden-Compra(-1).
     IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

     RUN Borra-Detalle.
     IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

     RUN Cerrar-Orden-Compra.
     IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
     
     /* Solo marcamos el FlgEst como Anulado */
     FIND CMOV WHERE 
          CMOV.CodCia = Almcmov.CodCia AND 
          CMOV.CodAlm = Almcmov.CodAlm AND 
          CMOV.TipMov = Almcmov.TipMov AND 
          CMOV.CodMov = Almcmov.CodMov AND 
          CMOV.NroSer = Almcmov.NroSer AND 
          CMOV.NroDoc = Almcmov.NroDoc EXCLUSIVE-LOCK NO-ERROR.
     IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
     ASSIGN CMOV.FlgEst = 'A'
            CMOV.Observ = "      A   N   U   L   A   D   O       "
            CMOV.Usuario = S-USER-ID .
     RELEASE CMOV.
  END.

  /* refrescamos los datos del viewer */
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

  /* refrescamos los datos del browse */
  RUN Procesa-Handle IN lh_Handle ('browse').
   
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

  /* Buscamos en la tabla de movimientos y pedimos datos segun lo configurado*/
  FIND FIRST Almtmovm WHERE  Almtmovm.CodCia = Almtdocm.CodCia AND
                       Almtmovm.Tipmov = Almtdocm.TipMov AND 
                       Almtmovm.Codmov = Almtdocm.CodMov NO-LOCK NO-ERROR.
  IF AVAILABLE Almtmovm THEN DO WITH FRAME {&FRAME-NAME}:
     ASSIGN Almcmov.CodCli:VISIBLE = Almtmovm.PidCli
            Almcmov.CodPro:VISIBLE = Almtmovm.PidPro
            Almcmov.NroRf1:VISIBLE = Almtmovm.PidRef1
            /*Almcmov.NroRf2:VISIBLE = Almtmovm.PidRef2*/
            Almcmov.NroRf3:VISIBLE = Almtmovm.PidRef3
                          I-CODMON = Almtmovm.CodMon.
     IF Almtmovm.CodMon <> 3 THEN DO:
        ASSIGN Almcmov.CodMon:SCREEN-VALUE = STRING(Almtmovm.CodMon,'9').
     END.
     IF Almtmovm.PidRef1 THEN ASSIGN Almcmov.NroRf1:LABEL = Almtmovm.GloRf1.
     /*IF Almtmovm.PidRef2 THEN ASSIGN Almcmov.NroRf2:LABEL = Almtmovm.GloRf2.*/
     ASSIGN Almcmov.NroRf2:LABEL = Almtmovm.GloRf2. /* OJO */
     IF Almtmovm.PidRef3 THEN ASSIGN Almcmov.NroRf3:LABEL = Almtmovm.GloRf3.
     IF Almtdocm.TipMov = "S" THEN DO:
        ASSIGN Almcmov.TpoCmb:VISIBLE = NO
               Almcmov.CodMon:SCREEN-VALUE = '1'.
     END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  /* Code placed here will execute AFTER standard behavior.    */

  IF AVAILABLE Almcmov THEN DO WITH FRAME {&FRAME-NAME}:
     IF Almcmov.CodPro:VISIBLE THEN DO:
             FIND gn-prov WHERE gn-prov.CodCia = pv-codcia AND 
                  gn-prov.CodPro = Almcmov.CodPro NO-LOCK NO-ERROR.
             IF AVAILABLE gn-prov THEN DISPLAY gn-prov.NomPro @ FILL-IN-NomPro WITH FRAME {&FRAME-NAME}.
     END.
     CASE Almcmov.FlgEst:
          WHEN "A" THEN DISPLAY "ANULADO" @ F-Status WITH FRAME {&FRAME-NAME}.
          WHEN "D" THEN DISPLAY "DESACTIVADO" @ F-Status WITH FRAME {&FRAME-NAME}.
          WHEN ""  THEN DISPLAY "ACTIVO" @ F-Status WITH FRAME {&FRAME-NAME}.
     END CASE.          

    FIND almtabla WHERE almtabla.Tabla = 'MD' 
                   AND  almtabla.Codigo = Almcmov.ModAdq:screen-value 
                  NO-LOCK NO-ERROR.
    IF AVAILABLE almtabla THEN 
       F-modalidad:screen-value = almtabla.nombre.
    ELSE 
       F-modalidad:screen-value = "".

     IF Almcmov.CodCli:VISIBLE THEN DO:
             FIND gn-clie WHERE gn-clie.CodCia = cl-codcia AND 
                  gn-clie.CodCli = Almcmov.CodCli NO-LOCK NO-ERROR.
             IF AVAILABLE gn-clie THEN DISPLAY gn-clie.NomCli @ FILL-IN-NomCli WITH FRAME {&FRAME-NAME}.
     END.
     /* Chequeador */
     FIND FIRST pl-pers WHERE pl-pers.codper = Almcmov.Libre_c03 NO-LOCK NO-ERROR.
     IF AVAILABLE pl-pers THEN FILL-IN-NomChq:SCREEN-VALUE =  TRIM(PL-PERS.patper) + ' ' +
         TRIM(PL-PERS.matper) + ', ' + PL-PERS.nomper.
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
       Almcmov.CodPro:SENSITIVE = NO.
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
  
/*   RUN ALM\R-IMPFMT (ROWID(almcmov)). */
  RUN alm/r-impfmt-1 (ROWID(almcmov) , NO).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OK-WAIT-STATE AS LOGICAL NO-UNDO.
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("GENERAL").
  
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
  
  /* Dispatch standard ADM method.                             */
  pMensaje = "".
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ).
  IF RETURN-VALUE = 'ADM-ERROR' AND pMensaje <> "" THEN DO:
      MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.

  /* Code placed here will execute AFTER standard behavior.    */
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("").
  
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
  {src/adm/template/snd-list.i "Almcmov"}
  {src/adm/template/snd-list.i "Almtdocm"}

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

  IF p-state = 'update-begin':U THEN RUN valida-update.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN ERROR.
  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
  IF p-state = 'update-begin':U THEN DO WITH FRAME {&FRAME-NAME}:
     Almcmov.NroRf1:SENSITIVE = NO.
     Almcmov.CodMon:SENSITIVE = NO.
     L-CREA = NO.
     RUN Actualiza-ITEM.
     RUN Procesa-Handle IN lh_Handle ('Pagina2').
     RUN Procesa-Handle IN lh_Handle ('browse').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ue_imp_contafinan V-table-Win 
PROCEDURE ue_imp_contafinan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN alm/r-impfmt-1 (ROWID(almcmov) , YES).

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
DEFINE VARIABLE F-CANT AS DECIMAL NO-UNDO.
DEFINE VARIABLE L-ASIG AS LOGICAL NO-UNDO.
DO WITH FRAME {&FRAME-NAME} :
   IF Almcmov.CodPro:VISIBLE THEN DO:
         FIND gn-prov WHERE gn-prov.CodCia = pv-CodCia  AND
              gn-prov.CodPro = Almcmov.CodPro:SCREEN-VALUE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE gn-prov THEN DO:
            MESSAGE "Codigo de Proveedor no Existe" VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO Almcmov.CodPro.
            RETURN "ADM-ERROR".   
         END.
   END.
   IF Almcmov.CodCli:VISIBLE THEN DO:
         FIND gn-clie WHERE gn-clie.CodCia = cl-CodCia AND 
              gn-clie.CodCli = Almcmov.CodCli:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE gn-clie THEN DO:
         MESSAGE "Codigo de Cliente no Existe" VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO Almcmov.CodCli.
         RETURN "ADM-ERROR".   
      END.
   END.
/*    IF Almcmov.NroRf2:SCREEN-VALUE = "" THEN DO:                 */
/*       MESSAGE "Digite el Nro. de Guia" VIEW-AS ALERT-BOX ERROR. */
/*       APPLY "ENTRY" TO Almcmov.NroRf2.                          */
/*       RETURN "ADM-ERROR".                                       */
/*    END.                                                         */
   IF Almcmov.NroRf3:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Digite el Nro. de Guia" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO Almcmov.NroRf3.
      RETURN "ADM-ERROR".
   END.
   
   IF string(Almcmov.TpoCmb:SCREEN-VALUE,"Z9.9999") = "0.0000" THEN DO:
      MESSAGE "Ingrese el Tipo de Cambio " VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO Almcmov.TpoCmb.
      RETURN "ADM-ERROR".
   END.
   
   
   F-CANT = 0. 
   L-ASIG = YES.
   FOR EACH ITEM:
       F-CANT = F-CANT + ITEM.CanDes.
       FIND Almmmate WHERE
            Almmmate.CodCia = S-CODCIA AND
            Almmmate.CodAlm = S-CODALM AND
            Almmmate.codmat = ITEM.CodMat NO-LOCK NO-ERROR.
       IF NOT AVAILABLE Almmmate THEN DO:
          L-ASIG = NO.
          LEAVE.
       END.
   END.
   IF NOT L-ASIG THEN DO:
      MESSAGE "Existen articulos no asignados al almacen" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO Almcmov.Observ.
      RETURN "ADM-ERROR".   
   END.
   IF F-CANT = 0 THEN DO:
      MESSAGE "No existen ITEMS por recibir" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO Almcmov.Observ.
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
  Purpose:     
  Parameters:  <none>
  Notes:      
------------------------------------------------------------------------------*/

  IF s-status-almacen = NO THEN DO:
      MESSAGE 'Almac�n INACTIVO' VIEW-AS ALERT-BOX ERROR.
      RETURN 'ADM-ERROR'.
  END.

  DEFINE VAR RPTA AS CHAR.

  IF NOT AVAILABLE Almcmov THEN DO:
     MESSAGE "No Existen Registros" VIEW-AS ALERT-BOX ERROR.
     RETURN "ADM-ERROR".
  END.

  IF Almcmov.FlgEst = 'A' THEN DO:
     MESSAGE "Documento Anulado" VIEW-AS ALERT-BOX ERROR.
     RETURN "ADM-ERROR".
  END.

    RUN alm/p-ciealm-01 (Almcmov.FchDoc, s-CodAlm).
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN "ADM-ERROR".

    FIND Almacen WHERE Almacen.CodCia = S-CODCIA AND
         Almacen.CodAlm = S-CODALM NO-LOCK NO-ERROR.
    RUN ALM/D-CLAVE.R(Almacen.Clave,OUTPUT RPTA).
    IF RPTA = "ERROR" THEN RETURN "ADM-ERROR".
    /* consistencia de la fecha del cierre del sistema */
    DEF VAR dFchCie AS DATE.
    RUN gn/fecha-de-cierre (OUTPUT dFchCie).
    IF almcmov.fchdoc <= dFchCie THEN DO:
        MESSAGE 'NO se puede anular/modificar ningun documento antes del' (dFchCie + 1)
            VIEW-AS ALERT-BOX WARNING.
        RETURN 'ADM-ERROR'.
    END.
    /* fin de consistencia */

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

