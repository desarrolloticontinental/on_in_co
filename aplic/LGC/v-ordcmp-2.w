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
DEFINE SHARED VARIABLE S-CODCIA  AS INTEGER.
DEFINE SHARED VARIABLE PV-CODCIA  AS INTEGER.
DEFINE SHARED VARIABLE S-CODDIV  AS CHAR.
DEFINE SHARED VARIABLE S-PROVEE  AS CHAR.
DEFINE SHARED VARIABLE S-USER-ID AS CHAR.
DEFINE SHARED VARIABLE S-TPOCMB  AS DECIMAL.
DEFINE SHARED VARIABLE S-CODMON  AS INTEGER.
DEFINE SHARED VARIABLE S-TPODOC AS CHAR.
DEFINE SHARED VARIABLE S-PORCFR AS DEC.

DEFINE VARIABLE L-CREA   AS LOGICAL NO-UNDO.
DEFINE VARIABLE I-NROREQ AS INTEGER NO-UNDO.

DEFINE SHARED VAR lh_Handle AS HANDLE.

DEFINE SHARED TEMP-TABLE DCMP LIKE LG-DOCmp.
DEFINE BUFFER CCMP FOR LG-COCmp.

DEFINE VARIABLE F-FACTOR     AS DECIMAL   NO-UNDO.

DEFINE VARIABLE X-TIPO AS CHAR INIT "CC".
DEFINE VARIABLE s-Control-Compras AS LOG NO-UNDO.   /* Control de 7 dias */

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
&Scoped-define EXTERNAL-TABLES LG-COCmp
&Scoped-define FIRST-EXTERNAL-TABLE LG-COCmp


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR LG-COCmp.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS LG-COCmp.Codmon LG-COCmp.CodPro ~
LG-COCmp.FlgEst[1] LG-COCmp.ModAdq LG-COCmp.FchEnt LG-COCmp.FchVto ~
LG-COCmp.CndCmp LG-COCmp.TpoCmb LG-COCmp.Observaciones LG-COCmp.UsoInt ~
LG-COCmp.CodAlm LG-COCmp.CodMaq 
&Scoped-define ENABLED-TABLES LG-COCmp
&Scoped-define FIRST-ENABLED-TABLE LG-COCmp
&Scoped-Define ENABLED-OBJECTS RECT-25 RECT-24 
&Scoped-Define DISPLAYED-FIELDS LG-COCmp.SerReq LG-COCmp.NroReq ~
LG-COCmp.NroDoc LG-COCmp.Codmon LG-COCmp.CodPro LG-COCmp.NomPro ~
LG-COCmp.Fchdoc LG-COCmp.FlgEst[1] LG-COCmp.ModAdq LG-COCmp.FchEnt ~
LG-COCmp.FchVto LG-COCmp.CndCmp LG-COCmp.TpoCmb LG-COCmp.Observaciones ~
LG-COCmp.ObsInt[1] LG-COCmp.UsoInt LG-COCmp.CodAlm LG-COCmp.CodMaq 
&Scoped-define DISPLAYED-TABLES LG-COCmp
&Scoped-define FIRST-DISPLAYED-TABLE LG-COCmp
&Scoped-Define DISPLAYED-OBJECTS F-SitDoc F-modalidad F-RucPro F-DesCnd ~
F-DirEnt F-Contacto F-HorRec F-TlfAlm x-NomMaq 

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
DEFINE VARIABLE F-Contacto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Contacto" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .69 NO-UNDO.

DEFINE VARIABLE F-DesCnd AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37.29 BY .69 NO-UNDO.

DEFINE VARIABLE F-DirEnt AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37.43 BY .69 NO-UNDO.

DEFINE VARIABLE F-HorRec AS CHARACTER FORMAT "X(256)":U 
     LABEL "Horario de Recepcion" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .69 NO-UNDO.

DEFINE VARIABLE F-modalidad AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23.43 BY .69 NO-UNDO.

DEFINE VARIABLE F-RucPro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY .69 NO-UNDO.

DEFINE VARIABLE F-SitDoc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Situacion" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .69
     FONT 0 NO-UNDO.

DEFINE VARIABLE F-TlfAlm AS CHARACTER FORMAT "X(256)":U 
     LABEL "Telefono" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .69 NO-UNDO.

DEFINE VARIABLE x-NomMaq AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .69 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92.72 BY 6.54.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92.72 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     LG-COCmp.SerReq AT ROW 1.12 COL 31.14 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.29 BY .69
     LG-COCmp.NroReq AT ROW 1.12 COL 35.43 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY .69
     LG-COCmp.NroDoc AT ROW 1.15 COL 11.14 COLON-ALIGNED
          LABEL "No. O/C"
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .69
     LG-COCmp.Codmon AT ROW 1.15 COL 45 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Soles", 1,
"D�lares", 2
          SIZE 14.86 BY .69
     F-SitDoc AT ROW 1.15 COL 75.14 COLON-ALIGNED
     LG-COCmp.CodPro AT ROW 1.85 COL 11.14 COLON-ALIGNED
          LABEL "Sr(es)" FORMAT "x(11)"
          VIEW-AS FILL-IN 
          SIZE 11 BY .69
     LG-COCmp.NomPro AT ROW 1.85 COL 23 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 35 BY .69
     LG-COCmp.Fchdoc AT ROW 1.85 COL 76.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     LG-COCmp.FlgEst[1] AT ROW 2.54 COL 11.14 COLON-ALIGNED
          LABEL "INCOTERM"
          VIEW-AS COMBO-BOX INNER-LINES 5
          DROP-DOWN-LIST
          SIZE 10.86 BY 1
     LG-COCmp.ModAdq AT ROW 3.5 COL 11.14 COLON-ALIGNED FORMAT "X(2)"
          VIEW-AS FILL-IN 
          SIZE 3.43 BY .69
     F-modalidad AT ROW 3.5 COL 15 COLON-ALIGNED NO-LABEL
     F-RucPro AT ROW 3.5 COL 46.14 COLON-ALIGNED NO-LABEL
     LG-COCmp.FchEnt AT ROW 3.5 COL 76.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     LG-COCmp.FchVto AT ROW 4.19 COL 76.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     LG-COCmp.CndCmp AT ROW 4.27 COL 11.14 COLON-ALIGNED
          LABEL "Forma de Pago" FORMAT "X(3)"
          VIEW-AS FILL-IN 
          SIZE 7.43 BY .69
     F-DesCnd AT ROW 4.27 COL 20.14 COLON-ALIGNED NO-LABEL
     LG-COCmp.TpoCmb AT ROW 4.88 COL 76.14 COLON-ALIGNED
          LABEL "Tipo de cambio" FORMAT "ZZ9.9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .69
     LG-COCmp.Observaciones AT ROW 4.96 COL 11.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 46.29 BY .69
     LG-COCmp.ObsInt[1] AT ROW 5.69 COL 60.14 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 30 BY 1.54
          BGCOLOR 15 FGCOLOR 9 
     LG-COCmp.UsoInt AT ROW 5.73 COL 13.14 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 46.72 BY 1.54
          BGCOLOR 11 
     LG-COCmp.CodAlm AT ROW 7.65 COL 15.29 COLON-ALIGNED
          LABEL "Almacen de Entrega"
          VIEW-AS FILL-IN 
          SIZE 4.72 BY .69
     F-DirEnt AT ROW 7.65 COL 20.86 COLON-ALIGNED NO-LABEL
     F-Contacto AT ROW 7.65 COL 76.57 COLON-ALIGNED
     F-HorRec AT ROW 8.35 COL 15.29 COLON-ALIGNED
     F-TlfAlm AT ROW 8.35 COL 76.57 COLON-ALIGNED
     LG-COCmp.CodMaq AT ROW 9.19 COL 15.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .69
     x-NomMaq AT ROW 9.19 COL 21 COLON-ALIGNED NO-LABEL
     "Uso Interno" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 6.23 COL 4
     "R.U.C." VIEW-AS TEXT
          SIZE 5.14 BY .69 AT ROW 3.5 COL 43.14
     "No.Requis." VIEW-AS TEXT
          SIZE 7.72 BY .69 AT ROW 1.12 COL 25.29
     RECT-25 AT ROW 7.54 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RECT-24 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.LG-COCmp
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
         HEIGHT             = 10.27
         WIDTH              = 99.14.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN LG-COCmp.CndCmp IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LG-COCmp.CodAlm IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LG-COCmp.CodPro IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN F-Contacto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-DesCnd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-DirEnt IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-HorRec IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-modalidad IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-RucPro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-SitDoc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-TlfAlm IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LG-COCmp.Fchdoc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX LG-COCmp.FlgEst[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LG-COCmp.ModAdq IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LG-COCmp.NomPro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LG-COCmp.NroDoc IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN LG-COCmp.NroReq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR LG-COCmp.ObsInt[1] IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LG-COCmp.SerReq IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LG-COCmp.TpoCmb IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN x-NomMaq IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME LG-COCmp.CndCmp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LG-COCmp.CndCmp V-table-Win
ON LEAVE OF LG-COCmp.CndCmp IN FRAME F-Main /* Forma de Pago */
DO:
  IF LG-COCmp.CndCmp:SCREEN-VALUE <> "" THEN DO:
     F-DesCnd:SCREEN-VALUE = "".
     FIND gn-concp WHERE gn-concp.Codig = LG-COCmp.CndCmp:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE gn-concp THEN F-DesCnd:SCREEN-VALUE = gn-concp.Nombr.
        DISPLAY gn-concp.Nombr @ F-DesCnd WITH FRAME {&FRAME-NAME}.
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LG-COCmp.CodAlm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LG-COCmp.CodAlm V-table-Win
ON LEAVE OF LG-COCmp.CodAlm IN FRAME F-Main /* Almacen de Entrega */
DO:
  IF SELF:SCREEN-VALUE = "" THEN RETURN.
     FIND Almacen WHERE Almacen.CodCia = S-CODCIA  AND
          Almacen.CodAlm = LG-COCmp.CodAlm:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE Almacen THEN
        DISPLAY Almacen.DirAlm @ F-DirEnt 
                Almacen.EncAlm @ F-Contacto 
                Almacen.HorRec @ F-HorRec 
                Almacen.TelAlm @ F-TlfAlm WITH FRAME {&FRAME-NAME}.
     ELSE DO:
          MESSAGE "Almacen no Registrado" VIEW-AS ALERT-BOX.
          RETURN NO-APPLY.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LG-COCmp.CodMaq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LG-COCmp.CodMaq V-table-Win
ON LEAVE OF LG-COCmp.CodMaq IN FRAME F-Main /* Destino */
DO:
  x-NomMaq:SCREEN-VALUE = ''.
  FIND Almtabla WHERE Almtabla.tabla = 'MQ' AND
    Almtabla.codigo = INPUT {&SELF-NAME} NO-LOCK NO-ERROR.
  IF AVAILABLE Almtabla THEN x-NomMaq:SCREEN-VALUE = almtabla.Nombre.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LG-COCmp.Codmon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LG-COCmp.Codmon V-table-Win
ON VALUE-CHANGED OF LG-COCmp.Codmon IN FRAME F-Main /* Mon */
DO:
  S-CODMON = INTEGER(SELF:SCREEN-VALUE).
  RUN Procesa-Handle IN lh_Handle ('Moneda').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LG-COCmp.CodPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LG-COCmp.CodPro V-table-Win
ON LEAVE OF LG-COCmp.CodPro IN FRAME F-Main /* Sr(es) */
DO:
  IF SELF:SCREEN-VALUE = "" THEN RETURN.
  FIND gn-prov WHERE gn-prov.CodCia = PV-CODCIA AND
        gn-prov.CodPro = LG-Cocmp.CodPro:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE gn-prov THEN
      DISPLAY gn-prov.NomPro @ LG-COCmp.NomPro
/*              gn-prov.Contactos[2] @ F-ConPro*/
              gn-prov.Ruc          @ F-RucPro WITH FRAME {&FRAME-NAME}.
  ELSE DO:
        MESSAGE "Proveedor no Registrado" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
  END.
  IF gn-prov.flgsit = 'C' THEN DO:
    MESSAGE 'Proveedor CESADO' VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.
  S-PROVEE = LG-Cocmp.CodPro:SCREEN-VALUE.
  APPLY "ENTRY":U TO LG-COCmp.Fchdoc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LG-COCmp.Fchdoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LG-COCmp.Fchdoc V-table-Win
ON LEAVE OF LG-COCmp.Fchdoc IN FRAME F-Main /* Fecha Emision */
DO:
  IF INPUT LG-COCmp.Fchdoc = ? THEN RETURN.
  FIND LAST gn-tcmb WHERE gn-tcmb.FECHA <= INPUT LG-COCmp.Fchdoc NO-LOCK NO-ERROR.
  IF AVAILABLE gn-tcmb THEN DO:
     DISPLAY gn-tcmb.compra @ LG-COCmp.TpoCmb WITH FRAME {&FRAME-NAME}.
     S-TPOCMB = gn-tcmb.compra.
  END.
  ELSE MESSAGE "Tipo de cambio no registrado" VIEW-AS ALERT-BOX WARNING.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LG-COCmp.FlgEst[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LG-COCmp.FlgEst[1] V-table-Win
ON VALUE-CHANGED OF LG-COCmp.FlgEst[1] IN FRAME F-Main /* INCOTERM */
DO:
  S-PORCFR = 0.
  FIND Lg-tabla WHERE Lg-tabla.codcia = s-codcia
    AND Lg-tabla.tabla = '01'
    AND Lg-tabla.codigo = SELF:SCREEN-VALUE
    NO-LOCK NO-ERROR.
  IF AVAILABLE Lg-tabla THEN s-PorCfr = lg-tabla.Valor[1].
  RUN Procesa-Handle IN lh_Handle ('Moneda').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LG-COCmp.ModAdq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LG-COCmp.ModAdq V-table-Win
ON LEAVE OF LG-COCmp.ModAdq IN FRAME F-Main /* Modalidad */
DO:
  IF LG-COCmp.ModAdq:screen-value <> "" THEN DO:
  FIND almtabla WHERE almtabla.Tabla = X-TIPO 
                 AND  almtabla.Codigo = LG-COCmp.ModAdq:screen-value 
                NO-LOCK NO-ERROR.
  IF AVAILABLE almtabla THEN 
     F-modalidad:screen-value = almtabla.nombre.
  ELSE 
     F-modalidad:screen-value = "".
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LG-COCmp.TpoCmb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LG-COCmp.TpoCmb V-table-Win
ON LEAVE OF LG-COCmp.TpoCmb IN FRAME F-Main /* Tipo de cambio */
DO:
  S-TPOCMB = DECIMAL(SELF:SCREEN-VALUE).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-DCMP V-table-Win 
PROCEDURE Actualiza-DCMP :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH DCMP:
    DELETE DCMP.
END.
IF NOT L-CREA THEN DO:
   FOR EACH LG-DOCmp NO-LOCK WHERE LG-DOCmp.CodCia = LG-COCmp.CodCia 
                      AND  LG-DOCmp.TpoDoc = LG-COCmp.TpoDoc 
                      AND  LG-DOCmp.NroDoc = LG-COCmp.NroDoc:
       CREATE DCMP.
       BUFFER-COPY Lg-docmp TO DCMP.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Detalle-Requisicion V-table-Win 
PROCEDURE Actualiza-Detalle-Requisicion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER I-FAC AS INTEGER.
   FOR EACH LG-DOCmp NO-LOCK WHERE LG-DOCmp.CodCia = LG-COCmp.CodCia 
                              AND  LG-DOCmp.TpoDoc = LG-COCmp.TpoDoc 
                              AND  LG-DOCmp.NroDoc = LG-COCmp.NroDoc
                             ON ERROR UNDO, RETURN "ADM-ERROR":
       FIND LG-DRequ WHERE LG-DRequ.CodCia = LG-DOCmp.CodCia 
                      AND  LG-DRequ.NroReq = LG-COCmp.NroReq 
                      AND  LG-DRequ.Codmat = LG-DOCmp.CodMat 
                     EXCLUSIVE-LOCK NO-ERROR.
       IF AVAILABLE LG-DRequ THEN DO:
          ASSIGN LG-DRequ.CanAten = LG-DRequ.CanAten + (I-FAC * LG-DOCmp.CanPedi ).
       END.
       RELEASE LG-DRequ.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Estado-Requisicion V-table-Win 
PROCEDURE Actualiza-Estado-Requisicion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ON ERROR UNDO, RETURN "ADM-ERROR":
   DEFINE VAR I-Nro AS INTEGER NO-UNDO.
   FOR EACH LG-DRequ NO-LOCK WHERE LG-DRequ.CodCia = LG-COCmp.CodCia 
                              AND  LG-CRequ.TpoReq = "N" 
                              AND  LG-DRequ.NroReq = LG-COCmp.NroReq:
       IF (LG-DRequ.CanApro - LG-DRequ.CanAten) > 0 THEN DO:
          I-NRO = 1.
          LEAVE.
       END.
   END.
   FIND LG-CRequ WHERE LG-CRequ.CodCia = LG-COCmp.CodCia 
                  AND  LG-CRequ.TpoReq = "N" 
                  AND  LG-CRequ.NroReq = LG-COCmp.NroReq 
                 EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE LG-CRequ THEN DO:
      IF I-NRO = 0 THEN ASSIGN LG-CRequ.FlgSit = "C".
      ELSE ASSIGN LG-CRequ.FlgSit = "P".
   END.
   RELEASE LG-CRequ.
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
  {src/adm/template/row-list.i "LG-COCmp"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "LG-COCmp"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Anula-Requisicion-Detalle V-table-Win 
PROCEDURE Anula-Requisicion-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR x-CanPedi AS DEC NO-UNDO.
  DEF VAR x-CanAten AS DEC NO-UNDO.
  
  FOR EACH LG-DOCmp OF Lg-COCmp NO-LOCK:
    x-CanPedi = LG-DOCmp.CanPedi.
    FOR EACH Lg-DRequ WHERE Lg-DRequ.codcia = s-codcia 
            AND Lg-DRequ.NroO_C = LG-COCmp.NroDoc
            AND Lg-DRequ.CodMat = LG-DOCmp.CodMat,
            FIRST Lg-CRequ OF Lg-DRequ:
        ASSIGN
            x-CanAten = MINIMUM(LG-DRequ.CanAten, x-CanPedi)
            Lg-DRequ.CanAten = LG-DRequ.CanAten - x-CanAten
            Lg-DRequ.NroO_C  = 0
            x-CanPedi        = x-CanPedi - x-CanAten
            LG-CRequ.FlgSit  = 'S'.      /* OJO */
    END.        
  END.
/*  FOR EACH LG-DOCmp OF Lg-COCmp NO-LOCK:
 *     x-CanPedi = LG-DOCmp.CanPedi.
 *     FOR EACH Lg-CRequ WHERE Lg-CRequ.codcia = s-codcia 
 *             AND LG-CRequ.NroO_C = LG-COCmp.NroDoc:
 *         FIND LG-DRequ OF Lg-CRequ WHERE LG-DRequ.Codmat = LG-DOCmp.CodMat EXCLUSIVE-LOCK NO-ERROR.
 *         IF AVAILABLE LG-DRequ 
 *         THEN ASSIGN
 *                 x-CanAten = MINIMUM(LG-DRequ.CanPedi, x-CanPedi)
 *                 LG-DRequ.CanAten = LG-DRequ.CanAten - x-CanAten
 *                 x-CanPedi = x-CanPedi - x-CanAten
 *                 LG-CRequ.FlgSit = 'S'.      /* OJO */
 *     END.        
 *   END.*/
  
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
   FOR EACH LG-DOCmp WHERE LG-DOCmp.CodCia = LG-COCmp.CodCia 
                      AND  LG-DOCmp.TpoDoc = LG-COCmp.TpoDoc 
                      AND  LG-DOCmp.NroDoc = LG-COCmp.NroDoc:
       DELETE LG-DOCmp.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cambiar-Revision V-table-Win 
PROCEDURE Cambiar-Revision :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* RHC 25.11.04 Solo aquellas ordenes en revision */
  DEF VAR f-ImpTot AS DEC NO-UNDO.
  
  f-ImpTot = Lg-Cocmp.ImpTot.
  IF Lg-Cocmp.codmon = 1 THEN f-ImpTot = f-ImpTot / Lg-Cocmp.TpoCmb.
  IF f-ImpTot > 1000 THEN RETURN.
  IF LOOKUP(TRIM(Lg-Cocmp.cndcmp), '000,001,002,003,107,115') > 0 THEN RETURN.

  IF LG-CoCmp.FlgSit = 'R'
  THEN DO:
    MESSAGE 'Desea cambiar el estado de la orden a EMITIDO?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE RPTA-1 AS LOG.
    IF RPTA-1 = YES THEN DO:
        FIND CURRENT Lg-cocmp EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Lg-cocmp THEN LG-COCmp.FlgSit  = 'G'.
        FIND CURRENT Lg-cocmp NO-LOCK NO-ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Control-Compras V-table-Win 
PROCEDURE Control-Compras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR x-Ok AS LOG NO-UNDO.  
  DEF VAR x-VtaEva AS DEC NO-UNDO.
  DEF VAR x-VtaHis AS DEC NO-UNDO.
  DEF VAR x-HisPro AS DEC NO-UNDO.
  DEF VAR x-FchDoc-1 AS DATE NO-UNDO.
  DEF VAR x-FchDoc-2 AS DATE NO-UNDO.
  DEF VAR x-FchDoc-3 AS DATE NO-UNDO.
  DEF VAR x-FchDoc-4 AS DATE NO-UNDO.
  DEF VAR x-FchDoc-5 AS DATE NO-UNDO.
  DEF VAR x-FchDoc-6 AS DATE NO-UNDO.
  DEF VAR x-Factor AS DEC NO-UNDO.
  DEF VAR x-ProVta AS DEC NO-UNDO.
  DEF VAR x-StkMin AS DEC NO-UNDO.
  DEF VAR x-Dias AS DEC NO-UNDO.
  DEF VAR x-Entrega AS INT INIT 1 NO-UNDO.
  DEF VAR x-Tramites AS INT NO-UNDO.

  ASSIGN
    x-Ok       = YES
    x-Dias     = 7
    x-FchDoc-2 = TODAY
    x-FchDoc-1 = TODAY - 30 
    x-FchDoc-3 = x-FchDoc-1 - 365.
    x-FchDoc-4 = x-FchDoc-2 - 365.
    x-FchDoc-5 = TODAY + 1 - 365.
    x-FchDoc-6 = x-FchDoc-5 + x-Dias.

  FOR EACH DCMP,
        FIRST Almmmate NO-LOCK WHERE Almmmate.CodCia = s-codcia
            AND Almmmate.codalm = LG-COCmp.CodAlm:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            AND Almmmate.codmat = DCMP.codmat,
        FIRST Almmmatg OF Almmmate NO-LOCK:
  
    {lgc/calc_stk_min.i}
  
    IF DCMP.CanPedi > (x-StkMin * x-Dias) THEN DO:
        MESSAGE 'Se ha superado el pronostico de ventas para los proximos' x-Dias 'dias' SKIP
            '    Codigo:' DCMP.codmat SKIP
            '    Compra:' DCMP.canpedi SKIP
            'Pronostico:' (x-StkMin * x-Dias) SKIP
            VIEW-AS ALERT-BOX WARNING.
        x-Ok = NO.
    END.
  END.
  IF x-Ok = NO
  THEN RETURN 'ADM-ERROR'.
  ELSE RETURN 'OK'.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Ean V-table-Win 
PROCEDURE Genera-Ean :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*IF Lg-COCmp.FlgSit = 'P'
  THEN*/ RUN lgc/genera-ean (ROWID(Lg-COCmp)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Orden-Compra V-table-Win 
PROCEDURE Genera-Orden-Compra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH DCMP WHERE DCMP.CanPedi > 0:
    CREATE LG-DOCmp.
    BUFFER-COPY DCMP TO Lg-DOcmp
        ASSIGN 
            LG-DOCmp.CodCia = LG-COCmp.CodCia 
            LG-DOCMP.CodDiv = LG-COCmp.CodDiv
            LG-DOCmp.TpoDoc = LG-COCmp.TpoDoc 
            LG-DOCmp.NroDoc = LG-COCmp.NroDoc.
    RELEASE LG-DOCmp.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime-Texto V-table-Win 
PROCEDURE Imprime-Texto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF LG-COCMP.FlgSit <> "A" THEN RUN lgc\r-imptxt(ROWID(LG-COCmp)).
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
  FIND LG-CORR WHERE LG-CORR.CodCia = S-CODCIA 
    AND LG-CORR.CodDiv = s-CodDiv
    AND  LG-CORR.CodDoc = "O/C" 
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE LG-CORR THEN DO:
    MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  L-CREA = YES.
  RUN Actualiza-DCMP.
  RUN Procesa-Handle IN lh_Handle ("Pagina2").
  RUN Procesa-Handle IN lh_Handle ('Browse-add').
  DO WITH FRAME {&FRAME-NAME}:
     DISPLAY 
        TODAY     @ LG-COCmp.Fchdoc 
        TODAY + 1 @ LG-COCmp.FchEnt 
        TODAY + 7 @ LG-COCmp.FchVto
        LG-CORR.NroDoc @ Lg-Cocmp.nrodoc.
     FIND LAST gn-tcmb WHERE gn-tcmb.FECHA <= TODAY NO-LOCK NO-ERROR.
     IF AVAILABLE gn-tcmb THEN DO:
        DISPLAY gn-tcmb.compra @ LG-COCmp.TpoCmb.
        S-TPOCMB = gn-tcmb.compra.
     END.
     S-CODMON = 1.
     /* Valor por defecto */
     s-PorCfr = 0.
     FIND FIRST LG-tabla WHERE Lg-tabla.codcia = s-codcia
        AND Lg-tabla.tabla = '01'
        NO-LOCK NO-ERROR.
     IF AVAILABLE Lg-Tabla THEN DO:
        s-PorCfr = Lg-Tabla.Valor[1].
        LG-COCmp.FlgEst[1]:SCREEN-VALUE = Lg-tabla.codigo.
     END.
  END.
  I-NROREQ = 0.
  S-PROVEE = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement V-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE X-NRODOC AS INTEGER NO-UNDO.
  DEFINE VARIABLE f-ImpTot AS DEC NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF L-CREA THEN DO:
     FIND LG-CORR WHERE LG-CORR.CodCia = S-CODCIA 
        AND LG-CORR.CodDiv = s-CodDiv
        AND  LG-CORR.CodDoc = "O/C" 
        EXCLUSIVE-LOCK NO-ERROR.
     X-NRODOC = LG-CORR.NroDoc.
     ASSIGN LG-CORR.NroDoc = LG-CORR.NroDoc + 1.
     RELEASE LG-CORR.
     ASSIGN LG-COCmp.CodCia = S-CODCIA
            LG-COCmp.CodDiv = s-CodDiv
            LG-COCmp.TpoDoc = s-TpoDoc
            LG-COCmp.NroDoc = X-NRODOC
            LG-COCmp.FlgSit = "G"
            LG-COCmp.NroReq = I-NROREQ
            LG-COCmp.Userid-com = S-USER-ID.
  END.
  FIND gn-prov WHERE gn-prov.CodCia = PV-CODCIA 
                AND  gn-prov.CodPro = LG-COCmp.CodPro 
               NO-LOCK NO-ERROR.
  IF AVAILABLE gn-prov THEN 
     ASSIGN LG-COCmp.NomPro = gn-prov.NomPro.
  ASSIGN 
    LG-COCmp.ImpTot = 0
    LG-COCmp.ImpExo = 0.
/* BLOQUEAMOS CALCULO ANTERIOR
  FOR EACH DCMP NO-LOCK:
      LG-COCmp.ImpTot = LG-COCmp.ImpTot + DCMP.ImpTot.
      IF DCMP.IgvMat = 0 THEN LG-COCmp.ImpExo = LG-COCmp.ImpExo + DCMP.ImpTot.
  END.
  FIND LAST LG-CFGIGV NO-LOCK NO-ERROR.
  ASSIGN 
    LG-COCmp.ImpIgv = (LG-COCmp.ImpTot - LG-COCmp.ImpExo) - 
                        ROUND((LG-COCmp.ImpTot - LG-COCmp.ImpExo) / (1 + LG-CFGIGV.PorIgv / 100),2)
    LG-COCmp.ImpBrt = LG-COCmp.ImpTot - LG-COCmp.ImpExo - LG-COCmp.ImpIgv
    LG-COCmp.ImpDto = 0
    LG-COCmp.ImpNet = LG-COCmp.ImpBrt - LG-COCmp.ImpDto.
*/
  FOR EACH DCMP NO-LOCK:
      LG-COCmp.ImpTot = LG-COCmp.ImpTot + DCMP.ImpTot.
  END.
  ASSIGN 
    LG-COCmp.ImpBrt = LG-COCmp.ImpTot - LG-COCmp.ImpExo - LG-COCmp.ImpIgv
    LG-COCmp.ImpDto = 0
    LG-COCmp.ImpNet = LG-COCmp.ImpBrt - LG-COCmp.ImpDto.
  
  IF NOT L-CREA THEN DO:
     RUN Actualiza-Detalle-Requisicion(-1).
     RUN Borra-Detalle.
  END.
/* RHC 17.01.2007 Sacarlo de la transaccion principal
  RUN Genera-Orden-Compra.
*/

/*  RUN Actualiza-Detalle-Requisicion(1).
 *   RUN Actualiza-Estado-Requisicion.*/
  
/*  /* RHC 25.11.04 Solo aquellas ordenes en revision */
 *   IF LG-CoCmp.FlgSit = 'R'
 *   THEN DO:
 *     MESSAGE 'Desea cambiar el estado de la orden a EMITIDO?'
 *         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
 *         UPDATE RPTA-1 AS LOG.
 *     IF RPTA-1 = YES THEN LG-COCmp.FlgSit  = 'G'.
 *   END.*/
  /* RHC 7.11.06 cambia l condicion Diana Su 
  IF s-Control-Compras = YES THEN Lg-Cocmp.FlgSit = 'R'.    /* EN REVISION */
  ****************************************** */
/* BLOQUEADO A PEDIDO DE DIANA 11.07.2007
  f-ImpTot = Lg-Cocmp.ImpTot.
  IF Lg-Cocmp.codmon = 1 THEN f-ImpTot = f-ImpTot / Lg-Cocmp.TpoCmb.
  IF f-ImpTot > 1000 THEN Lg-Cocmp.FlgSit = 'R'.    /* EN REVISION */
  IF LOOKUP(TRIM(Lg-Cocmp.cndcmp), '000,001,002,003,107,115') > 0 THEN Lg-Cocmp.FlgSit = 'R'.    /* EN REVISION */
*/

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

  RUN Procesa-Handle IN lh_Handle ("Pagina1").
  RUN Procesa-Handle IN lh_Handle ('Browse').

  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  MESSAGE '�Copia tambi�n el detalle?' VIEW-AS ALERT-BOX QUESTION
    BUTTONS YES-NO-CANCEL UPDATE rpta-1 AS LOG.
  IF rpta-1 = ? THEN RETURN 'ADM-ERROR'.
  S-CODMON = LG-COCmp.Codmon.
  S-PROVEE = LG-Cocmp.CodPro.
  I-NROREQ = LG-COCmp.NroReq.
  S-PORCFR = 0.
  FIND Lg-tabla WHERE Lg-tabla.codcia = s-codcia
    AND Lg-tabla.tabla = '01'
    AND Lg-tabla.codigo = LG-COCmp.FlgEst[1]
    NO-LOCK NO-ERROR.
  IF AVAILABLE Lg-tabla THEN s-PorCfr = lg-tabla.Valor[1].
  
  L-CREA = IF rpta-1 = YES THEN NO ELSE YES.
  RUN Actualiza-DCMP.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  L-CREA = YES.
  RUN Procesa-Handle IN lh_Handle ("Pagina2").
  RUN Procesa-Handle IN lh_Handle ('Browse-add').
  DO WITH FRAME {&FRAME-NAME}:
     DISPLAY TODAY     @ LG-COCmp.Fchdoc 
             TODAY + 1 @ LG-COCmp.FchEnt 
             TODAY + 7 @ LG-COCmp.FchVto.
     FIND LAST gn-tcmb WHERE gn-tcmb.FECHA <= TODAY NO-LOCK NO-ERROR.
     IF AVAILABLE gn-tcmb THEN DO:
        DISPLAY gn-tcmb.compra @ LG-COCmp.TpoCmb.
        S-TPOCMB = gn-tcmb.compra.
     END.
     ObsInt[1]:SENSITIVE = FALSE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE L-ATEPAR AS LOGICAL INIT NO NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF LOOKUP(LG-COCmp.FlgSit, "R,G") = 0 THEN DO:
     MESSAGE "La Orden de Compra no puede ser anulada" SKIP
              "se encuentra " ENTRY(LOOKUP(LG-COCmp.FlgSit,"A,T,V,P,C"),"Anulada,Totalmente atendida,Vencida,Pendiente,Cerrada")
              VIEW-AS ALERT-BOX ERROR.
     RETURN "ADM-ERROR".
  END.
  FOR EACH LG-DOCMP OF LG-COCMP:
      IF LG-DOCmp.CanAten > 0 THEN DO:
         L-ATEPAR = YES.
         LEAVE.
      END.
  END.
  IF L-ATEPAR THEN DO:
     MESSAGE "La Orden de Compra no puede ser anulada" SKIP
              "se encuentra parcialmente atendida" VIEW-AS ALERT-BOX ERROR.
     RETURN "ADM-ERROR".
  END.
  IF AVAILABLE LG-COCmp AND (LOOKUP(LG-COCmp.FlgSit,"A,T,V") = 0) THEN DO
     ON ERROR UNDO, RETURN "ADM-ERROR":
     FIND CCMP OF LG-COCmp EXCLUSIVE-LOCK NO-ERROR.
     IF AVAILABLE CCMP THEN DO:
        ASSIGN CCMP.FlgSit = "A"
               CCMP.Userid-com = S-USER-ID.
        /* RHC 19.07.06 */
        RUN Anula-Requisicion-Detalle.
        /* ************ */
/*        RUN Actualiza-Detalle-Requisicion(-1).*/
        RUN Borra-Detalle.
/*        FIND LG-CRequ WHERE LG-CRequ.CodCia = LG-COCmp.CodCia 
 *                        AND  LG-CRequ.TpoReq = "N" 
 *                        AND  LG-CRequ.NroReq = LG-COCmp.NroReq 
 *                       EXCLUSIVE-LOCK NO-ERROR.
 *         IF AVAILABLE LG-CRequ THEN ASSIGN LG-CRequ.FlgSit = "P".
 *         RELEASE LG-CRequ.*/
     END.
     RELEASE CCMP.
  END.
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
  RUN Procesa-Handle IN lh_Handle ('Browse').

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
  
  IF AVAILABLE LG-COCmp THEN DO WITH FRAME {&FRAME-NAME}:
     FIND gn-prov WHERE gn-prov.CodCia = PV-CODCIA 
                   AND  gn-prov.CodPro = LG-COCmp.CodPro 
                  NO-LOCK NO-ERROR.
              
     IF AVAILABLE gn-prov THEN 
        DISPLAY /*gn-prov.Contactos[2] @ F-ConPro*/
                gn-prov.Ruc          @ F-RucPro.
     FIND gn-concp WHERE gn-concp.Codig = LG-COCmp.CndCmp NO-LOCK NO-ERROR.
     IF AVAILABLE gn-concp THEN 
        DISPLAY gn-concp.Nombr @ F-DesCnd.           
     I-NROREQ = LG-COCmp.NroReq.
     S-PROVEE = LG-COCmp.CodPro.
     FIND Almacen WHERE Almacen.CodCia = LG-COCmp.CodCia 
                   AND  Almacen.CodAlm = LG-COCmp.CodAlm 
                  NO-LOCK NO-ERROR.
     IF AVAILABLE Almacen THEN
        DISPLAY Almacen.DirAlm @ F-DirEnt 
                Almacen.EncAlm @ F-Contacto 
                Almacen.HorRec @ F-HorRec 
                Almacen.TelAlm @ F-TlfAlm.
     IF LOOKUP(LG-COCmp.FlgSit,"X,G,P,A,T,V,R") > 0 THEN
        F-SitDoc:SCREEN-VALUE = ENTRY(LOOKUP(LG-COCmp.FlgSit,"X,G,P,A,T,V,R,C"),"Rechazado,Emitido,Aprobado,Anulado,Aten.Total,Vencida,En Revision,Cerrada").
        
    FIND almtabla WHERE almtabla.Tabla = 'MD' 
                   AND  almtabla.Codigo = LG-COCmp.ModAdq:screen-value 
                  NO-LOCK NO-ERROR.
    IF AVAILABLE almtabla THEN 
       F-modalidad:screen-value = almtabla.nombre.
    ELSE 
       F-modalidad:screen-value = "".

    IF LG-COCmp.CodMaq <> '' THEN DO:
        FIND Almtabla WHERE Almtabla.tabla = 'MQ' 
            AND Almtabla.codigo = LG-COCmp.CodMaq
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtabla
        THEN x-NomMaq:SCREEN-VALUE = almtabla.Nombre.
        ELSE x-NomMaq:SCREEN-VALUE = ''.
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

  IF NOT (AVAILABLE Lg-cocmp AND LOOKUP(LG-COCmp.FlgSit, "P,T") > 0)  THEN RETURN "ADM-ERROR".

  DEF VAR I AS INTEGER.
  DEF VAR MENS AS CHARACTER.
  DEF VAR x-Ok AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  SYSTEM-DIALOG PRINTER-SETUP UPDATE x-Ok.
  IF x-Ok = NO THEN RETURN.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'imprime':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DEF VAR x-Copias AS CHAR.
  
  RUN lgc/d-impodc (OUTPUT x-Copias).
  IF x-Copias = 'ADM-ERROR' THEN RETURN.
  DO i = 1 TO NUM-ENTRIES(x-Copias):
    CASE ENTRY(i, x-Copias):
        WHEN 'ALM' THEN MENS = 'ALMACEN'.
        WHEN 'CBD' THEN MENS = 'CONTABILIDAD'.
        WHEN 'PRO' THEN MENS = 'PROVEEDOR'.
        WHEN 'ARC' THEN MENS = 'ARCHIVO'.
    END CASE.
    IF LG-COCmp.FlgSit <> "A" THEN RUN lgc\r-impcmp(ROWID(LG-COCmp), MENS).
  END.
  
 /* RHC 30.09.05 
 *    DO I = 1 TO 4:
 *       CASE I:
 *           WHEN 1 THEN MENS = "ALMACEN".
 *           WHEN 2 THEN MENS = "CONTABILIDAD".
 *           WHEN 3 THEN MENS = "PROVEEDOR".
 *           WHEN 4 THEN MENS = "ARCHIVO".
 *       END CASE.
 *       IF LG-COCmp.FlgSit <> "A" THEN RUN lgc\r-impcmp(ROWID(LG-COCmp), MENS).
 *     END.*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH Lg-Tabla NO-LOCK WHERE Lg-Tabla.codcia = s-codcia
            AND Lg-Tabla.Tabla = '01':
        INTEGRAL.LG-COCmp.FlgEst[1]:ADD-LAST(lg-tabla.Codigo).
    END.            
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

  RUN Genera-Orden-Compra.

  RUN Cambiar-Revision.

  RUN Procesa-Handle IN lh_Handle ("Pagina1").
  RUN Procesa-Handle IN lh_Handle ('Browse').

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
    input-var-1 = "".
    CASE HANDLE-CAMPO:name:
        WHEN "CndCmp" THEN ASSIGN input-var-1 = "" /*"20"*/.
        WHEN "ModAdq" THEN ASSIGN input-var-1 = X-TIPO.
        WHEN "CodMaq" THEN ASSIGN input-var-1 = 'MQ'.
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
  {src/adm/template/snd-list.i "LG-COCmp"}

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
  IF p-state = 'update-begin':U THEN DO WITH FRAME {&FRAME-NAME}:
     L-CREA = NO.
     RUN Actualiza-DCMP.
     RUN Procesa-Handle IN lh_Handle ("Pagina2").
     RUN Procesa-Handle IN lh_Handle ('Browse').
     DO WITH FRAME {&FRAME-NAME}:
        Lg-Cocmp.ObsInt[1]:SENSITIVE = FALSE.
     END.
     
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

   IF LG-Cocmp.CodPro:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Codigo de proveedor no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO LG-Cocmp.CodPro.
      RETURN "ADM-ERROR".   
   END.

   IF LG-COCmp.CndCmp:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Condicion de Compra no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO LG-COCmp.CndCmp.
      RETURN "ADM-ERROR".         
   END.  

   IF LG-COCmp.CodAlm:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Almacen no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO LG-COCmp.CodAlm.
      RETURN "ADM-ERROR".         
   END.  
   
   FIND gn-prov WHERE gn-prov.CodCia = PV-CODCIA 
                 AND  gn-prov.CodPro = LG-Cocmp.CodPro:SCREEN-VALUE 
                NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gn-prov THEN DO:
      MESSAGE "Proveedor no Registrado" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO LG-Cocmp.CodPro.
      RETURN "ADM-ERROR".
   END.
   FIND Almacen WHERE Almacen.CodCia = S-CODCIA
                 AND  Almacen.CodAlm = LG-COCmp.CodAlm:SCREEN-VALUE 
                NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Almacen THEN DO:
      MESSAGE "Almacen no Registrado" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO LG-COCmp.CodAlm.
      RETURN "ADM-ERROR".   
   END.
/*   
   IF Almacen.AlmCsg AND SUBSTRING(LG-COCmp.CndCmp:SCREEN-VALUE,1,1) <> "8" THEN DO:
      MESSAGE "Condicion de Compra no es Consignacion" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO LG-COCmp.CndCmp.
      RETURN "ADM-ERROR".         
   END.  

   IF SUBSTRING(LG-COCmp.CndCmp:SCREEN-VALUE,1,1) = "8" AND NOT Almacen.AlmCsg THEN DO:
      MESSAGE "Almacen de entrega debe de ser de Consignacion" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO LG-COCmp.CodAlm.
      RETURN "ADM-ERROR".         
   END.  
*/
   IF LG-COCmp.ModAdq:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Modalidad de Compra no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO LG-COCmp.ModAdq.
      RETURN "ADM-ERROR".         
   END.  
   FIND almtabla WHERE almtabla.Tabla = X-TIPO AND  
                       almtabla.Codigo = LG-COCmp.ModAdq:screen-value 
                NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Almtabla THEN DO:
      MESSAGE "Modalidad de Compra no existe" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO LG-COCmp.ModAdq.
      RETURN "ADM-ERROR".         
   END. 
  IF LG-COCmp.CodMaq:SCREEN-VALUE <> '' THEN DO:
    FIND Almtabla WHERE Almtabla.tabla = 'MQ' 
        AND Almtabla.codigo = LG-COCmp.CodMaq:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtabla THEN DO:
        MESSAGE "Maquina no existe" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO LG-COCmp.CodMaq.
        RETURN "ADM-ERROR".         
    END.
  END.

  FIND Lg-Tabla WHERE Lg-Tabla.CodCia = s-CodCia
        AND Lg-Tabla.Tabla = '01'
        AND Lg-Tabla.Codigo = LG-COCmp.FlgEst[1]:SCREEN-VALUE
        NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Lg-Tabla THEN DO:
    MESSAGE "INCOTERM no existe" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO LG-COCmp.FlgEst[1].
    RETURN "ADM-ERROR".         
  END. 

  /* RHC 12.12.06 CATEGORIA CONTABLE */
  FOR EACH DCMP WHERE DCMP.CanPedi > 0, 
        FIRST Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia AND Almmmatg.codmat = DCMP.codmat:
    IF Almmmatg.CatConta[1] <> '' AND Lg-COcmp.ModAdq:SCREEN-VALUE <> Almmmatg.CatConta[1] 
    THEN DO:
        MESSAGE 'La modalidad de compra deber�a ser:' Almmmatg.CatConta[1] SKIP
            'Material:' DCMP.CodMat SKIP
            'Continuamos con la grabaci�n?'
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE rpta-1 AS LOG.
        IF rpta-1 = NO THEN DO:
            APPLY 'ENTRY':U TO LG-COCmp.ModAdq.
            RETURN 'ADM-ERROR'.
        END.
    END.
  END.

/*  
  /* RHC 25.08.06 CONTROL DE COMPRAS NO MAYOR A 7 DIAS PROYECTADO */
  s-Control-Compras = NO.
  IF LG-COCmp.ModAdq:SCREEN-VALUE = 'MC' THEN DO:   /* SOLO MERCADERIA */
    RUN Control-Compras.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE 'Continuamos con la grabaci�n?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE rpta AS LOG.
        IF rpta = NO THEN RETURN 'ADM-ERROR'.
        s-Control-Compras = YES.
    END.
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
/*Comentado por caso Jhon****
IF NOT AVAILABLE LG-COCmp THEN RETURN "ADM-ERROR".
IF LOOKUP(LG-COCmp.FlgSit, "R,G") = 0 THEN RETURN "ADM-ERROR".
S-TPOCMB = LG-COCmp.TpoCmb.
S-CODMON = LG-COCmp.Codmon.
S-PORCFR = 0.
FIND Lg-tabla WHERE Lg-tabla.codcia = s-codcia
    AND Lg-tabla.tabla = '01'
    AND Lg-tabla.codigo = LG-COCmp.FlgEst[1]
    NO-LOCK NO-ERROR.
IF AVAILABLE Lg-tabla THEN s-PorCfr = lg-tabla.Valor[1].
**************/
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
