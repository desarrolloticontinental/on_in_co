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

/* Public Variable Definitions ---                                       */
DEFINE SHARED TEMP-TABLE PedM LIKE FacDPedM. 
DEFINE SHARED VARIABLE S-CODCIA   AS INTEGER.
DEFINE SHARED VARIABLE CL-CODCIA   AS INTEGER.
DEFINE SHARED VARIABLE S-CODDOC   AS CHAR.
DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.
DEFINE SHARED VARIABLE S-USER-ID  AS CHAR.
DEFINE SHARED VARIABLE S-TERMINAL AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV   AS CHAR.
DEFINE SHARED VARIABLE S-CODVEN   AS CHAR.
DEFINE SHARED VARIABLE S-CODMON   AS INTEGER INIT 1.
DEFINE SHARED VARIABLE S-CODIGV   AS INTEGER INIT 1.
DEFINE SHARED VARIABLE S-CODCLI   AS CHAR.
DEFINE SHARED VARIABLE S-CODALM   AS CHARACTER.
DEFINE SHARED VARIABLE S-CNDVTA   AS CHAR.
DEFINE VARIABLE I-NroItm     AS INTEGER   NO-UNDO.
DEFINE SHARED VARIABLE S-TPOCMB AS DECIMAL.  
DEFINE SHARED VARIABLE X-NRODEC AS INTEGER INIT 2.
DEFINE SHARED VARIABLE S-NROTAR   AS CHAR.

/* Local Variable Definitions ---                                       */

DEFINE BUFFER B-CPedM FOR FacCPedM.

DEFINE VARIABLE L-CREA         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE I-NROPED       AS INTEGER   NO-UNDO.
DEFINE VARIABLE I-NROSER       AS INTEGER   NO-UNDO.
DEFINE VARIABLE S-PRINTER-NAME AS CHARACTER NO-UNDO.
DEFINE VARIABLE S-NROCOT       AS CHARACTER NO-UNDO.
DEFINE VARIABLE T-SALDO AS DECIMAL.
DEFINE VARIABLE F-totdias      AS INTEGER NO-UNDO.
DEFINE VARIABLE w-import       AS INTEGER NO-UNDO.

FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA 
               AND  FacCorre.CodDoc = S-CODDOC 
               AND  FacCorre.CodDiv = S-CODDIV
               AND  FacCorre.CodAlm = S-CodAlm 
               NO-LOCK NO-ERROR.
IF AVAILABLE FacCorre THEN 
   ASSIGN I-NroSer = FacCorre.NroSer
          S-PRINTER-NAME = FacCorre.Printer.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DEFINE VAR F-Observa AS CHAR NO-UNDO.
DEFINE VAR X-Codalm  AS CHAR NO-UNDO.
X-Codalm = S-CODALM.

DEFINE BUFFER B-CCB FOR CcbCDocu.

DEFINE stream entra .

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
&Scoped-define EXTERNAL-TABLES FacCPedM
&Scoped-define FIRST-EXTERNAL-TABLE FacCPedM


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR FacCPedM.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Faccpedm.CodCli Faccpedm.NomCli ~
Faccpedm.TpoCmb Faccpedm.DirCli Faccpedm.RucCli Faccpedm.fchven ~
Faccpedm.CodVen Faccpedm.ordcmp Faccpedm.FmaPgo Faccpedm.NroCard ~
Faccpedm.Glosa Faccpedm.CodMon Faccpedm.FlgIgv 
&Scoped-define ENABLED-TABLES Faccpedm
&Scoped-define FIRST-ENABLED-TABLE Faccpedm
&Scoped-Define ENABLED-OBJECTS RECT-21 F-NRODEC Remate 
&Scoped-Define DISPLAYED-FIELDS Faccpedm.NroPed Faccpedm.FchPed ~
Faccpedm.CodCli Faccpedm.NomCli Faccpedm.TpoCmb Faccpedm.DirCli ~
Faccpedm.RucCli Faccpedm.fchven Faccpedm.CodVen Faccpedm.ordcmp ~
Faccpedm.FmaPgo Faccpedm.NroCard Faccpedm.Glosa Faccpedm.CodMon ~
Faccpedm.FlgIgv 
&Scoped-define DISPLAYED-TABLES Faccpedm
&Scoped-define FIRST-DISPLAYED-TABLE Faccpedm
&Scoped-Define DISPLAYED-OBJECTS F-Estado F-nOMvEN F-CndVta C-TpoVta ~
F-NRODEC Remate 

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
DEFINE VARIABLE C-TpoVta AS CHARACTER FORMAT "X(256)":U INITIAL "Factura" 
     LABEL "Tipo Venta" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Factura","Letras","Boleta" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE F-CndVta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE F-Estado AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     FGCOLOR 12 FONT 0 NO-UNDO.

DEFINE VARIABLE F-nOMvEN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45.72 BY .81 NO-UNDO.

DEFINE VARIABLE F-NRODEC AS INTEGER FORMAT "9":U INITIAL 2 
     LABEL "Nro Dec" 
     VIEW-AS FILL-IN 
     SIZE 3.14 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 5.85.

DEFINE VARIABLE Remate AS LOGICAL INITIAL no 
     LABEL "Remate" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Faccpedm.NroPed AT ROW 1.19 COL 9 COLON-ALIGNED
          LABEL "Numero" FORMAT "XXX-XXXXXX"
          VIEW-AS FILL-IN 
          SIZE 13.14 BY .81
          FONT 0
     F-Estado AT ROW 1.19 COL 23 COLON-ALIGNED NO-LABEL
     Faccpedm.FchPed AT ROW 1.19 COL 75 COLON-ALIGNED
          LABEL "Fecha"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     Faccpedm.CodCli AT ROW 1.96 COL 9 COLON-ALIGNED HELP
          "" FORMAT "x(11)"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     Faccpedm.NomCli AT ROW 1.96 COL 22 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36.14 BY .81
     Faccpedm.TpoCmb AT ROW 1.96 COL 75 COLON-ALIGNED
          LABEL "T/  Cambio"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     Faccpedm.DirCli AT ROW 2.73 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 39 BY .81
     Faccpedm.RucCli AT ROW 2.73 COL 52 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     Faccpedm.fchven AT ROW 2.73 COL 75 COLON-ALIGNED
          LABEL "Vencimiento" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     Faccpedm.CodVen AT ROW 3.5 COL 9 COLON-ALIGNED FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .81
     F-nOMvEN AT ROW 3.5 COL 15 COLON-ALIGNED NO-LABEL
     Faccpedm.ordcmp AT ROW 3.5 COL 75 COLON-ALIGNED
          LABEL "Solicitud Cotizaci�n"
          VIEW-AS FILL-IN 
          SIZE 10.14 BY .81
     Faccpedm.FmaPgo AT ROW 4.27 COL 9 COLON-ALIGNED
          LABEL "Cond.Vta" FORMAT "X(3)"
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .81
     F-CndVta AT ROW 4.27 COL 15 COLON-ALIGNED NO-LABEL
     Faccpedm.NroCard AT ROW 4.27 COL 52 COLON-ALIGNED
          LABEL "Nro.Tarjeta"
          VIEW-AS FILL-IN 
          SIZE 11.57 BY .81
     C-TpoVta AT ROW 4.27 COL 75 COLON-ALIGNED
     Faccpedm.Glosa AT ROW 5.04 COL 6.28
          VIEW-AS FILL-IN 
          SIZE 51.43 BY .81
     Faccpedm.CodMon AT ROW 5.15 COL 77 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "S/.", 1,
"US$", 2
          SIZE 11.57 BY .81
     F-NRODEC AT ROW 5.81 COL 9 COLON-ALIGNED
     Remate AT ROW 5.85 COL 35
     Faccpedm.FlgIgv AT ROW 5.92 COL 77 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Si", yes,
"No", no
          SIZE 11.57 BY .81
     "Con IGV:" VIEW-AS TEXT
          SIZE 7 BY .5 AT ROW 6.08 COL 70
     "Moneda:" VIEW-AS TEXT
          SIZE 7 BY .5 AT ROW 5.23 COL 70
     RECT-21 AT ROW 1.08 COL 1.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.FacCPedM
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
         HEIGHT             = 6.23
         WIDTH              = 89.86.
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

/* SETTINGS FOR COMBO-BOX C-TpoVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Faccpedm.CodCli IN FRAME F-Main
   EXP-FORMAT EXP-HELP                                                  */
/* SETTINGS FOR FILL-IN Faccpedm.CodVen IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN F-CndVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-nOMvEN IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Faccpedm.FchPed IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Faccpedm.fchven IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Faccpedm.FmaPgo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Faccpedm.Glosa IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Faccpedm.NroCard IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Faccpedm.NroPed IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Faccpedm.ordcmp IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Faccpedm.TpoCmb IN FRAME F-Main
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

&Scoped-define SELF-NAME Faccpedm.CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Faccpedm.CodCli V-table-Win
ON LEAVE OF Faccpedm.CodCli IN FRAME F-Main /* Codigo */
DO:
   IF CAPS(SUBSTRING(SELF:SCREEN-VALUE,1,1)) = "A" THEN DO:
      MESSAGE "Codigo Incorrecto, Verifique ...... " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   S-CODALM = IF Remate:SCREEN-VALUE = "YES" THEN "79" ELSE X-CODALM.
   FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA 
                 AND  gn-clie.CodCli = SELF:SCREEN-VALUE 
                NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gn-clie THEN DO:
      MESSAGE "Codigo de cliente no existe" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   IF gn-clie.ClfCli = "E" THEN DO:
      MESSAGE "Cliente, NECESITA AUTORIZACION " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   /* RHC 09.09.04 NO debe ser cliente varios */
   IF SELF:SCREEN-VALUE = FacCfgGn.CliVar
   THEN DO:
    MESSAGE 'Codigo de clientes varios NO permitido'
        VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
   END.
   DO WITH FRAME {&FRAME-NAME} :
      DISPLAY gn-clie.NomCli @ FaccPedM.NomCli
            gn-clie.Ruc    @ FaccPedM.RucCli
            gn-clie.DirCli @ FaccPedM.DirCli
            gn-clie.CodVen @ FacCPedM.CodVen
            gn-clie.CndVta @ FacCPedM.FmaPgo
            gn-clie.NroCard @ FacCPedM.NroCard.
      S-CODCLI = FaccPedM.CodCli:SCREEN-VALUE.
      S-CNDVTA = gn-clie.CndVta.
      IF gn-clie.CodCli <> FacCfgGn.CliVar THEN DO:
       /*  FILL-IN_NomCli:SENSITIVE = NO.
 *          FILL-IN_RucCli:SENSITIVE = NO.
 *          FILL-IN_DirCli:SENSITIVE = NO.*/
         FacCPedM.NomCli:SENSITIVE = NO.
         FacCPedM.RucCli:SENSITIVE = NO.
         FacCPedM.DirCli:SENSITIVE = NO.
         APPLY "ENTRY" TO FacCPedM.CodVen.

      END.   
     ELSE DO: 
/*        FILL-IN_NomCli:SENSITIVE = YES.
 *         FILL-IN_RucCli:SENSITIVE = YES.
 *         FILL-IN_DirCli:SENSITIVE = YES.*/
        FacCPedM.NomCli:SENSITIVE = YES.
        FacCPedM.RucCli:SENSITIVE = YES.
        FacCPedM.DirCli:SENSITIVE = YES.

        APPLY "ENTRY" TO FacCPedM.NomCli.
   
     END. 
   END.  
   /* Ubica la Condicion Venta */
   FIND gn-convt WHERE gn-convt.Codig = FacCPedM.FmaPgo:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE gn-convt THEN DO:
       F-CndVta:SCREEN-VALUE = gn-convt.Nombr.
       f-totdias = gn-convt.totdias.
    END.  
    ELSE F-CndVta:SCREEN-VALUE = "".

    FIND TcmbCot WHERE  TcmbCot.Codcia = 0
                  AND  (TcmbCot.Rango1 <= gn-convt.totdias
                  AND   TcmbCot.Rango2 >= gn-convt.totdias)
                  NO-LOCK NO-ERROR.
    IF AVAIL TcmbCot THEN DO:
       DISPLAY TcmbCot.TpoCmb @ FacCPedM.TpoCmb
               WITH FRAME {&FRAME-NAME}.
       S-TPOCMB = TcmbCot.TpoCmb.  
   END.
      
  RUN Procesa-Handle IN lh_Handle ('browse').
  RUN Procesa-Handle IN lh_Handle ('Recalculo').
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Faccpedm.CodMon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Faccpedm.CodMon V-table-Win
ON VALUE-CHANGED OF Faccpedm.CodMon IN FRAME F-Main /* Cod!mon */
DO:
  S-CODMON = INTEGER(FaccPedM.CodMon:SCREEN-VALUE).
  RUN Procesa-Handle IN lh_Handle ('Recalculo').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Faccpedm.CodVen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Faccpedm.CodVen V-table-Win
ON LEAVE OF Faccpedm.CodVen IN FRAME F-Main /* Vendedor */
DO:
  F-NomVen = "".
  IF FacCPedM.CodVen:SCREEN-VALUE <> "" THEN DO: 
     FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA 
                  AND  gn-ven.CodVen = FacCPedM.CodVen:screen-value 
                 NO-LOCK NO-ERROR.
     IF AVAILABLE gn-ven THEN F-NomVen = gn-ven.NomVen.
  END.
  DISPLAY F-NomVen WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-NRODEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-NRODEC V-table-Win
ON LEAVE OF F-NRODEC IN FRAME F-Main /* Nro Dec */
DO:
  X-NRODEC = INTEGER(F-NRODEC:SCREEN-VALUE).
  IF X-NRODEC < 2 OR X-NRODEC > 4 THEN DO:
   MESSAGE "Numero de Decimales No Permitido " VIEW-AS ALERT-BOX ERROR.
   RETURN NO-APPLY.
  END.

  RUN Procesa-Handle IN lh_Handle ('Recalculo').

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Faccpedm.fchven
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Faccpedm.fchven V-table-Win
ON LEAVE OF Faccpedm.fchven IN FRAME F-Main /* Vencimiento */
DO:

      FIND TcmbCot WHERE  TcmbCot.Codcia = 0
                    AND  (TcmbCot.Rango1 <=  DATE(FaccPedM.FchVen:SCREEN-VALUE) - DATE(FaccPedM.FchPed:SCREEN-VALUE) + 1
                    AND   TcmbCot.Rango2 >= DATE(FaccPedM.FchVen:SCREEN-VALUE) - DATE(FaccPedM.FchPed:SCREEN-VALUE) + 1 )
                   NO-LOCK NO-ERROR.
      IF AVAIL TcmbCot THEN DO:
      
          DISPLAY TcmbCot.TpoCmb @ FacCPedM.TpoCmb
                  WITH FRAME {&FRAME-NAME}.
          S-TPOCMB = TcmbCot.TpoCmb.  
      END.
   
      RUN Procesa-Handle IN lh_Handle ('Recalculo').
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Faccpedm.FlgIgv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Faccpedm.FlgIgv V-table-Win
ON VALUE-CHANGED OF Faccpedm.FlgIgv IN FRAME F-Main /* Con IGV */
DO:
  
  S-CODIGV = IF FacCPedM.FlgIgv:SCREEN-VALUE = "YES" THEN 1 ELSE 2.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Faccpedm.FmaPgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Faccpedm.FmaPgo V-table-Win
ON LEAVE OF Faccpedm.FmaPgo IN FRAME F-Main /* Cond.Vta */
DO:
   IF FacCPedM.FmaPgo:SCREEN-VALUE <> "" THEN DO:
      F-CndVta:SCREEN-VALUE = "".
      S-CNDVTA = FacCPedM.FmaPgo:SCREEN-VALUE.
      FIND gn-convt WHERE gn-convt.Codig = FacCPedM.FmaPgo:SCREEN-VALUE NO-LOCK NO-ERROR.
      IF AVAILABLE gn-convt THEN DO:
         F-CndVta:SCREEN-VALUE = gn-convt.Nombr.
      END.   
   END.
   ELSE F-CndVta:SCREEN-VALUE = "".
   RUN Procesa-Handle IN lh_Handle ('Recalculo').
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Faccpedm.NroCard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Faccpedm.NroCard V-table-Win
ON LEAVE OF Faccpedm.NroCard IN FRAME F-Main /* Nro.Tarjeta */
DO:
  IF SELF:SCREEN-VALUE = "" THEN RETURN.
  SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999").
  FIND Gn-Card WHERE Gn-Card.NroCard = SELF:SCREEN-VALUE 
               NO-LOCK NO-ERROR.

     IF NOT AVAILABLE Gn-Card THEN DO:
        S-NROTAR = SELF:SCREEN-VALUE.
        RUN VTA\D-RegCar.R (INPUT S-NROTAR).
        IF S-NROTAR = "" THEN DO:
            APPLY "ENTRY" TO FaccPedM.NroCard.
            RETURN NO-APPLY.
        END.
        FIND Gn-Card WHERE Gn-Card.NroCard = SELF:SCREEN-VALUE 
                           NO-LOCK NO-ERROR.
  
     END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Remate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Remate V-table-Win
ON VALUE-CHANGED OF Remate IN FRAME F-Main /* Remate */
DO:
  S-CODALM = IF Remate:SCREEN-VALUE = "YES" THEN "79" ELSE X-CODALM.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Faccpedm.TpoCmb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Faccpedm.TpoCmb V-table-Win
ON LEAVE OF Faccpedm.TpoCmb IN FRAME F-Main /* T/  Cambio */
DO:
    S-TPOCMB = DEC(FacCPedM.TpoCmb:SCREEN-VALUE).
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Item V-table-Win 
PROCEDURE Actualiza-Item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH PedM:
    DELETE PedM.
END.
IF NOT L-CREA THEN DO:
   FOR EACH facdPedM OF faccPedM NO-LOCK:
       CREATE PedM.
       RAW-TRANSFER FacDPedM TO PedM.
       ASSIGN PedM.CodCia = facdPedM.CodCia 
              PedM.codmat = facdPedM.codmat 
              PedM.Factor = facdPedM.Factor 
              PedM.CanPed = facdPedM.CanPed 
              PedM.CanAte = 0
       /*       PedM.CanAte = FacDPedM.CanAte*/
              PedM.ImpDto = facdPedM.ImpDto 
              PedM.ImpLin = facdPedM.ImpLin 
              PedM.NroItm = facdPedM.NroItm 
              PedM.PorDto = facdPedM.PorDto 
              PedM.PreUni = facdPedM.PreUni 
              PedM.UndVta = facdPedM.UndVta
              PedM.AftIgv = FacdPedM.AftIgv 
              PedM.AftIsc = FacdPedM.AftIsc 
              PedM.ImpDto = FacdPedM.ImpDto 
              PedM.ImpIgv = FacdPedM.ImpIgv 
              PedM.ImpIsc = FacdPedM.ImpIsc 
              PedM.PreBas = FacdPedM.PreBas.
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
  {src/adm/template/row-list.i "FacCPedM"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "FacCPedM"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-PedMdo V-table-Win 
PROCEDURE Borra-PedMdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH FacDPedM WHERE FacDPedM.codcia = FacCPedM.codcia 
            AND  FacDPedM.coddoc = FacCPedM.coddoc 
            AND  FacDPedM.nroped = FacCPedM.nroped:
            DELETE FacDPedM.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-PedMdo V-table-Win 
PROCEDURE Genera-PedMdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE I-NITEM AS INTEGER NO-UNDO INIT 0.
   RUN Borra-PedMdo. 
   FOR EACH PedM NO-LOCK BY PedM.NroItm: 
       I-NITEM = I-NITEM + 1.
       CREATE FacDPedM.
              PedM.CodCia = FacCPedM.CodCia.
              PedM.CodDiv = FacCPedM.CodDiv.
              PedM.coddoc = FacCPedM.coddoc. 
              PedM.NroPed = FacCPedM.NroPed. 
       RAW-TRANSFER PedM TO FacDPedM.
              ASSIGN 
              FacDPedM.FchPed = FacCPedM.FchPed
              FacDPedM.Hora   = FacCPedM.Hora 
              FacDPedM.FlgEst = FacCPedM.FlgEst
              FacDPedM.codmat = PedM.codmat 
              FacDPedM.Factor = PedM.Factor 
              FacDPedM.CanPed = PedM.CanPed 
              FacDPedM.CanAte = PedM.CanAte
              FacDPedM.ImpDto = PedM.ImpDto 
              FacDPedM.ImpLin = PedM.ImpLin 
              FacDPedM.NroItm = I-NITEM 
              FacDPedM.PorDto = PedM.PorDto 
              FacDPedM.PreUni = PedM.PreUni 
              FacDPedM.UndVta = PedM.UndVta 
              FacDPedM.AftIgv = PedM.AftIgv 
              FacDPedM.AftIsc = PedM.AftIsc 
              FacDPedM.ImpIgv = PedM.ImpIgv 
              FacDPedM.ImpIsc = PedM.ImpIsc 
              FacDPedM.PreBas = PedM.PreBas              
              FacDPedM.Por_Dsctos[3] = PedM.Por_Dsctos[3]. 
       RELEASE FacDPedM.
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
DO TRANSACTION:

   DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
   DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
   FIND B-CPedM WHERE ROWID(B-CPedM) = ROWID(FacCPedM) EXCLUSIVE-LOCK NO-ERROR.
   B-CPedM.ImpDto = 0.
   B-CPedM.ImpIgv = 0.
   B-CPedM.ImpIsc = 0.
   B-CPedM.ImpTot = 0.
   B-CPedM.ImpExo = 0.
   FOR EACH PedM NO-LOCK: 
       B-CPedM.ImpDto = B-CPedM.ImpDto + PedM.ImpDto.
       F-Igv = F-Igv + PedM.ImpIgv.
       F-Isc = F-Isc + PedM.ImpIsc.
       B-CPedM.ImpTot = B-CPedM.ImpTot + PedM.ImpLin.
       IF NOT PedM.AftIgv THEN B-CPedM.ImpExo = B-CPedM.ImpExo + PedM.ImpLin.
   END.
   B-CPedM.ImpIgv = ROUND(F-IGV,2).
   B-CPedM.ImpIsc = ROUND(F-ISC,2).
   B-CPedM.ImpBrt = B-CPedM.ImpTot - B-CPedM.ImpIgv - B-CPedM.ImpIsc + 
                    B-CPedM.ImpDto - B-CPedM.ImpExo.
   B-CPedM.ImpVta = B-CPedM.ImpBrt - B-CPedM.ImpDto.


   /*
   /*******Descuento Por Tarjeta **************************/
   DO WITH FRAME {&FRAME-NAME}:
      IF FaccPedM.NroCard:SCREEN-VALUE <> "" THEN DO:
         B-CPedM.PorDto = 0.25.
      END.
   END.
           
   /******************************************************/
   */
    B-CPedM.ImpTot = ROUND(B-CPedM.ImpTot * (1 - B-cPedM.PorDto / 100),2).
    B-CPedM.ImpVta = ROUND(B-CPedM.ImpTot / (1 + B-CPedM.PorIgv / 100),2).
    B-CPedM.ImpIgv = B-CPedM.ImpTot - B-CPedM.ImpVta.
    B-CPedM.ImpBrt = B-CPedM.ImpTot - B-CPedM.ImpIgv - B-CPedM.ImpIsc + 
                     B-CPedM.ImpDto - B-CPedM.ImpExo.
   RELEASE B-CPedM.
   
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importar-Cabecera V-table-Win 
PROCEDURE Importar-Cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT parameter X-ARCHIVO AS CHAR.
IF X-Archivo = ? THEN RETURN.
Def var x as integer init 0.
Def var lin as char.
Input stream entra from value(x-archivo).

    Import stream entra unformatted lin.  
    FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA 
                  AND  gn-clie.CodCli = trim(entry(1,lin,'|')) 
                 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie THEN DO:
       MESSAGE "Codigo de cliente no existe" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.
    
    FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA 
                 AND  gn-ven.CodVen = trim(entry(2,lin,'|')) 
                NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-ven THEN DO:
       MESSAGE "Codigo de vendedor no existe" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.

    FIND gn-convt WHERE gn-convt.Codig = trim(entry(3,lin,'|'))
                        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-convt THEN DO:
       MESSAGE "Condicion de Pago no existe" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.


    FIND TcmbCot WHERE  TcmbCot.Codcia = 0
                  AND  (TcmbCot.Rango1 <=  DATE(FaccPedM.FchPed:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - DATE(FaccPedM.FchVen:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 1
                  AND   TcmbCot.Rango2 >= DATE(FaccPedM.FchPed:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - DATE(FaccPedM.FchVen:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 1 )
                 NO-LOCK NO-ERROR.
    IF AVAIL TcmbCot THEN DO:
        DISPLAY TcmbCot.TpoCmb @ FacCPedM.TpoCmb
                WITH FRAME {&FRAME-NAME}.
        S-TPOCMB = TcmbCot.TpoCmb.  
    END.

    
    f-totdias = DATE(FaccPedM.FchPed:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - DATE(FaccPedM.FchVen:SCREEN-VALUE IN FRAME {&FRAME-NAME}) + 1.
    S-CODCLI = trim(entry(1,lin,'|')).
    S-CNDVTA = trim(entry(3,lin,'|')).
    S-CODVEN = trim(entry(2,lin,'|')).

    DO WITH FRAME {&FRAME-NAME}:
        DISPLAY  S-CODCLI @ FacCPedM.Codcli
                 Gn-Clie.NomCli @ FacCPedM.Nomcli
                 Gn-Clie.Ruc    @ FacCPedM.Ruc
                 S-CODVEN @ FacCPedM.CodVen
                 Gn-ven.NomVen @ F-Nomven
                 S-CNDVTA @ FacCPedM.FmaPgo
                 Gn-Convt.Nombr @ F-CndVta.
    END.



input stream entra close.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir-Txt V-table-Win 
PROCEDURE Imprimir-Txt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       init ""
------------------------------------------------------------------------------*/
  DEFINE VARIABLE o-file AS CHARACTER.

  IF FacCPedM.FlgEst <> "A" THEN DO:
/*      RUN VTA/d-file.r (OUTPUT o-file).*/

      RUN VTA\R-ImpTxt.r(ROWID(FacCPedM), 
                         o-file).

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
  S-CODMON = 1.
  S-NroCot = "".
  X-NRODEC = IF s-CodDiv = '00013' THEN 4 ELSE 2.

  FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA 
                AND  gn-clie.CodCli = FacCfgGn.CliVar
                 NO-LOCK NO-ERROR.

  FIND gn-convt WHERE gn-convt.Codig = gn-clie.CndVta 
                       NO-LOCK NO-ERROR.

  FIND TcmbCot WHERE  TcmbCot.Codcia = 0
                AND  (TcmbCot.Rango1 <= TODAY - TODAY + 1
                AND   TcmbCot.Rango2 >= TODAY - TODAY + 1)
               NO-LOCK NO-ERROR.
  IF AVAIL TcmbCot THEN DO:
      S-TPOCMB = TcmbCot.TpoCmb.  
  END.
    
  DO WITH FRAME {&FRAME-NAME}:
     DISPLAY TODAY @ FacCPedM.FchPed
             TODAY @ FacCPedM.FchVen
             /*TODAY @ FacCPedM.FchEnt*/
             S-TPOCMB @ FacCPedM.TpoCmb.
             /*FacCfgGn.CliVar @ FacCPedM.CodCli.*/
     IF s-CodDiv = '00013'      /* Expolibreria */
        AND TODAY < DATE(02,23,2004)
     THEN DISPLAY DATE(02,23,2004) @ FacCPedM.FchVen.
     FacCPedM.CodMon:SCREEN-VALUE = "1".
     C-TpoVta:SENSITIVE = YES. 
     C-TpoVta:SCREEN-VALUE = ENTRY(1,C-TpoVta:LIST-ITEMS).
     FacCPedM.TpoCmb:SENSITIVE = NO.
     F-Nrodec:HIDDEN = NO.
     F-NRODEC:SCREEN-VALUE = STRING(X-NRODEC,"9").
     /*X-NRODEC = INTEGER(F-NRODEC:SCREEN-VALUE). */
     Remate:HIDDEN = NO.
     Remate:SCREEN-VALUE = STRING(Remate).
     S-CODCLI = FaccPedM.CodCli:SCREEN-VALUE.

  END.
  RUN Actualiza-Item.
  RUN Procesa-Handle IN lh_Handle ('Pagina2').

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
     
     ASSIGN C-TPOVTA.
            X-NRODEC = INTEGER(F-NRODEC:SCREEN-VALUE).
          
     IF L-CREA THEN DO:
         RUN Numero-de-PedMdo(YES).
         ASSIGN FacCPedM.CodCia = S-CODCIA
                FacCPedM.CodDoc = s-coddoc 
                FacCPedM.FchPed = TODAY 
                FacCPedM.CodAlm = S-CODALM
                FacCPedM.PorIgv = FacCfgGn.PorIgv 
                FacCPedM.NroPed = STRING(I-NroSer,"999") + STRING(I-NroPed,"999999")
                FacCPedM.CodDiv = S-CODDIV                
                FaccPedM.NroCard = FaccPedM.NroCard:SCREEN-VALUE .
         DISPLAY FacCPedM.NroPed.
     END.
     ASSIGN 
        FacCPedM.Hora = STRING(TIME,"HH:MM")
        FacCPedM.Usuario = S-USER-ID
        FacCPedM.TipVta  = STRING(LOOKUP(C-TpoVta,C-TpoVta:LIST-ITEMS))
        /*FacCPedM.NomCli  = FILL-IN_NomCli:screen-value*/
        FacCPedM.NomCli 
        FacCPedM.RucCli
        FacCPedM.DirCli  
        FacCPedM.Observa = F-Observa.
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

  DO WITH FRAME {&FRAME-NAME}:
     FaccPedM.NomCli:SENSITIVE = NO.
     FaccPedM.RucCli:SENSITIVE = NO.
     FaccPedM.DirCli:SENSITIVE = NO.
  END. 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-Handle IN lh_Handle ('Pagina1').
  RUN Procesa-Handle IN lh_Handle ('Browse').
  

  
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
  IF NOT AVAILABLE FaccPedM THEN RETURN "ADM-ERROR".
/*  IF LOOKUP(FaccPedM.FlgEst,"C,A") > 0 THEN  RETURN "ADM-ERROR".*/
  S-CODMON = FaccPedM.CodMon.
  S-CODCLI = FaccPedM.CodCli.
/*  NRO_PED = FaccPedM.NroPed.*/
  L-CREA = NO.
  RUN Actualiza-Item.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  L-CREA = YES.
  DO WITH FRAME {&FRAME-NAME}:
     DISPLAY "" @ FaccPedM.NroPed
             "" @ F-Estado.
  END.
  RUN Procesa-Handle IN lh_Handle ('Pagina2').
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
    IF FacCPedM.FlgEst = "A" THEN DO:
       MESSAGE "El PedMdo ya fue anulado" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.
    IF FacCPedM.FlgEst = "C" THEN DO:
       MESSAGE "No puede eliminar un PedMdo TOTALMENTE atendido" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.
    FIND FIRST facdPedM OF faccPedM WHERE CanAte > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE FacDPedM THEN DO:
       MESSAGE "No puede eliminar un PedMdo PARCIALMENTE atendido" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.

    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR":
/*        DELETE FROM FacDPedM WHERE FacDPedM.codcia = FacCPedM.codcia 
 *                               AND  FacDPedM.coddoc = FacCPedM.coddoc 
 *                               AND  FacDPedM.nroped = FacCPedM.nroped.*/
       FIND B-CPedM WHERE ROWID(B-CPedM) = ROWID(FacCPedM) EXCLUSIVE-LOCK NO-ERROR.
       IF AVAILABLE B-CPedM THEN 
          ASSIGN B-CPedM.FlgEst = "A"
                 B-CPedM.Glosa = " A N U L A D O".
       RELEASE B-CPedM.
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
  C-TpoVta:SENSITIVE IN FRAME {&FRAME-NAME} = NO. 
  F-Nrodec:HIDDEN IN FRAME {&FRAME-NAME} = YES. 
  Remate:HIDDEN IN FRAME {&FRAME-NAME} = YES. 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE FacCPedM THEN DO WITH FRAME {&FRAME-NAME}:
     FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA 
                   AND  gn-clie.CodCli = FacCPedM.CodCli 
                  NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie  THEN DO: 
        DISPLAY FaccPedM.NomCli 
                FaccPedM.RucCli  
                FaccPedM.DirCli.
        CASE FaccPedM.FlgEst:
          WHEN "A" THEN DISPLAY " ANULADO " @ F-Estado WITH FRAME {&FRAME-NAME}.
          WHEN "C" THEN DISPLAY "ATENDIDO " @ F-Estado WITH FRAME {&FRAME-NAME}.
          WHEN "P" THEN DISPLAY "PENDIENTE" @ F-Estado WITH FRAME {&FRAME-NAME}.
          WHEN "V" THEN DISPLAY " VENCIDO " @ F-Estado WITH FRAME {&FRAME-NAME}.
       END CASE.         
    END.  
    F-NomVen:screen-value = "".
    FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA 
                  AND  gn-ven.CodVen = FacCPedM.CodVen 
                 NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ven THEN F-NomVen:screen-value = gn-ven.NomVen.
    F-CndVta:SCREEN-VALUE = "".
    FIND gn-convt WHERE gn-convt.Codig = FacCPedM.FmaPgo NO-LOCK NO-ERROR.
    IF AVAILABLE gn-convt THEN F-CndVta:SCREEN-VALUE = gn-convt.Nombr.
    C-TpoVta:SCREEN-VALUE = ENTRY(INTEGER(FacCPedM.TipVta),C-TpoVta:LIST-ITEMS).
    IF FaccPedM.FchVen < TODAY THEN DISPLAY " VENCIDO " @ F-Estado WITH FRAME {&FRAME-NAME}.
    /*
    FIND almtabla WHERE almtabla.tabla = 'CP'
        AND almtabla.codigo = faccPedM.codpos
        NO-LOCK NO-ERROR.
    IF AVAILABLE almtabla
    THEN FILL-IN-Postal:SCREEN-VALUE = almtabla.nombre.
    ELSE FILL-IN-Postal:SCREEN-VALUE = ''.
    */
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
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'NO'
  THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        FacCPedM.CodCli:SENSITIVE = NO
        FacCPedM.fchven:SENSITIVE = NO
        FacCPedM.FlgIgv:SENSITIVE = NO
        FacCPedM.FmaPgo:SENSITIVE = NO
        FacCPedM.NroCard:SENSITIVE = NO
        FacCPedM.CodMon:SENSITIVE = NO
        FacCPedM.TpoCmb:SENSITIVE = NO.
    F-NroDec:SENSITIVE = NO.
  END.
  ELSE F-NroDec:SENSITIVE = YES.
   
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
  
  IF FacCPedM.FlgEst <> "A" THEN DO:
      RUN VTA\R-ImpCot (ROWID(FacCPedM)).
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

  F-Observa = FacCPedM.Observa.
  RUN vta\d-cotiza.r (INPUT-OUTPUT F-Observa,  
                        F-CndVta:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                        FacCPedM.FlgIgv:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                        (DATE(FacCPedM.FchVen:SCREEN-VALUE IN FRAME {&FRAME-NAME}) - DATE(FacCPedM.FchPed:SCREEN-VALUE IN FRAME {&FRAME-NAME})) + 1
                        ).
  IF F-Observa = '***' THEN RETURN "ADM-ERROR".
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  /* Code placed here will execute AFTER standard behavior.    */
  
  RUN Genera-PedMdo.    /* Detalle del PedMdo */ 
  RUN Graba-Totales.
  RUN Procesa-Handle IN lh_Handle ('Pagina1'). 
  RUN Procesa-Handle IN lh_Handle ('browse'). 
  
  C-TpoVta:SENSITIVE IN FRAME {&FRAME-NAME} = NO. 

  IF L-CREA = YES THEN DO:
    MESSAGE "Desea Imprimir Cotizacion?" SKIP(1)
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "" UPDATE choice AS LOGICAL.
    CASE choice:
      WHEN TRUE THEN /* Yes */
      DO:
        RUN dispatch IN THIS-PROCEDURE ('imprime':U).
      END.
    END CASE.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Numero-de-PedMdo V-table-Win 
PROCEDURE Numero-de-PedMdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER L-INCREMENTA AS LOGICAL.
  IF L-INCREMENTA THEN
      FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
           FacCorre.CodDoc = S-CODDOC AND
           FacCorre.CodDiv = S-CODDIV AND
           Faccorre.Codalm = S-CodAlm
           EXCLUSIVE-LOCK NO-ERROR.
/*  ELSE 
 *       FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
 *            FacCorre.CodDoc = S-CODDOC AND
 *            FacCorre.CodDiv = S-CODDIV NO-LOCK NO-ERROR.*/
  IF AVAILABLE FacCorre THEN DO:
     ASSIGN I-NroPed = FacCorre.Correlativo.
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
    DO WITH FRAME {&FRAME-NAME}:
        CASE HANDLE-CAMPO:name:
            WHEN "" THEN ASSIGN input-var-1 = "".
            WHEN "" THEN ASSIGN input-var-2 = "".
            WHEN "" THEN ASSIGN input-var-3 = "".
            WHEN 'CodPos' THEN input-var-1 = 'CP'.
        END CASE.
    END.

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
  {src/adm/template/snd-list.i "FacCPedM"}

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
     RUN Actualiza-Item.
     
     FacCPedM.TpoCmb:SENSITIVE = NO.
     
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
DEFINE VARIABLE F-TOT AS DECIMAL INIT 0 NO-UNDO.
DO WITH FRAME {&FRAME-NAME} :
   IF FacCPedM.CodCli:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Codigo de cliente no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO FacCPedM.CodCli.
      RETURN "ADM-ERROR".   
   END.
   FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA 
                 AND  gn-clie.CodCli = FacCPedM.CodCli:SCREEN-VALUE 
                NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gn-clie THEN DO:
      MESSAGE "Codigo de cliente no existe" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO FacCPedM.CodCli.
      RETURN "ADM-ERROR".   
   END.
   IF FacCPedM.CodVen:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Codigo de Vendedor no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO FacCPedM.CodVen.
      RETURN "ADM-ERROR".   
   END.
   FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA 
                AND  gn-ven.CodVen = FacCPedM.CodVen:screen-value 
               NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gn-ven THEN DO:
      MESSAGE "Codigo de Vendedor no existe" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO FacCPedM.CodVen.
      RETURN "ADM-ERROR".   
   END.
   IF FacCPedM.FmaPgo:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Condicion Venta no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO FacCPedM.FmaPgo.
      RETURN "ADM-ERROR".   
   END.
   FIND gn-convt WHERE gn-convt.Codig = FacCPedM.FmaPgo:SCREEN-VALUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gn-convt THEN DO:
      MESSAGE "Condicion Venta no existe" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO FacCPedM.FmaPgo.
      RETURN "ADM-ERROR".   
   END.
   FOR EACH PedM NO-LOCK: 
       F-Tot = F-Tot + PedM.ImpLin.
   END.
   IF F-Tot = 0 THEN DO:
      MESSAGE "Importe total debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO FacCPedM.CodCli.
      RETURN "ADM-ERROR".   
   END.
   /*
    IF FacCPedM.CodPos:SCREEN-VALUE = ''
    THEN DO:
        MESSAGE 'Ingrese el c�digo postal'
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO FacCPedM.CodPos.
        RETURN 'ADM-ERROR':U.
    END.
    FIND almtabla WHERE almtabla.tabla = 'CP'
        AND almtabla.codigo = FacCPedM.CodPos:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE almtabla
    THEN DO:
        MESSAGE 'C�digo Postal no Regsitrado'
            VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY':U TO FacCPedM.CodPos.
        RETURN 'ADM-ERROR':U.
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
DEFINE VAR RPTA AS CHAR.

IF NOT AVAILABLE FacCPedM THEN RETURN "ADM-ERROR".
IF LOOKUP(FacCPedM.FlgEst,"C,A") > 0 THEN  RETURN "ADM-ERROR".
/* Si tiene atenciones parciales tambien se bloquea */
FIND FIRST facdPedM OF faccPedM WHERE CanAte > 0 NO-LOCK NO-ERROR.
IF AVAILABLE facdPedM 
THEN DO:
    MESSAGE "La Cotizaci�n tiene atenciones parciales" VIEW-AS ALERT-BOX WARNING.
    RETURN "ADM-ERROR".
END.

S-CODMON = FacCPedM.CodMon.
S-CODCLI = FacCPedM.CodCli.
S-CODIGV = IF FacCPedM.FlgIgv THEN 1 ELSE 2.
C-TpoVta:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
S-TPOCMB = FacCPedM.TpoCmb.
X-NRODEC = IF s-CodDiv = '00013' THEN 4 ELSE 2.
S-CNDVTA = FacCPedM.FmaPgo.

IF FacCPedM.fchven < TODAY THEN DO:
    FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
                  AND  Almacen.CodAlm = S-CODALM 
                 NO-LOCK NO-ERROR.
    RUN ALM/D-CLAVE.R(Almacen.Clave,OUTPUT RPTA).
    IF RPTA = "ERROR" THEN RETURN "ADM-ERROR".
END.
F-Nrodec:HIDDEN = NO.
     F-NRODEC:SCREEN-VALUE = STRING(X-NRODEC,"9").

RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

