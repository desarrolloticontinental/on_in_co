&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-CPEDI FOR FacCPedi.
DEFINE BUFFER B-DPEDI FOR FacDPedi.
DEFINE SHARED TEMP-TABLE ITEM LIKE FacDPedi.
DEFINE SHARED TEMP-TABLE ITEM-3 LIKE FacDPedi.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
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

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR cl-codcia AS INT.
DEF SHARED VAR s-coddoc AS CHAR.
DEF SHARED VAR s-nroser AS INT.
DEF SHARED VAR s-codmon AS INT.
DEF SHARED VAR s-CodCli AS CHAR.
DEF SHARED VAR s-porigv AS DEC.
DEF SHARED VAR s-cndvta AS CHAR.
DEF SHARED VAR s-tpocmb AS DEC.
DEF SHARED VAR s-codven AS CHAR.
DEF SHARED VAR lh_Handle AS HANDLE.
DEF SHARED VAR s-nrodec AS INT.
DEF SHARED VAR s-tpoped AS CHAR.
DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR s-flgigv AS LOG.
DEF SHARED VAR s-codalm AS CHAR.
DEF SHARED VAR S-FLGSIT AS CHAR.
DEF SHARED VAR s-NroPed AS CHAR.
DEF SHARED VAR s-adm-new-record AS CHAR.
DEF SHARED VAR s-FlgEmpaque LIKE gn-divi.FlgEmpaque.
DEFINE SHARED VAR s-VentaMayorista LIKE GN-DIVI.VentaMayorista.

DEF SHARED TEMP-TABLE T-DPEDI LIKE FacDPedi.

DEF VAR s-FlgEnv AS LOG.
DEF VAR s-copia-registro AS LOG.
DEF VAR s-cndvta-validos AS CHAR.
DEF VAR F-Observa        AS CHAR.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DEFINE VAR x-ClientesVarios AS CHAR.
x-ClientesVarios = FacCfgGn.CliVar.     /* 11 digitos */

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
&Scoped-define EXTERNAL-TABLES FacCPedi
&Scoped-define FIRST-EXTERNAL-TABLE FacCPedi


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR FacCPedi.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS FacCPedi.FlgSit FacCPedi.fchven ~
FacCPedi.CodCli FacCPedi.RucCli FacCPedi.Atencion FacCPedi.NomCli ~
FacCPedi.ordcmp FacCPedi.DirCli FacCPedi.Cmpbnte FacCPedi.LugEnt ~
FacCPedi.CodMon FacCPedi.NroCard FacCPedi.TpoCmb FacCPedi.CodVen ~
FacCPedi.FlgIgv FacCPedi.FmaPgo FacCPedi.Libre_d01 FacCPedi.Glosa ~
FacCPedi.CodRef FacCPedi.NroRef 
&Scoped-define ENABLED-TABLES FacCPedi
&Scoped-define FIRST-ENABLED-TABLE FacCPedi
&Scoped-Define DISPLAYED-FIELDS FacCPedi.NroPed FacCPedi.FchPed ~
FacCPedi.FlgSit FacCPedi.fchven FacCPedi.CodCli FacCPedi.RucCli ~
FacCPedi.Atencion FacCPedi.usuario FacCPedi.NomCli FacCPedi.ordcmp ~
FacCPedi.DirCli FacCPedi.Cmpbnte FacCPedi.LugEnt FacCPedi.CodMon ~
FacCPedi.NroCard FacCPedi.TpoCmb FacCPedi.CodVen FacCPedi.FlgIgv ~
FacCPedi.FmaPgo FacCPedi.Libre_d01 FacCPedi.Glosa FacCPedi.CodRef ~
FacCPedi.NroRef 
&Scoped-define DISPLAYED-TABLES FacCPedi
&Scoped-define FIRST-DISPLAYED-TABLE FacCPedi
&Scoped-Define DISPLAYED-OBJECTS F-Estado F-Nomtar f-NomVen F-CndVta 

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
     SIZE 20 BY .81
     BGCOLOR 15 FGCOLOR 12 FONT 0 NO-UNDO.

DEFINE VARIABLE F-Nomtar AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE f-NomVen AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FacCPedi.NroPed AT ROW 1.27 COL 16 COLON-ALIGNED WIDGET-ID 58
          LABEL "N�mero" FORMAT "XXX-XXXXXX"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     F-Estado AT ROW 1.27 COL 39 COLON-ALIGNED WIDGET-ID 114
     FacCPedi.FchPed AT ROW 1.27 COL 99 COLON-ALIGNED WIDGET-ID 46
          LABEL "Fecha de Emisi�n"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     FacCPedi.FlgSit AT ROW 2.08 COL 18 NO-LABEL WIDGET-ID 120
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Efectivo", "",
"Tarjeta de Cr�dito", "T":U
          SIZE 25 BY .81
          BGCOLOR 8 FGCOLOR 0 
     FacCPedi.fchven AT ROW 2.08 COL 99 COLON-ALIGNED WIDGET-ID 48 FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     FacCPedi.CodCli AT ROW 2.88 COL 16 COLON-ALIGNED WIDGET-ID 38
          LABEL "Cliente"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     FacCPedi.RucCli AT ROW 2.88 COL 39 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
     FacCPedi.Atencion AT ROW 2.88 COL 59 COLON-ALIGNED WIDGET-ID 88
          LABEL "DNI" FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     FacCPedi.usuario AT ROW 2.88 COL 99 COLON-ALIGNED WIDGET-ID 66
          LABEL "Digitado por"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     FacCPedi.NomCli AT ROW 3.69 COL 16 COLON-ALIGNED WIDGET-ID 54 FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
     FacCPedi.ordcmp AT ROW 3.69 COL 99 COLON-ALIGNED WIDGET-ID 118
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     FacCPedi.DirCli AT ROW 4.5 COL 16 COLON-ALIGNED WIDGET-ID 44
          LABEL "Direcci�n" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
     FacCPedi.Cmpbnte AT ROW 4.5 COL 101 NO-LABEL WIDGET-ID 102
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Factura", "FAC":U,
"Boleta", "BOL":U,
"Ticket", "TCK":U
          SIZE 23 BY .81
     FacCPedi.LugEnt AT ROW 5.31 COL 16 COLON-ALIGNED WIDGET-ID 112
          LABEL "Lugar de entrega" FORMAT "x(60)"
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
     FacCPedi.CodMon AT ROW 5.31 COL 101 NO-LABEL WIDGET-ID 78
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Soles", 1,
"D�lares", 2
          SIZE 15 BY .81
     FacCPedi.NroCard AT ROW 6.12 COL 16 COLON-ALIGNED WIDGET-ID 56
          LABEL "Tarjeta"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
     F-Nomtar AT ROW 6.12 COL 26 COLON-ALIGNED NO-LABEL WIDGET-ID 92
     FacCPedi.TpoCmb AT ROW 6.12 COL 99 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 8.57 BY .81
     FacCPedi.CodVen AT ROW 6.92 COL 16 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
     f-NomVen AT ROW 6.92 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     FacCPedi.FlgIgv AT ROW 6.92 COL 101 WIDGET-ID 116
          LABEL "Afecto a IGV"
          VIEW-AS TOGGLE-BOX
          SIZE 12 BY .77
     FacCPedi.FmaPgo AT ROW 7.73 COL 16 COLON-ALIGNED WIDGET-ID 50
          LABEL "Condici�n de Venta"
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
     F-CndVta AT ROW 7.73 COL 22 COLON-ALIGNED NO-LABEL WIDGET-ID 90
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     FacCPedi.Libre_d01 AT ROW 7.73 COL 101 NO-LABEL WIDGET-ID 96
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "2", 2,
"3", 3,
"4", 4
          SIZE 12 BY .81
     FacCPedi.Glosa AT ROW 8.54 COL 16 COLON-ALIGNED WIDGET-ID 110
          LABEL "Glosa" FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 60 BY .81
     FacCPedi.CodRef AT ROW 8.54 COL 99 COLON-ALIGNED WIDGET-ID 126
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     FacCPedi.NroRef AT ROW 8.54 COL 105 COLON-ALIGNED NO-LABEL WIDGET-ID 128
          VIEW-AS FILL-IN 
          SIZE 11 BY .81 TOOLTIP "XXX-XXXXXX"
     "Moneda:" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 5.58 COL 95 WIDGET-ID 84
     "Comprobante:" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 4.77 COL 91 WIDGET-ID 106
     "Redondedo del P.U.:" VIEW-AS TEXT
          SIZE 15 BY .5 AT ROW 8 COL 86 WIDGET-ID 100
     "Cancela con:" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 2.35 COL 8 WIDGET-ID 124
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: INTEGRAL.FacCPedi
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: B-CPEDI B "?" ? INTEGRAL FacCPedi
      TABLE: B-DPEDI B "?" ? INTEGRAL FacDPedi
      TABLE: ITEM T "SHARED" ? INTEGRAL FacDPedi
      TABLE: ITEM-3 T "SHARED" ? INTEGRAL FacDPedi
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
         HEIGHT             = 10.92
         WIDTH              = 126.43.
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
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:PRIVATE-DATA     = 
                "sdfsdfsdfsdfsdf".

/* SETTINGS FOR FILL-IN FacCPedi.Atencion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN FacCPedi.CodCli IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FacCPedi.DirCli IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN F-CndVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Nomtar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-NomVen IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FacCPedi.FchPed IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FacCPedi.fchven IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR TOGGLE-BOX FacCPedi.FlgIgv IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FacCPedi.FmaPgo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FacCPedi.Glosa IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN FacCPedi.LugEnt IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN FacCPedi.NomCli IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN FacCPedi.NroCard IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FacCPedi.NroPed IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN FacCPedi.usuario IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
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

&Scoped-define SELF-NAME FacCPedi.Cmpbnte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.Cmpbnte V-table-Win
ON VALUE-CHANGED OF FacCPedi.Cmpbnte IN FRAME F-Main /* Tipo Comprobante */
DO:
    DO WITH FRAM {&FRAME-NAME}:
        IF SELF:SCREEN-VALUE = 'FAC' THEN DO:
            FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA 
                AND gn-clie.CodCli = FacCPedi.CodCli:SCREEN-VALUE 
                NO-LOCK NO-ERROR.
            ASSIGN
                FacCPedi.DirCli:SENSITIVE = NO
                FacCPedi.NomCli:SENSITIVE = NO
                FacCPedi.Atencion:SENSITIVE = NO.
            IF AVAILABLE gn-clie THEN DO:
                ASSIGN
                    FacCPedi.DirCli:SCREEN-VALUE = GN-CLIE.DirCli
                    FacCPedi.NomCli:SCREEN-VALUE = GN-CLIE.NomCli
                    FacCPedi.RucCli:SCREEN-VALUE = gn-clie.Ruc.
            END.
        END.
        ELSE DO:
            ASSIGN
                FacCPedi.DirCli:SENSITIVE = YES
                FacCPedi.NomCli:SENSITIVE = YES
                FacCPedi.Atencion:SENSITIVE = YES.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FacCPedi.CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.CodCli V-table-Win
ON LEAVE OF FacCPedi.CodCli IN FRAME F-Main /* Cliente */
DO:
  IF SELF:SCREEN-VALUE = '' THEN RETURN.
  /*IF SELF:SCREEN-VALUE = S-CODCLI THEN RETURN. */

  FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA
    AND  gn-clie.CodCli = SELF:SCREEN-VALUE 
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE gn-clie THEN DO:      /* CREA EL CLIENTE NUEVO */
      S-CODCLI = SELF:SCREEN-VALUE.
      RUN vtamay/d-regcli (INPUT-OUTPUT S-CODCLI).
      IF S-CODCLI = "" THEN DO:
          APPLY "ENTRY" TO Faccpedi.CodCli.
          RETURN NO-APPLY.
      END.
      FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA 
          AND  gn-clie.CodCli = S-CODCLI 
          NO-LOCK NO-ERROR.
      SELF:SCREEN-VALUE = s-codcli.
  END.
  /* BLOQUEO DEL CLIENTE */
  IF gn-clie.FlgSit = "I" THEN DO:
      MESSAGE "Cliente esta Inactivo" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  IF gn-clie.FlgSit = "C" THEN DO:
      MESSAGE "Cliente esta Cesado" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  /* RHC Convenio 17.04.07 NO instituciones publicas */
  IF Gn-Clie.Canal = '006' THEN DO:
      MESSAGE 'El cliente pertenece a una institucion PUBLICA' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO FacCPedi.CodCli.
      RETURN NO-APPLY.
  END.
  s-CodCli = SELF:SCREEN-VALUE.
  /* *********************************************** */

  /* Cargamos las condiciones de venta v�lidas */
  s-cndvta-validos = "000".  /* Por defecto CONTADO */

  DISPLAY 
    gn-clie.NomCli WHEN s-codcli <> x-ClientesVarios @ Faccpedi.NomCli
    gn-clie.Ruc WHEN s-codcli <> x-ClientesVarios   @ Faccpedi.RucCli
    gn-clie.DirCli WHEN s-codcli <> x-ClientesVarios @ Faccpedi.DirCli
    ENTRY(1, s-cndvta-validos) @ FacCPedi.FmaPgo  /* La primera del cliente */
    gn-clie.NroCard WHEN s-codcli <> x-ClientesVarios @ FacCPedi.NroCard
    /*gn-clie.CodVen WHEN Faccpedi.CodVen:SCREEN-VALUE = '' @ Faccpedi.CodVen */
    WITH FRAME {&FRAME-NAME}.
  ASSIGN
      S-CNDVTA = ENTRY(1, s-cndvta-validos).

  /* Tarjeta */
  FIND Gn-Card WHERE Gn-Card.NroCard = gn-clie.nrocard NO-LOCK NO-ERROR.
  IF AVAILABLE GN-CARD 
  THEN ASSIGN
            F-NomTar:SCREEN-VALUE = GN-CARD.NomClie[1]
            FacCPedi.NroCard:SENSITIVE = NO.
  ELSE ASSIGN
            F-NomTar:SCREEN-VALUE = ''
            FacCPedi.NroCard:SENSITIVE = YES.
  
  /* Ubica la Condicion Venta */
  FIND gn-convt WHERE gn-convt.Codig = FacCPedi.FmaPgo:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE gn-convt 
  THEN DO:
       F-CndVta:SCREEN-VALUE = gn-convt.Nombr.
  END.  
  ELSE F-CndVta:SCREEN-VALUE = "".

  /* Vendedor */
  F-NomVen:SCREEN-VALUE = "".
  FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA 
      AND  gn-ven.CodVen = FacCPedi.CodVen:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAILABLE gn-ven THEN F-NomVen:SCREEN-VALUE = gn-ven.NomVen.

  /* Recalculamos cotizacion */
  RUN Procesa-Handle IN lh_Handle ('Recalculo').

  /* Determina si es boleta o factura */
  IF FacCPedi.RucCli:SCREEN-VALUE = ''
  THEN Faccpedi.Cmpbnte:SCREEN-VALUE = 'BOL'.
  ELSE Faccpedi.Cmpbnte:SCREEN-VALUE = 'FAC'.
  APPLY 'VALUE-CHANGED' TO Faccpedi.Cmpbnte.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.CodCli V-table-Win
ON LEFT-MOUSE-DBLCLICK OF FacCPedi.CodCli IN FRAME F-Main /* Cliente */
DO:
    ASSIGN
        input-var-1 = ''
        input-var-2 = ''
        input-var-3 = ''.
    IF s-TpoPed = "M" THEN input-var-1 = "006".
    RUN vtagn/c-gn-clie-01 ('Clientes').
    IF output-var-1 <> ? THEN FacCPedi.CodCli:SCREEN-VALUE = output-var-2.
    /*APPLY 'ENTRY':U TO FacCPedi.CodCli.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FacCPedi.CodMon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.CodMon V-table-Win
ON VALUE-CHANGED OF FacCPedi.CodMon IN FRAME F-Main /* Cod!mon */
DO:
  S-CODMON = INTEGER(Faccpedi.CodMon:SCREEN-VALUE).
  RUN Procesa-Handle IN lh_Handle ('Recalculo').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FacCPedi.CodVen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.CodVen V-table-Win
ON LEAVE OF FacCPedi.CodVen IN FRAME F-Main /* Vendedor */
DO:
  F-NomVen:SCREEN-VALUE = "".
  IF SELF:SCREEN-VALUE = "" THEN RETURN.
  FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA 
    AND  gn-ven.CodVen = FacCPedi.CodVen:SCREEN-VALUE
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE gn-ven THEN DO:
      MESSAGE "Vendedor NO v�lido" VIEW-AS ALERT-BOX ERROR.
      SELF:SCREEN-VALUE = "".
      RETURN NO-APPLY.
  END.
  F-NomVen:SCREEN-VALUE = gn-ven.NomVen.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.CodVen V-table-Win
ON LEFT-MOUSE-DBLCLICK OF FacCPedi.CodVen IN FRAME F-Main /* Vendedor */
OR F8 OF Faccpedi.CodVen
DO:
    ASSIGN
        input-var-1 = ''
        input-var-2 = ''
        input-var-3 = ''.
    RUN lkup/c-vende ('Vendedor').
    IF output-var-1 <> ? THEN FacCPedi.CodVen:SCREEN-VALUE = output-var-2.
    /*APPLY 'ENTRY':U TO FacCPedi.CodVen.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FacCPedi.FlgIgv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.FlgIgv V-table-Win
ON VALUE-CHANGED OF FacCPedi.FlgIgv IN FRAME F-Main /* Afecto a IGV */
DO:
    s-FlgIgv = INPUT {&self-name}.
    IF s-FlgIgv = YES THEN s-PorIgv = FacCfgGn.PorIgv.
    RUN Procesa-Handle IN lh_Handle ('Recalculo').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FacCPedi.FlgSit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.FlgSit V-table-Win
ON VALUE-CHANGED OF FacCPedi.FlgSit IN FRAME F-Main /* Situaci�n */
DO:
  s-FlgSit = SELF:SCREEN-VALUE.
  RUN Procesa-Handle IN lh_Handle ('Recalculo').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FacCPedi.FmaPgo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.FmaPgo V-table-Win
ON LEAVE OF FacCPedi.FmaPgo IN FRAME F-Main /* Condici�n de Venta */
DO:
    F-CndVta:SCREEN-VALUE = ''.
    IF SELF:SCREEN-VALUE = '' THEN RETURN.
    FIND gn-convt WHERE gn-convt.Codig = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-convt THEN DO:
        MESSAGE 'Condici�n de venta NO v�lida' VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.
    F-CndVta:SCREEN-VALUE = gn-convt.Nombr.
    /* Filtrado de las condiciones de venta */
    IF LOOKUP(SELF:SCREEN-VALUE, s-cndvta-validos) = 0 THEN DO:
        MESSAGE 'Condici�n de venta NO autorizada para este cliente'
            VIEW-AS ALERT-BOX WARNING.
        RETURN NO-APPLY.
    END.
    s-CndVta = SELF:SCREEN-VALUE.
    RUN Procesa-Handle IN lh_Handle ('Recalculo').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.FmaPgo V-table-Win
ON LEFT-MOUSE-DBLCLICK OF FacCPedi.FmaPgo IN FRAME F-Main /* Condici�n de Venta */
OR F8 OF Faccpedi.FmaPgo
DO:
    ASSIGN
        input-var-1 = s-cndvta-validos
        input-var-2 = ''
        input-var-3 = ''.
    RUN vta/d-cndvta.
    IF output-var-1 <> ? THEN FacCPedi.Fmapgo:SCREEN-VALUE = output-var-2.
    /*APPLY 'ENTRY':U TO FacCPedi.Fmapgo.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FacCPedi.Libre_d01
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.Libre_d01 V-table-Win
ON VALUE-CHANGED OF FacCPedi.Libre_d01 IN FRAME F-Main /* Libre_d01 */
DO:
    s-NroDec = INTEGER(SELF:SCREEN-VALUE).
    RUN Procesa-Handle IN lh_Handle ('Recalculo').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FacCPedi.NroCard
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.NroCard V-table-Win
ON LEAVE OF FacCPedi.NroCard IN FRAME F-Main /* Tarjeta */
DO:
    F-NomTar:SCREEN-VALUE = ''.
    IF SELF:SCREEN-VALUE = "" THEN RETURN.
    ASSIGN
      SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999") NO-ERROR.
    FIND Gn-Card WHERE Gn-Card.NroCard = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Gn-Card THEN DO:
        MESSAGE 'Tarjeta de Cliente NO v�lida' VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    F-NomTar:SCREEN-VALUE = GN-CARD.NomClie[1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FacCPedi.NroCard V-table-Win
ON LEFT-MOUSE-DBLCLICK OF FacCPedi.NroCard IN FRAME F-Main /* Tarjeta */
OR F8 OF Faccpedi.NroCard
DO:
    ASSIGN
        input-var-1 = ''
        input-var-2 = ''
        input-var-3 = ''.
    RUN lkup/c-gncard ('Tarjetas').
    IF output-var-1 <> ? THEN FacCPedi.NroCard:SCREEN-VALUE = output-var-2.
    /*APPLY 'ENTRY':U TO FacCPedi.NroCard.*/
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
  {src/adm/template/row-list.i "FacCPedi"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "FacCPedi"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna-Cotizacion V-table-Win 
PROCEDURE Asigna-Cotizacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE I-NITEM AS INTEGER NO-UNDO.
  DEFINE VARIABLE f-Factor AS DEC NO-UNDO.
  DEFINE VARIABLE x-CanPed AS DEC NO-UNDO.
  DEFINE VARIABLE s-StkComprometido AS DEC.
  DEFINE VARIABLE s-StkDis AS DEC NO-UNDO.
  DEFINE VARIABLE F-CANPED AS DECIMAL NO-UNDO.
  DEFINE VARIABLE x-StkAct AS DEC NO-UNDO.
  DEFINE VARIABLE x-CodAlm AS CHAR NO-UNDO.
  DEFINE VARIABLE i AS INT NO-UNDO.

  DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
  DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
  DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
  DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
  DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
  DEFINE VARIABLE x-TipDto AS CHAR NO-UNDO.

  /* BUSCAMOS COTIZACIONES PENDIENTES */
  input-var-1 = "C/M".
  input-var-2 = FacCPedi.CodCli:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
  RUN lkup/C-PedCot ("Cotizaciones Pendientes").
  IF output-var-1 = ? THEN RETURN "ADM-ERROR".
  FIND B-CPedi WHERE ROWID(B-CPedi) = output-var-1 NO-LOCK NO-ERROR.
  /* chequeamos cotizaci�n */
  FIND FIRST B-DPEDI OF B-CPEDI WHERE B-DPEDI.CanAte > 0 NO-LOCK NO-ERROR.
  IF B-CPEDI.FchVen < TODAY AND NOT AVAILABLE B-DPEDI THEN DO:
      MESSAGE 'Cotizaci�n VENCIDA' VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.

  /* PINTAMOS INFORMACION EN PANTALLA */
  ASSIGN
      s-FlgEnv = YES.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          s-CodCli = B-CPEDI.CodCli
          s-CodMon = B-CPEDI.CodMon                   /* >>> OJO <<< */
          F-NomVen = ""
          F-CndVta = ""
          F-NomTar = ''
          S-CNDVTA = B-CPEDI.FmaPgo
          S-FlgIgv = B-CPedi.FlgIgv
          s-NroDec = B-CPEDI.Libre_d01.
      FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA 
          AND  gn-ven.CodVen = B-CPEDI.CodVen 
          NO-LOCK NO-ERROR.
      IF AVAILABLE gn-ven THEN F-NomVen = gn-ven.NomVen.
      FIND gn-convt WHERE gn-convt.Codig = B-CPEDI.FmaPgo NO-LOCK NO-ERROR.
      IF AVAILABLE gn-convt THEN F-CndVta = gn-convt.Nombr.
      FIND Gn-Card WHERE Gn-Card.NroCard = B-CPEDI.NroCard NO-LOCK NO-ERROR.
      IF AVAILABLE GN-CARD THEN F-NomTar = GN-CARD.NomClie[1].
      DISPLAY 
          B-CPEDI.CodCli @ Faccpedi.CodCli
          B-CPEDI.NomCli @ Faccpedi.NomCli
          B-CPEDI.RucCli @ Faccpedi.RucCli
          B-CPEDI.Atencion @ Faccpedi.Atencion
          B-CPEDI.DirCli @ Faccpedi.Dircli
          B-CPEDI.LugEnt @ Faccpedi.LugEnt
          B-CPEDI.NroCard @ FacCPedi.NroCard
          B-CPEDI.CodVen @ Faccpedi.CodVen
          B-CPEDI.FmaPgo @ Faccpedi.FmaPgo
          B-CPEDI.Glosa  @ Faccpedi.Glosa
          B-CPEDI.CodDoc @ Faccpedi.CodRef
          B-CPEDI.NroPed @ FacCPedi.NroRef
          F-CndVta           
          F-NomVen
          F-NomTar.
      ASSIGN
          Faccpedi.Cmpbnte:SCREEN-VALUE = B-CPEDI.Cmpbnte        
          Faccpedi.CodMon:SCREEN-VALUE = STRING(B-CPEDI.CodMon)
          Faccpedi.FlgIgv:SCREEN-VALUE = (IF B-CPEDI.FlgIgv = YES THEN "YES" ELSE "NO")
          Faccpedi.Libre_d01:SCREEN-VALUE = STRING(B-CPEDI.Libre_d01).
      ASSIGN
          Faccpedi.CodCli:SENSITIVE = NO
          Faccpedi.NomCli:SENSITIVE = NO
          Faccpedi.DirCli:SENSITIVE = NO
          Faccpedi.RucCli:SENSITIVE = NO
          Faccpedi.CodMon:SENSITIVE = NO
          Faccpedi.Cmpbnte:SENSITIVE = NO
          Faccpedi.Libre_d01:SENSITIVE = NO.
  END.

  DEFINE FRAME F-Mensaje
    'Procesando: ' Facdpedi.codmat SKIP(1)
    'Espere un momento por favor ...' SKIP
    WITH CENTERED NO-LABELS OVERLAY VIEW-AS DIALOG-BOX TITLE 'TRASLADANDO COTIZACION'.

  EMPTY TEMP-TABLE ITEM.

  i-nItem = 0.
  /* CARGAMOS STOCK DISPONIBLE */
  DEF VAR t-AlmDes AS CHAR NO-UNDO.
  DEF VAR t-CanPed AS DEC NO-UNDO.
  DETALLES:
  FOR EACH Facdpedi OF B-CPEDI NO-LOCK WHERE (Facdpedi.CanPed - Facdpedi.CanAte) > 0,
      FIRST Almmmatg OF Facdpedi NO-LOCK
      BY Facdpedi.NroItm:
      DISPLAY Facdpedi.codmat WITH FRAME F-Mensaje.
      /* SE VA A DESCARGAR DEL ALMACEN QUE SE COTIZ� */
      ASSIGN
          f-Factor = Facdpedi.Factor
          t-AlmDes = ''
          t-CanPed = 0
          F-CANPED = (FacDPedi.CanPed - FacDPedi.CanAte)
          x-CodAlm = FacDPedi.AlmDes.
      /* FILTROS */
      FIND Almmmate WHERE Almmmate.codcia = s-codcia
          AND Almmmate.codalm = x-CodAlm  /* *** OJO *** */
          AND Almmmate.codmat = Facdpedi.CodMat
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Almmmate THEN DO:
          MESSAGE 'Producto' Facdpedi.codmat 'NO asignado al almac�n' x-CodAlm
              VIEW-AS ALERT-BOX WARNING.
          NEXT DETALLES.
      END.
      x-StkAct = Almmmate.StkAct.
      RUN vtagn/Stock-Comprometido (Facdpedi.CodMat, x-CodAlm, OUTPUT s-StkComprometido).
      s-StkDis = x-StkAct - s-StkComprometido.
      IF s-StkDis <= 0 THEN DO:
          MESSAGE 'NO hay stock el el almac�n' x-codalm 'para el producto' facdpedi.codmat
              VIEW-AS ALERT-BOX WARNING.
          NEXT DETALLES.
      END.
      /* DEFINIMOS LA CANTIDAD */
      x-CanPed = f-CanPed * f-Factor.
      IF s-StkDis < x-CanPed THEN DO:
          f-CanPed = ((S-STKDIS - (S-STKDIS MODULO f-Factor)) / f-Factor).
      END.
      f-CanPed = f-CanPed * f-Factor.
      /* EMPAQUE */
      IF s-FlgEmpaque = YES THEN DO:
          MESSAGE 'empaque'.
          IF s-VentaMayorista = 1 THEN DO:
              IF Almmmatg.DEC__03 > 0 THEN f-CanPed = (TRUNCATE((f-CanPed / Almmmatg.DEC__03),0) * Almmmatg.DEC__03).
          END.
          ELSE DO:
              FIND VtaListaMay WHERE VtaListaMay.codcia = s-codcia
                  AND VtaListaMay.coddiv = s-coddiv
                  AND VtaListaMay.codmat = Almmmatg.codmat
                  NO-LOCK NO-ERROR.
              IF AVAILABLE VtaListaMay AND VtaListaMay.CanEmp > 0 THEN f-CanPed = (TRUNCATE((f-CanPed / VtaListaMay.CanEmp),0) * VtaListaMay.CanEmp).
          END.
      END.
      f-CanPed = ((f-CanPed - (f-CanPed MODULO f-Factor)) / f-Factor).
      IF f-CanPed <= 0 THEN DO:
          MESSAGE 'NO se cubre el m�nimo de despacho para el producto' facdpedi.codmat
              VIEW-AS ALERT-BOX WARNING.
          NEXT DETALLES.
      END.

      ASSIGN
          t-CanPed = f-CanPed
          t-AlmDes = x-CodAlm.
      IF t-CanPed > 0 THEN DO:
          /* GRABACION */
          I-NITEM = I-NITEM + 1.
          CREATE ITEM.
          BUFFER-COPY FacDPedi 
              TO ITEM
              ASSIGN 
                  ITEM.CodCia = s-codcia
                  ITEM.CodDiv = s-coddiv
                  ITEM.CodDoc = s-coddoc
                  ITEM.NroPed = ''
                  ITEM.ALMDES = t-AlmDes  /* *** OJO *** */
                  ITEM.NroItm = I-NITEM
                  ITEM.CanPed = t-CanPed    /* << OJO << */
                  ITEM.CanAte = 0.
          ASSIGN
              ITEM.Libre_d01 = (FacDPedi.CanPed - FacDPedi.CanAte)
              ITEM.Libre_d02 = t-CanPed.
              /*ITEM.Libre_c01 = '*'.*/
          IF ITEM.CanPed <> FacdPedi.CanPed THEN DO:
              MESSAGE "El Precio del Articulo  " ITEM.Codmat  SKIP
                     "Sera recalculado........."
                     VIEW-AS ALERT-BOX WARNING.
              RUN vta2/PrecioMayorista-Cont (s-CodCia,
                                             s-CodDiv,
                                             s-CodCli,
                                             s-CodMon,
                                             s-TpoCmb,
                                             OUTPUT f-Factor,
                                             ITEM.codmat,
                                             s-FlgSit,
                                             ITEM.undvta,
                                             ITEM.CanPed,
                                             s-NroDec,
                                             ITEM.almdes,   /* Necesario para REMATES */
                                   OUTPUT f-PreBas,
                                   OUTPUT f-PreVta,
                                   OUTPUT f-Dsctos,
                                   OUTPUT y-Dsctos,
                                   OUTPUT x-TipDto
                                   ).
              IF RETURN-VALUE = "ADM-ERROR" THEN DO:
                  DELETE ITEM.
                  NEXT DETALLES.
              END.
              ASSIGN
                  ITEM.PreUni = F-PREVTA
                  ITEM.Por_Dsctos[2] = z-Dsctos
                  ITEM.Por_Dsctos[3] = y-Dsctos
                  ITEM.Libre_c04 = x-TipDto.
              ASSIGN
                  ITEM.ImpLin = ITEM.CanPed * ITEM.PreUni * 
                          ( 1 - ITEM.Por_Dsctos[1] / 100 ) *
                          ( 1 - ITEM.Por_Dsctos[2] / 100 ) *
                          ( 1 - ITEM.Por_Dsctos[3] / 100 ).
              IF ITEM.Por_Dsctos[1] = 0 AND ITEM.Por_Dsctos[2] = 0 AND ITEM.Por_Dsctos[3] = 0 
                  THEN ITEM.ImpDto = 0.
              ELSE ITEM.ImpDto = ITEM.CanPed * ITEM.PreUni - ITEM.ImpLin.
              ASSIGN
                  ITEM.ImpLin = ROUND(ITEM.ImpLin, 2)
                  ITEM.ImpDto = ROUND(ITEM.ImpDto, 2).
              IF ITEM.AftIsc 
                  THEN ITEM.ImpIsc = ROUND(ITEM.PreBas * ITEM.CanPed * (Almmmatg.PorIsc / 100),4).
              IF ITEM.AftIgv 
                  THEN ITEM.ImpIgv = ITEM.ImpLin - ROUND( ITEM.ImpLin  / ( 1 + (s-PorIgv / 100) ), 4 ).
          END.
      END.
  END.
  HIDE FRAME F-Mensaje.
  RUN Procesa-Handle IN lh_Handle ('Pagina3').
  RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra-Pedido V-table-Win 
PROCEDURE Borra-Pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER p-Ok AS LOG.
  
  DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      FOR EACH Facdpedi OF Faccpedi:
          /* BORRAMOS SALDO EN LAS COTIZACIONES */
          FIND B-DPEDI WHERE B-DPEDI.CodCia = Faccpedi.CodCia 
              AND  B-DPEDI.CodDiv = Faccpedi.CodDiv
              AND  B-DPEDI.CodDoc = Faccpedi.CodRef 
              AND  B-DPEDI.NroPed = Faccpedi.NroRef
              AND  B-DPEDI.CodMat = Facdpedi.CodMat 
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE B-DPEDI 
          THEN ASSIGN
                B-DPEDI.FlgEst = 'P'
                B-DPEDI.CanAte = B-DPEDI.CanAte - Facdpedi.CanPed.  /* <<<< OJO <<<< */
          RELEASE B-DPEDI.
          IF p-Ok = YES
          THEN DELETE Facdpedi.
          ELSE Facdpedi.FlgEst = 'A'.   /* <<< OJO <<< */
      END.    
      FIND B-CPedi WHERE 
           B-CPedi.CodCia = S-CODCIA AND  
           B-CPedi.CodDiv = S-CODDIV AND  
           B-CPedi.CodDoc = Faccpedi.CodRef AND  
           B-CPedi.NroPed = Faccpedi.NroRef
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE B-CPedi THEN B-CPedi.FlgEst = "P".
      RELEASE B-CPedi.
  END.
  RETURN "OK".

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

    EMPTY TEMP-TABLE ITEM.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal V-table-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE ITEM.

FOR EACH Facdpedi OF Faccpedi NO-LOCK WHERE Facdpedi.Libre_c05 <> 'OF':
    CREATE ITEM.
    BUFFER-COPY Facdpedi TO ITEM.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Copia-Items V-table-Win 
PROCEDURE Copia-Items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE ITEM.
    FOR EACH FacdPedi OF FaccPedi NO-LOCK WHERE Facdpedi.Libre_c05 <> 'OF':
        CREATE ITEM.
        BUFFER-COPY FacDPedi 
            TO ITEM.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Excel V-table-Win 
PROCEDURE Genera-Excel :
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
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 10.
DEFINE VARIABLE x-item                  AS INTEGER .
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.

DEFINE VARIABLE x-desmon                AS CHARACTER.
DEFINE VARIABLE x-enletras              AS CHARACTER.

RUN bin/_numero(FacCPedi.imptot, 2, 1, OUTPUT X-EnLetras).
X-EnLetras = X-EnLetras + (IF FacCPedi.codmon = 1 THEN " NUEVOS SOLES" ELSE " DOLARES AMERICANOS").
X-desmon = (IF FacCPedi.codmon = 1 THEN " NUEVOS SOLES" ELSE " DOLARES AMERICANOS").

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
chWorkSheet:Columns("B"):ColumnWidth = 6.
chWorkSheet:Columns("C"):ColumnWidth = 45.
chWorkSheet:Columns("D"):ColumnWidth = 15.
chWorkSheet:Columns("E"):ColumnWidth = 8.
chWorkSheet:Columns("F"):ColumnWidth = 20.
chWorkSheet:Columns("G"):ColumnWidth = 20.
chWorkSheet:Columns("H"):ColumnWidth = 20.

chWorkSheet:Columns("A:A"):NumberFormat = "0".
chWorkSheet:Columns("B:B"):NumberFormat = "@".
chWorkSheet:Columns("C:C"):NumberFormat = "@".
chWorkSheet:Columns("D:D"):NumberFormat = "@".
chWorkSheet:Columns("E:E"):NumberFormat = "@".
chWorkSheet:Columns("F:F"):NumberFormat = "0.0000".
chWorkSheet:Columns("G:G"):NumberFormat = "0.00".
chWorkSheet:Columns("H:H"):NumberFormat = "0.00".

chWorkSheet:Range("A10:H10"):Font:Bold = TRUE.
chWorkSheet:Range("A10"):Value = "Item".
chWorkSheet:Range("B10"):Value = "Codigo".
chWorkSheet:Range("C10"):Value = "Descripcion".
chWorkSheet:Range("D10"):Value = "Marca".
chWorkSheet:Range("E10"):Value = "Unidad".
chWorkSheet:Range("F10"):Value = "Precio".
chWorkSheet:Range("G10"):Value = "Cantidad".
chWorkSheet:Range("H10"):Value = "Importe".

chWorkSheet:Range("A2"):Value = "Pedido No : " + FacCPedi.NroPed.
chWorkSheet:Range("A3"):Value = "Fecha     : " + STRING(FacCPedi.FchPed,"99/99/9999").
chWorkSheet:Range("A4"):Value = "Cliente   : " + FacCPedi.Codcli + " " + FacCPedi.Nomcli.
chWorkSheet:Range("A5"):Value = "Vendedor  : " + FacCPedi.CodVen + " " + F-Nomven:SCREEN-VALUE IN FRAME {&FRAME-NAME}.
chWorkSheet:Range("A6"):Value = "Moneda    : " + x-desmon.

FOR EACH FacDPedi OF FacCPedi :
    x-item  = x-item + 1.
    FIND Almmmatg WHERE Almmmatg.Codcia = S-CODCIA AND
                        Almmmatg.CodMat = FacDPedi.CodMat
                        NO-LOCK NO-ERROR.
    iColumn = iColumn + 1.
    cColumn = STRING(iColumn).

    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = x-item.
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = FacDPedi.codmat.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = almmmatg.desmat.
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = almmmatg.desmar.
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = FacDPedi.undvta.
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = FacDPedi.preuni.
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = FacDPedi.canped.
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = FacDPedi.implin.

END.

iColumn = iColumn + 2.                           
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "SON : " + x-enletras.

iColumn = iColumn + 2.                           
cColumn = STRING(iColumn).
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):Value = FacCPedi.imptot.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Pedido V-table-Win 
PROCEDURE Genera-Pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE I-NITEM AS INTEGER NO-UNDO INIT 0.
  DEFINE VARIABLE f-Factor AS DEC NO-UNDO.
  DEFINE VARIABLE x-CanPed AS DEC NO-UNDO.
  DEFINE VARIABLE s-StkComprometido AS DEC.
  DEFINE VARIABLE s-StkDis AS DEC NO-UNDO.

  DEF VAR f-PreBas AS DEC NO-UNDO.
  DEF VAR f-PreVta AS DEC NO-UNDO.
  DEF VAR f-Dsctos AS DEC NO-UNDO.
  DEF VAR y-Dsctos AS DEC NO-UNDO.
  DEF VAR SW-LOG1  AS LOGI NO-UNDO.
  DEF VAR x-StkAct AS DEC NO-UNDO.

  /* POR CADA ITEM VOLVEMOS A VERIFICAR EL STOCK DISPONIBLE
    SI NO HAY STOCK RECALCULAMOS EL PRECIO DE VENTA */
  /* Borramos data sobrante */
  FOR EACH ITEM WHERE ITEM.CanPed <= 0:
      DELETE ITEM.
  END.
  /* RHC 06.07.2012 Promociones especiales */
  RUN vtamay/promociones-manuales (INPUT-OUTPUT TABLE ITEM).
  /* ************************************* */

  /* RHC 09/08/2012 DEPURAMOS LOS REPETIDOS */
  EMPTY TEMP-TABLE ITEM-3.
  FOR EACH ITEM WHERE ITEM.CanPed > 0 BREAK BY ITEM.CodMat:
      IF FIRST-OF(ITEM.CodMat) THEN DO:
          CREATE ITEM-3.
          BUFFER-COPY ITEM TO ITEM-3.
      END.
  END.
  EMPTY TEMP-TABLE ITEM.
  FOR EACH ITEM-3:
      CREATE ITEM.
      BUFFER-COPY ITEM-3 TO ITEM.
  END.

  /* RECALCULAMOS LOS PRECIOS */
  RUN Procesa-Handle IN lh_handle ('Recalculo').
  /* ********************* */
  EMPTY TEMP-TABLE ITEM-3.
  DETALLE:
  FOR EACH ITEM, FIRST Almmmatg OF ITEM NO-LOCK BY ITEM.NroItm: 
      ASSIGN
          ITEM.Libre_d01 = ITEM.CanPed.
          /*ITEM.Libre_c01 = '*'.*/
      /* RUTINA QUE VERIFICA NUEVAMENTE EL STOCK DISPONIBLE Y AJUSTA LA CANTIDAD EN CASO NECESARIO */
      f-Factor = ITEM.Factor.
/*       FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas */
/*           AND Almtconv.Codalter = ITEM.UndVta                 */
/*           NO-LOCK NO-ERROR.                                   */
/*       f-Factor = Almtconv.Equival / Almmmatg.FacEqu.          */
      RUN vta2/Stock-Comprometido (ITEM.CodMat, ITEM.AlmDes, OUTPUT s-StkComprometido).
      FIND Almmmate WHERE Almmmate.codcia = s-codcia
          AND Almmmate.codalm = ITEM.AlmDes
          AND Almmmate.codmat = ITEM.CodMat
          NO-LOCK NO-ERROR .
      x-StkAct = Almmmate.StkAct.
      s-StkDis = x-StkAct - s-StkComprometido.
      IF s-StkDis < 0 THEN s-StkDis = 0.    /* *** OJO *** */
      x-CanPed = ITEM.CanPed * f-Factor.
      IF s-StkDis < x-CanPed THEN DO:
          /* CONTROL DE AJUTES */
          CREATE ITEM-3.
          BUFFER-COPY ITEM TO ITEM-3
              ASSIGN ITEM-3.CanAte = 0.     /* Valor por defecto */    
          /* AJUSTAMOS Y RECALCULAMOS IMPORTES */
          ASSIGN
              ITEM.CanPed = s-StkDis / f-Factor
              ITEM.Libre_c01 = '*'.
          IF Almtconv.Multiplos <> 0 THEN DO:
              IF (ITEM.CanPed / Almtconv.Multiplos) <> INTEGER(ITEM.CanPed / Almtconv.Multiplos) THEN DO:
                  ITEM.CanPed = TRUNCATE(ITEM.CanPed / Almtconv.Multiplos, 0) * Almtconv.Multiplos.
              END.
          END.
          IF s-FlgEmpaque = YES AND Almmmatg.CanEmp > 0 THEN DO:
              ITEM.CanPed = (TRUNCATE((ITEM.CanPed * ITEM.Factor / Almmmatg.CanEmp),0) * Almmmatg.CanEmp) / ITEM.Factor.
          END.
          ASSIGN ITEM-3.CanAte = ITEM.CanPed.       /* CANTIDAD AJUSTADA */

          RUN vta2/PrecioContaMayorista (s-CodCia,
                        s-CodDiv,
                        s-CodCli,
                        s-CodMon,
                        s-TpoCmb,
                        OUTPUT f-Factor,
                        ITEM.CodMat,
                        s-FlgSit,
                        ITEM.UndVta,
                        ITEM.CanPed,
                        s-NroDec,
                        ENTRY(1, s-CodAlm),
                        OUTPUT f-PreBas,
                        OUTPUT f-PreVta,
                        OUTPUT f-Dsctos,
                        OUTPUT y-Dsctos).
          ASSIGN
              ITEM.Por_Dsctos[3] = y-Dsctos
              ITEM.PreUni = f-PreVta.
          ASSIGN
              ITEM.ImpLin = ITEM.CanPed * ITEM.PreUni * 
                    ( 1 - ITEM.Por_Dsctos[1] / 100 ) *
                    ( 1 - ITEM.Por_Dsctos[2] / 100 ) *
                    ( 1 - ITEM.Por_Dsctos[3] / 100 ).
          IF ITEM.Por_Dsctos[1] = 0 AND ITEM.Por_Dsctos[2] = 0 AND ITEM.Por_Dsctos[3] = 0 
              THEN ITEM.ImpDto = 0.
          ELSE ITEM.ImpDto = ITEM.CanPed * ITEM.PreUni - ITEM.ImpLin.
          ASSIGN
              ITEM.ImpLin = ROUND(ITEM.ImpLin, 2)
              ITEM.ImpDto = ROUND(ITEM.ImpDto, 2).
          IF ITEM.AftIsc 
              THEN ITEM.ImpIsc = ROUND(ITEM.PreBas * ITEM.CanPed * (Almmmatg.PorIsc / 100),4).
          ELSE ITEM.ImpIsc = 0.
          IF ITEM.AftIgv 
              THEN ITEM.ImpIgv = ITEM.ImpLin - ROUND(ITEM.ImpLin  / (1 + (s-PorIgv / 100)),4).
          ELSE ITEM.ImpIgv = 0.
      END.
  END.

  FOR EACH ITEM WHERE ITEM.CanPed > 0 BY ITEM.NroItm:
      I-NITEM = I-NITEM + 1.
      CREATE Facdpedi.
      BUFFER-COPY ITEM TO Facdpedi
          ASSIGN
            Facdpedi.CodCia = Faccpedi.CodCia
            Facdpedi.CodDiv = Faccpedi.CodDiv
            Facdpedi.coddoc = Faccpedi.coddoc
            Facdpedi.NroPed = Faccpedi.NroPed
            Facdpedi.FchPed = Faccpedi.FchPed
            Facdpedi.Hora   = Faccpedi.Hora 
            Facdpedi.FlgEst = Faccpedi.FlgEst
            Facdpedi.NroItm = I-NITEM
            Facdpedi.CanPick = Facdpedi.CanPed.   /* OJO */
  END.
  /* verificamos que al menos exista 1 item grabado */
  FIND FIRST Facdpedi OF Faccpedi NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Facdpedi 
  THEN RETURN 'ADM-ERROR'.
  ELSE RETURN 'OK'.

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
  FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK.
  ASSIGN
      s-Copia-Registro = NO
      s-PorIgv = FacCfgGn.PorIgv.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-Handle IN lh_Handle ('Pagina2').
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          s-CodMon = 1
          s-CodCli = x-ClientesVarios
          s-CndVta = ''
          s-TpoCmb = FacCfgGn.TpoCmb[1] 
          s-NroDec = 4
          s-FlgIgv = YES    /* Venta AFECTA a IGV */
          s-FlgSit = ""
          FacCPedi.CodMon:SCREEN-VALUE = "Soles"
          FacCPedi.Cmpbnte:SCREEN-VALUE = "FAC"
          FacCPedi.Libre_d01:SCREEN-VALUE = STRING(s-NroDec, '9')
          FacCPedi.FlgIgv:SCREEN-VALUE = "YES"
          Faccpedi.FlgSit:SCREEN-VALUE = s-FlgSit
          s-NroPed = ""
          s-adm-new-record = "YES".
      DISPLAY 
          STRING(FacCorre.NroSer, '999') + STRING(FacCorre.Correlativo, '999999') @ FacCPedi.NroPed
          TODAY @ FacCPedi.FchPed
          S-TPOCMB @ FacCPedi.TpoCmb
          TODAY @ FacCPedi.FchVen 
          s-CodCli @ FacCPedi.CodCli.
          /*s-CodVen @ Faccpedi.codven*/
      RUN Borra-Temporal.
      RUN Procesa-Handle IN lh_Handle ('Pagina2').
      APPLY 'ENTRY':U TO FacCPedi.CodCli.
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
  DEFINE VARIABLE I-NITEM AS INTEGER NO-UNDO INIT 0.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES' THEN DO:
      {vtagn/i-faccorre-01.i &Codigo = s-coddoc &Serie = s-nroser}
      ASSIGN 
          FacCPedi.CodCia = S-CODCIA
          FacCPedi.CodDiv = S-CODDIV
          FacCPedi.CodDoc = s-coddoc 
          FacCPedi.CodAlm = s-CodAlm    /* Lista de Almacenes V�lidos de Venta */
          FacCPedi.FchPed = TODAY 
          FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
          FacCPedi.TpoPed = s-TpoPed
          FacCPedi.FlgEst = "P".    /* PENDIENTE */
      ASSIGN
          FacCorre.Correlativo = FacCorre.Correlativo + 1.
      /* TRACKING */
      RUN vtagn/pTracking-04 (s-CodCia,
                        s-CodDiv,
                        Faccpedi.CodDoc,
                        Faccpedi.NroPed,
                        s-User-Id,
                        'GNP',
                        'P',
                        DATETIME(TODAY, MTIME),
                        DATETIME(TODAY, MTIME),
                        Faccpedi.CodDoc,
                        Faccpedi.NroPed,
                        Faccpedi.CodRef,
                        Faccpedi.NroRef).
  END.
  ELSE DO:
      RUN Borra-Pedido (YES).
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
  END.
  ASSIGN 
      FacCPedi.PorIgv = s-PorIgv
      FacCPedi.Hora = STRING(TIME,"HH:MM")
      FacCPedi.Usuario = S-USER-ID
      FacCPedi.FlgEnv = s-FlgEnv.

  /* Detalle del Pedido */
  RUN Genera-Pedido.    /* Detalle del pedido */
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      MESSAGE 'NO se pudo generar el pedido' SKIP
          'NO hay stock suficiente en los almacenes' VIEW-AS ALERT-BOX ERROR.
      UNDO, RETURN 'ADM-ERROR'.
  END.

  RUN Venta-Corregida.
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      RUN Procesa-Handle IN lh_Handle ('browse2').
      UNDO, RETURN 'ADM-ERROR'.
  END.

  /* RHC 08.08.11 agrupamos las promociones */
  RUN vta2/promociones-pedidos-cred ( ROWID(Faccpedi), YES).
  IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

  /* Grabamos Totales */
  {vta2/graba-totales-cotizacion-cred.i}

  /* Actualizamos la cotizacion */
  RUN vtagn/actualiza-cotizacion ( ROWID(Faccpedi), +1 ).
  IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

  IF AVAILABLE (FacCorre) THEN RELEASE FacCorre.
  IF AVAILABLE (Facdpedi) THEN RELEASE Facdpedi.
  IF AVAILABLE (B-CPEDI) THEN RELEASE B-CPEDI.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DEF VAR x-CodRef AS CHAR.
  DEF VAR x-NroRef AS CHAR.

  IF NOT AVAILABLE FaccPedi THEN RETURN "ADM-ERROR".
  FIND FacCorre WHERE FacCorre.CodCia = S-CODCIA 
      AND FacCorre.CodDoc = S-CODDOC 
      AND FacCorre.NroSer = s-NroSer
      NO-LOCK NO-ERROR.
  IF FacCorre.FlgEst = NO THEN DO:
      MESSAGE 'Esta serie est� bloqueada para hacer movimientos' VIEW-AS ALERT-BOX WARNING.
      RETURN 'ADM-ERROR'.
  END.
  IF LOOKUP(FacCPedi.CodRef, "C/M") > 0 THEN DO:
      MESSAGE 'NO se puede copiar un pedido basado en una cotizaci�n'
          VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  MESSAGE 'Se va a proceder a con la copia' SKIP
      'El pedido ORIGINAL puede que sea ANULADO' SKIP
      'Continuamos (S-N)?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      UPDATE rpta AS LOG.
  IF rpta = NO THEN RETURN 'ADM-ERROR'.
  FIND CURRENT Faccpedi EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE FacCPedi THEN RETURN "ADM-ERROR".
  IF Faccpedi.FlgEst = 'P' THEN Faccpedi.FlgEst = 'A'.
  FIND CURRENT Faccpedi NO-LOCK NO-ERROR.

  ASSIGN
      s-CodMon = Faccpedi.codmon
      s-CodCli = Faccpedi.codcli
      s-CndVta = Faccpedi.fmapgo
      s-TpoCmb = FacCfgGn.TpoCmb[1]
      s-FlgIgv = Faccpedi.FlgIgv
      s-Copia-Registro = YES    /* <<< OJO >>> */
      s-PorIgv = FacCfgGn.PorIgv
      s-NroDec = Faccpedi.Libre_d01
      x-CodRef = FacCPedi.CodDoc
      x-NroRef = FacCPedi.NroPed.
  RUN Copia-Items.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      DISPLAY 
          "" @ F-Estado
          STRING(FacCorre.NroSer, '999') + STRING(FacCorre.Correlativo, '999999') @ FacCPedi.NroPed
          TODAY @ FacCPedi.FchPed
          S-TPOCMB @ FacCPedi.TpoCmb
          TODAY @ FacCPedi.FchVen.
          /*x-CodRef @ FacCPedi.CodRef
          x-NroRef @ FacCPedi.NroRef.*/
      APPLY "ENTRY":U TO FacCPedi.CodCli.
  END.
  RUN Procesa-Handle IN lh_Handle ('Pagina2').
  RUN Procesa-Handle IN lh_Handle ('Browse').
  RUN Procesa-Handle IN lh_Handle ('Recalculo').
  RUN Procesa-Handle IN lh_Handle ("Disable-btn-cotiza").

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
    IF FacCPedi.FlgEst = "A" THEN DO:
       MESSAGE "El pedido ya fue anulado" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.
    IF FacCPedi.FlgEst = "C" THEN DO:
       MESSAGE "No puede eliminar un pedido TOTALMENTE atendido" VIEW-AS ALERT-BOX ERROR.
       RETURN "ADM-ERROR".
    END.

  DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      FIND CURRENT FacCPedi EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE FacCPedi THEN UNDO, RETURN 'ADM-ERROR'.
      ASSIGN                 
          FacCPedi.UsrAprobacion = s-user-id
          FacCPedi.FchAprobacion = TODAY
          FacCPedi.FlgEst = 'A'
          FacCPedi.Glosa  = "ANULADO POR: " + TRIM (s-user-id) + " EL DIA: " + STRING(TODAY) + " " + STRING(TIME, 'HH:MM').
      /* BORRAMOS DETALLE */
      RUN Borra-Pedido (FALSE).
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

      FIND CURRENT FacCPedi NO-LOCK.
      RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
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
  IF AVAILABLE FacCPedi THEN DO WITH FRAME {&FRAME-NAME}:
      RUN vta2/p-faccpedi-flgest (Faccpedi.flgest, Faccpedi.coddoc, OUTPUT f-Estado).
      DISPLAY f-Estado.
      F-Nomtar:SCREEN-VALUE = ''.
      FIND FIRST Gn-Card WHERE Gn-Card.NroCard = FacCPedi.NroCar NO-LOCK NO-ERROR.
      IF AVAILABLE Gn-Card THEN F-NomTar:SCREEN-VALUE = GN-CARD.NomClie[1].
      F-NomVen:SCREEN-VALUE = "".
      FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA 
          AND  gn-ven.CodVen = FacCPedi.CodVen 
          NO-LOCK NO-ERROR.
      IF AVAILABLE gn-ven THEN F-NomVen:screen-value = gn-ven.NomVen.
      F-CndVta:SCREEN-VALUE = "".
      FIND gn-convt WHERE gn-convt.Codig = FacCPedi.FmaPgo NO-LOCK NO-ERROR.
      IF AVAILABLE gn-convt THEN F-CndVta:SCREEN-VALUE = gn-convt.Nombr.
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
          FacCPedi.NomCli:SENSITIVE = NO
          FacCPedi.DirCli:SENSITIVE = NO
          FacCPedi.RucCli:SENSITIVE = NO
          FacCPedi.fchven:SENSITIVE = NO
          FacCPedi.TpoCmb:SENSITIVE = NO
          FacCPedi.FmaPgo:SENSITIVE = NO
          FacCPedi.FlgIgv:SENSITIVE = NO
          FacCPedi.Libre_d01:SENSITIVE = NO
          FacCPedi.CodMon:SENSITIVE = NO
          FacCPedi.CodRef:SENSITIVE = NO
          FacCPedi.NroRef:SENSITIVE = NO.
      RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
      IF RETURN-VALUE = 'NO' THEN FacCPedi.CodCli:SENSITIVE = NO.
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
  IF Faccpedi.FlgEst <> "A" THEN RUN vta2/r-impped-cont (ROWID(Faccpedi)).

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
  /* Env�o de Pedido */
  s-FlgEnv = YES.
/*   MESSAGE 'El Pedido es para enviar?'                           */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO                 */
/*       TITLE 'Confirmacion de Pedidos a Enviar' UPDATE s-FlgEnv. */

  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      /*RUN Procesa-Handle IN lh_Handle ('browse2').*/
      /*UNDO, RETURN 'ADM-ERROR'.*/
      RETURN 'ADM-ERROR'.
  END.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-Handle IN lh_Handle ('Pagina1'). 
  RUN Procesa-Handle IN lh_Handle ('browse'). 

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
  {src/adm/template/snd-list.i "FacCPedi"}

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
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE F-TOT AS DECIMAL INIT 0 NO-UNDO.
  DEFINE VARIABLE F-BOL AS DECIMAL INIT 0 NO-UNDO.

  DO WITH FRAME {&FRAME-NAME} :
      /* VALIDACION DEL CLIENTE */
      IF FacCPedi.CodCli:SCREEN-VALUE = "" THEN DO:
          MESSAGE "Codigo de cliente no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
          APPLY "ENTRY" TO FacCPedi.CodCli.
          RETURN "ADM-ERROR".   
      END.
      FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA
          AND  gn-clie.CodCli = FacCPedi.CodCli:SCREEN-VALUE 
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE gn-clie THEN DO:
         MESSAGE "Codigo de cliente no existe" VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO FacCPedi.CodCli.
         RETURN "ADM-ERROR".   
      END.
      IF gn-clie.FlgSit = "I" THEN DO:
         MESSAGE "Cliente esta Inactivo" VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO FacCPedi.CodCli.
         RETURN "ADM-ERROR".   
      END.
      IF gn-clie.FlgSit = "C" THEN DO:
         MESSAGE "Cliente esta Cesado" VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO FacCPedi.CodCli.
         RETURN "ADM-ERROR".   
      END.
      IF gn-clie.canal = '006' THEN DO:
          MESSAGE 'Cliente no permitido en este canal de venta de venta'
              VIEW-AS ALERT-BOX ERROR.
          APPLY "ENTRY" TO FacCPedi.CodCli.
          RETURN "ADM-ERROR".   
      END.
      IF Faccpedi.Cmpbnte:SCREEN-VALUE = "FAC" AND FacCpedi.RucCli:SCREEN-VALUE = '' THEN DO:
          MESSAGE "El Cliente NO tiene R.U.C." VIEW-AS ALERT-BOX ERROR.
          APPLY "ENTRY" TO FacCPedi.CodCli.
          RETURN "ADM-ERROR".   
      END.
    /* VALIDACION DEL VENDEDOR */
     IF FacCPedi.CodVen:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Codigo de Vendedor no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FacCPedi.CodVen.
        RETURN "ADM-ERROR".   
     END.
     FIND gn-ven WHERE gn-ven.CodCia = S-CODCIA 
         AND  gn-ven.CodVen = FacCPedi.CodVen:SCREEN-VALUE 
         NO-LOCK NO-ERROR.
     IF NOT AVAILABLE gn-ven THEN DO:
        MESSAGE "Codigo de Vendedor no existe" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FacCPedi.CodVen.
        RETURN "ADM-ERROR".   
     END.
     ELSE DO:
         IF gn-ven.flgest = "C" THEN DO:
             MESSAGE "Codigo de Vendedor Cesado" VIEW-AS ALERT-BOX ERROR.
             APPLY "ENTRY" TO FacCPedi.CodVen.
             RETURN "ADM-ERROR".   
         END.
     END.
    /* VALIDACION DE LA CONDICION DE VENTA */    
     IF FacCPedi.FmaPgo:SCREEN-VALUE = "" THEN DO:
        MESSAGE "Condicion Venta no debe ser blanco" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FacCPedi.FmaPgo.
        RETURN "ADM-ERROR".   
     END.
     FIND gn-convt WHERE gn-convt.Codig = FacCPedi.FmaPgo:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF NOT AVAILABLE gn-convt THEN DO:
        MESSAGE "Condicion Venta no existe" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FacCPedi.FmaPgo.
        RETURN "ADM-ERROR".   
     END.
     IF FacCPedi.FmaPgo:SCREEN-VALUE <> "000" THEN DO:
        MESSAGE "Condicion Venta debe ser contado" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FacCPedi.FmaPgo.
        RETURN "ADM-ERROR".   
     END.
    /* VALIDACION DE LA TARJETA */
     IF Faccpedi.NroCar:SCREEN-VALUE <> "" THEN DO:
         FIND Gn-Card WHERE Gn-Card.NroCard = Faccpedi.NroCar:SCREEN-VALUE
             NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Gn-Card THEN DO:
             MESSAGE "Numero de Tarjeta Incorrecto, Verifique... " VIEW-AS ALERT-BOX ERROR.
             APPLY "ENTRY" TO FacCPedi.NroCar.
             RETURN "ADM-ERROR".   
         END.   
     END.           
     /* VALIDACION DE ITEMS */
     FOR EACH ITEM NO-LOCK:
         F-Tot = F-Tot + ITEM.ImpLin.
     END.
     IF F-Tot = 0 THEN DO:
        MESSAGE "Importe total debe ser mayor a cero" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FacCPedi.CodCli.
        RETURN "ADM-ERROR".   
     END.
     /* VALIDACION DE MONTO MINIMO POR BOLETA */
     F-BOL = IF INTEGER(FacCPedi.CodMon:SCREEN-VALUE) = 1 
             THEN F-TOT
             ELSE F-Tot * DECIMAL(FacCPedi.TpoCmb:SCREEN-VALUE).
     IF ( Faccpedi.Cmpbnte:SCREEN-VALUE = 'BOL' 
          OR Faccpedi.Cmpbnte:SCREEN-VALUE = 'TCK' )
          AND F-BOL > 700 
         AND (FacCPedi.Atencion:SCREEN-VALUE = '' 
             OR LENGTH(FacCPedi.Atencion:SCREEN-VALUE, "CHARACTER") < 8)
     THEN DO:
         MESSAGE "Venta Mayor a 700.00" SKIP
                 "Debe ingresar en DNI"
             VIEW-AS ALERT-BOX ERROR.
         APPLY "ENTRY" TO FacCPedi.Atencion.
         RETURN "ADM-ERROR".   
     END.
     /* VALIDACION DE IMPORTE MINIMO POR COTIZACION */
     IF s-FlgEnv = YES THEN DO:
         DEF VAR pImpMin AS DEC NO-UNDO.
         RUN gn/pMinCotPed (s-CodCia,
                            s-CodDiv,
                            s-CodDoc,
                            OUTPUT pImpMin).
         IF pImpMin > 0 AND f-Bol < pImpMin THEN DO:
             MESSAGE 'El importe m�nimo para el env�o es de S/.' pImpMin
                 VIEW-AS ALERT-BOX ERROR.
             RETURN "ADM-ERROR".
         END.
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

DEFINE VAR RPTA AS CHAR.

IF NOT AVAILABLE FacCPedi THEN RETURN "ADM-ERROR".
IF FacCPedi.FlgEst <> "P" THEN DO:
    MESSAGE 'Acceso denegado' VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
IF FacCPedi.FchPed < TODAY THEN DO:
    MESSAGE 'Acceso denegado' VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
ASSIGN
    S-CODMON = FacCPedi.CodMon
    S-CODCLI = FacCPedi.CodCli
    S-TPOCMB = FacCPedi.TpoCmb
    S-CNDVTA = FacCPedi.FmaPgo
    s-cndvta-validos = FacCPedi.FmaPgo
    s-Copia-Registro = NO
    s-PorIgv = Faccpedi.porigv
    s-NroDec = Faccpedi.Libre_d01
    s-FlgIgv = Faccpedi.FlgIgv
    s-FlgSit = Faccpedi.FlgSit
    s-NroPed = Faccpedi.NroPed
    s-adm-new-record = "NO".

RUN Carga-Temporal.

RUN Procesa-Handle IN lh_Handle ('Pagina2').
RUN Procesa-Handle IN lh_Handle ('browse').

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Venta-Corregida V-table-Win 
PROCEDURE Venta-Corregida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST ITEM-3 NO-LOCK NO-ERROR.
IF NOT AVAILABLE ITEM-3 THEN RETURN 'OK'.
RUN vta2/d-vtacorr-cont.
/* RHC Se va a mostrar el error pero continuamos con la grabaci�n */
/*RETURN 'ADM-ERROR'.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
