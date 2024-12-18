&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE C-Detalle NO-UNDO LIKE INTEGRAL.Almdmov.
DEFINE TEMP-TABLE Detalle NO-UNDO LIKE INTEGRAL.Almdmov
       INDEX LLave01 CodMat.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
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

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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
DEF SHARED VAR s-codalm AS CHAR.
DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR lh_handle AS HANDLE.

DEF VAR R-ROWID AS ROWID NO-UNDO.

DEFINE VAR x-timedesde AS INT.
DEFINE VAR x-timehasta AS INT.

FIND FIRST Almtmovm WHERE Almtmovm.CodCia = S-CODCIA AND
     Almtmovm.Tipmov = "I" AND
     Almtmovm.CodMov = 02
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtmovm THEN DO:
   MESSAGE "No existe movimiento 02 de Ingreso por Compras" VIEW-AS ALERT-BOX ERROR.
   RETURN "ADM-ERROR".
END.
/* CHEQUEAMOS LA CONFIGURACION DE CORRELATIVOS */
FIND FIRST Almtdocm WHERE Almtdocm.CodCia = S-CODCIA AND
     Almtdocm.CodAlm = S-CODALM AND
     Almtdocm.TipMov = "I" AND
     Almtdocm.Codmov = Almtmovm.Codmov NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtdocm THEN DO:
   MESSAGE "No esta asignado el movimiento 02" SKIP
           "de Ingreso por Compras al almacen" VIEW-AS ALERT-BOX ERROR.
   RETURN "ADM-ERROR".
END.

DEF VAR x-HorIni LIKE faccpedi.horsac NO-UNDO.
DEF VAR x-FchIni LIKE faccpedi.fecsac NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Detalle INTEGRAL.Almmmatg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table Detalle.NroItm Detalle.codmat ~
INTEGRAL.Almmmatg.DesMat Detalle.CodUnd Detalle.CanDes ~
INTEGRAL.Almmmatg.Pesmat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table Detalle.codmat ~
Detalle.CanDes 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table Detalle
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table Detalle
&Scoped-define QUERY-STRING-br_table FOR EACH Detalle WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH INTEGRAL.Almmmatg OF Detalle NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH Detalle WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH INTEGRAL.Almmmatg OF Detalle NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table Detalle INTEGRAL.Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table Detalle
&Scoped-define SECOND-TABLE-IN-QUERY-br_table INTEGRAL.Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS x-NroRf1 Btn-OrdCmp x-NroRf3 x-Observ ~
txtTTrans br_table txtMsg 
&Scoped-Define DISPLAYED-OBJECTS x-NroRf1 x-CodPro x-NomPro x-NroRf3 ~
x-Observ txtTTxx txtTTrans txtMsg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE ocxTimer AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chocxTimer AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-OrdCmp 
     LABEL "O/C" 
     SIZE 8.43 BY 1.08
     FONT 6.

DEFINE VARIABLE txtMsg AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 77.14 BY .81 NO-UNDO.

DEFINE VARIABLE txtTTrans AS CHARACTER FORMAT "X(60)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.65
     BGCOLOR 15 FGCOLOR 12 FONT 8 NO-UNDO.

DEFINE VARIABLE txtTTxx AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 48.86 BY 1.65
     BGCOLOR 15 FGCOLOR 12 FONT 8 NO-UNDO.

DEFINE VARIABLE x-CodPro AS CHARACTER FORMAT "X(256)":U 
     LABEL "Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE x-NomPro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .81 NO-UNDO.

DEFINE VARIABLE x-NroRf1 AS CHARACTER FORMAT "x(10)" 
     LABEL "Orden de Compra" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81.

DEFINE VARIABLE x-NroRf3 AS CHARACTER FORMAT "X(10)":U 
     LABEL "G/R" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE x-Observ AS CHARACTER FORMAT "X(60)":U 
     LABEL "Observaciones" 
     VIEW-AS FILL-IN 
     SIZE 62 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      Detalle, 
      INTEGRAL.Almmmatg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      Detalle.NroItm FORMAT ">>>>9":U
      Detalle.codmat COLUMN-LABEL "Codigo" FORMAT "X(14)":U
      INTEGRAL.Almmmatg.DesMat FORMAT "X(45)":U
      Detalle.CodUnd COLUMN-LABEL "Unidad" FORMAT "X(4)":U
      Detalle.CanDes FORMAT "ZZ,ZZZ,ZZ9.9999":U
      INTEGRAL.Almmmatg.Pesmat COLUMN-LABEL "Peso!Articulo" FORMAT "->>,>>9.9999":U
  ENABLE
      Detalle.codmat
      Detalle.CanDes
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 105.14 BY 9.23
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     x-NroRf1 AT ROW 1 COL 17 COLON-ALIGNED
     Btn-OrdCmp AT ROW 1 COL 83
     x-CodPro AT ROW 1.85 COL 17 COLON-ALIGNED
     x-NomPro AT ROW 1.85 COL 31 COLON-ALIGNED NO-LABEL
     x-NroRf3 AT ROW 2.73 COL 17 COLON-ALIGNED WIDGET-ID 2
     x-Observ AT ROW 3.54 COL 17 COLON-ALIGNED WIDGET-ID 4
     txtTTxx AT ROW 4.73 COL 53 RIGHT-ALIGNED NO-LABEL WIDGET-ID 20
     txtTTrans AT ROW 4.73 COL 51.72 COLON-ALIGNED WIDGET-ID 24
     br_table AT ROW 6.77 COL 3.86
     txtMsg AT ROW 16.38 COL 1.86 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     SPACE(12.01) SKIP(0.00)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: C-Detalle T "?" NO-UNDO INTEGRAL Almdmov
      TABLE: Detalle T "?" NO-UNDO INTEGRAL Almdmov
      ADDITIONAL-FIELDS:
          INDEX LLave01 CodMat
      END-FIELDS.
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 18.08
         WIDTH              = 109.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm-vm/method/vmbrowser.i}
{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table txtTTrans F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       txtTTrans:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN txtTTxx IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       txtTTxx:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN x-CodPro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-NomPro IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.Detalle,INTEGRAL.Almmmatg OF Temp-Tables.Detalle"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   = Temp-Tables.Detalle.NroItm
     _FldNameList[2]   > Temp-Tables.Detalle.codmat
"Detalle.codmat" "Codigo" "X(14)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = INTEGRAL.Almmmatg.DesMat
     _FldNameList[4]   > Temp-Tables.Detalle.CodUnd
"Detalle.CodUnd" "Unidad" "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.Detalle.CanDes
"Detalle.CanDes" ? "ZZ,ZZZ,ZZ9.9999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.Almmmatg.Pesmat
"Almmmatg.Pesmat" "Peso!Articulo" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME ocxTimer ASSIGN
       FRAME           = FRAME F-Main:HANDLE
       ROW             = 3.31
       COLUMN          = 78.72
       HEIGHT          = 3.85
       WIDTH           = 14.29
       WIDGET-ID       = 22
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      ocxTimer:NAME = "ocxTimer":U .
/* ocxTimer OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      ocxTimer:MOVE-AFTER(x-NroRf3:HANDLE IN FRAME F-Main).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Detalle.codmat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Detalle.codmat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF Detalle.codmat IN BROWSE br_table /* Codigo */
DO:
  /* Vamos a buscar primero el codigo de barras, luego el codigo interno */
  DEF VAR pCodMat LIKE Detalle.codmat.
  DEF VAR pCanDes LIKE Detalle.candes.

  IF SELF:SCREEN-VALUE = '' THEN RETURN.
  pCodMat = SELF:SCREEN-VALUE.
  RUN alm/p-codbrr (INPUT-OUTPUT pCodMat, INPUT-OUTPUT pCanDes, s-codcia).
  ASSIGN
    SELF:SCREEN-VALUE = pCodMat
    NO-ERROR.
  IF pcodmat = '' THEN DO:
    Detalle.candes:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = '1'.
    RETURN NO-APPLY.
  END.
  FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = SELF:SCREEN-VALUE
    NO-LOCK.

  FIND Lg-docmp WHERE Lg-docmp.codcia = s-codcia
    AND Lg-docmp.tpodoc = 'N'
    AND Lg-docmp.nrodoc = INTEGER(x-NroRf1)
    AND LG-docmp.codmat = SELF:SCREEN-VALUE 
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Lg-docmp THEN DO:
    MESSAGE 'Art�culo NO registrado en la Orden de Compra'
        VIEW-AS ALERT-BOX ERROR.
    SELF:SCREEN-VALUE = ''.
    Detalle.candes:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = '1'.
    RETURN NO-APPLY.
  END.
  DISPLAY
    Almmmatg.desmat 
    LG-DOCmp.UndCmp @ Detalle.codund
    pCanDes @ Detalle.CanDes
    WITH BROWSE {&BROWSE-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-OrdCmp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-OrdCmp B-table-Win
ON CHOOSE OF Btn-OrdCmp IN FRAME F-Main /* O/C */
DO:
  RUN Asigna-Orden-Compra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ocxTimer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ocxTimer B-table-Win OCX.Tick
PROCEDURE ocxTimer.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR ztime AS INT.

x-timehasta = TIME.

zTime = x-timehasta - x-timedesde.
DO WITH FRAME {&FRAME-NAME}:
    txtTTrans:SCREEN-VALUE = STRING(ztime,"HH:MM:SS").
    txtTTxx:SCREEN-VALUE = "Tiempo transcurrido ".
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-NroRf1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-NroRf1 B-table-Win
ON ENTRY OF x-NroRf1 IN FRAME F-Main /* Orden de Compra */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        txtTTrans:VISIBLE = FALSE.
        txtTTxx:VISIBLE = FALSE.
        /*ocxTimer:ENABLED = FALSE.*/
    END.    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-NroRf1 B-table-Win
ON LEAVE OF x-NroRf1 IN FRAME F-Main /* Orden de Compra */
DO:
    IF SELF:SCREEN-VALUE = "" THEN RETURN.
    FIND Lg-cocmp WHERE codcia = s-codcia
        AND nrodoc = INTEGER(SELF:SCREEN-VALUE)
        AND FlgSit = "P"
        AND tpodoc = "N"
        AND codalm = s-codalm
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Lg-cocmp THEN DO:
        MESSAGE "Orden de Compra NO v�lida" VIEW-AS ALERT-BOX WARNING.
        RETURN NO-APPLY.
    END.
    IF Lg-cocmp.fchvto < TODAY THEN DO:
        MESSAGE 'La orden de compra ya venci� el' Lg-cocmp.fchvto 
            VIEW-AS ALERT-BOX WARNING.
        RETURN NO-APPLY.
    END.
    ASSIGN
        x-NroRf1 = STRING(LG-COCmp.NroDoc)
        x-CodPro = LG-COCmp.CodPro
        x-NomPro = LG-COCmp.NomPro
        Btn-OrdCmp:SENSITIVE = NO
        x-NroRf1:SENSITIVE = NO.
    /* Arranca el ciclo */
    ASSIGN
        x-FchIni = TODAY
        x-HorIni = STRING(TIME, 'HH:MM').

        x-timedesde = TIME.
        txtTTrans:VISIBLE = TRUE.            
        txtTTxx:VISIBLE = TRUE.


    DISPLAY x-NroRf1 x-CodPro x-NomPro WITH FRAME {&FRAME-NAME}.
    EMPTY TEMP-TABLE Detalle.
    RUN dispatch IN THIS-PROCEDURE ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-NroRf3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-NroRf3 B-table-Win
ON LEAVE OF x-NroRf3 IN FRAME F-Main /* G/R */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME x-Observ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL x-Observ B-table-Win
ON LEAVE OF x-Observ IN FRAME F-Main /* Observaciones */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

txtTTrans:VISIBLE = FALSE.
txtTTxx:SCREEN-VALUE = "Tiempo transcurrido :".
txtTTxx:VISIBLE = FALSE.

ON RETURN OF Detalle.CanDes, Detalle.codmat DO:
    APPLY 'TAB':U.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Detalle-Orden-Compra B-table-Win 
PROCEDURE Actualiza-Detalle-Orden-Compra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER I-Factor AS INTEGER.

  FOR EACH Almdmov OF Almcmov NO-LOCK ON ERROR UNDO, RETURN "ADM-ERROR":
    FIND LG-DOCmp WHERE 
        LG-DOCmp.CodCia = Almdmov.CodCia AND
        LG-DOCmp.TpoDoc = "N" AND
        LG-DOCmp.NroDoc = INTEGER(Almcmov.NroRf1) AND
        LG-DOCmp.Codmat = Almdmov.CodMat EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE lg-docmp
    THEN LG-DOCmp.CanAten = LG-DOCmp.CanAten + (Almdmov.CanDes * I-Factor).
    RELEASE LG-DOCmp.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna-Orden-Compra B-table-Win 
PROCEDURE Asigna-Orden-Compra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  input-var-1 = "P".       /* Solo aprobadas */
  input-var-2 = ''.

  RUN LKUP\C-OCPEN("Ordenes de Compra Pendientes").

  IF output-var-1 = ? THEN RETURN.

  FIND LG-COCmp WHERE ROWID(LG-COCmp) = output-var-1 NO-LOCK NO-ERROR.
  ASSIGN
      x-NroRf1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Lg-cocmp.nrodoc).
  APPLY "LEAVE":U TO x-NroRf1.

/*   IF AVAILABLE LG-COCmp THEN DO WITH FRAME {&FRAME-NAME}:           */
/*       IF Lg-cocmp.fchvto < TODAY THEN DO:                           */
/*           MESSAGE 'La orden de compra ya venci� el' Lg-cocmp.fchvto */
/*               VIEW-AS ALERT-BOX ERROR.                              */
/*           RETURN.                                                   */
/*       END.                                                          */
/*                                                                     */
/*     ASSIGN                                                          */
/*         x-NroRf1 = STRING(LG-COCmp.NroDoc)                          */
/*         x-CodPro = LG-COCmp.CodPro                                  */
/*         x-NomPro = LG-COCmp.NomPro.                                 */
/*     /* Arranca el ciclo */                                          */
/*     ASSIGN                                                          */
/*         x-FchIni = TODAY                                            */
/*         x-HorIni = STRING(TIME, 'HH:MM').                           */
/*     DISPLAY                                                         */
/*         x-NroRf1                                                    */
/*         x-CodPro                                                    */
/*         x-NomPro.                                                   */
/*     FOR EACH Detalle:                                               */
/*         DELETE Detalle.                                             */
/*     END.                                                            */
/*     RUN dispatch IN THIS-PROCEDURE ('open-query':U).                */
/*   END.                                                              */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cerrar-Orden-Compra B-table-Win 
PROCEDURE Cerrar-Orden-Compra :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR I-NRO AS INTEGER INIT 0 NO-UNDO.

  FOR EACH LG-DOCmp NO-LOCK WHERE 
            LG-DOCmp.CodCia = s-CodCia AND
            LG-DOCmp.TpoDoc = "N" AND
            LG-DOCmp.NroDoc = INTEGER(Almcmov.NroRf1):
       IF (LG-DOCmp.CanPedi - LG-DOCmp.CanAten) > 0 THEN DO:
          I-NRO = 1.
          LEAVE.
       END.
  END.

  FIND LG-COCmp WHERE 
        LG-COCmp.CodCia = Almcmov.CodCia AND
        LG-COCmp.TpoDoc = "N" AND
        LG-COCmp.NroDoc = INTEGER(Almcmov.NroRf1) EXCLUSIVE-LOCK NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN 'ADM-ERROR'.
  IF I-NRO = 0 THEN LG-COCmp.FlgSit = "T".
  ELSE              LG-COCmp.FlgSit = "P".
  LG-COCmp.FchAte = Almcmov.FchDoc.
  RELEASE LG-COCmp.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cierre-de-guia B-table-Win 
PROCEDURE Cierre-de-guia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR x-UsrChq AS CHAR NO-UNDO.
  
  /* Primero veamos si es consistente */
  EMPTY TEMP-TABLE C-Detalle.
  
  FOR EACH Detalle:
    FIND C-Detalle WHERE C-Detalle.codmat = Detalle.codmat
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE C-Detalle THEN CREATE C-Detalle.
    BUFFER-COPY Detalle TO C-Detalle
        ASSIGN
            C-Detalle.candes = C-Detalle.candes + Detalle.candes
            C-Detalle.ImpCto = ROUND(C-Detalle.CanDes * C-Detalle.PreUni,2).

  END.

/*   FOR EACH Lg-docmp NO-LOCK WHERE Lg-docmp.codcia = s-codcia                */
/*         AND Lg-docmp.tpodoc = 'N'                                           */
/*         AND Lg-docmp.nrodoc = INTEGER(x-nrorf1)                             */
/*         AND (Lg-docmp.canped - Lg-docmp.canaten) > 0:                       */
/*     FIND C-Detalle WHERE C-Detalle.codmat = Lg-docmp.codmat                 */
/*         NO-LOCK NO-ERROR.                                                   */
/*     IF NOT AVAILABLE C-Detalle THEN NEXT.                                   */
/*     IF C-Detalle.CanDes > (Lg-docmp.CanPed - LG-DOCmp.CanAten) THEN DO:     */
/*         MESSAGE 'Se ha excedido la cantidad en el art�culo' Lg-docmp.codmat */
/*             VIEW-AS ALERT-BOX ERROR.                                        */
/*         RETURN 'ADM-ERROR'.                                                 */
/*     END.                                                                    */
/*   END.                                                                      */

  FOR EACH C-Detalle:
      FIND Lg-docmp WHERE Lg-docmp.codcia = s-codcia
          AND Lg-docmp.tpodoc = 'N'
          AND Lg-docmp.nrodoc = INTEGER(x-nrorf1)
          AND Lg-docmp.codmat = C-Detalle.codmat
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Lg-docmp THEN DO:
          MESSAGE 'Producto' C-Detalle.codmat 'NO est� registrado en la orden de compra'
              VIEW-AS ALERT-BOX ERROR.
          RETURN 'ADM-ERROR'.
      END.
      IF C-Detalle.CanDes > (Lg-docmp.CanPed - LG-DOCmp.CanAten) THEN DO:
          MESSAGE 'Se ha excedido la cantidad en el art�culo' Lg-docmp.codmat
              VIEW-AS ALERT-BOX ERROR.
          RETURN 'ADM-ERROR'.
      END.
  END.
  RUN vtamay/d-chqped (OUTPUT x-UsrChq).
  IF x-UsrChq = '' THEN RETURN 'ADM-ERROR'.

  DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR'
      WITH FRAME {&FRAME-NAME}:
    FIND Lg-cocmp WHERE Lg-cocmp.codcia = s-codcia
        AND Lg-cocmp.tpodoc = 'N'       /* Nacional */
        AND Lg-cocmp.nrodoc = INTEGER(x-nrorf1)
        EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'NO pudo bloquear la O/C' x-nrorf1 VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        x-NroRf3 
        x-Observ.
    CREATE Almcmov.
    ASSIGN 
        Almcmov.CodCia  = Almtdocm.CodCia 
        Almcmov.CodAlm  = Almtdocm.CodAlm 
        Almcmov.TipMov  = Almtdocm.TipMov
        Almcmov.CodMov  = Almtdocm.CodMov
        Almcmov.NroSer  = 000
        Almcmov.HorRcp  = STRING(TIME,"HH:MM:SS")
        Almcmov.CodPro  = x-CodPro
        Almcmov.NroRf1  = x-NroRf1
        Almcmov.NroRf3  = x-NroRf3
        Almcmov.Observ  = x-Observ
        Almcmov.NomRef  = x-NomPro
        Almcmov.CodMon  = Lg-cocmp.codmon
        Almcmov.ModAdq  = Lg-cocmp.modadq.
    FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= TODAY NO-LOCK NO-ERROR.
    IF AVAILABLE gn-tcmb THEN Almcmov.TpoCmb = gn-tcmb.venta.

    FIND Almacen WHERE Almacen.CodCia = S-CODCIA AND
        Almacen.CodAlm = Almtdocm.CodAlm EXCLUSIVE-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'NO pudo bloquear el almac�n' Almtdocm.CodAlm VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.        
    END.
        
    ASSIGN 
        Almcmov.NroDoc = Almacen.CorrIng
        Almacen.CorrIng = Almacen.CorrIng + 1.
    ASSIGN 
        Almcmov.usuario = S-USER-ID
        Almcmov.ImpIgv  = 0
        Almcmov.ImpMn1  = 0
        Almcmov.ImpMn2  = 0.
    /* CONTROL DE CHEQUEO */
    ASSIGN 
        Almcmov.Libre_c02 = 'C'
        Almcmov.Libre_c03 = x-usrchq
        Almcmov.Libre_c04 = STRING(TIME,'HH:MM')
        Almcmov.Libre_c05 = ''
        Almcmov.Libre_f01 = TODAY
        Almcmov.Libre_f02 = x-fchini
        Almcmov.Libre_c05 = x-horini.
    /* GENERAMOS NUEVO DETALLE */
    RUN Genera-Detalle.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE 'NO pudo generar el detalle' VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    IF Almcmov.codmon = 1 
    THEN ASSIGN Almcmov.ImpMn2 = ROUND(Almcmov.ImpMn1 / Almcmov.tpocmb, 2).
    ELSE ASSIGN Almcmov.ImpMn1 = ROUND(Almcmov.ImpMn2 * Almcmov.tpocmb, 2).
  
    RUN Actualiza-Detalle-Orden-Compra(1).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE 'NO pudo actualizar el saldo de la O/C' VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    RUN Cerrar-Orden-Compra.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE 'NO se pudo cerrar la O/C' VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    CREATE LogTabla.
    ASSIGN
        logtabla.codcia = s-codcia
        logtabla.Dia    = TODAY
        logtabla.Evento = 'CHEQUEADO'
        logtabla.Hora   = STRING(TIME, 'HH:MM')
        logtabla.Tabla  = 'Lg-cocmp'
        logtabla.Usuario = s-user-id
        logtabla.ValorLlave = STRING(x-NroRf1).
    /*RUN alm/R-IMPFMT (ROWID(almcmov)).     /* Impresion del formato */*/
    RUN alm/r-impfmt-1 (ROWID(almcmov), NO).   /* Imprime Almacen y Proveedor */
    RELEASE Almacen.
    RELEASE Almcmov.
    RELEASE LogTabla.
  END.
  
  /* INICIALIZAMOS VARIABLES Y TEMPORALES */
  FOR EACH Detalle:
    DELETE Detalle.
  END.
  RUN dispatch IN THIS-PROCEDURE ('open-query':U).
  ASSIGN
    x-CodPro = ''
    x-NomPro = ''
    x-NroRF1 = ''
    x-NroRf3 = ''
    x-Observ = ''.
  DISPLAY x-CodPro x-NomPro x-NroRF1 x-NroRf3 x-Observ WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load B-table-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "b-chqocn.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chocxTimer = ocxTimer:COM-HANDLE
    UIB_S = chocxTimer:LoadControls( OCXFile, "ocxTimer":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "b-chqocn.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-Detalle B-table-Win 
PROCEDURE Genera-Detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VAR F-PesUnd AS DECIMAL NO-UNDO.

  FOR EACH C-Detalle WHERE C-Detalle.codmat <> "" 
        ON ERROR UNDO, RETURN "ADM-ERROR"
        ON STOP UNDO, RETURN "ADM-ERROR":
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
        Almdmov.codmat = C-Detalle.codmat
        Almdmov.CanDes = C-Detalle.CanDes
        Almdmov.CodUnd = C-Detalle.CodUnd
        Almdmov.Factor = C-Detalle.Factor
        Almdmov.Pesmat = C-Detalle.Pesmat
        Almdmov.ImpCto = ROUND(C-Detalle.CanDes * C-Detalle.PreUni,2)
        Almdmov.PreLis = C-Detalle.PreLis
        Almdmov.PreUni = C-Detalle.PreUni
        Almdmov.Dsctos[1] = C-Detalle.Dsctos[1]
        Almdmov.Dsctos[2] = C-Detalle.Dsctos[2]
        Almdmov.Dsctos[3] = C-Detalle.Dsctos[3]
        Almdmov.IgvMat = C-Detalle.IgvMat
        Almdmov.CodAjt = 'A'
        Almdmov.HraDoc = Almcmov.HorRcp
               R-ROWID = ROWID(Almdmov).
    FIND Almmmatg WHERE Almmmatg.CodCia = Almdmov.CodCia 
        AND Almmmatg.CodMat = Almdmov.codmat NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmatg AND NOT Almmmatg.AftIgv THEN  Almdmov.IgvMat = 0.
    RUN ALM\ALMACSTK (R-ROWID).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE 'Error en el producto' Almdmov.codmat SKIP
            'En el almac�n' Almdmov.codalm
            VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    RUN ALM\ALMACPR1 (R-ROWID,"U").
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE 'Error en el producto' Almdmov.codmat SKIP
            'En el almac�n' Almdmov.codalm
            VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* RHC 03.04.04 BLOQUEADO, SE ACTUALIZA EN LA NOCHE
    RUN ALM\ALMACPR2 (R-ROWID,"U").
    *************************************************** */
    IF Almcmov.codmon = 1 THEN DO:
        Almcmov.ImpMn1 = Almcmov.ImpMn1 + Almdmov.ImpMn1.
    END.
    ELSE DO:
          Almcmov.ImpMn2 = Almcmov.ImpMn2 + Almdmov.ImpMn2.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF x-NroRf1 = '' THEN DO:
    MESSAGE 'Debe ingresar primero la Orden de Compra'
        VIEW-AS ALERT-BOX WARNING.
    RETURN 'ADM-ERROR'.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-handle IN lh_handle('disable-botones').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DEF VAR x-Item AS INT INIT 1 NO-UNDO.
  
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES' THEN DO:
    DEF BUFFER B-DETA FOR Detalle.
    FOR EACH B-DETA NO-LOCK BY B-DETA.NroItm:
        x-Item = x-Item + 1.
    END.
  END.
  ELSE x-Item = Detalle.NroItm.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
    Detalle.codcia = s-codcia
    Detalle.codalm = s-codalm
    Detalle.nroitm = x-item
    Detalle.codund = Lg-docmp.undcmp
    Detalle.candes = DECIMAL(Detalle.candes:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    Detalle.PreLis = LG-DOCmp.PreUni
    Detalle.Dsctos[1] = LG-DOCmp.Dsctos[1]
    Detalle.Dsctos[2] = LG-DOCmp.Dsctos[2]
    Detalle.Dsctos[3] = LG-DOCmp.Dsctos[3]
    Detalle.IgvMat = LG-DOCmp.IgvMat
    Detalle.PreUni = ROUND(LG-DOCmp.PreUni * (1 - (LG-DOCmp.Dsctos[1] / 100)) * 
                            (1 - (LG-DOCmp.Dsctos[2] / 100)) * 
                            (1 - (LG-DOCmp.Dsctos[3] / 100)),4)
    Detalle.ImpCto = ROUND(Detalle.CanDes * Detalle.PreUni,2).
    
  FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
        AND  Almmmatg.codmat = Detalle.codmat  
        NO-LOCK NO-ERROR.
         
  FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
        AND  Almtconv.Codalter = Detalle.CodUnd 
        NO-LOCK NO-ERROR.
  Detalle.Factor = Almtconv.Equival / Almmmatg.FacEqu.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-busca B-table-Win 
PROCEDURE local-busca :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OK-WAIT-STATE AS LOGICAL NO-UNDO.
  ASSIGN  input-var-1 = ""
          input-var-2 = ""
          input-var-3 = ""
          output-var-1 = ?
          OK-WAIT-STATE = SESSION:SET-WAIT-STATE("GENERAL").

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'busca':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
    /*RUN PL/C-XXX.W("").*/
    IF OUTPUT-VAR-1 <> ? THEN DO:
         FIND {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} WHERE
              ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = OUTPUT-VAR-1
              NO-LOCK NO-ERROR.
         IF AVAIL {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
            REPOSITION {&BROWSE-NAME}  TO ROWID OUTPUT-VAR-1.
         END.
    END.
  END.
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-handle IN lh_handle('enable-botones').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
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
  RUN Procesa-handle IN lh_handle('enable-botones').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nueva-Orden B-table-Win 
PROCEDURE Nueva-Orden :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        x-CodPro = ""
        x-NomPro = ""
        x-NroRf1 = ""
        x-NroRf3 = ""
        x-Observ = ""
        Btn-OrdCmp:SENSITIVE = YES
        x-NroRf1:SENSITIVE = YES.

    txtTTrans:VISIBLE = FALSE.
    txtTTxx:SCREEN-VALUE = "".
    txtTTxx:VISIBLE = FALSE.

    DISPLAY x-CodPro x-NomPro x-NroRf1 x-NroRf3 x-Observ.
    EMPTY TEMP-TABLE Detalle.
    RUN dispatch IN THIS-PROCEDURE ('open-query':U).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros B-table-Win 
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
        WHEN "" THEN.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros B-table-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Detalle"}
  {src/adm/template/snd-list.i "INTEGRAL.Almmmatg"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida B-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND Lg-docmp WHERE Lg-docmp.codcia = s-codcia
    AND Lg-docmp.tpodoc = 'N'
    AND Lg-docmp.nrodoc = INTEGER(x-NroRf1)
    AND LG-docmp.codmat = Detalle.codmat:SCREEN-VALUE IN BROWSE {&browse-name}
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Lg-docmp THEN DO:
    MESSAGE 'Art�culo NO registrado en la Orden de Compra'
        VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY' TO Detalle.codmat IN BROWSE {&browse-name}.
    RETURN 'ADM-ERROR'.
END.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update B-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN Procesa-handle IN lh_handle('disable-botones').

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

