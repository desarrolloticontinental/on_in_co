&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE SHARED TEMP-TABLE T-DDocu NO-UNDO LIKE VtaDDocu.



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

DEFINE SHARED VARIABLE s-codcia AS INT.
DEFINE SHARED VARIABLE s-coddiv AS CHAR.
DEFINE SHARED VARIABLE S-CODCLI AS CHAR.
DEFINE SHARED VARIABLE S-CODMON AS INTEGER.
DEFINE SHARED VARIABLE S-CNDVTA   AS CHAR.
DEFINE SHARED VARIABLE S-FLGIGV AS LOGICAL.
DEFINE SHARED VARIABLE S-PORIGV AS DECIMAL.
DEFINE SHARED VARIABLE s-adm-new-record AS CHAR.
DEFINE SHARED VARIABLE lh_handle AS HANDLE.
DEFINE SHARED VARIABLE s-codped AS CHAR.
DEFINE SHARED VARIABLE s-nroped AS CHAR.
DEFINE SHARED VARIABLE s-codref AS CHAR.
DEFINE SHARED VARIABLE s-nroref AS CHAR.

DEF BUFFER BT-DDOCU FOR T-DDOCU.
DEFINE VARIABLE f-Factor AS DEC INIT 1 NO-UNDO.

DEFINE VARIABLE F-PREVTA AS DEC DECIMALS 6 NO-UNDO.
DEFINE VARIABLE F-PREBAS AS DEC DECIMALS 6 NO-UNDO.
DEFINE VARIABLE F-DSCTOS AS DEC DECIMALS 4 NO-UNDO.
DEFINE VARIABLE F-DSCTOS-1 AS DEC DECIMALS 4 NO-UNDO.
DEFINE VARIABLE F-PorImp AS DEC DECIMALS 4 NO-UNDO.

DEFINE VARIABLE s-local-new-record AS CHAR.

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
&Scoped-define INTERNAL-TABLES T-DDocu Almmmatg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table T-DDocu.NroItm T-DDocu.CodMat ~
Almmmatg.DesMat T-DDocu.AlmDes T-DDocu.UndVta T-DDocu.PreBas T-DDocu.PorDto ~
T-DDocu.CanPed T-DDocu.PreUni T-DDocu.PorDto1 T-DDocu.PorDto2 ~
T-DDocu.PorDto3 T-DDocu.ImpLin 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table T-DDocu.CodMat ~
T-DDocu.AlmDes T-DDocu.CanPed 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table T-DDocu
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table T-DDocu
&Scoped-define QUERY-STRING-br_table FOR EACH T-DDocu WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH Almmmatg OF T-DDocu NO-LOCK ~
    BY T-DDocu.NroItm
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH T-DDocu WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH Almmmatg OF T-DDocu NO-LOCK ~
    BY T-DDocu.NroItm.
&Scoped-define TABLES-IN-QUERY-br_table T-DDocu Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table T-DDocu
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_ImpBrt FILL-IN_ImpExo ~
FILL-IN_ImpIgv FILL-IN_ImpDto FILL-IN_ImpVta FILL-IN_ImpTot 

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


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN_ImpBrt AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Importe Bruto" 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ImpDto AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Importe Descuento" 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY 1.

DEFINE VARIABLE FILL-IN_ImpExo AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Importe Exonerado" 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY 1.

DEFINE VARIABLE FILL-IN_ImpIgv AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Importe I.G.V." 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY 1.

DEFINE VARIABLE FILL-IN_ImpTot AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Importe Total" 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY 1.

DEFINE VARIABLE FILL-IN_ImpVta AS DECIMAL FORMAT "->>>,>>>,>>9.99" INITIAL 0 
     LABEL "Valor Venta" 
     VIEW-AS FILL-IN 
     SIZE 18.57 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      T-DDocu, 
      Almmmatg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      T-DDocu.NroItm COLUMN-LABEL "Item" FORMAT ">>9":U
      T-DDocu.CodMat COLUMN-LABEL "Producto" FORMAT "X(13)":U WIDTH 8.29
      Almmmatg.DesMat FORMAT "X(60)":U
      T-DDocu.AlmDes COLUMN-LABEL "Almac�n" FORMAT "x(3)":U
      T-DDocu.UndVta FORMAT "x(5)":U
      T-DDocu.PreBas FORMAT ">,>>>,>>9.999999":U
      T-DDocu.PorDto COLUMN-LABEL "% Dscto!Base" FORMAT ">>9.9999":U
      T-DDocu.CanPed FORMAT "->>>,>>9.9999":U
      T-DDocu.PreUni FORMAT ">>>,>>9.999999":U
      T-DDocu.PorDto1 FORMAT ">>9.9999":U
      T-DDocu.PorDto2 COLUMN-LABEL "%Dto2" FORMAT ">>9.9999":U
      T-DDocu.PorDto3 COLUMN-LABEL "%Dto3" FORMAT ">>9.9999":U
      T-DDocu.ImpLin FORMAT ">>,>>>,>>9.99":U
  ENABLE
      T-DDocu.CodMat HELP "F8 solicita ayuda"
      T-DDocu.AlmDes HELP "F8 solicita ayuda"
      T-DDocu.CanPed HELP "F8 solicita ayuda"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 9.42
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     FILL-IN_ImpBrt AT ROW 10.69 COL 19 COLON-ALIGNED WIDGET-ID 2
     FILL-IN_ImpExo AT ROW 10.69 COL 59 COLON-ALIGNED WIDGET-ID 8
     FILL-IN_ImpIgv AT ROW 10.69 COL 99 COLON-ALIGNED WIDGET-ID 10
     FILL-IN_ImpDto AT ROW 11.77 COL 19 COLON-ALIGNED WIDGET-ID 6
     FILL-IN_ImpVta AT ROW 11.77 COL 59 COLON-ALIGNED WIDGET-ID 14
     FILL-IN_ImpTot AT ROW 11.77 COL 99 COLON-ALIGNED WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: T-DDocu T "SHARED" NO-UNDO INTEGRAL VtaDDocu
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
         HEIGHT             = 12.08
         WIDTH              = 159.57.
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
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ImpBrt IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ImpDto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ImpExo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ImpIgv IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ImpTot IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_ImpVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.T-DDocu,INTEGRAL.Almmmatg OF Temp-Tables.T-DDocu"
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "Temp-Tables.T-DDocu.NroItm|yes"
     _FldNameList[1]   > Temp-Tables.T-DDocu.NroItm
"Temp-Tables.T-DDocu.NroItm" "Item" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.T-DDocu.CodMat
"Temp-Tables.T-DDocu.CodMat" "Producto" "X(13)" "character" ? ? ? ? ? ? yes "F8 solicita ayuda" no no "8.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.Almmmatg.DesMat
"INTEGRAL.Almmmatg.DesMat" ? "X(60)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.T-DDocu.AlmDes
"Temp-Tables.T-DDocu.AlmDes" "Almac�n" ? "character" ? ? ? ? ? ? yes "F8 solicita ayuda" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.T-DDocu.UndVta
     _FldNameList[6]   = Temp-Tables.T-DDocu.PreBas
     _FldNameList[7]   > Temp-Tables.T-DDocu.PorDto
"Temp-Tables.T-DDocu.PorDto" "% Dscto!Base" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.T-DDocu.CanPed
"Temp-Tables.T-DDocu.CanPed" ? ? "decimal" ? ? ? ? ? ? yes "F8 solicita ayuda" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.T-DDocu.PreUni
"Temp-Tables.T-DDocu.PreUni" ? ">>>,>>9.999999" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = Temp-Tables.T-DDocu.PorDto1
     _FldNameList[11]   > Temp-Tables.T-DDocu.PorDto2
"Temp-Tables.T-DDocu.PorDto2" "%Dto2" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.T-DDocu.PorDto3
"Temp-Tables.T-DDocu.PorDto3" "%Dto3" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.T-DDocu.ImpLin
"Temp-Tables.T-DDocu.ImpLin" ? ">>,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



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


&Scoped-define SELF-NAME T-DDocu.CodMat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-DDocu.CodMat br_table _BROWSE-COLUMN B-table-Win
ON F8 OF T-DDocu.CodMat IN BROWSE br_table /* Producto */
OR left-mouse-dblclick OF T-DDOCU.CodMat
DO:
  ASSIGN
      input-var-1 = s-codref
      input-var-2 = s-nroref
      input-var-3 = ''
      output-var-1 = ?.
  RUN vtagn/c-vtaddocu-01 ("Productos por despachar").
  IF output-var-1 <> ? THEN DO:
      DISPLAY
          output-var-2 @ T-DDOCU.CodMat 
          output-var-3 @ Almmmatg.DesMat
          WITH BROWSE {&browse-name}.
      APPLY 'ENTRY':U to {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-DDocu.CodMat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-DDocu.CodMat IN BROWSE br_table /* Producto */
DO:
    IF SELF:SCREEN-VALUE = "" THEN RETURN.
    IF s-local-new-record = "NO" THEN RETURN.

    DEF VAR pCodMat AS CHAR NO-UNDO.
    pCodMat = SELF:SCREEN-VALUE.
    RUN vtagn/p-codbrr-01 (INPUT-OUTPUT pCodMat).
    ASSIGN
        SELF:SCREEN-VALUE = pCodMat.
    IF pCodMat = "" THEN RETURN NO-APPLY.
    /* Valida Maestro Productos */
    FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
        AND Almmmatg.codmat = SELF:SCREEN-VALUE 
        USE-INDEX matg01 NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE "Codigo de Articulo no Existe" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF Almmmatg.TpoArt <> "A" THEN DO:
        MESSAGE "Articulo no Activo" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF Almmmatg.Chr__01 = "" THEN DO:
        MESSAGE "Articulo no tiene unidad de Oficina" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.

    /* VERIFICAMOS SI TOMAMOS EL PRECIO DE LA COTIZACION O DE LA LISTA DE PRECIOS */
    FIND Vtaddocu WHERE Vtaddocu.codcia = s-codcia
        AND Vtaddocu.codped = s-codref
        AND Vtaddocu.nroped = s-nroref
        AND Vtaddocu.codmat = SELF:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Vtaddocu THEN DO:
        MESSAGE "Producto no registrado en la cotizaci�n"
            VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    ASSIGN
        f-Factor = Vtaddocu.factor.
    FIND gn-divi WHERE gn-divi.codcia = s-codcia
        AND gn-divi.coddiv = s-coddiv NO-LOCK.
    IF GN-DIVI.FlgPreVta = NO THEN DO:          /* SE ACTUALIZA EL PRECIO UNITARIO */
        RUN vtagn/PrecioVenta (s-codcia,
                               s-coddiv,
                               s-codcli,
                               s-codmon,
                               f-factor,
                               Almmmatg.codmat,
                               s-cndvta,
                               6,
                               OUTPUT f-PreBas,
                               OUTPUT f-PreVta,
                               OUTPUT f-Dsctos).
                               /*OUTPUT f-Dsctos-1).*/
    END.
    ELSE ASSIGN
            f-PreBas = Vtaddocu.PreBas
            f-PreVta = Vtaddocu.PreUni
            f-Dsctos = Vtaddocu.PorDto.
            /*f-Dsctos-1 = Vtaddocu.PorDto1.*/

    DISPLAY 
        Almmmatg.DesMat @ Almmmatg.DesMat 
        Vtaddocu.UndVta @ T-DDOCU.UndVta 
        F-PREBAS @ T-DDOCU.PreBas
        F-DSCTOS @ T-DDOCU.PorDto
        /*F-DSCTOS-1 @ T-DDOCU.PorDto1*/
        F-PREVTA @ T-DDOCU.PreUni 
        Vtaddocu.PorDto1 @ T-DDocu.PorDto1
        Vtaddocu.PorDto2 @ T-DDocu.PorDto2 
        Vtaddocu.PorDto3 @ T-DDocu.PorDto3
        WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-DDocu.AlmDes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-DDocu.AlmDes br_table _BROWSE-COLUMN B-table-Win
ON F8 OF T-DDocu.AlmDes IN BROWSE br_table /* Almac�n */
OR left-mouse-dblclick OF T-DDOCU.AlmDes
DO:
  ASSIGN
      input-var-1 = T-DDocu.CodMat:SCREEN-VALUE IN BROWSE {&browse-name}.
  RUN vtagn/d-almmmate-02.
  IF output-var-1 <> ? THEN DO:
      SELF:SCREEN-VALUE = output-var-2.
      APPLY 'ENTRY':U TO {&self-name}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-DDocu.AlmDes br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-DDocu.AlmDes IN BROWSE br_table /* Almac�n */
DO:
  IF SELF:SCREEN-VALUE = "" THEN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-DDocu.CanPed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-DDocu.CanPed br_table _BROWSE-COLUMN B-table-Win
ON F8 OF T-DDocu.CanPed IN BROWSE br_table /* Cantidad */
OR left-mouse-dblclick OF T-DDOCU.CanPed
DO:
    ASSIGN
        input-var-1 = T-DDocu.CodMat:SCREEN-VALUE IN BROWSE {&browse-name}.
    RUN vtagn/d-almmmate-02.
    IF output-var-1 <> ? THEN DO:
        SELF:SCREEN-VALUE = output-var-3.
        APPLY 'ENTRY':U TO {&self-name}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-DDocu.CanPed br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-DDocu.CanPed IN BROWSE br_table /* Cantidad */
DO:
    IF DEC(SELF:SCREEN-VALUE) = 0 THEN RETURN.
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = T-DDOCU.codmat:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN RETURN.
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
        AND Almtconv.Codalter = Almmmatg.Chr__01 
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
        MESSAGE "Codigo de unidad no existe" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.
    IF Almtconv.Multiplos <> 0 THEN DO:
        IF DEC(T-DDOCU.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / Almtconv.Multiplos <> 
            INT(DEC(T-DDOCU.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / Almtconv.Multiplos) THEN DO:
            MESSAGE " La Cantidad debe de ser un, " SKIP
                " multiplo de : " Almtconv.Multiplos
                VIEW-AS ALERT-BOX WARNING.
            RETURN NO-APPLY.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON 'RETURN':U OF  T-DDocu.CanPed, T-DDOCU.CodMat, T-DDOCU.AlmDes
DO:
    APPLY 'tab'.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calculo-Precios-Supermercados B-table-Win 
PROCEDURE Calculo-Precios-Supermercados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   F-PorImp = 1.

   /* RHC 12.06.08 al tipo de cambio de la familia */
   IF S-CODMON = 1 THEN DO:
      IF Almmmatg.MonVta = 1 THEN
          ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * F-FACTOR.
      ELSE
          ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * Almmmatg.TpoCmb * F-FACTOR.
   END.
   IF S-CODMON = 2 THEN DO:
      IF Almmmatg.MonVta = 2 THEN
         ASSIGN F-PREBAS = (Almmmatg.PreOfi * F-PorImp) * F-FACTOR.
      ELSE
         ASSIGN F-PREBAS = ((Almmmatg.PreOfi * F-PorImp) / Almmmatg.TpoCmb) * F-FACTOR.
   END.
   f-PreVta = f-PreBas.

   FIND FacTabla WHERE FacTabla.CodCia = s-codcia
       AND FacTabla.Tabla = 'AU' 
       AND FacTabla.Codigo = s-codcli
       NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Factabla THEN RETURN.

   IF Factabla.Valor[1] <> 0 
   THEN f-PreVta = f-PreBas * ( 1 - Factabla.Valor[1] / 100 ).
   ELSE f-PreVta = f-PreBas * ( 1 + Factabla.Valor[2] / 100 ).

    RUN src/BIN/_ROUND1(F-PREVTA, 2, OUTPUT F-PREVTA).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF s-codcli = '' THEN DO:
      MESSAGE 'Ingrese el cliente' VIEW-AS ALERT-BOX WARNING.
      RETURN "ADM-ERROR".
  END.
  IF s-cndvta = '' THEN DO:
      MESSAGE 'Ingrese la condici�n de venta' VIEW-AS ALERT-BOX WARNING.
      RETURN "ADM-ERROR".
  END.
  s-local-new-record = "YES".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-Handle IN lh_handle ('Disable').

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
  DEF VAR x-NroItm AS INT INIT 1 NO-UNDO.
  RUN GET-ATTRIBUTE("ADM-NEW-RECORD").
  IF RETURN-VALUE = "NO" THEN x-NroItm = T-DDOCU.NroItm.
  ELSE DO:
      FOR EACH BT-DDOCU BY BT-DDOCU.NroItm:
          x-NroItm = BT-DDOCU.NroItm + 1.
      END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      T-DDOCU.nroitm = x-NroItm
      T-DDOCU.codcia = s-codcia
      T-DDOCU.undvta = T-DDOCU.undvta:SCREEN-VALUE IN BROWSE {&browse-name}
      T-DDOCU.factor = f-Factor
      T-DDocu.PreUni = DECIMAL (T-DDocu.PreUni:SCREEN-VALUE IN BROWSE {&browse-name})
      T-DDocu.PreBas = DECIMAL (T-DDocu.PreBas:SCREEN-VALUE IN BROWSE {&browse-name})
      T-DDocu.PorDto = DECIMAL (T-DDocu.PorDto:SCREEN-VALUE IN BROWSE {&browse-name})
      T-DDocu.PorDto1 = DECIMAL (T-DDocu.PorDto1:SCREEN-VALUE IN BROWSE {&browse-name})
      T-DDocu.PorDto2 = DECIMAL (T-DDocu.PorDto2:SCREEN-VALUE IN BROWSE {&browse-name})
      T-DDocu.PorDto3 = DECIMAL (T-DDocu.PorDto3:SCREEN-VALUE IN BROWSE {&browse-name}).

  /* CALCULO GENERAL DE LOS IMPORTES POR LINEA */
  {vtagn/i-vtaddocu-01.i}

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
  s-local-new-record = "NO".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Procesa-Handle  IN lh_handle ('Enable').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Totales.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF s-local-new-record = "YES" THEN DO:
      T-DDocu.AlmDes:READ-ONLY IN BROWSE {&browse-name} = NO.
      T-DDocu.CodMat:READ-ONLY IN BROWSE {&browse-name} = NO.
  END.
  ELSE DO:
      T-DDocu.AlmDes:READ-ONLY IN BROWSE {&browse-name} = YES.
      T-DDocu.CodMat:READ-ONLY IN BROWSE {&browse-name} = YES.
      APPLY 'entry':u TO T-DDocu.CanPed IN BROWSE {&browse-name}.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN Totales.

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
  RUN dispatch IN THIS-PROCEDURE ('display-fields').
  RUN Totales.
  RUN Procesa-Handle  IN lh_handle ('Enable').
  s-local-new-record = "NO".

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
  {src/adm/template/snd-list.i "T-DDocu"}
  {src/adm/template/snd-list.i "Almmmatg"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totales B-table-Win 
PROCEDURE Totales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN
    FILL-IN_ImpBrt = 0
    FILL-IN_ImpDto = 0
    FILL-IN_ImpExo = 0
    FILL-IN_ImpIgv = 0
    FILL-IN_ImpTot = 0
    FILL-IN_ImpVta = 0.
FOR EACH BT-DDOCU:
    ASSIGN
        FILL-IN_ImpBrt = FILL-IN_ImpBrt + BT-DDOCU.ImpBrt
        FILL-IN_ImpDto = FILL-IN_ImpDto + BT-DDOCU.ImpDto
        FILL-IN_ImpExo = FILL-IN_ImpExo + BT-DDOCU.ImpExo
        FILL-IN_ImpIgv = FILL-IN_ImpIgv + BT-DDOCU.ImpIgv
        FILL-IN_ImpTot = FILL-IN_ImpTot + BT-DDOCU.ImpLin
        FILL-IN_ImpVta = FILL-IN_ImpVta + BT-DDOCU.ImpVta.

END.
DISPLAY
    FILL-IN_ImpBrt FILL-IN_ImpDto FILL-IN_ImpExo FILL-IN_ImpIgv FILL-IN_ImpTot FILL-IN_ImpVta
    WITH FRAME {&FRAME-NAME}.

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

/* Productos repetidos */
IF s-local-new-record = "YES" THEN DO:
    IF CAN-FIND(FIRST BT-DDOCU WHERE BT-DDOCU.codmat = T-DDocu.CodMat:SCREEN-VALUE IN BROWSE {&browse-name}
                AND BT-DDOCU.almdes = T-DDOCU.AlmDes:SCREEN-VALUE IN BROWSE {&browse-name})
        THEN DO:
        MESSAGE 'Producto duplicado' VIEW-AS ALERT-BOX ERROR.
        RETURN "ADM-ERROR".
    END.
END.

/* Almacen */
IF T-DDOCU.AlmDes:SCREEN-VALUE IN BROWSE {&browse-name} = "" THEN DO:
    MESSAGE "Almac�n no registrado" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY":U TO T-DDocu.AlmDes IN BROWSE {&browse-name}.
    RETURN "ADM-ERROR".
END.
FIND VtaAlmDiv WHERE VtaAlmDiv.CodCia = s-codcia
    AND VtaAlmDiv.CodDiv = s-coddiv
    AND VtaAlmDiv.CodAlm = T-DDOCU.AlmDes:SCREEN-VALUE IN BROWSE {&browse-name}
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaAlmDiv THEN DO:
    MESSAGE "Almac�n no v�lido para descargar" VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY":U TO T-DDocu.AlmDes IN BROWSE {&browse-name}.
    RETURN "ADM-ERROR".
END.

/* CONSISTENCIA DE CANTIDADES */
DEF VAR f-CanPed AS DEC NO-UNDO.
DEF VAR f-CanAte AS DEC NO-UNDO.

/* Cantidad despachada */
f-CanAte = 0.
f-CanPed = 0.
FOR EACH BT-DDOCU WHERE BT-DDOCU.codmat = T-DDocu.CodMat:SCREEN-VALUE IN BROWSE {&browse-name}:
    IF s-local-new-record = "NO" AND ROWID(BT-DDOCU) = ROWID(T-DDOCU) THEN NEXT.
    f-CanPed = f-CanPed + BT-DDOCU.canped.
END.
f-CanPed = f-CanPed + DECIMAL(T-DDOCU.CanPed:SCREEN-VALUE IN BROWSE {&Browse-name}).

/* �es una modificacion? */
IF s-adm-new-record = "NO" THEN DO:
    FOR EACH Vtaddocu NO-LOCK WHERE Vtaddocu.codcia = s-codcia
        AND Vtaddocu.codped = s-codped
        AND Vtaddocu.nroped = s-nroped
        AND Vtaddocu.codmat = T-DDOCU.codmat:SCREEN-VALUE IN BROWSE {&Browse-name}:
        f-CanAte = f-CanAte + Vtaddocu.canped.
    END.
END.
/* ********************* */
/* Item de la Cotizaci�n */
FIND Vtaddocu WHERE Vtaddocu.codcia = s-codcia
    AND Vtaddocu.codped = s-codref
    AND Vtaddocu.nroped = s-nroref
    AND Vtaddocu.codmat = T-DDOCU.codmat:SCREEN-VALUE IN BROWSE {&Browse-name}
    NO-LOCK.
IF f-CanPed > (Vtaddocu.canped - Vtaddocu.CanAte + f-CanAte) THEN DO:
      MESSAGE 'NO se puede atender mas de' (Vtaddocu.canped - Vtaddocu.CanAte + f-CanAte) Vtaddocu.undvta
          VIEW-AS ALERT-BOX ERROR.
      /*DISPLAY (Vtaddocu.canped - Vtaddocu.CanAte + f-CanAte) @ T-DDocu.CanPed WITH BROWSE {&browse-name}.*/
      APPLY "ENTRY":U TO T-DDocu.CanPed IN BROWSE {&browse-name}.
      RETURN "ADM-ERROR".
END.
/* ******************** CONSISTENCIA STOCK DISPONIBLE ******************** */
DEF VAR s-StkComprometido AS DEC NO-UNDO.
DEF VAR s-StkDis AS DEC NO-UNDO.
DEF VAR x-CanPed AS DEC NO-UNDO.

FIND Almmmate WHERE Almmmate.codcia = s-codcia
    AND Almmmate.codalm = T-DDOCU.AlmDes:SCREEN-VALUE IN BROWSE {&Browse-name}
    AND Almmmate.codmat = T-DDOCU.CodMat:SCREEN-VALUE IN BROWSE {&browse-name}
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmate THEN DO:
    MESSAGE 'Producto no asignado al almac�n' T-DDOCU.AlmDes:SCREEN-VALUE IN BROWSE {&Browse-name}
        VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY":U TO T-DDocu.AlmDes IN BROWSE {&browse-name}.
    RETURN "ADM-ERROR".
END.
RUN vtagn/p-Stock-Comprometido (T-DDOCU.CodMat:SCREEN-VALUE IN BROWSE {&browse-name},
                           T-DDOCU.AlmDes:SCREEN-VALUE IN BROWSE {&Browse-name},
                           OUTPUT s-StkComprometido).
/* buscamos si ya est� grabado */
IF s-adm-new-record = 'NO' THEN DO:
    FOR EACH Vtaddocu NO-LOCK WHERE Vtaddocu.codcia = s-codcia
        AND Vtaddocu.codped = s-codped
        AND Vtaddocu.nroped = s-nroped
        AND Vtaddocu.codmat = T-DDOCU.codmat:SCREEN-VALUE IN BROWSE {&Browse-name}:
        s-StkComprometido = s-StkComprometido - (Vtaddocu.CanPed * Vtaddocu.Factor).
    END.
END.
s-StkDis = Almmmate.StkAct - s-StkComprometido.
x-CanPed = DECIMAL (T-DDOCU.CanPed:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) * f-Factor.
IF s-StkDis < x-CanPed THEN DO:
    MESSAGE "No hay STOCK suficiente" SKIP(1)
        "       STOCK ACTUAL : " Almmmate.StkAct Almmmatg.undbas SKIP
        "  STOCK COMPROMETIDO: " s-StkComprometido Almmmatg.undbas SKIP
        "   STOCK DISPONIBLE : " S-STKDIS Almmmatg.undbas SKIP
        VIEW-AS ALERT-BOX ERROR.
    APPLY 'entry':u TO T-DDOCU.CanPed.
    RETURN "ADM-ERROR".
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
s-local-new-record = "NO".
f-Factor = T-DDOCU.Factor.
RUN Procesa-Handle  IN lh_handle ('Disable').
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

