&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
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
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR s-nomcia AS CHAR.

DEF VAR s-coddoc AS CHAR INIT 'H/R'.
DEF VAR x-Estado AS CHAR NO-UNDO.

DEF VAR s-task-no AS INT INIT 0 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES DI-RutaC

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table DI-RutaC.NroDoc DI-RutaC.FchSal ~
DI-RutaC.CodVeh DI-RutaC.Nomtra fEstado() @ x-Estado DI-RutaC.HorSal ~
DI-RutaC.HorRet DI-RutaC.KmtIni DI-RutaC.KmtFin 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH DI-RutaC WHERE ~{&KEY-PHRASE} ~
      AND DI-RutaC.CodCia = s-codcia ~
 AND DI-RutaC.CodDiv = s-coddiv ~
 AND DI-RutaC.CodDoc = s-coddoc ~
 AND DI-RutaC.FlgEst = "C" NO-LOCK ~
    BY DI-RutaC.NroDoc DESCENDING
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH DI-RutaC WHERE ~{&KEY-PHRASE} ~
      AND DI-RutaC.CodCia = s-codcia ~
 AND DI-RutaC.CodDiv = s-coddiv ~
 AND DI-RutaC.CodDoc = s-coddoc ~
 AND DI-RutaC.FlgEst = "C" NO-LOCK ~
    BY DI-RutaC.NroDoc DESCENDING.
&Scoped-define TABLES-IN-QUERY-br_table DI-RutaC
&Scoped-define FIRST-TABLE-IN-QUERY-br_table DI-RutaC


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEstado B-table-Win 
FUNCTION fEstado RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fImporte B-table-Win 
FUNCTION fImporte RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      DI-RutaC SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      DI-RutaC.NroDoc FORMAT "X(9)":U
      DI-RutaC.FchSal COLUMN-LABEL "Fecha de !Salida" FORMAT "99/99/9999":U
      DI-RutaC.CodVeh COLUMN-LABEL "Placa" FORMAT "X(10)":U
      DI-RutaC.Nomtra FORMAT "X(30)":U
      fEstado() @ x-Estado COLUMN-LABEL "Situacion"
      DI-RutaC.HorSal COLUMN-LABEL "Hora!Salida" FORMAT "XX:XX":U
      DI-RutaC.HorRet COLUMN-LABEL "Hora!Retorno" FORMAT "XX:XX":U
      DI-RutaC.KmtIni COLUMN-LABEL "Kilometraje!Salida" FORMAT ">>>,>>9":U
      DI-RutaC.KmtFin COLUMN-LABEL "Kilometraje!Llegada" FORMAT ">>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 112 BY 6.69
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 6.85
         WIDTH              = 121.57.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmbrowser.i}
{src/adm/method/browser.i}
{src/adm-vm/method/vmviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.DI-RutaC"
     _Options          = "NO-LOCK KEY-PHRASE"
     _OrdList          = "INTEGRAL.DI-RutaC.NroDoc|no"
     _Where[1]         = "INTEGRAL.DI-RutaC.CodCia = s-codcia
 AND INTEGRAL.DI-RutaC.CodDiv = s-coddiv
 AND INTEGRAL.DI-RutaC.CodDoc = s-coddoc
 AND DI-RutaC.FlgEst = ""C"""
     _FldNameList[1]   = INTEGRAL.DI-RutaC.NroDoc
     _FldNameList[2]   > INTEGRAL.DI-RutaC.FchSal
"DI-RutaC.FchSal" "Fecha de !Salida" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.DI-RutaC.CodVeh
"DI-RutaC.CodVeh" "Placa" "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = INTEGRAL.DI-RutaC.Nomtra
     _FldNameList[5]   > "_<CALC>"
"fEstado() @ x-Estado" "Situacion" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.DI-RutaC.HorSal
"DI-RutaC.HorSal" "Hora!Salida" "XX:XX" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > INTEGRAL.DI-RutaC.HorRet
"DI-RutaC.HorRet" "Hora!Retorno" "XX:XX" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > INTEGRAL.DI-RutaC.KmtIni
"DI-RutaC.KmtIni" "Kilometraje!Salida" ">>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > INTEGRAL.DI-RutaC.KmtFin
"DI-RutaC.KmtFin" "Kilometraje!Llegada" ">>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON FIND OF Di-RutaC DO:
    IF NOT CAN-FIND(FIRST Di-RutaD OF Di-RutaC WHERE LOOKUP(Di-RutaD.FlgEst, 'N,D') > 0 NO-LOCK)
        AND NOT CAN-FIND(FIRST Di-RutaG OF Di-RutaC WHERE LOOKUP(Di-RutaG.FlgEst, 'N,D') > 0 NO-LOCK)
        THEN RETURN ERROR.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Guias-Transf B-table-Win 
PROCEDURE Carga-Guias-Transf :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-Importe AS DEC NO-UNDO.

FOR EACH Di-RutaG OF Di-RutaC NO-LOCK WHERE LOOKUP(Di-RutaG.FlgEst, 'N,D') > 0,
    FIRST Almcmov WHERE Almcmov.CodCia = Di-RutaG.CodCia
    AND Almcmov.CodAlm = Di-RutaG.CodAlm
    AND Almcmov.TipMov = Di-RutaG.Tipmov
    AND Almcmov.CodMov = Di-RutaG.Codmov
    AND Almcmov.NroSer = Di-RutaG.serref
    AND Almcmov.NroDoc = Di-RutaG.nroref NO-LOCK,
    FIRST Almacen OF Almcmov NO-LOCK:
    FIND AlmTabla WHERE AlmTabla.Tabla = 'HR'
        AND AlmTabla.Codigo = Di-RutaG.FlgEstDet
        AND almtabla.NomAnt = 'N'
        NO-LOCK NO-ERROR.
    CASE Di-RutaG.FlgEst:
        WHEN "N" THEN DO:
            FOR EACH almdmov OF almcmov NO-LOCK, FIRST Almmmatg OF almdmov NO-LOCK:
                x-Importe = (Almdmov.candes * Almdmov.factor * Almmmatg.ctolis).
                IF Almmmatg.MonVta = 2 THEN x-Importe = x-Importe * ALmmmatg.TpoCmb.
                CREATE w-report.
                ASSIGN
                    w-report.Task-No = s-task-no
                    w-report.Llave-C = s-user-id
                    w-report.Llave-I = Di-RutaC.CodCia
                    w-report.Campo-C[1] = Di-RutaC.CodDiv
                    w-report.Campo-C[2] = Di-RutaC.CodDoc
                    w-report.Campo-C[3] = Di-RutaC.NroDoc
                    w-report.Campo-C[4] = Almacen.Descripcion
                    w-report.Campo-C[5] = almcmov.CodRef
                    w-report.Campo-C[6] = almcmov.NroRef
                    w-report.Campo-C[7] = almcmov.CodAlm
                    w-report.Campo-I[1] = almdmov.NroItm
                    w-report.Campo-C[8] = almdmov.CodMat
                    w-report.Campo-C[9] = Almmmatg.desmat
                    w-report.Campo-C[10] = Almmmatg.desmar
                    w-report.Campo-C[11] = Almdmov.CodUnd
                    w-report.Campo-C[12] = (IF AVAILABLE AlmTabla THEN almtabla.Nombre ELSE '')
                    w-report.Campo-C[13] =  ""
                    w-report.Campo-F[1] = almdmov.CanDes
                    w-report.Campo-F[2] = x-Importe
                    w-report.Campo-D[1] = Di-RutaC.FchDoc.
            END.
        END.
    END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Guias-Venta B-table-Win 
PROCEDURE Carga-Guias-Venta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Di-RutaD OF Di-RutaC NO-LOCK WHERE LOOKUP(Di-RutaD.FlgEst, 'N,D') > 0:
    FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddoc = DI-RutaD.CodRef
        AND ccbcdocu.nrodoc = DI-RutaD.NroRef
        NO-LOCK.
    FIND AlmTabla WHERE AlmTabla.Tabla = 'HR'
        AND AlmTabla.Codigo = DI-RutaD.FlgEstDet
        AND almtabla.NomAnt = 'N'
        NO-LOCK NO-ERROR.
    CASE Di-RutaD.FlgEst:
        WHEN "N" THEN DO:
            FOR EACH ccbddocu OF ccbcdocu NO-LOCK, FIRST Almmmatg OF Ccbddocu NO-LOCK:
                CREATE w-report.
                ASSIGN
                    w-report.Task-No = s-task-no
                    w-report.Llave-C = s-user-id
                    w-report.Llave-I = Di-RutaC.CodCia
                    w-report.Campo-C[1] = Di-RutaC.CodDiv
                    w-report.Campo-C[2] = Di-RutaC.CodDoc
                    w-report.Campo-C[3] = Di-RutaC.NroDoc
                    w-report.Campo-C[4] = Ccbcdocu.NomCli
                    w-report.Campo-C[5] = Ccbcdocu.CodRef
                    w-report.Campo-C[6] = Ccbcdocu.NroRef
                    w-report.Campo-C[7] = Ccbcdocu.CodAlm
                    w-report.Campo-I[1] = Ccbddocu.NroItm
                    w-report.Campo-C[8] = Ccbddocu.CodMat
                    w-report.Campo-C[9] = Almmmatg.desmat
                    w-report.Campo-C[10] = Almmmatg.desmar
                    w-report.Campo-C[11] = Ccbddocu.undvta
                    w-report.Campo-C[12] = (IF AVAILABLE AlmTabla THEN almtabla.Nombre ELSE '')
                    w-report.Campo-C[13] =  Ccbcdocu.RucCli
                    w-report.Campo-F[1] = Ccbddocu.CanDes
                    w-report.Campo-F[2] = Ccbddocu.ImpLin
                    w-report.Campo-D[1] = Di-RutaC.FchDoc.
            END.
        END.
        WHEN "D" THEN DO:
            FOR EACH Di-RutaDv WHERE Di-RutaDv.CodCia = Di-RutaD.codcia
                AND Di-RutaDv.CodDiv = Di-RutaD.coddiv
                AND Di-RutaDv.CodDoc = Di-RutaD.coddoc
                AND Di-RutaDv.NroDoc = Di-RutaD.nrodoc
                AND Di-RutaDv.CodRef = Di-RutaD.codref
                AND Di-RutaDv.NroRef = Di-RutaD.nroref
                AND Di-RutaDv.CanDev > 0, 
                FIRST Almmmatg OF Di-RutaDv NO-LOCK:
                FIND Ccbddocu OF Ccbcdocu WHERE Ccbddocu.codmat = Di-RutaDv.CodMat 
                    NO-LOCK NO-ERROR.
                CREATE w-report.
                ASSIGN
                    w-report.Task-No = s-task-no
                    w-report.Llave-C = s-user-id
                    w-report.Llave-I = Di-RutaC.CodCia
                    w-report.Campo-C[1] = Di-RutaC.CodDiv
                    w-report.Campo-C[2] = Di-RutaC.CodDoc
                    w-report.Campo-C[3] = Di-RutaC.NroDoc
                    w-report.Campo-C[4] = Ccbcdocu.NomCli
                    w-report.Campo-C[5] = Ccbcdocu.CodRef
                    w-report.Campo-C[6] = Ccbcdocu.NroRef
                    w-report.Campo-C[7] = Ccbcdocu.CodAlm
                    /*w-report.Campo-I[1] = Ccbddocu.NroItm*/
                    w-report.Campo-C[8] = Di-RutaDv.CodMat
                    w-report.Campo-C[9] = Almmmatg.desmat
                    w-report.Campo-C[10] = Almmmatg.desmar
                    w-report.Campo-C[11] = Di-RutaDv.undvta
                    w-report.Campo-C[12] = (IF AVAILABLE AlmTabla THEN almtabla.Nombre ELSE '')
                    w-report.Campo-C[13] =  Ccbcdocu.RucCli
                    w-report.Campo-F[1] = Di-RutaDv.CanDev
                    w-report.Campo-F[2] = (IF AVAILABLE Ccbddocu THEN Di-RutaDv.CanDev * (Ccbddocu.ImpLin / Ccbddocu.CanDes)
                        ELSE 0)
                    w-report.Campo-D[1] = Di-RutaC.FchDoc.
            END.
        END.
    END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargo-por-Devolucion B-table-Win 
PROCEDURE Cargo-por-Devolucion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR RB-REPORT-LIBRARY AS CHAR.              /* Archivo PRL a usar */
DEF VAR RB-REPORT-NAME AS CHAR.                 /* Nombre del reporte */
DEF VAR RB-INCLUDE-RECORDS AS CHAR.             /* "O" si necesita filtro */
DEF VAR RB-FILTER AS CHAR.                      /* Filtro de impresion */
DEF VAR RB-OTHER-PARAMETERS AS CHAR INITIAL "". /* Otros parametros */

RB-REPORT-LIBRARY = SEARCH('aplic/dist/rbdist.prl').
IF Rb-REPORT-LIBRARY = ? THEN DO:
    MESSAGE 'NO se encontr� las libreria RBDIST.PRL' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
RB-REPORT-NAME = "Cargo de Devolucion".
RB-INCLUDE-RECORDS = "O".

DEF VAR k AS INT NO-UNDO.

s-task-no = 0.
rloop:
DO k = 1 TO  {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
    IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(k) THEN DO:
        IF Di-RutaC.FlgEst <> "C" THEN NEXT rloop.
/*         FIND FIRST Di-RutaD OF Di-RutaC WHERE LOOKUP(Di-RutaD.FlgEst, 'N,D') > 0 */
/*             NO-LOCK NO-ERROR.                                                    */
/*         IF NOT AVAILABLE Di-RutaD THEN NEXT rloop.                               */
        /* Cargamos el temporal */
        IF s-task-no = 0 THEN DO:
            REPEAT:
                s-task-no = RANDOM(1, 999999).
                IF NOT CAN-FIND(FIRST w-report WHERE w-report.task-no = s-task-no
                                AND w-report.llave-c = s-user-id NO-LOCK)
                    THEN DO:
                    CREATE w-report.
                    ASSIGN
                        w-report.task-no = s-task-no
                        w-report.llave-c = s-user-id
                        w-report.Campo-C[1] = "*666*".
                    LEAVE.
                END.
            END.
        END.
        RUN Carga-Guias-Venta.
        RUN Carga-Guias-Transf.
    END.
END.
FIND FIRST w-report WHERE w-report.task-no = s-task-no
    AND w-report.llave-c = s-user-id
    AND w-report.Campo-C[1] = "*666*" NO-ERROR.
IF AVAILABLE w-report THEN DELETE w-report.

RB-FILTER = "w-report.task-no = " + STRING(s-task-no) + 
    " AND w-report.Llave-C = '" + s-user-id + "'".
RB-OTHER-PARAMETERS = "s-nomcia=" + s-nomcia.
RUN lib/_Imprime2 (
                 RB-REPORT-LIBRARY,
                 RB-REPORT-NAME,
                 RB-INCLUDE-RECORDS,
                 RB-FILTER,
                 RB-OTHER-PARAMETERS
                 ).
FOR EACH w-report WHERE w-report.task-no = s-task-no AND w-report.Llave-C = s-user-id:
    DELETE w-report.
END.

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
  {src/adm/template/snd-list.i "DI-RutaC"}

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
  Notes:  EN CASO DE ERROR RETORNAR : RETURN "ADM-ERROR"
------------------------------------------------------------------------------*/
RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update B-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Consistenciar la modificacion de la fila
  Parameters:  Retornar "ADM-ERROR" en caso de bloquear la modificacion
  Notes:       
------------------------------------------------------------------------------*/
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEstado B-table-Win 
FUNCTION fEstado RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RUN alm/f-flgrut ("C", Di-RutaC.flgest, OUTPUT x-Estado).
  RETURN x-Estado.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fImporte B-table-Win 
FUNCTION fImporte RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pImporte AS DEC NO-UNDO.
  FOR EACH Almdmov OF Almcmov NO-LOCK, FIRST Almmmatg OF Almdmov NO-LOCK:
      pImporte = pImporte + (Almdmov.candes * Almdmov.factor * Almmmatg.ctolis).
  END.

  RETURN pImporte.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
