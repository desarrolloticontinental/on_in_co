&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-CDocu FOR CcbCDocu.
DEFINE BUFFER B-PHRC FOR DI-RutaC.
DEFINE BUFFER B-PHRD FOR DI-RutaD.
DEFINE TEMP-TABLE T-RutaC NO-UNDO LIKE DI-RutaC.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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
DEF INPUT PARAMETER pHojasdeRuta AS CHAR.

/* Local Variable Definitions ---                                       */
DEF VAR hora1 AS CHAR.
DEF VAR hora2 AS CHAR.

DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR s-codcia AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME BROWSE-9

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES T-RutaC gn-vehic DI-RutaC

/* Definitions for BROWSE BROWSE-9                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-9 T-RutaC.NroDoc T-RutaC.FchDoc ~
T-RutaC.HorSal T-RutaC.KmtIni T-RutaC.CodVeh gn-vehic.Marca T-RutaC.CtoRut ~
T-RutaC.DesRut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-9 
&Scoped-define QUERY-STRING-BROWSE-9 FOR EACH T-RutaC NO-LOCK, ~
      EACH gn-vehic WHERE gn-vehic.CodCia = T-RutaC.CodCia ~
  AND gn-vehic.placa = T-RutaC.CodVeh NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-9 OPEN QUERY BROWSE-9 FOR EACH T-RutaC NO-LOCK, ~
      EACH gn-vehic WHERE gn-vehic.CodCia = T-RutaC.CodCia ~
  AND gn-vehic.placa = T-RutaC.CodVeh NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-9 T-RutaC gn-vehic
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-9 T-RutaC
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-9 gn-vehic


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-BROWSE-9}
&Scoped-define QUERY-STRING-D-Dialog FOR EACH DI-RutaC SHARE-LOCK
&Scoped-define OPEN-QUERY-D-Dialog OPEN QUERY D-Dialog FOR EACH DI-RutaC SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-D-Dialog DI-RutaC
&Scoped-define FIRST-TABLE-IN-QUERY-D-Dialog DI-RutaC


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-9 txtHora TxtMinuto FILL-IN_KmtFin ~
FILL-IN_CtoRut txtSerie txtNro Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS txtHora TxtMinuto FILL-IN_KmtFin ~
FILL-IN_CtoRut txtSerie txtNro 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "img/b-cancel.bmp":U
     LABEL "Cancelar" 
     SIZE 12 BY 1.54
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "img/b-ok.bmp":U
     LABEL "Aceptar" 
     SIZE 12 BY 1.54
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_CtoRut AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     LABEL "Costo S/." 
     VIEW-AS FILL-IN 
     SIZE 8.57 BY .81.

DEFINE VARIABLE FILL-IN_KmtFin AS DECIMAL FORMAT ">>>,>>9" INITIAL 0 
     LABEL "Kilometraje de llegada" 
     VIEW-AS FILL-IN 
     SIZE 8.57 BY .81.

DEFINE VARIABLE txtHora AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "HH" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .81.

DEFINE VARIABLE TxtMinuto AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "MM" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .81.

DEFINE VARIABLE txtNro AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Nro" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .81.

DEFINE VARIABLE txtSerie AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Serie" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-9 FOR 
      T-RutaC, 
      gn-vehic SCROLLING.

DEFINE QUERY D-Dialog FOR 
      DI-RutaC SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-9 D-Dialog _STRUCTURED
  QUERY BROWSE-9 NO-LOCK DISPLAY
      T-RutaC.NroDoc COLUMN-LABEL "Hoja de Ruta" FORMAT "X(9)":U
      T-RutaC.FchDoc FORMAT "99/99/9999":U
      T-RutaC.HorSal COLUMN-LABEL "Hora de!Salida" FORMAT "XX:XX":U
            WIDTH 6.14
      T-RutaC.KmtIni COLUMN-LABEL "Kilometraje!de Salida" FORMAT ">>>,>>9":U
      T-RutaC.CodVeh COLUMN-LABEL "Placa del!Veh�culo" FORMAT "X(10)":U
      gn-vehic.Marca FORMAT "x(20)":U
      T-RutaC.CtoRut COLUMN-LABEL "Costo S/." FORMAT ">>>,>>9.99":U
      T-RutaC.DesRut COLUMN-LABEL "Detalle de la Ruta" FORMAT "X(50)":U
            WIDTH 22.14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 98 BY 8.27
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     BROWSE-9 AT ROW 1.19 COL 2 WIDGET-ID 100
     txtHora AT ROW 9.54 COL 21.72 COLON-ALIGNED WIDGET-ID 10
     TxtMinuto AT ROW 9.54 COL 28.72 COLON-ALIGNED WIDGET-ID 12
     FILL-IN_KmtFin AT ROW 10.42 COL 18 COLON-ALIGNED
     FILL-IN_CtoRut AT ROW 11.19 COL 18 COLON-ALIGNED WIDGET-ID 6
     txtSerie AT ROW 12.15 COL 22.57 COLON-ALIGNED WIDGET-ID 16
     txtNro AT ROW 12.15 COL 31 COLON-ALIGNED WIDGET-ID 18
     Btn_OK AT ROW 9.85 COL 73
     Btn_Cancel AT ROW 9.85 COL 86
     "(Formato 24 Horas)" VIEW-AS TEXT
          SIZE 14.14 BY .5 AT ROW 9.62 COL 35.86 WIDGET-ID 22
     "Guia del Transportista :" VIEW-AS TEXT
          SIZE 16 BY .5 AT ROW 12.35 COL 4 WIDGET-ID 20
     "SOLO EN CASO DE TAXI" VIEW-AS TEXT
          SIZE 19 BY .5 AT ROW 11.38 COL 29 WIDGET-ID 8
     "Llegada :" VIEW-AS TEXT
          SIZE 7 BY .5 AT ROW 9.69 COL 13.29 WIDGET-ID 14
     SPACE(80.99) SKIP(3.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "CIERRE DE HOJA DE RUTA"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: B-CDocu B "?" ? INTEGRAL CcbCDocu
      TABLE: B-PHRC B "?" ? INTEGRAL DI-RutaC
      TABLE: B-PHRD B "?" ? INTEGRAL DI-RutaD
      TABLE: T-RutaC T "?" NO-UNDO INTEGRAL DI-RutaC
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{src/adm-vm/method/vmviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-9 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-9
/* Query rebuild information for BROWSE BROWSE-9
     _TblList          = "Temp-Tables.T-RutaC,INTEGRAL.gn-vehic WHERE Temp-Tables.T-RutaC ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _JoinCode[2]      = "INTEGRAL.gn-vehic.CodCia = Temp-Tables.T-RutaC.CodCia
  AND INTEGRAL.gn-vehic.placa = Temp-Tables.T-RutaC.CodVeh"
     _FldNameList[1]   > Temp-Tables.T-RutaC.NroDoc
"T-RutaC.NroDoc" "Hoja de Ruta" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.T-RutaC.FchDoc
     _FldNameList[3]   > Temp-Tables.T-RutaC.HorSal
"T-RutaC.HorSal" "Hora de!Salida" "XX:XX" "character" ? ? ? ? ? ? no ? no no "6.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.T-RutaC.KmtIni
"T-RutaC.KmtIni" "Kilometraje!de Salida" ">>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.T-RutaC.CodVeh
"T-RutaC.CodVeh" "Placa del!Veh�culo" "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = INTEGRAL.gn-vehic.Marca
     _FldNameList[7]   > Temp-Tables.T-RutaC.CtoRut
"T-RutaC.CtoRut" "Costo S/." ">>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.T-RutaC.DesRut
"T-RutaC.DesRut" "Detalle de la Ruta" ? "character" ? ? ? ? ? ? no ? no no "22.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-9 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _TblList          = "INTEGRAL.DI-RutaC"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* CIERRE DE HOJA DE RUTA */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* Aceptar */
DO:
  RUN Valida.
  IF RETURN-VALUE = 'ADM-ERROR'
  THEN RETURN NO-APPLY.
  MESSAGE 'Se va a proceder al cierre de la Hoja de Ruta' SKIP
    'Continuamos con el proceso?' VIEW-AS ALERT-BOX QUESTION
    BUTTONS YES-NO UPDATE rpta-1 AS LOG.
  IF rpta-1 = NO THEN RETURN NO-APPLY.

  /**/
  DEFINE VAR lHRuta AS CHAR.
  DEFINE VAR lOrdenes AS CHAR.
  DEFINE VAR lNroDoc AS CHAR.
  DEFINE VAR lCodDoc AS CHAR.
  DEFINE VAR lRowId AS ROWID.

  ASSIGN
      /*
      FILL-IN_CodRut FILL-IN_CtoRut FILL-IN_HorRet FILL-IN_KmtFin.
      */
      FILL-IN_CtoRut FILL-IN_KmtFin txtHora txtMinuto txtSerie txtNro.

  PRINCIPAL:
  FOR EACH T-RutaC TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      {lib/lock-genericov3.i &Tabla="Di-RutaC" ~
          &Alcance="FIRST" ~
          &Condicion="Di-RutaC.codcia = T-RutaC.codcia ~
          AND Di-RutaC.coddiv = T-RutaC.coddiv ~
          AND Di-RutaC.coddoc = T-RutaC.coddoc ~
          AND Di-RutaC.nrodoc = T-RutaC.nrodoc" ~
          &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
          &Accion="RETRY" ~
          &Mensaje="YES" ~
          &TipoError="UNDO, RETURN 'ADM-ERROR'" ~
          }
      IF DI-RutaC.FlgEst = 'C' THEN NEXT.   /* Ya fue cerrado por otro usuario */
      ASSIGN
          DI-RutaC.HorRet = STRING(txtHora,"99") + STRING(txtMinuto,"99") /* FILL-IN_HorRet*/
          DI-RutaC.KmtFin = FILL-IN_KmtFin
          DI-RutaC.GuiaTransportista = STRING(txtSerie,"999") + "-" + STRING(txtNro,"99999999") /*FILL-IN_CodRut*/
          DI-RutaC.FlgEst = 'C'
          DI-RutaC.UsrCierre = s-user-id
          DI-RutaC.FchCierre = STRING(DATETIME(TODAY, MTIME), '99/99/9999 HH:MM:SS').
      /* Actualizamo PRE-HOJA DE RUTA */
      RUN Actualiza-PreHoja.
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
      /* *************************** */
      FIND GN-VEHIC WHERE gn-vehic.codcia = DI-RutaC.codcia
          AND gn-vehic.placa = DI-RutaC.CodVeh
          NO-LOCK NO-ERROR.
      IF AVAILABLE Gn-vehic AND INDEX(gn-vehic.Marca, 'TAXI') > 0 THEN DI-RutaC.CtoRut = FILL-IN_CtoRut.
      /* RHC 19.06.2012 Cerramos Ccbcbult POR ORDENES DE DESPACHO */
      FOR EACH Di-RutaD OF Di-RutaC NO-LOCK WHERE Di-RutaD.FlgEst = "C",    /* SOLO ENTREGADOS */
          FIRST Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = Di-RutaC.codcia
          AND Ccbcdocu.coddoc = Di-RutaD.codref
          AND Ccbcdocu.nrodoc = Di-RutaD.nroref,
          FIRST Ccbcbult EXCLUSIVE-LOCK WHERE CcbCBult.CodCia = Ccbcdocu.codcia
          AND CcbCBult.CodDoc = Ccbcdocu.Libre_C01      /*Ccbcdocu.codped*/
          AND CcbCBult.NroDoc = Ccbcdocu.Libre_C02      /*Ccbcdocu.nroped*/
          AND CcbCBult.Chr_01 = "P":
          CcbCBult.Chr_01 = "C".
      END.
      /* RHC 20.08.2012 Cerramos Ccbcbult POR TRANSFERENCIAS */
      /* Ic 13Mar205 considerar OTR */
      FOR EACH Di-RutaG OF Di-RutaC NO-LOCK WHERE Di-RutaG.FlgEst = "C",    /* SOLO ENTREGADOS */
          EACH Ccbcbult EXCLUSIVE-LOCK USE-INDEX Llave03 WHERE CcbCBult.CodCia = Di-RutaC.codcia
          AND CcbCBult.CodDoc = "TRA"
          AND CcbCBult.NroDoc = STRING(Di-RutaG.serref, '999') + STRING(Di-RutaG.nroref, '999999'):
          IF CcbCBult.Chr_01 = "P" THEN CcbCBult.Chr_01 = "C".
      END.
      FOR EACH Di-RutaG OF Di-RutaC NO-LOCK WHERE Di-RutaG.FlgEst = "C",    /* SOLO ENTREGADOS */
          EACH Ccbcbult EXCLUSIVE-LOCK USE-INDEX Llave03 WHERE CcbCBult.CodCia = Di-RutaC.codcia
          AND CcbCBult.CodDoc = "OTR"
          AND CcbCBult.NroDoc = STRING(Di-RutaG.serref, '999') + STRING(Di-RutaG.nroref, '999999'):
          IF CcbCBult.Chr_01 = "P" THEN CcbCBult.Chr_01 = "C".
      END.
      /* ------------- R A C K S --------------------------- */
      /* Ic - 13Mar2015 */
      lOrdenes = "".
      FOR EACH Di-RutaD OF Di-RutaC NO-LOCK ,
          FIRST Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = Di-RutaC.codcia
          AND Ccbcdocu.coddoc = Di-RutaD.codref
          AND Ccbcdocu.nrodoc = Di-RutaD.nroref,
          EACH Ccbcbult EXCLUSIVE-LOCK WHERE CcbCBult.CodCia = Ccbcdocu.codcia
          AND CcbCBult.CodDoc = Ccbcdocu.Libre_C01      /*O/D*/
          AND CcbCBult.NroDoc = Ccbcdocu.Libre_C02:      /*Nro O/D*/
          IF NOT (CcbCBult.Chr_01 = "P" AND LOOKUP(Ccbcdocu.Libre_C02,lOrdenes) = 0) THEN NEXT.
          /* Guias devueltas TOTALES */
          IF Ccbcdocu.Libre_C01 = 'O/D' AND (di-rutad.flgest = 'X' OR 
                                             di-rutad.flgest = 'N' OR 
                                             di-rutad.flgest = 'NR') THEN DO:

              lHRuta = "*" + TRIM(di-rutad.nrodoc) + "*".
              /* Busco la Orden en el RACK */
              FIND FIRST vtadtabla WHERE vtadtabla.codcia = s-codcia AND
                                vtadtabla.tabla = 'MOV-RACK-DTL' AND
                                vtadtabla.libre_c03 = 'O/D' AND
                                vtadtabla.llavedetalle = ccbcdocu.libre_c02 AND
                                vtadtabla.libre_c05 matches lHRuta NO-LOCK NO-ERROR.
              IF AVAILABLE vtadtabla THEN DO:
                  ASSIGN CcbCBult.DEC_01 = CcbCBult.DEC_01 - vtadtabla.libre_d01.
                  IF CcbCBult.DEC_01 < 0 THEN DO:
                      ASSIGN ccbcbult.DEC_01 = 0.
                  END.
                  IF lOrdenes <> "" THEN DO:
                      lOrdenes = lOrdenes + ",".
                  END.
                  lOrdenes = lOrdenes + trim(Ccbcdocu.Libre_C02).
              END.
          END.
      END.
      /* Las Ordenes de Transferencias OTR y/o TRA */
      lOrdenes = "".
      FOR EACH Di-RutaG OF Di-RutaC NO-LOCK :
          /* Para NO ENTREGADOS/DEVUELTOS */
          IF di-rutag.flgest = 'X' OR di-rutag.flgest = 'N' OR di-rutag.flgest = 'NR' THEN DO:
            FIND FIRST almcmov WHERE almcmov.codcia = DI-RutaC.codcia AND 
                                    almcmov.codalm = di-rutaG.codalm AND
                                    almcmov.tipmov = di-rutaG.tipmov AND
                                    almcmov.codmov = di-rutaG.codmov AND 
                                    almcmov.nroser = di-rutaG.serref AND
                                    almcmov.nrodoc = di-rutaG.nroref NO-LOCK NO-ERROR.
            IF AVAILABLE almcmov THEN DO:
                IF almcmov.codref = 'OTR' THEN DO:
                    /* Orden de Transferencia */
                    lCodDoc = almcmov.codref.
                    lNroDoc = almcmov.nroref.
                END.
                ELSE DO:
                    /* Transferencia entre almacenes */
                    lCodDoc = 'TRA'.
                    lNroDoc = STRING(almcmov.nroser,"999") + STRING(almcmov.nrodoc,"999999").
                END.
                /* Buscar los bultos */
                FIND FIRST CcbCBult WHERE CcbCBult.codcia = s-codcia AND 
                            CcbCBult.coddoc = lCodDoc AND 
                            CcbCBult.nrodoc = lNroDoc NO-LOCK NO-ERROR.
                IF AVAILABLE CcbCBult AND LOOKUP(lNroDoc,lOrdenes) = 0 THEN DO:
                    lHRuta = "*" + TRIM(di-rutaG.nrodoc) + "*".
                    /* Chequeo si la O/D esta en el detalle del RACK */
                    FIND FIRST vtadtabla WHERE vtadtabla.codcia = DI-RutaC.codcia AND
                          vtadtabla.tabla = 'MOV-RACK-DTL' AND 
                          vtadtabla.libre_c03 = lCodDoc AND 
                          vtadtabla.llavedetalle = lNroDoc AND
                          vtadtabla.libre_c05 matches lHRuta NO-LOCK NO-ERROR.
                    IF AVAILABLE vtadtabla THEN DO:

                        lRowid = ROWID(CcbCBult).

                        DEFINE BUFFER b-CcbCBult FOR CcbCBult.
                        FIND FIRST b-CcbCBult WHERE ROWID(b-CcbCBult) = lRowid EXCLUSIVE NO-ERROR.
                        IF AVAILABLE b-CcbCBult THEN DO:
                            ASSIGN b-CcbCBult.DEC_01 = b-CcbCBult.DEC_01 - vtadtabla.libre_d01.
                            IF b-CcbCBult.DEC_01 < 0 THEN DO:
                                ASSIGN b-ccbcbult.DEC_01 = 0.
                            END.
                            IF lOrdenes <> "" THEN DO:
                                lOrdenes = lOrdenes + ",".
                            END.
                            lOrdenes = lOrdenes + trim(lNroDoc).
                        END.
                        RELEASE CcbCBult.
                    END.
                END.
            END.
          END.
      END.
      /* Ic - 13Mar2015 */
      /* **************************************************************************************** */
      /* RHC 15/05/18 CONTROL DE REPROGRAMACIONES *********************************************** */
      /* **************************************************************************************** */
      RUN Control-de-Reprogramacion.
      IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
          MESSAGE 'NO se pudo generar el control de O/D a reprogramar' SKIP
              "Hoja de Ruta:" Di-RutaC.NroDoc
              VIEW-AS ALERT-BOX ERROR.
          UNDO, RETURN 'ADM-ERROR'.
      END.
      /* **************************************************************************************** */
      /* **************************************************************************************** */
  END.
  IF AVAILABLE Di-RutaC THEN RELEASE Di-RutaC.
  IF AVAILABLE B-PHRC THEN RELEASE B-PHRC.
  IF AVAILABLE B-PHRD THEN RELEASE B-PHRD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-9
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-PreHoja D-Dialog 
PROCEDURE Actualiza-PreHoja :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF ENTRY(1, DI-RutaC.Libre_c03) <> "PHR" THEN RETURN 'OK'.

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    {lib/lock-genericov3.i 
        &Tabla="B-PHRC" ~
        &Condicion="B-PHRC.codcia = DI-RutaC.codcia
        AND B-PHRC.coddiv = DI-RutaC.coddiv
        AND B-PHRC.coddoc = ENTRY(1, DI-RutaC.libre_c03)
        AND B-PHRC.nrodoc = ENTRY(2, DI-RutaC.libre_c03)" ~
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
        &Accion="RETRY" ~
        &Mensaje="YES" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'" ~
        }
    /* Eliminamos los registros que NO han sido entregados */
    /* RHC 06/03/2019 OJO: EL control pasa a REPROGRAMACION
    FOR EACH Di-RutaD OF Di-RutaC NO-LOCK WHERE Di-RutaD.FlgEst <> "C",    /* SOLO NO ENTREGADOS */
        FIRST Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = Di-RutaC.codcia
        AND Ccbcdocu.coddoc = Di-RutaD.codref
        AND Ccbcdocu.nrodoc = Di-RutaD.nroref,
        FIRST B-PHRD OF B-PHRC EXCLUSIVE-LOCK WHERE B-PHRD.codref = Ccbcdocu.Libre_c01
        AND B-PHRD.nroref = Ccbcdocu.Libre_c02 ON ERROR UNDO, THROW:
        DELETE B-PHRD.
    END.
    FOR EACH Di-RutaG OF Di-RutaC NO-LOCK WHERE Di-RutaG.FlgEst <> "C",    /* SOLO NO ENTREGADOS */
        FIRST almcmov NO-LOCK WHERE almcmov.codcia = DI-RutaG.codcia AND 
        almcmov.codalm = di-rutaG.codalm AND
        almcmov.tipmov = di-rutaG.tipmov AND
        almcmov.codmov = di-rutaG.codmov AND 
        almcmov.nroser = di-rutaG.serref AND
        almcmov.nrodoc = di-rutaG.nroref,
        FIRST B-PHRD OF B-PHRC EXCLUSIVE-LOCK WHERE B-PHRD.codref = almcmov.codref
        AND B-PHRD.nroref = almcmov.nroref ON ERROR UNDO, THROW:
        DELETE B-PHRD.
    END.
    */
END.
IF AVAILABLE(B-PHRC) THEN RELEASE B-PHRC.
IF AVAILABLE(B-PHRD) THEN RELEASE B-PHRD.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal D-Dialog 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR k AS INT NO-UNDO.

EMPTY TEMP-TABLE T-RutaC.
DO k = NUM-ENTRIES(pHojasdeRuta) TO 1 BY -1:
    FIND Di-rutac WHERE Di-rutac.codcia = s-codcia
        AND Di-rutac.coddiv = s-coddiv
        AND Di-rutac.coddoc = "H/R"
        AND Di-rutac.nrodoc = ENTRY(k, pHojasdeRuta)
        NO-LOCK NO-ERROR.
    IF AVAILABLE Di-rutac THEN DO:
        CREATE T-RutaC.
        BUFFER-COPY Di-rutac TO T-RutaC.
    END.

END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Control-de-Reprogramacion D-Dialog 
PROCEDURE Control-de-Reprogramacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER GUIAS FOR Ccbcdocu.        
DEF VAR x-Guias AS INT NO-UNDO.        
DEF VAR x-Guias-2 AS INT NO-UNDO.
DEF VAR x-ImpTot AS DEC NO-UNDO.
DEF VAR x-Peso AS DEC NO-UNDO.
DEF VAR x-Volumen AS DEC NO-UNDO.
DEF BUFFER B-RUTAD FOR DI-RUTAD.
DEF BUFFER B-CDOCU FOR Ccbcdocu.
FOR EACH Di-RutaD OF Di-RutaC NO-LOCK WHERE Di-RutaD.codref = "G/R" 
    AND Di-RutaD.flgest = "N"           /* No Entregado */
    AND Di-RutaD.Libre_c02 = "R",       /* Reprogramar */
    FIRST Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = Di-RutaD.codcia
        AND Ccbcdocu.coddoc = Di-RutaD.codref   /* G/R */
        AND Ccbcdocu.nrodoc = Di-RutaD.nroref
        AND Ccbcdocu.Libre_c01 = "O/D"
    BREAK BY Ccbcdocu.Libre_c02:      /* Quiebre por n�mero de O/D */
    IF FIRST-OF(Ccbcdocu.Libre_c02) THEN DO:
        /* Limpiamos contador de guias por O/D */
        x-Guias = 0.
        x-ImpTot = 0.
        x-Peso = 0.
        x-Volumen = 0.
    END.
    x-Guias = x-Guias + 1.
    x-ImpTot = x-ImpTot + Ccbcdocu.ImpTot.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK, FIRST Almmmatg OF Ccbddocu NO-LOCK:
        x-Peso = x-Peso + (Almmmatg.PesMat * Ccbddocu.CanDes * Ccbddocu.Factor).
        x-Volumen = x-Volumen + (Almmmatg.Libre_d02 * Ccbddocu.CanDes * Ccbddocu.Factor / 1000000).
    END.
    IF LAST-OF(Ccbcdocu.Libre_c02) THEN DO:
        /* Verificamos si TODAS la G/R han sido NO ENTREGADAS */
        x-Guias-2 = 0.
        FOR EACH B-RUTAD OF Di-RutaC NO-LOCK WHERE B-RUTAD.codref = "G/R",
            FIRST B-CDOCU NO-LOCK WHERE B-CDOCU.codcia = B-RUTAD.codcia
                AND B-CDOCU.coddoc = B-RUTAD.codref   /* G/R */
                AND B-CDOCU.nrodoc = B-RUTAD.nroref
                AND B-CDOCU.Libre_c01 = Ccbcdocu.libre_c01
                AND B-CDOCU.Libre_c02 = Ccbcdocu.libre_c02:
            x-Guias-2 = x-Guias-2 + 1.
        END.
/*         FOR EACH GUIAS NO-LOCK WHERE GUIAS.codcia = s-codcia */
/*             AND GUIAS.coddoc = "G/R"                         */
/*             AND GUIAS.libre_c01 = Ccbcdocu.libre_c01         */
/*             AND GUIAS.libre_c02 = Ccbcdocu.libre_c02         */
/*             AND GUIAS.flgest = "F":                          */
/*             x-Guias-2 = x-Guias-2 + 1.                       */
/*         END.                                                 */
        
        IF x-Guias = x-Guias-2 THEN DO:
            /* TODAS HAN SIDO "NO ENTREGADAS" */
            /* RHC 29/05/18 Si se repite el ciclo chancamos la anterior */
            FIND Almcdocu WHERE AlmCDocu.CodCia = Ccbcdocu.CodCia
                AND AlmCDocu.CodLlave = DI-RutaC.CodDiv            /* OJO */
                AND AlmCDocu.CodDoc = Ccbcdocu.Libre_c01    /* O/D */
                AND AlmCDocu.NroDoc = Ccbcdocu.Libre_c02
                NO-LOCK NO-ERROR.
            IF AVAILABLE Almcdocu THEN DO:
                FIND CURRENT Almcdocu EXCLUSIVE-LOCK NO-ERROR.
                IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
            END.
            ELSE CREATE Almcdocu.
            ASSIGN
                AlmCDocu.CodCia = Ccbcdocu.CodCia
                AlmCDocu.CodDoc = Ccbcdocu.Libre_c01    /* O/D */
                AlmCDocu.NroDoc = Ccbcdocu.Libre_c02
                AlmCDocu.CodLlave = DI-RutaC.CodDiv            /* OJO */
                AlmCDocu.FchDoc = TODAY
                AlmCDocu.FchCreacion = TODAY
                AlmCDocu.FlgEst = "P"       /* "P": por reprogramar, "C": reprogramado, "A": no se reprograma */
                AlmCDocu.UsrCreacion = s-user-id
                AlmCDocu.Libre_c01 = DI-RutaC.CodDoc    /* H/R */
                AlmCDocu.Libre_c02 = DI-RutaC.NroDoc
                AlmCDocu.Libre_c03 = DI-RutaD.FlgEstDet     /* Motivo */
                AlmCDocu.Libre_d01 = x-ImpTot
                AlmCDocu.Libre_d02 = x-Peso
                AlmCDocu.Libre_d03 = x-Volumen
                NO-ERROR.
            IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
            RELEASE AlmCDocu.
        END.
    END.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY txtHora TxtMinuto FILL-IN_KmtFin FILL-IN_CtoRut txtSerie txtNro 
      WITH FRAME D-Dialog.
  ENABLE BROWSE-9 txtHora TxtMinuto FILL-IN_KmtFin FILL-IN_CtoRut txtSerie 
         txtNro Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN Carga-Temporal.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*   DO WITH FRAME {&FRAME-NAME}:                                                */
/*       FIND GN-VEHIC WHERE gn-vehic.codcia = DI-RutaC.codcia                   */
/*           AND gn-vehic.placa = DI-RutaC.CodVeh:SCREEN-VALUE                   */
/*           NO-LOCK NO-ERROR.                                                   */
/*       IF AVAILABLE GN-VEHIC THEN FILL-IN-Marca:SCREEN-VALUE = gn-vehic.Marca. */
/*       IF INDEX(FILL-IN-Marca:SCREEN-VALUE, 'TAXI') > 0                        */
/*           THEN DI-RutaC.CtoRut:SENSITIVE = YES.                               */
/*   END.                                                                        */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Parametros D-Dialog 
PROCEDURE Procesa-Parametros :
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recoge-Parametros D-Dialog 
PROCEDURE Recoge-Parametros :
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
        WHEN "" THEN ASSIGN input-var-1 = "".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
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
  {src/adm/template/snd-list.i "T-RutaC"}
  {src/adm/template/snd-list.i "gn-vehic"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida D-Dialog 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR lHora AS INT.
DEFINE VAR lMinuto AS INT.
DEFINE VAR lSerie AS INT.
DEFINE VAR lNroDoc  AS INT.

DEFINE VAR lGuiaTrans AS CHAR.

DO WITH FRAME {&FRAME-NAME}:
    /* 08Jul2013 - Ic
    hora1 = SUBSTRING(FILL-IN_HorRet:SCREEN-VALUE, 1, 2).
    IF INT(hora1) >= 24 THEN DO:
      MESSAGE 'ingresar menor a 24 horas'.
      APPLY 'ENTRY' TO FILL-IN_HorRet.
      RETURN 'ADM-ERROR'.
    END.
    hora2 = SUBSTRING(FILL-IN_HorRet:SCREEN-VALUE, 3, 2).
    IF INT(hora2) >= 60 THEN DO:
      MESSAGE 'ingresar menor a 60 minutos'.
      APPLY 'ENTRY' TO FILL-IN_HorRet.
      RETURN 'ADM-ERROR'.
    END.
    IF FILL-IN_HorRet:SCREEN-VALUE = '' THEN DO:
        MESSAGE 'Debe ingresar la hora de retorno' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO FILL-IN_HorRet.
        RETURN 'ADM-ERROR'.
    END.
    IF FILL-IN_CodRut:SCREEN-VALUE = '' THEN DO:  
        MESSAGE 'Debe ingresar la Guia del Transportista' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO FILL-IN_CodRut.
        RETURN 'ADM-ERROR'.
    END.    
    */

    /* 08Jul2013 - Ic  */
    lHora = int(txtHora:SCREEN-VALUE).
    lMinuto = INT(txtMinuto:SCREEN-VALUE).
    lSerie  = INT(txtSerie:SCREEN-VALUE).
    lNroDoc = INT(txtNro:SCREEN-VALUE).

    IF lHora < 0 OR lHora > 24 THEN DO:
        MESSAGE 'La hora es incorrecta' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO txtHora.
        RETURN 'ADM-ERROR'.
    END.
    IF lMinuto < 0 OR lMinuto > 59 THEN DO:
        MESSAGE 'Minutos es incorrecta' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO txtMinuto.
        RETURN 'ADM-ERROR'.
    END.
    IF lHora = 0 AND lMinuto = 0 THEN DO:
        MESSAGE 'Ingrese la Hora/Minuto de Retorno' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO txtHora.
        RETURN 'ADM-ERROR'.
    END.
    /* No menor que la salida */
    FIND Di-rutac WHERE Di-rutac.codcia = s-codcia
        AND Di-rutac.coddiv = s-coddiv
        AND Di-rutac.coddoc = "H/R"
        AND Di-rutac.nrodoc = pHojasdeRuta
        NO-LOCK NO-ERROR.
    IF DI-RutaC.FchDoc = TODAY THEN DO:
        IF DI-RutaC.HorSal > (STRING(lHora,"99") + STRING(lMinuto,"99")) THEN DO:
            MESSAGE 'Error en la hora de Retorno' VIEW-AS ALERT-BOX ERROR.
            APPLY 'ENTRY' TO txtHora.
            RETURN 'ADM-ERROR'.
        END.
    END.

    IF lSerie < 0 THEN DO:
        MESSAGE 'Nro Serie esta errada' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO txtSerie.
        RETURN 'ADM-ERROR'.
    END.
    IF lNroDoc < 0 THEN DO:
        MESSAGE 'Nro Documento esta errada' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO txtNro.
        RETURN 'ADM-ERROR'.
    END.
    IF lSerie = 0 AND lNroDoc = 0 THEN DO:
        MESSAGE 'Serie/NroDcto esta errado' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO txtSerie.
        RETURN 'ADM-ERROR'.
    END.
    /* 08Jul2013 */

    IF DECIMAL(FILL-IN_KmtFin:SCREEN-VALUE) <= 0 THEN DO:
        MESSAGE 'Debe ingresar el kilometraje de retorno' VIEW-AS ALERT-BOX ERROR.
        APPLY 'ENTRY' TO FILL-IN_KmtFin.
        RETURN 'ADM-ERROR'.
    END.
    ELSE DO:
        FIND Di-rutac WHERE Di-rutac.codcia = s-codcia
            AND Di-rutac.coddiv = s-coddiv
            AND Di-rutac.coddoc = "H/R"
            AND Di-rutac.nrodoc = pHojasdeRuta
            NO-LOCK NO-ERROR.
        IF DECIMAL(FILL-IN_KmtFin:SCREEN-VALUE) < DI-RutaC.KmtIni THEN DO:
            MESSAGE 'Kilometraje menor al de salida' VIEW-AS ALERT-BOX ERROR.
            APPLY 'ENTRY' TO FILL-IN_KmtFin.
            RETURN 'ADM-ERROR'.
        END.
    END.

    /* >>> Ic 14May2015 - Valida */
    lGuiaTrans = STRING(lSerie,"999") + "-" + STRING(lnrodoc,"99999999").
    FOR EACH T-RutaC NO-LOCK:
        IF t-RutaC.guiatransportista <> lGuiaTrans THEN DO:
            MESSAGE 'Existen Hojas de RUTA con diferente Guia de Transportista' VIEW-AS ALERT-BOX ERROR.
            APPLY 'ENTRY' TO txtSerie.
            RETURN 'ADM-ERROR'.
        END.
    END.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

