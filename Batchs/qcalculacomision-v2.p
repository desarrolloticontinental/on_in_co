&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR s-codcia  AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.

DEF VAR x-CodDiv LIKE gn-divi.coddiv NO-UNDO.

DEF TEMP-TABLE t-cdoc LIKE ccbcdocu
    FIELD imptot2Old AS DEC
    INDEX Llave01 AS PRIMARY codcia coddoc nrodoc codven.

DEF TEMP-TABLE t-ddoc NO-UNDO LIKE ccbddocu
    FIELD CodVen      AS CHAR
    FIELD PorComision AS DEC
    FIELD PorCastigo  AS DEC
    FIELD ImpComision AS DEC
    FIELD PorComisOld AS DEC
    FIELD ImpComisOld AS DEC
    FIELD Metodo1 LIKE ComiDetail.Metodo1
    FIELD Metodo2 LIKE ComiDetail.Metodo2
    FIELD CanalVtaProc AS CHAR
    FIELD CanalVtaDiv AS CHAR
    INDEX Llave01 AS PRIMARY codcia coddoc nrodoc codven codmat.

DEF BUFFER b-cdoc FOR ccbcdocu.
    
DEF TEMP-TABLE detalle NO-UNDO
    FIELD coddiv AS CHAR
    FIELD codven AS CHAR
    FIELD codfam AS CHAR
    FIELD implin AS DEC
    FIELD implin1 AS DEC
    FIELD implin2 AS DEC
    FIELD impcom1 AS DEC
    FIELD impcom2 AS DEC
    FIELD implinOld AS DEC
    FIELD implin1Old AS DEC
    FIELD implin2Old AS DEC
    FIELD impcom1Old AS DEC
    FIELD impcom2Old AS DEC
    FIELD imptot2Old AS DEC.

DEFINE TEMP-TABLE tt-comision-ventas NO-UNDO
        FIELD tt-tipo AS CHAR FORMAT "x(1)"
        FIELD tt-codven AS CHAR FORMAT "x(3)"
        FIELD tt-desven AS CHAR FORMAT "x(50)"
        FIELDS tt-codfam AS CHAR FORMAT "x(3)"
        FIELDS tt-desfam AS CHAR FORMAT "x(50)"
        FIELD tt-imp-vta AS DEC
        FIELD tt-imp-vta-sin-igv AS DEC
        FIELDS tt-por-comi AS DEC
        FIELD tt-imp-comi AS DEC
        INDEX idx01 IS PRIMARY tt-tipo tt-codven tt-codfam.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 7.54
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND FIRST ComiConfig WHERE ComiConfig.CodCia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE ComiConfig THEN DO:
    DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    QUIT.
END.

IF ComiConfig.FlagRecalculo = NO THEN DO:
    DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    QUIT.
END.
IF ComiConfig.Estado  = "P" THEN DO:
    DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    QUIT.
END.

DEF VAR x-FchDoc-1 AS DATE NO-UNDO.
DEF VAR x-FchDoc-2 AS DATE NO-UNDO.
DEF VAR COMBO-BOX-Divisiones AS CHAR NO-UNDO.
DEF VAR j AS INTE NO-UNDO.

ASSIGN
    x-FchDoc-1 = ComiConfig.Desde
    x-FchDoc-2 = ComiConfig.Hasta
    COMBO-BOX-Divisiones = ComiConfig.Divisiones.

/* Verificación de filtros */
IF x-FchDoc-1 = ? OR x-FchDoc-2 = ? OR x-FchDoc-1 > x-FchDoc-2 THEN RETURN.
IF COMBO-BOX-Divisiones = "" THEN RETURN.
/* Fin de filtros */

/* Inicio del proceso */
DISPLAY
    'Bloqueando ComiConfig'
    DATETIME(TODAY,MTIME)
    SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.

RUN Inicio NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    DISPLAY
        'No pudo bloquear ComConfig. Fin del proceso'
        DATETIME(TODAY,MTIME)
        SKIP
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    QUIT.
END.

DISPLAY
    'Inicio del proceso'
    DATETIME(TODAY,MTIME)
    SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
DO j = 1 TO NUM-ENTRIES(COMBO-BOX-Divisiones):
    x-CodDiv = ENTRY(j, COMBO-BOX-Divisiones).
    DISPLAY
        x-CodDiv 
        DATETIME(TODAY,MTIME)
        SKIP
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    /* Cargamos tablas temporales */
    RUN Carga-Temporal-New.
END.
/* Graba informacion */
RUN Carga-Base.
IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
    DISPLAY 'Final:' TODAY STRING(TIME, 'HH:MM:SS') SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    QUIT.
END.

RUN Termino NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    DISPLAY
        'No pudo cerrar ComConfig. Fin del proceso'
        DATETIME(TODAY,MTIME)
        SKIP
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    QUIT.
END.

IF AVAILABLE ComiConfig THEN RELEASE ComiConfig.
DISPLAY
    'Programa terminó con éxito'
    DATETIME(TODAY,MTIME)
    SKIP
    WITH STREAM-IO NO-BOX NO-LABELS.
PAUSE 0.
QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Carga-Base) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Base Procedure 
PROCEDURE Carga-Base :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH ComiDetail EXCLUSIVE-LOCK WHERE ComiDetail.CodCia = s-codcia 
    ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    DELETE ComiDetail.
END.

FOR EACH T-CDOC NO-LOCK, 
    EACH T-DDOC NO-LOCK WHERE T-DDOC.codcia = T-CDOC.codcia
    AND T-DDOC.coddoc = T-CDOC.coddoc
    AND T-DDOC.nrodoc = T-CDOC.nrodoc
    AND T-DDOC.codven = T-CDOC.codven,      /* OJO */
    FIRST Almmmatg OF T-DDOC, 
    FIRST gn-divi OF T-CDOC,
    FIRST Almtfami OF Almmmatg NO-LOCK, 
    FIRST Almsfami OF Almmmatg NO-LOCK:
    CREATE ComiDetail.
    ASSIGN
        ComiDetail.CanalVenta = t-ddoc.canalvtaproc /*GN-DIVI.CanalVenta        */
        ComiDetail.CanalVentaDiv = t-ddoc.canalvtadiv
        ComiDetail.CanDes = T-DDOC.candes
        ComiDetail.CodCia = s-codcia
        ComiDetail.CodDiv = GN-DIVI.CodDiv + ' - ' + GN-DIVI.DesDiv
        ComiDetail.CodDoc = T-CDOC.coddoc
        ComiDetail.CodFam = Almtfami.codfam + ' - ' + Almtfami.desfam
        ComiDetail.CodMat = Almmmatg.codmat + ' - ' + Almmmatg.DesMat
        ComiDetail.DesMar = Almmmatg.desmar
        ComiDetail.Dias   = T-DDOC.ImpDcto_Adelanto[5]
        ComiDetail.Escala = T-DDOC.ImpDcto_Adelanto[4]
        ComiDetail.FchDoc = T-CDOC.fchdoc
        ComiDetail.ImpComiOld = T-DDOC.ImpComisOld
        ComiDetail.ImpComision = T-DDOC.ImpComision
        ComiDetail.ImpCto = T-DDOC.ImpDcto_Adelanto[1]
        ComiDetail.ImpTot = T-DDOC.implin
        ComiDetail.MargenUtil = T-DDOC.ImpDcto_Adelanto[2]
        ComiDetail.NroDoc = T-CDOC.nrodoc
        ComiDetail.PorCastigo = T-DDOC.PorCastigo
        ComiDetail.PorComiOld = T-DDOC.PorComisOld
        ComiDetail.PorComision = T-DDOC.PorComision
        ComiDetail.PorMargenUtil = T-DDOC.ImpDcto_Adelanto[3]
        ComiDetail.SubFam = AlmSFami.codfam + ' - ' + AlmSFami.dessub
        ComiDetail.UndStk = T-DDOC.undvta
        ComiDetail.Metodo1 = T-DDOC.Metodo1
        ComiDetail.Metodo2 = T-DDOC.Metodo2.
    ComiDetail.FmaPgo = T-CDOC.fmapgo.
    FIND gn-convt WHERE gn-ConVt.Codig = T-CDOC.fmapgo NO-LOCK NO-ERROR.
    IF AVAILABLE gn-convt THEN ComiDetail.FmaPgo = gn-ConVt.Codig + ' - ' + gn-ConVt.Nombr.
    ComiDetail.CodVen = T-CDOC.codven.
    FIND gn-ven OF T-CDOC NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ven THEN ComiDetail.CodVen = gn-ven.CodVen + ' - ' + gn-ven.NomVen.
    ComiDetail.CodCli = T-CDOC.codcli + ' - ' + T-CDOC.nomcli.
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia AND gn-clie.codcli = T-CDOC.codcli NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN ComiDetail.CodCli = gn-clie.CodCli + ' - ' + gn-clie.NomCli.
END.
RELEASE ComiDetail.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Temporal-New) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal-New Procedure 
PROCEDURE Carga-Temporal-New :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*{vta2/i-comisiones-new.i}*/


  DEF VAR x-FchCan AS DATE NO-UNDO.
  DEF VAR x-ImpTot AS DEC NO-UNDO.

  DEF VAR x-Signo  AS INT NO-UNDO.
  DEF VAR x-FchDoc AS DATE NO-UNDO.

  DEF VAR x-Utilidad AS DEC NO-UNDO.
  DEF VAR x-MargenUtil AS DEC NO-UNDO.
  DEF VAR x-CtoLis AS DEC NO-UNDO.
  /*DEF VAR x-PreUni AS DEC NO-UNDO.*/
  DEF VAR x-Campana AS LOG NO-UNDO.
  DEF VAR x-TotDias AS DEC NO-UNDO.
  DEF VAR x-PorComi AS DEC DECIMALS 6 NO-UNDO.
  DEF VAR x-Contador AS INT NO-UNDO.
  DEF VAR x-Codigo AS CHAR NO-UNDO.
  
  DEFINE VAR lCanalVtaDiv AS CHAR.
  DEFINE VAR lCanalVta AS CHAR.

  DEFINE VAR x-ImpNacSIGV AS DECI NO-UNDO.
  DEFINE VAR x-ImpIgv AS DECI NO-UNDO.
  DEFINE VAR x-Comprobantes AS CHAR NO-UNDO.

  FOR EACH FacDocum NO-LOCK WHERE FacDocum.CodCia = s-codcia AND FacDocum.TpoDoc <> ?:
      x-Comprobantes = x-Comprobantes + (IF x-Comprobantes = '' THEN '' ELSE ',') + FacDocum.CodDoc.
  END.
  /* RHC 9/11/2020: Mayra limita los comprobantes a FAC, BOL, LET y N/C */
  x-Comprobantes = "FAC,BOL,LET,N/C".

  RLOOP:
  FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia AND GN-DIVI.coddiv = x-CodDiv,
      EACH Ccbcdocu NO-LOCK WHERE CcbCDocu.CodCia = gn-divi.codcia
      AND CcbCDocu.CodDiv = gn-divi.coddiv
      AND CcbCDocu.FchDoc >= x-FchDoc-1
      AND CcbCDocu.FchDoc <= x-FchDoc-2
      AND CcbCDocu.FlgEst = "C":
      IF NOT LOOKUP(Ccbcdocu.CodDoc, x-Comprobantes) > 0 THEN NEXT.
      /* RHC 09/11/2020 M.P. */
      CASE TRUE:
          WHEN LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL') > 0 THEN DO:
              /* NO debe estar cancelado por LETRA */
              FOR EACH Ccbdcaja NO-LOCK WHERE Ccbdcaja.codcia = s-codcia 
                  AND Ccbdcaja.codref = Ccbcdocu.coddoc
                  AND Ccbdcaja.nroref = Ccbcdocu.nrodoc:
                  IF Ccbdcaja.coddoc BEGINS 'CJ' THEN NEXT RLOOP.    /* CJE, CJA */
              END.
          END.
      END CASE.
      /* ******************* */
      x-Contador = x-Contador + 1.
      FIND FIRST T-CDOC WHERE T-CDOC.codcia = s-codcia
          AND T-CDOC.coddoc = Ccbcdocu.coddoc
          AND T-CDOC.nrodoc = Ccbcdocu.nrodoc
          AND T-CDOC.codven = Ccbcdocu.codven
          NO-ERROR.
      IF NOT AVAILABLE T-CDOC THEN CREATE T-CDOC.
      BUFFER-COPY Ccbcdocu
          TO T-CDOC
          ASSIGN
          T-CDOC.codcia = s-codcia
          T-CDOC.coddiv = Ccbcdocu.coddiv 
          T-CDOC.coddoc = Ccbcdocu.coddoc
          T-CDOC.nrodoc = Ccbcdocu.nrodoc
          T-CDOC.fchdoc = Ccbcdocu.fchdoc
          T-CDOC.codven = Ccbcdocu.codven
          T-CDOC.fmapgo = Ccbcdocu.fmapgo
          T-CDOC.imptot2 = 0.
      ASSIGN
          x-Signo  = 1
          x-FchDoc = Ccbcdocu.fchdoc
          x-Campana = NO
          x-TotDias = 0.
      FIND FacDocum OF CcbCDocu NO-LOCK NO-ERROR. 
      IF AVAILABLE FacDocum THEN x-Signo = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1. 
      IF Ccbcdocu.flgest = 'A' THEN x-Signo = 0.
      FIND LAST VtaTabla WHERE VtaTabla.codcia = s-codcia
              AND VtaTabla.Tabla = "CAMPAÑAS"
              AND x-FchDoc >= VtaTabla.Rango_Fecha[1]
              AND x-FchDoc <= VtaTabla.Rango_Fecha[2]
          NO-LOCK NO-ERROR.
      IF AVAILABLE VtaTabla THEN x-Campana = YES.
      FIND gn-convt WHERE gn-ConVt.Codig =  Ccbcdocu.FmaPgo NO-LOCK NO-ERROR.
      IF AVAILABLE gn-convt THEN x-TotDias = gn-ConVt.TotDias.
      DETALLE:
      FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK,
          FIRST Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia AND Almmmatg.codmat = Ccbddocu.codmat :
          /* Ventas SIN IGV solamente */
          IF Almmmatg.codfam = '009' THEN NEXT.
          /* Ventas con Margen de Utilidad solamente */
          ASSIGN
              x-MargenUtil = 0
              x-CtoLis = Almmmatg.CtoLis * Ccbddocu.CanDes.
          IF Almmmatg.MonVta = 2 THEN x-CtoLis = x-CtoLis * Almmmatg.TpoCmb.
          /* RHC 27/06/2017 Solicitado por Camus */
          IF Almmmatg.CtoLis <= 0 THEN DO:
              FIND LAST AlmStkge WHERE AlmStkge.CodCia = Almmmatg.codcia
                  AND AlmStkge.codmat = Almmmatg.codmat
                  AND AlmStkge.Fecha <= TODAY
                  NO-LOCK NO-ERROR.
              /* El costo promedio siempre está en SOLES y SIN IGV */
              IF AVAILABLE AlmStkge THEN x-CtoLis = AlmStkge.CtoUni * Ccbddocu.CanDes.
          END.
          /* *********************************** */
          /* Calculamos importe SIN IGV */
          ASSIGN
              x-ImpIgv = 0
              x-ImpNacSIGV = (Ccbddocu.ImpLin - Ccbddocu.ImpDto2).
          IF Ccbddocu.AftIgv = YES THEN x-ImpIgv = Ccbddocu.ImpIgv.
          IF x-ImpIgv > 0 AND Ccbddocu.ImpDto2 > 0 THEN DO:
              x-ImpIgv =  x-ImpIgv -  
                  ( Ccbddocu.ImpDto2 / ( 1 + Ccbcdocu.PorIgv / 100) * Ccbcdocu.PorIgv / 100 ).
          END.
          ASSIGN
              x-ImpNacSIGV = x-ImpNacSIGV - x-ImpIgv.
          IF Ccbcdocu.CodMon = 2 THEN x-ImpNacSIGV = x-ImpNacSIGV * Almmmatg.TpoCmb.
          ASSIGN
              x-ImpNacSIGV = x-ImpNacSIGV * x-Signo.
          /* *********************************** */
          ASSIGN
              x-Utilidad = x-ImpNacSIGV - x-CtoLis
              x-MargenUtil = x-Utilidad / x-CtoLis * 100.
          /* INICIO */
          FIND FIRST T-DDOC WHERE T-DDOC.codcia = T-CDOC.codcia
              AND T-DDOC.coddoc = T-CDOC.coddoc
              AND T-DDOC.nrodoc = T-CDOC.nrodoc
              AND T-DDOC.codven = T-CDOC.codven
              AND T-DDOC.CodMat = Ccbddocu.codmat NO-ERROR.
          IF NOT AVAILABLE T-DDOC THEN CREATE T-DDOC.
          ASSIGN
              T-DDOC.codcia = s-codcia
              T-DDOC.coddoc = T-CDOC.coddoc
              T-DDOC.nrodoc = T-CDOC.nrodoc
              T-DDOC.coddiv = T-CDOC.coddiv
              T-DDOC.codven = T-CDOC.codven
              T-DDOC.codmat = Ccbddocu.codmat
              T-DDOC.candes = Ccbddocu.CanDes
              T-DDOC.undvta = Almmmatg.undstk
              T-DDOC.implin = x-ImpNacSIGV
              T-DDOC.Flg_Factor = Almmmatg.TipArt
              T-DDOC.ImpComision = 0
              T-DDOC.ImpDcto_Adelanto[1] = x-CtoLis
              T-DDOC.ImpDcto_Adelanto[2] = x-Utilidad
              T-DDOC.ImpDcto_Adelanto[3] = x-MargenUtil
              T-DDOC.ImpDcto_Adelanto[5] = x-TotDias.
          ASSIGN
              x-PorComi = 0.

          /* 10Nov2016 - Excepciones - Vendedores con otro CANALVENTA  */
          DEFINE BUFFER b-tabgener FOR tabgener.
          lCanalVtaDiv = GN-DIVI.CanalVenta.
          lCanalVta = GN-DIVI.CanalVenta.
          /* RHC 11/05/2017 Solicitado por Cesar Camus */
          IF GN-DIVI.CanalVenta <> "FER" THEN DO:
              FIND FIRST b-tabgener WHERE b-tabgener.codcia = s-codcia 
                  AND b-tabgener.clave = 'VEND-EXC-COM' 
                  AND b-tabgener.codigo = Ccbcdocu.codven
                  NO-LOCK NO-ERROR.
              IF AVAILABLE b-tabgener THEN lCanalVta = b-tabgener.libre_c01.
          END.
          /*--*/
          ASSIGN 
              t-ddoc.canalvtadiv =  lCanalVtaDiv
              t-ddoc.canalvtaproc = lCanalVta.
          /* Excepciones Fin */

          /* 1ro Comisiones por Canal de Venta vs % Margen de Utilidad vs Linea vs Campaña */
          CASE Almmmatg.CodFam:
              WHEN "010" THEN x-Codigo = lCanalVta + "|" + Almmmatg.CodFam.
              OTHERWISE x-Codigo = lCanalVta + "|" + "999".
          END CASE.

          FOR EACH TabGener NO-LOCK WHERE TabGener.codcia = s-codcia
              AND TabGener.Clave = "%COMI-CV"
              AND TabGener.Codigo =  x-Codigo
              BY TabGener.ValorIni:
              IF x-MargenUtil >= TabGener.ValorIni AND x-MargenUtil < TabGener.ValorFin THEN DO:
                  IF x-Campana = YES 
                      THEN x-PorComi = TabGener.Parametro[1].
                      ELSE x-PorComi = TabGener.Parametro[2].
                  ASSIGN
                      T-DDOC.ImpComision = x-Utilidad * x-PorComi / 100
                      T-DDOC.Metodo1     = T-DDOC.ImpComision
                      T-DDOC.ImpDcto_Adelanto[4] = TabGener.ValorIni.
                  /* 2do Médoto */
                  T-DDOC.Metodo2 = (x-Utilidad * TabGener.ValorIni / x-MargenUtil) * x-PorComi / 100.
                  LEAVE.
              END.
          END.
          ASSIGN
              T-DDOC.PorComision = x-PorComi.
          /* Castigo por Condicion de Venta */
          FOR EACH TabGener NO-LOCK WHERE TabGener.codcia = s-codcia
              AND TabGener.Clave = "%COMI-FP"
              AND TabGener.Codigo = (IF Almmmatg.CodFam = "010" THEN Almmmatg.CodFam ELSE "999")
              BY TabGener.ValorIni:
              IF x-TotDias >= TabGener.ValorIni AND x-TotDias < TabGener.ValorFin THEN DO:
                  T-DDOC.ImpComision = T-DDOC.ImpComision * (1 - TabGener.Parametro[1] / 100).
                  ASSIGN
                      T-DDOC.PorCastigo = TabGener.Parametro[1].
                  LEAVE.
              END.
          END.
      END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Inicio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicio Procedure 
PROCEDURE Inicio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND CURRENT ComiConfig EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF ERROR-STATUS:ERROR THEN RETURN ERROR.
ASSIGN
    ComiConfig.Inicio  = NOW
    ComiConfig.Termino = ?
    ComiConfig.Estado  = "P".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Termino) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Termino Procedure 
PROCEDURE Termino :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND CURRENT ComiConfig EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
IF ERROR-STATUS:ERROR THEN RETURN ERROR.
ASSIGN
    ComiConfig.FlagRecalculo = NO
    ComiConfig.Termino = NOW
    ComiConfig.Estado  = "T".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

