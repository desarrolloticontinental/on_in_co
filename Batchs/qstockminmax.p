&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Calcular los maximos y minimos y tambien el consumos diario

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR s-codcia  AS INT INIT 1.
DEF VAR pv-codcia AS INT INIT 0.

DEF TEMP-TABLE Detalle
    FIELD FchDoc AS DATE
    FIELD CanDes AS DEC
    INDEX Llave01 AS PRIMARY FchDoc.
DEF VAR x-CodAlm AS CHAR NO-UNDO.
DEF VAR x-CodMat AS CHAR NO-UNDO.
DEF VAR x-FchIni AS DATE NO-UNDO.
DEF VAR x-FchFin AS DATE NO-UNDO.
DEF TEMP-TABLE T-MATE LIKE Almmmate.

DEF VAR x-Items AS INT NO-UNDO.
DEF VAR x-Promedio AS DEC NO-UNDO.
DEF VAR x-Desviacion AS DEC NO-UNDO.
DEF VAR x-Fecha AS DATE NO-UNDO.
DEF VAR x-Factor AS DEC NO-UNDO.

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
         HEIGHT             = 7
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR x-Almacenes AS CHAR NO-UNDO.

FOR EACH Almacen NO-LOCK WHERE codcia = s-codcia AND autmov = YES:
  /* Almacenes que NO son propios */
  IF Almacen.FlgRep = NO THEN NEXT.
  /* RHC 09.09.04 EL ALMACEN DE CONSIGN. NO TIENE MOVIMIENTO CONTABLE  */
  IF Almacen.AlmCsg = YES THEN NEXT.

  IF x-Almacenes = '' 
    THEN x-Almacenes = TRIM(Almacen.codalm).
    ELSE x-Almacenes = x-Almacenes + ',' + TRIM(Almacen.codalm).
END.

DEF VAR i AS INT NO-UNDO.

DISPLAY 'INICIO' TODAY STRING(TIME, 'hh:mm') SKIP WITH STREAM-IO NO-BOX NO-LABELS.

DO i = 1 TO NUM-ENTRIES(x-Almacenes):
    x-CodAlm = ENTRY(I, x-Almacenes).
    RUN Carga-Temporal.
    RUN Graba-Min-Max.
END.

DISPLAY 'TERMINO' TODAY STRING(TIME, 'hh:mm') SKIP WITH STREAM-IO NO-BOX NO-LABELS.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Carga-Temporal) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal Procedure 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR pRowid AS ROWID.
DEF VAR pDiasUtiles AS INT.
DEF VAR pVentaDiaria AS DEC.
DEF VAR pFactor AS DEC.
DEF VAR pDiasMaximo AS INT.
DEF VAR pDiasMinimo AS INT.
DEF VAR pReposicion AS DEC.
DEF VAR pComprometido AS DEC.
DEF VAR x-StockMinimo AS DEC NO-UNDO.
DEF VAR x-StockMaximo AS DEC NO-UNDO.

/* Buscamos los valores generales */
FIND FIRST AlmCfgGn WHERE almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almcfggn THEN DO:
    DISPLAY 'Debe configurar los parámetros generales' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
    RETURN.
END.

EMPTY TEMP-TABLE T-MATE.

FOR EACH Almmmate NO-LOCK WHERE Almmmate.codcia = s-codcia AND Almmmate.codalm = x-CodAlm,
    FIRST Almmmatg OF Almmmate NO-LOCK:
    /* Valores por defecto */
    ASSIGN
        pDiasMaximo = AlmCfgGn.DiasMaximo
        pDiasMinimo = AlmCfgGn.DiasMinimo
        pDiasUtiles = AlmCfgGn.DiasUtiles.
    /* Buscamos parametros del proveedor */
    FIND gn-prov WHERE gn-prov.codcia = pv-codcia
        AND gn-prov.codpro = Almmmatg.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov AND (gn-prov.stkmin > 0 AND gn-prov.stkmax > 0) THEN DO:
        ASSIGN
            pDiasMaximo = gn-prov.stkmax
            pDiasMinimo = gn-prov.stkmin.
    END.
    ASSIGN
        x-StockMinimo = 0
        x-StockMaximo = 0
        pVentaDiaria  = 0
        pFactor = 0.
    IF Almmmatg.TpoArt <> "D" THEN DO:
        /* Venta Diaria */
        pRowid = ROWID(Almmmate).
        RUN venta-diaria (pRowid, pDiasUtiles, Almmmate.CodAlm, OUTPUT pVentaDiaria, OUTPUT pFactor).
        /* Stock Minimo y Maximo */
        x-StockMinimo = ROUND (pDiasMinimo * pVentaDiaria, 0).
        x-StockMaximo = ROUND (pDiasMaximo * pVentaDiaria, 0).
    END.
    CREATE T-MATE.
    BUFFER-COPY Almmmate TO T-MATE
        ASSIGN
        T-MATE.StkMin = x-StockMinimo
        T-MATE.StkMax = x-StockMaximo
        T-MATE.StkRep = pVentaDiaria
        T-MATE.Libre_d01 = pFactor.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Carga-Ventas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Ventas Procedure 
PROCEDURE Carga-Ventas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR X-CODDIA AS INTEGER NO-UNDO INIT 1.
DEFINE VAR X-CODANO AS INTEGER NO-UNDO.
DEFINE VAR X-CODMES AS INTEGER NO-UNDO.
DEFINE VAR I        AS INTEGER NO-UNDO.
DEFINE VAR x-AnoMesDia AS CHAR.

FOR EACH dwh_ventas_despacho WHERE dwh_ventas_despacho.codcia = s-codcia
    AND dwh_ventas_despacho.codmat = x-codmat
    AND dwh_ventas_despacho.almdes = x-codalm
    AND dwh_ventas_despacho.fecha >= INTEGER( STRING(YEAR(x-FchIni),"9999") + STRING(MONTH(x-FchIni),"99") + STRING(DAY(x-FchIni),"99") )
    AND dwh_ventas_despacho.fecha <= INTEGER( STRING(YEAR(x-FchFin),"9999") + STRING(MONTH(x-FchFin),"99") + STRING(DAY(x-FchFin),"99") ):
    ASSIGN
        x-AnoMesDia = STRING(dwh_ventas_despacho.fecha, '99999999')
        x-Fecha = DATE( INTEGER(SUBSTRING(x-AnoMesDia, 5,2)),
                        INTEGER(SUBSTRING(x-AnoMesDia, 7,2)),
                        INTEGER(SUBSTRING(x-AnoMesDia, 1,4)) ).
    FIND Detalle WHERE Detalle.FchDoc = X-FECHA NO-ERROR.
    IF NOT AVAILABLE Detalle THEN CREATE Detalle.
    ASSIGN
        Detalle.fchdoc = x-Fecha
        Detalle.candes = Detalle.candes + dwh_ventas_despacho.Cantidad * x-factor.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Graba-Min-Max) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Min-Max Procedure 
PROCEDURE Graba-Min-Max :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DISABLE TRIGGERS FOR LOAD OF Logmmate.
DISABLE TRIGGERS FOR LOAD OF Almmmate.

FOR EACH Logmmate WHERE Logmmate.codcia = s-codcia
    AND Logmmate.logevento = "STOCKMINIMO":
    DELETE Logmmate.
END.
FOR EACH T-MATE:
    FIND Almmmate OF T-MATE EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmate THEN NEXT.
/*     DISPLAY TODAY STRING(TIME, 'hh:mm') t-mate.codalm t-mate.codmat t-mate.stkmin t-mate.stkmax SKIP WITH STREAM-IO NO-BOX NO-LABELS. */
/*     PAUSE 0.                                                                                                                          */
    ASSIGN
        Almmmate.StkMin = T-MATE.StkMin
        Almmmate.StkMax = T-MATE.StkMax
        Almmmate.StkRep = T-MATE.StkRep
        Almmmate.Libre_d01 = T-MATE.Libre_d01.
    RELEASE Almmmate.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Venta-Diaria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Venta-Diaria Procedure 
PROCEDURE Venta-Diaria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pRowid AS ROWID.
DEF INPUT PARAMETER pDiasUtiles AS INT.
DEF INPUT PARAMETER pAlmacenes AS CHAR.
DEF OUTPUT PARAMETER pVentaDiaria AS DEC.
DEF OUTPUT PARAMETER pFactor AS DEC.

FIND Almmmate WHERE ROWID(Almmmate) = pRowid
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmate THEN RETURN.

FIND Almmmatg OF Almmmate NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN RETURN.

FIND Almacen WHERE Almacen.codcia = s-codcia
    AND Almacen.codalm = pAlmacenes NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almacen THEN RETURN.

/* determinamos las fechas de venta */
DEF VAR x-FchIni-Ant AS DATE NO-UNDO.
DEF VAR x-FchFin-Ant AS DATE NO-UNDO.
DEF VAR x-FchIni-Hoy AS DATE NO-UNDO.
DEF VAR x-FchFin-Hoy AS DATE NO-UNDO.
DEF VAR x-Venta-Ant AS DEC NO-UNDO.
DEF VAR x-Venta-Hoy AS DEC NO-UNDO.
DEF VAR x-Venta-Mes AS DEC NO-UNDO.

DEF VAR x-Mes AS INT NO-UNDO.
DEF VAR x-Ano AS INT NO-UNDO.
DEF VAR x-Meses AS INT INIT 3 NO-UNDO.      /* Meses de estudio estadístico */

/* 21.08.09 ahora hay que retroceder 36 dias */
x-FchFin-Ant = TODAY - 365.
x-FchIni-Ant = x-FchFin-Ant - x-Meses * 30.

RUN ventas-promedio (x-FchIni-Ant, x-FchFin-Ant, pAlmacenes, Almmmatg.codmat, OUTPUT x-Venta-Ant).

/* 21.08.09 ahora hay que retroceder 90 dias */
x-FchFin-Hoy = TODAY.
x-FchIni-Hoy = x-FchFin-Hoy - x-Meses * 30.

RUN ventas-promedio (x-FchIni-Hoy, x-FchFin-Hoy, pAlmacenes, Almmmatg.codmat, OUTPUT x-Venta-Hoy).

/* 21.08.09 ahora hay que retroceder 365 dias */
/* x-FchFin-Ant = TODAY - 365.       */
/* x-FchIni-Ant = x-FchFin-Ant - 30. */
x-FchIni-Ant = TODAY - 365.
x-FchFin-Ant = x-FchIni-Ant + 30.

RUN ventas-promedio (x-FchIni-Ant, x-FchFin-Ant, pAlmacenes, Almmmatg.codmat, OUTPUT x-Venta-Mes).

IF x-Venta-Ant <= 0 THEN DO:
    pVentaDiaria = x-Venta-Hoy.
    RETURN.
END.
IF ( x-Venta-Hoy / x-Venta-Ant ) > 3 OR ( x-Venta-Hoy / x-Venta-Ant ) < (1 / 3) THEN DO: 
    pVentaDiaria = x-Venta-Hoy.
    RETURN.
END.
IF ( x-Venta-Hoy / x-Venta-Mes ) > 3 OR ( x-Venta-Hoy / x-Venta-Mes ) < (1 / 3) THEN DO: 
    pVentaDiaria = x-Venta-Hoy.
    RETURN.
END.
ASSIGN
    pFactor = x-Venta-Hoy / x-Venta-Ant
    pVentaDiaria = x-Venta-Hoy / x-Venta-Ant * x-Venta-Mes.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Ventas-Promedio) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ventas-Promedio Procedure 
PROCEDURE Ventas-Promedio :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pFchIni AS DATE.
DEF INPUT PARAMETER pFchFin AS DATE.
DEF INPUT PARAMETER pCodAlm AS CHAR.
DEF INPUT PARAMETER pCodMat AS CHAR.
DEF OUTPUT PARAMETER pVtaPromedio AS DEC.


/* CALCULO ESTADISTICO DE LAS VENTAS DIARIAS */
FIND Almacen WHERE Almacen.codcia = s-codcia
    AND Almacen.codalm = pCodAlm
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almacen THEN RETURN.

EMPTY TEMP-TABLE Detalle.
/* PRIMERA PASADA */
ASSIGN
    x-CodMat = pCodMat
    x-FchIni = pFchIni
    x-FchFin = pFchFin
    x-Factor = 1
    pVtaPromedio = 0.
RUN Carga-Ventas.

/* SEGUNDA PASADA: CARGAMOS LAS VENTAS POR EL CODIGO EQUIVALENTE */
FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.codmat = pCodMat
    NO-LOCK.
IF NUM-ENTRIES(Almmmatg.Libre_c03, '|') = 2 THEN DO:
    ASSIGN
        x-Factor = DECIMAL(ENTRY(2, Almmmatg.Libre_c03, '|'))
        x-CodMat = ENTRY(1, Almmmatg.Libre_c03, '|').
    RUN Carga-Ventas.
END.
/* FIND Vtapmatg WHERE Vtapmatg.codcia = s-codcia                                                            */
/*     AND Vtapmatg.codmat = pCodMat                                                                         */
/*     NO-LOCK NO-ERROR.                                                                                     */
/* IF AVAILABLE Vtapmatg AND Vtapmatg.Tipo = "E" AND Vtapmatg.codequ <> "" AND Vtapmatg.factor <> 0 THEN DO: */
/*     ASSIGN                                                                                                */
/*         x-Factor = Vtapmatg.Factor                                                                        */
/*         x-CodMat = Vtapmatg.codequ.                                                                       */
/*     RUN Carga-Ventas.                                                                                     */
/* END.                                                                                                      */

/* Desviacion estandar */
ASSIGN
    x-Items = 0
    x-Promedio = 0.
DO x-Fecha = pFchIni TO pFchFin:
    FIND Detalle WHERE Detalle.fchdoc = x-Fecha NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Detalle AND WEEKDAY(x-Fecha) = 1 THEN NEXT.    /* DOMINGO SIN VENTA */
    x-Items = x-Items + 1.
    IF AVAILABLE Detalle THEN x-Promedio = x-Promedio + Detalle.candes.
END.
x-Promedio = x-Promedio / x-Items.

DO x-Fecha = pFchIni TO pFchFin:
    FIND Detalle WHERE Detalle.fchdoc = x-Fecha NO-LOCK NO-ERROR.
    IF AVAILABLE Detalle 
    THEN x-Desviacion = x-Desviacion + EXP( ( Detalle.CanDes - x-Promedio ) , 2 ).
    ELSE x-Desviacion = x-Desviacion + EXP( ( 0 - x-Promedio ) , 2 ).
END.
x-Desviacion = SQRT ( x-Desviacion / ( x-Items - 1 ) ).

/* Eliminamos los items que están fuera de rango */
FOR EACH Detalle:
    IF Detalle.candes > (x-Promedio + 3 * x-Desviacion) OR Detalle.candes < (x-Promedio - 3 * x-Desviacion) 
    THEN DO:
        DELETE Detalle.
        x-Items = x-Items - 1.
    END.
END.
x-Promedio = 0.
FOR EACH Detalle:
    x-Promedio = x-Promedio + Detalle.candes.
END.
pVtaPromedio = x-Promedio / x-Items.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

