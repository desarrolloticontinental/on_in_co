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

/*DEFINE VAR s-codcia AS INT NO-UNDO.*/

DEFINE VAR iPeriodo AS INT NO-UNDO.
DEFINE VAR fDesde AS DATE NO-UNDO.
DEFINE VAR fHasta AS DATE NO-UNDO.

DEFINE VAR iAgrupador AS INT NO-UNDO.
DEFINE VAR cAgrupador AS CHAR NO-UNDO.
DEFINE VAR cDivisiones AS LONGCHAR NO-UNDO.

DEFINE VAR lMonedaSoles AS LOG NO-UNDO.
DEFINE VAR lIncluidoIGV AS LOG NO-UNDO.

DEFINE VAR lCrecimiento_ventas      AS LOG INIT NO NO-UNDO.
DEFINE VAR lCrecimiento_utilidad    AS LOG INIT NO NO-UNDO.
DEFINE VAR lCrecimiento_qty         AS LOG INIT NO NO-UNDO.
DEFINE VAR lCrecimiento_imp         AS LOG INIT NO NO-UNDO.

DEFINE VAR dFactorVentas            AS DEC INIT 0 NO-UNDO.
DEFINE VAR dFactorUtilidad          AS DEC INIT 0 NO-UNDO.
DEFINE VAR dFactorCantidad          AS DEC INIT 0 NO-UNDO.
DEFINE VAR dFactorImporte           AS DEC INIT 0 NO-UNDO.

DEFINE VAR iMaximoRegistrosParaProbar AS INT INIT 0 NO-UNDO.
DEFINE VAR cPeriodoDescripcion AS CHAR.
DEFINE VAR cCategoriasContables AS CHAR NO-UNDO.

DEFINE VAR cDBname AS CHAR NO-UNDO.

/*s-codcia = 1.*/
lMonedaSoles = YES.
lIncluidoIGV = NO.
cAgrupador = "".
iPeriodo = -1.
iAgrupador = -1.
cDivisiones = "".
cDBname = "ContiProductivo".

iMaximoRegistrosParaProbar = 0. /* 15000.        0:Todos */

DEFINE TEMP-TABLE tvtas_cab LIKE ventas_cabecera.
DEFINE TEMP-TABLE tvtas_det LIKE ventas_detalle.
DEFINE TEMP-TABLE tvtas_cab_ant LIKE ventas_cabecera.
DEFINE TEMP-TABLE tvtas_det_ant LIKE ventas_detalle.

DEFINE TEMP-TABLE tclf_acumulador LIKE clf_acumulador
    FIELD descripcion AS CHAR FORMAT 'x(60)'.

DEFINE TEMP-TABLE tclf_calculo
    FIELD codmat    AS  CHAR    FORMAT 'x(8)' INIT ""
    FIELD cdescripcion AS CHAR    FORMAT 'x(100)' INIT ""
    FIELD cmarca AS CHAR    FORMAT 'x(50)' INIT ""
    FIELD cundstk AS CHAR    FORMAT 'x(10)' INIT ""
    FIELD dventas   AS  DEC     FORMAT '->>,>>>,>>>,>>9.9999' INIT 0 DECIMALS 4
    FIELD dutilidad   AS  DEC     FORMAT '->>,>>>,>>>,>>9.9999' INIT 0 DECIMALS 4 
    FIELD dcantidad_act   AS  DEC     FORMAT '->>,>>>,>>>,>>9.9999' INIT 0 DECIMALS 4
    FIELD dimporte_act   AS  DEC     FORMAT '->>,>>>,>>>,>>9.9999' INIT 0 DECIMALS 4
    FIELD dcantidad_ant   AS  DEC     FORMAT '->>,>>>,>>>,>>9.9999' INIT 0 DECIMALS 4
    FIELD dimporte_ant   AS  DEC     FORMAT '->>,>>>,>>>,>>9.9999' INIT 0 DECIMALS 4
    FIELD dcantidad   AS  DEC     FORMAT '->>,>>>,>>>,>>9.9999' INIT 0 DECIMALS 4
    FIELD dimporte   AS  DEC     FORMAT '->>,>>>,>>>,>>9.9999' INIT 0 DECIMALS 4
    FIELD dpuntajeventas   AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dpuntajeutilidad   AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dpuntajecantidad   AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dpuntajeimporte   AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dsumapuntaje          AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dpesopuntaje          AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dpesoventas           AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dpesoutilidad         AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dpesocantidad         AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dpesoimporte          AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6 
    FIELD dpesoacumulado        AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD dpesopuntajefinal     AS  DEC     FORMAT '->>,>>>,>>>,>>9.999999' INIT 0 DECIMALS 6
    FIELD cclasificacion AS CHAR FORMAT 'x(5)'
    FIELD cOrigen AS CHAR FORMAT 'x(50)'
    INDEX idx01 codmat
    INDEX idx02 dpesoacumulado DESC.

DEFINE TEMP-TABLE tclf_calculo_art NO-UNDO
    FIELD codmat    AS  CHAR    FORMAT 'x(8)' INIT ""
    FIELD sipasa    AS  CHAR    FORMAT 'x(1)' INIT ""
    INDEX idx01 codmat.

DEFINE TEMP-TABLE tclf_calculo_ant NO-UNDO LIKE tclf_calculo .

/*

1.-
  ventas1 = suma de las ventas del periodo
  ordenamos de mayor a menor
  
  Ventas
  codmat    ImpVtas factor      factor1 peso
  00001     2000    2000/2000   1       1 * 0.50
  00002     1800    1800/2000   0.9     0.9 * 0.50
  00001     200     200/2000    0.1     0.1 * 0.50
  
  
  Utilidad        
  codmat    Utilidad    factor      factor1 peso
  00001     800         800/800     1       1 * 0.45
  00002     400         400/800     0.9     0.9 * 0.45
  00001     1500        150/800     0.1     0.1 * 0.45
  
  asi con Cantidad o Importe
    
2.-   
  Peso acumulado = ventas.peso + utilidad.peso + cantidad y/o importe
  
*/


DEFINE IMAGE IMAGE-1 FILENAME "IMG\Coti.ico" SIZE 12 BY 1.5.
DEF VAR FI-MENSAJE AS CHAR FORMAT "X(60)" .

DEFINE FRAME F-Proceso
    IMAGE-1 AT ROW 1.5 COL 5
    "Espere un momento" VIEW-AS TEXT
    SIZE 18 BY 1 AT ROW 1.5 COL 16 FONT 6
    "por favor ...." VIEW-AS TEXT
    SIZE 10 BY 1 AT ROW 2.5 COL 19 FONT 6
    SKIP
    Fi-Mensaje NO-LABEL FONT 6
    SKIP     
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
    BGCOLOR 15 FGCOLOR 0 
    TITLE "Procesando ..." FONT 7.

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
         HEIGHT             = 11.08
         WIDTH              = 58.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-calcular_clasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcular_clasificacion Procedure 
PROCEDURE calcular_clasificacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER piPeriodo AS INT NO-UNDO.        /* Clf_Periodos */
DEFINE INPUT PARAMETER piAgrupador AS INT NO-UNDO.      /* Clf_Agrupador */
DEFINE INPUT PARAMETER plIncluyeIGV AS LOG NO-UNDO.     /* NO: debe ser por defecto */
DEFINE INPUT PARAMETER plMonedaSoles AS LOG NO-UNDO.    /* YES: debe ser por defecto */
DEFINE OUTPUT PARAMETER pcRetVal AS CHAR NO-UNDO.

IF plIncluyeIGV = ?     THEN plIncluyeIGV = NO.
IF plMonedaSoles = ?    THEN plMonedaSoles = YES.

DEFINE VAR dSumaAcumuladores AS DEC NO-UNDO.

iPeriodo = piPeriodo.
iAgrupador = piAgrupador.
fDesde = ?.
fHasta = ?.
pcRetVal = "".
cAgrupador = "".
lMonedaSoles = plMonedaSoles.
lIncluidoIGV = plIncluyeIGV.

/* ******************************************************************************** */
/* Verificar acumuladores */
/* ******************************************************************************** */
RUN valida_acumuladores(OUTPUT pcRetVal).
IF NOT (TRUE <> (pcRetVal > "")) THEN DO:
    RETURN "ADM-ERROR".
END.
/* ******************************************************************************** */
/* Verificar Categorias contables */
/* ******************************************************************************** */
RUN categ_contable_consideradas(OUTPUT cCategoriasContables).
IF TRUE <> (cCategoriasContables > "") THEN DO:
    pcRetVal = "Debe configurar al menos una Categor�a Contable a considerar".
    RETURN "ADM-ERROR".
END.
/* ******************************************************************************** */
/* Validar el periodo (campa�a) */
/* ******************************************************************************** */
RUN valida_periodo(OUTPUT pcRetVal).
IF NOT (TRUE <> (pcRetVal > "")) THEN DO:
    RETURN "ADM-ERROR".
END.
/* ******************************************************************************** */
/* Validar agrupador */
/* ******************************************************************************** */
RUN valida_agrupador(OUTPUT pcRetVal).
IF NOT (TRUE <> (pcRetVal > "")) THEN DO:
    RETURN "ADM-ERROR".
END.
/* ******************************************************************************** */

/* Conectar a Base de datos productivo */
RUN conectar_dbproductivo.
IF RETURN-VALUE = "ADM-ERROR" THEN DO:
    pcRetVal = "No se pudo conectar a la base de datos productivo Continental".
    RETURN "ADM-ERROR".
END.

/* ******************************************************************************** */
/* LOGICA PRINCIPAL */
/* ******************************************************************************** */
RUN calculo_procesar.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN lib\Tools-to-excel PERSISTENT SET hProc.

def var c-csv-file as char no-undo.
def var c-xls-file as char no-undo. /* will contain the XLS file path created */

c-xls-file = 'd:\PruebaClasificacionArticulos_' + cPeriodoDescripcion + ".xlsx".

run pi-crea-archivo-csv IN hProc (input  buffer tclf_calculo:handle,
                        /*input  session:temp-directory + "file"*/ c-xls-file,
                        output c-csv-file) .

run pi-crea-archivo-xls  IN hProc (input  buffer tclf_calculo:handle,
                        input  c-csv-file,
                        output c-xls-file) .

DELETE PROCEDURE hProc.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calculo_acumuladores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo_acumuladores Procedure 
PROCEDURE calculo_acumuladores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR dImpVentas AS DEC NO-UNDO.
DEFINE VAR dImpMargen AS DEC NO-UNDO.
DEFINE VAR dQtyCrecimiento AS DEC NO-UNDO.
DEFINE VAR dImpCrecimiento AS DEC NO-UNDO.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula acumuladores - Periodo Actual").

DEF VAR iContador AS INTE INIT 0 NO-UNDO.
/* Periodo ACTUAL */
FOR EACH tvtas_det NO-LOCK:
    dQtyCrecimiento = 0.
    dImpCrecimiento = 0.
    IF lMonedaSoles THEN DO:
        dImpVentas = IF (lIncluidoIgv) THEN tvtas_det.impnaccigv ELSE tvtas_det.impnacsigv.
        dImpMargen = IF (lIncluidoIgv) THEN tvtas_det.promnaccigv ELSE tvtas_det.promnacsigv.        
    END.
    ELSE DO:
        dImpVentas = IF (lIncluidoIgv) THEN tvtas_det.impextcigv ELSE tvtas_det.impextsigv.
        dImpMargen = IF (lIncluidoIgv) THEN tvtas_det.promextcigv ELSE tvtas_det.promextsigv.
    END.
    dImpMargen = dImpVentas - dImpMargen.
    /**/
    IF lCrecimiento_qty THEN DO:
        dQtyCrecimiento = tvtas_det.cantidad.
    END.
    IF lCrecimiento_imp THEN DO:
        dImpCrecimiento = dImpVentas.
    END.
    /**/
    IF iContador MODULO 1000 = 0
        THEN DISPLAY "Calcula ACUMULADORES PERIODO ACTUAL " + tvtas_det.codmat @ Fi-Mensaje WITH FRAME F-Proceso.
    iContador = iContador + 1.

    FIND FIRST tclf_calculo WHERE tclf_calculo.codmat = tvtas_det.codmat EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE tclf_calculo THEN DO:
        CREATE tclf_calculo.
        ASSIGN 
            tclf_calculo.codmat = tvtas_det.codmat
            tclf_calculo.cOrigen = "PERIODO ACTUAL".
    END.
    /* Importes/Cantidades */
    ASSIGN 
        tclf_calculo.dventas = tclf_calculo.dventas + dImpVentas
        tclf_calculo.dutilidad = tclf_calculo.dutilidad + dImpMargen
        tclf_calculo.dcantidad_act = tclf_calculo.dcantidad_act + dQtyCrecimiento
        tclf_calculo.dimporte_act = tclf_calculo.dimporte_act + dImpCrecimiento.
END.
HIDE FRAME F-Proceso.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula acumuladores - Periodo Anterior").
/* Periodo ANTERIOR */
iContador = 0.
FOR EACH tvtas_det_ant NO-LOCK:
    dQtyCrecimiento = 0.
    dImpCrecimiento = 0.
    IF lCrecimiento_imp THEN DO:
        IF lMonedaSoles THEN DO:
            dImpCrecimiento = IF (lIncluidoIgv) THEN tvtas_det_ant.impnaccigv ELSE tvtas_det_ant.impnacsigv.
        END.
        ELSE DO:
            dImpCrecimiento = IF (lIncluidoIgv) THEN tvtas_det_ant.impextcigv ELSE tvtas_det_ant.impextsigv.
        END.
    END.
    /**/
    IF lCrecimiento_qty THEN DO:
        dQtyCrecimiento = tvtas_det_ant.cantidad.
    END.
    /**/

    IF dQtyCrecimiento = 0 AND dImpCrecimiento = 0  THEN NEXT.
    IF iContador MODULO 1000 = 0
        THEN DISPLAY "Calcula ACUMULADORES PERIODO ANTERIOR " + tvtas_det_ant.codmat @ Fi-Mensaje WITH FRAME F-Proceso.
    iContador = iContador + 1.

    FIND FIRST tclf_calculo WHERE tclf_calculo.codmat = tvtas_det_ant.codmat EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE tclf_calculo THEN DO:
        CREATE tclf_calculo.
        ASSIGN 
            tclf_calculo.codmat = tvtas_det_ant.codmat
            tclf_calculo.cOrigen = "PERIODO ANTERIOR".
    END.
    /* Importes/Cantidades */
    ASSIGN  
        tclf_calculo.dcantidad_ant = tclf_calculo.dcantidad_ant + dQtyCrecimiento
        tclf_calculo.dimporte_ant = tclf_calculo.dimporte_ant + dImpCrecimiento.
END.
HIDE FRAME F-Proceso.
RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula acumuladores - TERMINO").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calculo_clasificacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo_clasificacion Procedure 
PROCEDURE calculo_clasificacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR iNroClasificaciones AS INT INIT 0 NO-UNDO.
DEFINE VAR iSec AS INT INIT 0 NO-UNDO.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula clasificacion - ordenando valores").
FOR EACH clf_Clasificaciones NO-LOCK:
    iNroClasificaciones = iNroClasificaciones + 1.
END.

DEFINE VAR dClasificacionValor AS DECIMAL EXTENT.
DEFINE VAR cClasificacionValor AS CHAR EXTENT.
EXTENT(dClasificacionValor) =  iNroClasificaciones.
EXTENT(cClasificacionValor) =  iNroClasificaciones.

iNroClasificaciones = 0.
FOR EACH clf_Clasificaciones NO-LOCK BY valor:
    iNroClasificaciones = iNroClasificaciones + 1.
    dClasificacionValor[iNroClasificaciones] = clf_Clasificaciones.valor.
    cClasificacionValor[iNroClasificaciones] = clf_Clasificaciones.codigo.
END.

DEFINE VAR dValorInicial AS DEC.
DEFINE VAR cClasificacion AS CHAR.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula clasificacion - calculando").
DEF VAR iContador AS INTE INIT 0 NO-UNDO.
FOR EACH tclf_calculo:
    IF iContador MODULO 1000 = 0
        THEN DISPLAY "Calcula CLASIFICACION " + tclf_calculo.codmat @ Fi-Mensaje WITH FRAME F-Proceso.
    iContador = iContador + 1.
    cClasificacion = "X".
    dValorInicial = 0.
    BUSCAR:
    REPEAT iSec = 1 TO iNroClasificaciones:
        IF tclf_calculo.dpesopuntajefinal > dValorInicial AND tclf_calculo.dpesopuntajefinal <= dClasificacionValor[iSec] THEN DO:
            cClasificacion = cClasificacionValor[iSec].
            LEAVE BUSCAR.
        END.
    END.
    ASSIGN tclf_calculo.cClasificacion = cClasificacion.
    /* */
END.
HIDE FRAME F-Proceso.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calculo_peso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo_peso Procedure 
PROCEDURE calculo_peso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR dTotalVentas AS DEC.
DEFINE VAR dTotalUtilidad AS DEC.
DEFINE VAR dTotalCantidad AS DEC.
DEFINE VAR dTotalImporte AS DEC.

DEFINE VAR dSumaPuntajes AS DEC.
DEFINE VAR dValorAnterior AS DEC.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula pesos - calcular puntajes y totalizar").
DEF VAR iContador AS INTE INIT 0 NO-UNDO.
FOR EACH tclf_calculo EXCLUSIVE-LOCK:
    IF iContador MODULO 1000 = 0
        THEN DISPLAY "Calcula pesos - calcular puntajes y totalizar " + tclf_calculo.codmat @ Fi-Mensaje WITH FRAME F-Proceso.
    iContador = iContador + 1.
    ASSIGN 
        tclf_calculo.dcantidad = (tclf_calculo.dcantidad_act - tclf_calculo.dcantidad_ant)
        tclf_calculo.dimporte = (tclf_calculo.dimporte_act - tclf_calculo.dimporte_ant).

    ASSIGN  
        tclf_calculo.dpuntajeventas = tclf_calculo.dventas * dFactorVentas
        tclf_calculo.dpuntajeutilidad = tclf_calculo.dutilidad * dFactorUtilidad
        tclf_calculo.dpuntajecantidad = tclf_calculo.dcantidad * dFactorCantidad
        tclf_calculo.dpuntajeimporte = tclf_calculo.dimporte * dFactorImporte
        tclf_calculo.dsumapuntaje = tclf_calculo.dpuntajeventas + 
                                    tclf_calculo.dpuntajeutilidad + 
                                    tclf_calculo.dpuntajecantidad + 
                                    tclf_calculo.dpuntajeimporte.
    dSumaPuntajes = dSumaPuntajes + tclf_calculo.dsumapuntaje.
END.
HIDE FRAME F-Proceso.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula pesos - calcular peso x articulo").
iContador = 0.
FOR EACH tclf_calculo EXCLUSIVE-LOCK:
    IF iContador MODULO 1000 = 0
        THEN DISPLAY "Calcula pesos - calcular peso x articulo " + tclf_calculo.codmat @ Fi-Mensaje WITH FRAME F-Proceso.
    iContador = iContador + 1.
    ASSIGN 
        tclf_calculo.dpesoventas = IF (dSumaPuntajes > 0)   THEN ROUND(tclf_calculo.dpuntajeventas / dSumaPuntajes,6)   ELSE 0
        tclf_calculo.dpesoutilidad = IF (dSumaPuntajes > 0) THEN ROUND(tclf_calculo.dpuntajeUtilidad / dSumaPuntajes,6) ELSE 0
        tclf_calculo.dpesocantidad = IF (dSumaPuntajes > 0) THEN ROUND(tclf_calculo.dpuntajeCantidad / dSumaPuntajes,6) ELSE 0
        tclf_calculo.dpesoimporte = IF (dSumaPuntajes > 0)  THEN ROUND(tclf_calculo.dpuntajeImporte / dSumaPuntajes,6)  ELSE 0
        tclf_calculo.dpesoacumulado = + (tclf_calculo.dpesoventas + tclf_calculo.dpesoutilidad + tclf_calculo.dpesoCantidad + tclf_calculo.dpesoimporte).
    ASSIGN 
        tclf_calculo.dpesopuntaje = IF (dSumaPuntajes > 0) THEN round(tclf_calculo.dsumapuntaje / dSumaPuntajes,6) ELSE 0.
END.
HIDE FRAME F-Proceso.

dValorAnterior = 0.
iContador = 0.
FOR EACH tclf_calculo EXCLUSIVE-LOCK BY tclf_calculo.dsumapuntaje DESC:
    IF iContador MODULO 1000 = 0
        THEN DISPLAY "Calcula pesos - puntaje final " + tclf_calculo.codmat @ Fi-Mensaje WITH FRAME F-Proceso.
    iContador = iContador + 1.
    ASSIGN tclf_calculo.dpesopuntajefinal = tclf_calculo.dpesopuntaje + dValorAnterior.
    IF tclf_calculo.dpesopuntaje <= 0 OR tclf_calculo.dpesopuntajefinal > 1 THEN DO:
        ASSIGN tclf_calculo.dpesopuntajefinal = 1.
        dValorAnterior = 0.
        IF tclf_calculo.dpesopuntajefinal >= 1 THEN dValorAnterior = 99.
    END.
    ELSE DO:
        dValorAnterior = tclf_calculo.dpesopuntajefinal.
    END.
END.
HIDE FRAME F-Proceso.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calculo_procesar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo_procesar Procedure 
PROCEDURE calculo_procesar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR fDesdeAnt AS DATE NO-UNDO.
DEFINE VAR fHastaAnt AS DATE NO-UNDO.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","INICIO").

IF lCrecimiento_ventas = YES OR lCrecimiento_utilidad = YES OR lCrecimiento_Qty = YES OR lCrecimiento_Imp = YES THEN DO:
    /* VENTAS */
    /* Data del periodo actual */
    RUN lib/p-write-log-txt.r("Clasificacion Articulos","Data ventas periodo ACTUAL").  
    RUN extrae_ventas(fDesde, fHasta, YES).

    /* Data del periodo anterior */
    RUN lib/p-write-log-txt.r("Clasificacion Articulos","Data ventas periodo ANTERIOR").
    fDesdeAnt = ADD-INTERVAL(fDesde,-1,'year').
    fHastaAnt = ADD-INTERVAL(fHasta,-1,'year').
    RUN extrae_ventas(fDesdeAnt, fHastaAnt, NO).

    /* ACUMULADORES */
    RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula ACUMULADORES").
    RUN calculo_acumuladores.
END.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula PESOS").
RUN calculo_peso.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Calcula CLASIFICACION").
RUN calculo_clasificacion.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","PRODUCTOS SIN VENTAS").
RUN calculo_productos_sin_ventas.

IF CONNECTED(cDBname) THEN DISCONNECT VALUE(cDBname) NO-ERROR.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","FIN").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-calculo_productos_sin_ventas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calculo_productos_sin_ventas Procedure 
PROCEDURE calculo_productos_sin_ventas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR cDescrip AS CHAR.
DEFINE VAR cDesmar AS CHAR.
DEFINE VAR cCtgCtble AS CHAR.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Productos sin ventas - descripcion articulos").
FOR EACH tclf_calculo:
    /* */
    FIND FIRST DimProducto WHERE DimProducto.codmat = tclf_calculo.codmat NO-LOCK NO-ERROR.
    IF AVAILABLE DimProducto THEN DO:
        RUN lib/limpiar-texto.r(DimProducto.desmat," ",OUTPUT cDescrip).
        RUN lib/limpiar-texto.r(DimProducto.desmar," ",OUTPUT cMarca).

        ASSIGN 
            tclf_calculo.cdescripcion = replace(replace(REPLACE(cDescrip,";"," "),'"',' '),"'"," ")
            tclf_calculo.cmarca = replace(replace(REPLACE(cDesmar,";"," "),'"',' '),"'"," ")
            tclf_calculo.cundstk = DimProducto.undstk.
    END.
END.

RUN lib/p-write-log-txt.r("Clasificacion Articulos","Productos sin ventas - inexistentes").
DEF VAR iContador AS INTE INIT 0 NO-UNDO.
FOR EACH DimProducto NO-LOCK:
    IF DimProducto.tpoart <> 'A' THEN NEXT.
    cCtgCtble = DimProducto.CtgCtble.

    /* Aqui se validara las categorias contables ????????????????? */
    IF LOOKUP(cCtgCtble,cCategoriasContables) = 0 THEN NEXT.

    FIND FIRST tclf_calculo WHERE DimProducto.codmat = tclf_calculo.codmat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tclf_calculo THEN DO:
        IF iContador MODULO 1000 = 0
            THEN DISPLAY "PRODUCTOS SIN VENTAS " + DimProducto.codmat @ Fi-Mensaje WITH FRAME F-Proceso.
        iContador = iContador + 1.


        CREATE tclf_calculo.
        ASSIGN 
            tclf_calculo.codmat = DimProducto.codmat
            tclf_calculo.cOrigen = "SIN VENTAS"
            tclf_calculo.cClasificacion = "F".

        RUN lib/limpiar-texto.r(DimProducto.desmat," ",OUTPUT cDescrip).
        RUN lib/limpiar-texto.r(DimProducto.desmar," ",OUTPUT cMarca).

        ASSIGN 
            tclf_calculo.cdescripcion = replace(replace(REPLACE(cDescrip,";"," "),'"',' '),"'"," ")
            tclf_calculo.cmarca = replace(replace(REPLACE(cDesmar,";"," "),'"',' '),"'"," ")
            tclf_calculo.cundstk = DimProducto.undstk.
    END.
END.
HIDE FRAME F-Proceso.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-categ_contable_consideradas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE categ_contable_consideradas Procedure 
PROCEDURE categ_contable_consideradas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER xCategoriasContables AS CHAR NO-UNDO.

xCategoriasContables = "".
FOR EACH DimClfCatContable NO-LOCK:
    IF NOT (TRUE <> (xCategoriasContables > "")) THEN xCategoriasContables = xCategoriasContables + ",".
    xCategoriasContables = xCategoriasContables + DimClfCatContable.codigo.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-conectar_dbproductivo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE conectar_dbproductivo Procedure 
PROCEDURE conectar_dbproductivo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RETURN.

IF NOT CONNECTED(cDBname) THEN DO:
    DEFINE VAR cStrConnect AS CHAR.

    cStrConnect = "-db integral -ld " + cDBname + " -H 192.168.100.210 -S  65010".

    CONNECT VALUE(cStrConnect) NO-ERROR.

    IF ERROR-STATUS:ERROR  THEN DO:        
        
        MESSAGE "ERROR CONECCION" SKIP
            ERROR-STATUS:GET-MESSAGE(1 ).
        
        RETURN "ADM-ERROR".
    END.
    ELSE DO:
        RETURN "OK".
    END.
END.

RETURN "OK".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-extrae_ventas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extrae_ventas Procedure 
PROCEDURE extrae_ventas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pdDesde AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pdHasta AS DATE NO-UNDO.
DEFINE INPUT PARAMETER plTabla AS LOGICAL NO-UNDO.         /* Yes: tabla ventas periodo actual */

DEFINE VAR cCodDiv AS CHAR NO-UNDO.
DEFINE VAR iConteo AS INT NO-UNDO.
DEFINE VAR cCtgCtble AS CHAR.

IF plTabla = YES THEN DO:
    EMPTY TEMP-TABLE tvtas_cab.
    EMPTY TEMP-TABLE tvtas_det.
END.
ELSE DO:
    EMPTY TEMP-TABLE tvtas_cab_ant.
    EMPTY TEMP-TABLE tvtas_det_ant.
END.

iConteo = 0.

DEF VAR iContador AS INTE INIT 0 NO-UNDO.

VENTAS:
FOR EACH ventas_detalle 
    WHERE ventas_detalle.datekey >= pdDesde 
    AND ventas_detalle.datekey <= pdHasta NO-LOCK,
    FIRST DimProducto NO-LOCK WHERE DimProducto.codmat = Ventas_Detalle.codmat:

    /* 1) Por las divisiones v�lidas */
    IF cDivisiones <> "*" THEN DO:
        cCodDiv = TRIM(ventas_detalle.coddiv).
        IF LOOKUP(cCodDiv, cDivisiones) = 0 THEN NEXT.        
    END.

    /* 2) Por el art�culo */
    IF DimProducto.tpoart <> 'A' THEN NEXT.
    cCtgCtble = DimProducto.CtgCtble.
    IF LOOKUP(cCtgCtble,cCategoriasContables) = 0 THEN NEXT.

    IF plTabla = YES THEN DO:
        IF iContador MODULO 1000 = 0
            THEN DISPLAY "Data ventas periodo ACTUAL " + STRING(ventas_detalle.datekey,'99/99/9999') + ' ' + Ventas_Detalle.codmat @ Fi-Mensaje WITH FRAME F-Proceso.
        CREATE tvtas_det.
        BUFFER-COPY ventas_detalle TO tvtas_det.
    END.
    ELSE DO:
        IF iContador MODULO 1000 = 0
            THEN DISPLAY "Data ventas periodo ANTERIOR " + STRING(ventas_detalle.datekey,'99/99/9999') + ' ' + Ventas_Detalle.codmat @ Fi-Mensaje WITH FRAME F-Proceso.
        CREATE tvtas_det_ant.
        BUFFER-COPY ventas_detalle TO tvtas_det_ant.
    END.
    iContador = iContador + 1.

    IF iMaximoRegistrosParaProbar > 0 THEN DO:
        iConteo = iConteo + 1.
        IF iConteo >= iMaximoRegistrosParaProbar THEN LEAVE VENTAS.
    END.
END.
HIDE FRAME F-Proceso.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valida_acumuladores) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida_acumuladores Procedure 
PROCEDURE valida_acumuladores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER cRetVal AS CHAR NO-UNDO.

DEFINE VAR dSumaAcumuladores AS DEC NO-UNDO.
DEFINE VAR cDescripcion AS CHAR NO-UNDO.

dSumaAcumuladores = 0.
cRetVal = "".

EMPTY TEMP-TABLE tclf_acumulador.

VALIDA:
FOR EACH clf_acumulador NO-LOCK:
    dSumaAcumuladores = dSumaAcumuladores + clf_acumulador.peso.
    CREATE tclf_acumulador.
    BUFFER-COPY clf_acumulador TO tclf_acumulador.
    cDescripcion = "NO EXISTE ACUMULADOR (" + STRING(clf_acumulador.id_clfacumulador) + ")".    
    FIND FIRST DimClfAcumulador WHERE DimClfAcumulador.id_ClfAcumulador = clf_acumulador.id_clfacumulador NO-LOCK NO-ERROR.
    IF AVAILABLE DimClfAcumulador THEN DO:        
        IF TRUE <> (DimClfAcumulador.descripcion > "") THEN DO:
            cDescripcion = "ACUMULADOR (" + STRING(clf_acumulador.id_clfacumulador) + ") NO TIENE DESCRIPCION".                
        END.
        ELSE DO:
            ASSIGN tclf_acumulador.descripcion = DimClfAcumulador.descripcion.
            cDescripcion = "".
        END.
    END.
    IF NOT (TRUE <> (cDescripcion > "")) THEN DO:
        /* En caso no exista el acumulador o no tenga descripci�n */
        cRetVal = cDescripcion.
        LEAVE VALIDA.
    END.
    /* Por defecto los valores est�n en NO y los factores en 0 (cero) */
    CASE TRIM(DimClfAcumulador.descripcion):
        WHEN "VENTAS" THEN DO:
            IF clf_acumulador.peso > 0 THEN lCrecimiento_ventas = YES.
            dFactorVentas = clf_acumulador.peso.
        END.
        WHEN "UTILIDAD" THEN DO:
            IF clf_acumulador.peso > 0 THEN lCrecimiento_utilidad = YES. 
            dFactorUtilidad = clf_acumulador.peso.
        END.
        WHEN "CRECIMIENTO_CANTIDAD" THEN DO:
            IF clf_acumulador.peso > 0 THEN lCrecimiento_qty = YES.
            dFactorCantidad = clf_acumulador.peso.
        END.
        WHEN "CRECIMIENTO_IMPORTE" THEN DO:
            IF clf_acumulador.peso > 0 THEN lCrecimiento_imp = YES.
            dFactorImporte = clf_acumulador.peso.
        END.
    END CASE.
END.

IF TRUE <> (cRetVal > "") THEN DO:
    /* NO hay errores */
    IF dSumaAcumuladores <> 1 THEN DO:
        cRetVal = "La suma de los acumuladores es diferente de 1 (100%)".
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valida_agrupador) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida_agrupador Procedure 
PROCEDURE valida_agrupador :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER cRetVal AS CHAR NO-UNDO.

FIND FIRST clf_agrupador WHERE clf_agrupador.id_agrupador = iAgrupador NO-LOCK NO-ERROR.
IF NOT AVAILABLE clf_agrupador THEN DO:
    cRetVal = "El Id del agrupador no existe (" + STRING(iAgrupador) + ")".
    RETURN "ADM-ERROR".
END.
IF TRUE <> (clf_agrupador.divisiones > "") THEN DO:
    cRetVal = "El agrupador (" + STRING(iAgrupador) + " - " + clf_agrupador.codigo + ") no tiene divisiones asignadas".
    RETURN "ADM-ERROR".
END.

cAgrupador = clf_agrupador.codigo.
cDivisiones = TRIM(clf_agrupador.divisiones).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-valida_periodo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida_periodo Procedure 
PROCEDURE valida_periodo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER cRetVal AS CHAR NO-UNDO.

FIND FIRST clf_periodos WHERE clf_periodos.id_periodo = iPeriodo NO-LOCK NO-ERROR.
IF NOT AVAILABLE clf_periodos THEN DO:
    cRetVal = "El Id del periodo no existe (" + STRING(iPeriodo) + ")".
    RETURN "ADM-ERROR".
END.
IF clf_periodos.fecha_inicio = ? OR clf_periodos.fecha_termino = ? THEN DO:
    cRetVal = "Las fechas estan erradas del periodo (" + STRING(iPeriodo) + " - " + clf_periodos.descripcion + ")".
    RETURN "ADM-ERROR".
END.
IF clf_periodos.fecha_inicio > clf_periodos.fecha_termino THEN DO:
    cRetVal = "La fecha de inicio debe ser menor/igual a termino - Periodo("  + STRING(iPeriodo) + " - " + clf_periodos.descripcion +  ")".
    RETURN "ADM-ERROR".
END.

fDesde = clf_periodos.fecha_inicio.
fHasta = clf_periodos.fecha_termino.

cPeriodoDescripcion = TRIM(clf_periodos.descripcion).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

