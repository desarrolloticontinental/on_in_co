&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER COTIZACION FOR FacCPedi.
DEFINE BUFFER PEDIDO FOR FacCPedi.



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

/*
    RUN D:\newsie\on_in_co\Batchs\qdwhtransaccion.p
*/

DEF VAR x-CodFchI AS DATE NO-UNDO.
DEF VAR x-CodFchF AS DATE NO-UNDO.
DEF VAR x-PorIgv AS DEC DECIMALS 4 NO-UNDO.
DEF VAR x-fechaInicial AS DATE.
DEF VAR x-fechaFinal AS DATE.

x-fechaInicial = DATE(8,01,2024).    /* Mes, Dia, Año */
x-fechaFinal = DATE(8,31,2024).

ASSIGN
    x-CodFchF = x-fechaInicial
    x-CodFchI = ADD-INTERVAL(x-fechainicial, -5, "days").

    x-CodFchI = x-fechaInicial.
    x-CodFchF = x-fechaFinal.


DEF BUFFER B-CDOCU FOR CcbCdocu.
DEF BUFFER B-DDOCU FOR CcbDdocu.

DEFINE TEMP-TABLE tTransaccionVentas
    FIELD codcomprobante    AS CHAR FORMAT 'x(5)'   COLUMN-LABEL "cod.comprobante"
    FIELD nrocomprobante    AS CHAR FORMAT 'x(15)'   COLUMN-LABEL "cod.comprobante"
    FIELD fechaventa        AS CHAR COLUMN-LABEL "fecha venta"
    FIELD codcliente        AS CHAR FORMAT 'x(15)'   COLUMN-LABEL "cod.cliente"
    FIELD divisionventa     AS CHAR FORMAT 'x(8)'   COLUMN-LABEL "division venta"
    FIELD codigoarticulo    AS CHAR FORMAT 'x(8)'   COLUMN-LABEL "cod.articulo"
    FIELD codigolinea       AS CHAR FORMAT 'x(5)'   COLUMN-LABEL "cod.linea"
    FIELD codigosublinea    AS CHAR FORMAT 'x(5)'   COLUMN-LABEL "cod.Sublinea"
    FIELD cantidad          AS DEC  COLUMN-LABEL "cantidad" DECIMALS 4
    FIELD preciounitario    AS DEC  COLUMN-LABEL "P.Unitario" DECIMALS 4
    FIELD importedescuento  AS DEC  COLUMN-LABEL "Imp.Dscto" DECIMALS 4
    FIELD importeigv        AS DEC  COLUMN-LABEL "Imp.IGV" DECIMALS 4
    FIELD importevalorventa AS DEC  COLUMN-LABEL "Imp.valor venta" DECIMALS 4
    FIELD importeventa      AS DEC  COLUMN-LABEL "Imp.venta" DECIMALS 4
    FIELD codigomarca       AS CHAR FORMAT 'x(5)'   COLUMN-LABEL "cod.marca"
    FIELD condicionventa    AS CHAR FORMAT 'x(5)'   COLUMN-LABEL "cond.vta"
    FIELD codigolicencia    AS CHAR FORMAT 'x(5)'   COLUMN-LABEL "cod.licencia"
    FIELD divisiondespacho  AS CHAR FORMAT 'x(8)'   COLUMN-LABEL "div.despacho"
    FIELD pesounitario      AS DEC  COLUMN-LABEL "Peso Unitario" DECIMALS 4
    FIELD pesototal         AS DEC  COLUMN-LABEL "Peso total" DECIMALS 4
    FIELD volunitario      AS DEC  COLUMN-LABEL "Volumen Unitario" DECIMALS 4
    FIELD voltotal         AS DEC  COLUMN-LABEL "Volumen total" DECIMALS 4
    FIELD costokardexunitario      AS DEC  COLUMN-LABEL "CostoKardex Unitario" DECIMALS 4
    FIELD costokardextotal      AS DEC  COLUMN-LABEL "CostoKardex Total" DECIMALS 4
    FIELD anio              AS INT  COLUMN-LABEL "Año"
    FIELD mes               AS INT  COLUMN-LABEL "Mes"
    FIELD flgend  AS CHAR FORMAT 'x(8)'   COLUMN-LABEL "FIN"
    .

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
   Temp-Tables and Buffers:
      TABLE: COTIZACION B "?" ? INTEGRAL FacCPedi
      TABLE: PEDIDO B "?" ? INTEGRAL FacCPedi
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 19.04
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* INFORMACION DETALLADA Y DEPURADA */
PUT 'inicio del proceso: ' x-CodFchI ' ' x-CodFchF SKIP.

PUT 'carga ventas: ' NOW SKIP. 
PAUSE 0.
RUN carga-transaccion-ventas.

/*
PUT 'pasa estadisticas' NOW SKIP.
PAUSE 0.
DEF STREAM Reporte.
RUN pasa-estadisticas.
*/

PUT '** fin del proceso ** ' NOW SKIP. 
PAUSE 0.

QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-carga-transaccion-ventas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-transaccion-ventas Procedure 
PROCEDURE carga-transaccion-ventas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR cCodComprobantes AS CHAR.
DEFINE VAR cCodDoc AS CHAR.
DEFINE VAR iContador AS INT.
DEFINE VAR x-Archivo AS CHAR.

DEFINE VAR cFecha AS CHAR.
DEFINE VAR cFile AS CHAR.

cCodComprobantes = "FAC,BOL,N/C".

REPEAT iContador = 1 TO NUM-ENTRIES(cCodComprobantes,","):
    cCodDoc = ENTRY(iContador,cCodComprobantes,",").
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 1:
        FOR EACH CcbCdocu WHERE CcbCdocu.CodCia = 1
            AND CcbCdocu.divori = gn-divi.CodDiv
            AND (CcbCdocu.FchDoc >= x-CodFchI AND CcbCdocu.FchDoc <= x-CodFchF) AND
            ccbcdocu.coddoc = cCodDoc NO-LOCK:
            IF LOOKUP(CcbCDocu.FlgEst, "A,X") > 0 THEN NEXT.    /* ANULADO y CERRADO */
            IF LOOKUP(CcbCDocu.TpoFac, "B") > 0   THEN NEXT.    /* VIENE DE UNA BAJA DE SUNAT */
            /* (A) FACTURAS POR ANTICIPOS Y/O SERVICIOS */
            IF LOOKUP(CcbCDocu.CodDoc,"FAC,BOL") > 0 AND LOOKUP(CcbCdocu.TpoFac, 'A,S,V') > 0 THEN DO:
                /*NEXT ESTADISTICAS.*/
                NEXT.
            END.
            IF LOOKUP(Ccbcdocu.CodDoc, 'N/C,A/C') > 0 THEN DO:
                FIND B-CDOCU WHERE B-CDOCU.Codcia = CcbCdocu.Codcia
                    AND B-CDOCU.CodDoc = CcbCdocu.Codref
                    AND B-CDOCU.NroDoc = CcbCdocu.Nroref
                    NO-LOCK NO-ERROR.
                IF AVAILABLE B-CDOCU THEN DO:
                    /* NOTAS DE CREDITO QUE TIENE COMO REFERENCIA (A) */
                    IF CcbCDocu.CodDoc = "N/C" AND LOOKUP(B-CDOCU.TpoFac, 'A,S,V') > 0 THEN DO:
                        NEXT.
                    END.
                END.
            END.
            /* Detalle del Documento */
            x-PorIgv = Ccbcdocu.porigv.
            RUN carga-transaccion-ventas-detalle(Ccbcdocu.coddoc, Ccbcdocu.nrodoc).
        END.
    END.
END.
/*
x-Archivo = "/home/v/IN/dbs/" + "ventas_detalle" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".
*/

cFecha = STRING(x-CodFchI,"99/99/9999").
cFile = ENTRY(3,cFecha,"/") + "-" + ENTRY(2,cFecha,"/") + "-" + ENTRY(1,cFecha,"/").

cFecha = STRING(x-CodFchF,"99/99/9999").
cFile = cFile + "_" + ENTRY(3,cFecha,"/") + "-" + ENTRY(2,cFecha,"/") + "-" + ENTRY(1,cFecha,"/").

x-Archivo = "d:\" + "transaccion_ventas_" + STRING(DAY(x-CodFchI),'99') + 
    STRING(MONTH(x-CodFchI),'99') + STRING(YEAR(x-CodFchI),'9999') + ".txt".

x-Archivo = "d:\" + "transaccion_ventas" + cFile + ".txt".


OUTPUT TO VALUE(x-archivo) KEEP-MESSAGES.
FOR EACH tTransaccionVentas NO-LOCK:
    /*EXPORT delimiter "~029" tTransaccionVentas.*/
    EXPORT delimiter ";" tTransaccionVentas.
END.
OUTPUT CLOSE.
/* ******************************************************************************* */
/*
DEFINE VARIABLE comm-line AS CHARACTER FORMAT "x(70)".                                        
comm-line = "/usr/bin/qonvtaexport5".
OS-COMMAND VALUE(comm-line) SILENT NO-CONSOLE.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-carga-transaccion-ventas-detalle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-transaccion-ventas-detalle Procedure 
PROCEDURE carga-transaccion-ventas-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pCodDoc AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pNroDoc AS CHAR NO-UNDO.

DEFINE VAR x-TpoCmbCmp AS DEC INIT 1.
DEFINE VAR x-TpoCmbVta AS DEC INIT 1.
DEFINE VAR x-Signo AS DEC INIT 1.
DEFINE VAR dIgvItem AS DEC.
DEF VAR x-CtoUni AS DEC NO-UNDO.

DEFINE VAR fFechaVenta AS CHAR.
DEFINE VAR cAnio AS CHAR.
DEFINE VAR cMes AS CHAR.
DEFINE VAR cDia AS CHAR.
DEFINE VAR cAAAAMMDD AS CHAR.

/* Venta en Dolares */
IF Ccbcdocu.CodMon = 2 THEN DO:
    FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF NOT AVAIL Gn-Tcmb THEN 
        FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
    IF AVAIL Gn-Tcmb THEN 
        ASSIGN
            x-TpoCmbCmp = Gn-Tcmb.Compra
            x-TpoCmbVta = Gn-Tcmb.Venta.
END.

FOR EACH ccbddocu WHERE ccbddocu.codcia = 1 AND ccbddocu.coddoc = pCodDoc AND ccbddocu.nrodoc = pNroDoc
     NO-LOCK,FIRST almmmatg OF ccbddocu NO-LOCK:

    dIgvItem = x-PorIgv.
    IF ccbddocu.aftigv THEN dIgvItem = 0. /* No esta afecto a igv */
    x-Signo = 1.
    IF ccbcdocu.coddoc = 'n/c' THEN x-Signo = -1.

    fFechaVenta = string(ccbcdocu.fchdoc,"99/99/9999").

    cAnio = ENTRY(3,fFechaVenta,"/").
    cMes = ENTRY(2,fFechaVenta,"/").
    cDia = ENTRY(1,fFechaVenta,"/").

    cAAAAMMDD = cAnio + "-" + cMes + "-" + cDia.

    /* Costo Kardex */
    x-CtoUni = 0.
    FIND LAST AlmStkGe WHERE Almstkge.codcia = 1
        AND Almstkge.codmat = Ccbddocu.codmat
        AND Almstkge.fecha <= Ccbcdocu.fchdoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almstkge AND Almstkge.CtoUni <> ? THEN x-CtoUni = AlmStkge.CtoUni.
   
    CREATE tTransaccionVentas.
    ASSIGN
        tTransaccionVentas.codcomprobante   = ccbcdocu.coddoc
        tTransaccionVentas.nrocomprobante   = ccbcdocu.nrodoc
        tTransaccionVentas.fechaventa       = cAAAAMMDD
        tTransaccionVentas.codcliente       = ccbcdocu.codcli
        tTransaccionVentas.divisionventa    = ccbcdocu.divori
        tTransaccionVentas.codigoarticulo   = ccbddocu.codmat
        tTransaccionVentas.codigolinea      = IF AVAILABLE almmmatg THEN almmmatg.codfam ELSE "ERROR"
        tTransaccionVentas.codigosublinea   = IF AVAILABLE almmmatg THEN almmmatg.subfam ELSE "ERROR"
        tTransaccionVentas.cantidad         = ccbddocu.candes * ccbddocu.factor
        tTransaccionVentas.anio             = YEAR(ccbcdocu.fchdoc)
        tTransaccionVentas.mes              = MONTH(ccbcdocu.fchdoc).        

        IF ccbddocu.cPreUniSinImpuesto <= 0 AND TRUE <> (ccbddocu.cTipoAfectacion > "") THEN DO:
            /* No paso por arimetica de Sunat */
            ASSIGN 
            tTransaccionVentas.preciounitario   = ccbddocu.preuni / ((100 + dIgvItem) / 100)
            tTransaccionVentas.importedescuento = ccbddocu.impDto / ((100 + dIgvItem) / 100)
            tTransaccionVentas.importeigv       = ccbddocu.impigv
            tTransaccionVentas.importevalorventa    = (ccbddocu.implin - ccbddocu.impDto2) - ccbddocu.impigv
            tTransaccionVentas.importeventa     = ccbddocu.implin - ccbddocu.impDto2.
        END.
        ELSE DO:
            ASSIGN
            tTransaccionVentas.preciounitario   = ccbddocu.cPreUniSinImpuesto
            tTransaccionVentas.importedescuento = ccbddocu.ImporteDescuento
            tTransaccionVentas.importeigv       = ccbddocu.ImporteIGV
            tTransaccionVentas.importevalorventa    = ccbddocu.ImporteTotalSinImpuesto
            tTransaccionVentas.importeventa     = ccbddocu.ImporteTotalSinImpuesto + ccbddocu.ImporteIGV.
        END.

        /* Aplicamos en tipo de cambio */
        IF Ccbcdocu.CodMon = 2 THEN DO:
            ASSIGN
            tTransaccionVentas.preciounitario   = tTransaccionVentas.preciounitario * x-TpoCmbCmp
            tTransaccionVentas.importedescuento = tTransaccionVentas.importedescuento * x-TpoCmbCmp
            tTransaccionVentas.importeigv       = tTransaccionVentas.importeigv * x-TpoCmbCmp
            tTransaccionVentas.importevalorventa    = tTransaccionVentas.importevalorventa * x-TpoCmbCmp
            tTransaccionVentas.importeventa     = tTransaccionVentas.importeventa * x-TpoCmbCmp.
        END.

        ASSIGN
        tTransaccionVentas.codigomarca      = IF AVAILABLE almmmatg THEN almmmatg.codmar ELSE "ERR"
        tTransaccionVentas.condicionventa   = ccbcdocu.fmapgo
        tTransaccionVentas.codigolicencia   = IF AVAILABLE almmmatg THEN almmmatg.codfam ELSE "ERR"
        tTransaccionVentas.divisiondespacho = ccbddocu.coddiv
        tTransaccionVentas.pesounitario   = IF AVAILABLE almmmatg THEN almmmatg.pesmat ELSE 0.00
        tTransaccionVentas.pesototal      = tTransaccionVentas.pesounitario * tTransaccionVentas.cantidad
        tTransaccionVentas.volunitario    = IF AVAILABLE almmmatg THEN almmmatg.libre_D02 ELSE 0.00
        tTransaccionVentas.voltotal       = tTransaccionVentas.volunitario * tTransaccionVentas.cantidad
        tTransaccionVentas.costokardexunitario    = x-CtoUni
        tTransaccionVentas.costokardextotal       = tTransaccionVentas.costokardexunitario * tTransaccionVentas.cantidad.
        ASSIGN
        tTransaccionVentas.pesototal        = tTransaccionVentas.pesounitario * x-signo
        tTransaccionVentas.voltotal         = tTransaccionVentas.volunitario * x-signo
        tTransaccionVentas.importedescuento = tTransaccionVentas.importedescuento * x-signo
        tTransaccionVentas.importeigv       = tTransaccionVentas.importeigv * x-signo
        tTransaccionVentas.importevalorventa = tTransaccionVentas.importevalorventa * x-signo
        tTransaccionVentas.importeventa     = tTransaccionVentas.importeventa * x-signo
        tTransaccionVentas.cantidad         = tTransaccionVentas.cantidad * x-signo
        tTransaccionVentas.costokardextotal       = tTransaccionVentas.costokardextotal * x-signo
        tTransaccionVentas.flgend = "final".        

END.

RELEASE ccbddocu NO-ERROR.

/*     DEF VAR x-CtoUni AS DEC NO-UNDO.                                                      */
/*     x-CtoUni = 0.                                                                         */
/*     FIND LAST AlmStkGe WHERE Almstkge.codcia = Ccbcdocu.codcia                            */
/*         AND Almstkge.codmat = Ccbddocu.codmat                                             */
/*         AND Almstkge.fecha <= Ccbcdocu.fchdoc                                             */
/*         NO-LOCK NO-ERROR.                                                                 */
/*     IF AVAILABLE Almstkge AND Almstkge.CtoUni <> ? THEN x-CtoUni = AlmStkge.CtoUni.       */
/*     IF Ccbcdocu.codcia = 1 and Ccbcdocu.coddoc = 'N/C' and Ccbcdocu.cndcre = "N" THEN DO: */
/*         FIND CcbTabla WHERE CcbTabla.CodCia = s-codcia AND                                */
/*             CcbTabla.Tabla = "N/C" AND                                                    */
/*             CcbTabla.Codigo = Ccbcdocu.codcta                                             */
/*             NO-LOCK NO-ERROR.                                                             */
/*         IF AVAILABLE CcbTabla AND CcbTabla.Libre_c02 = "NO" THEN x-CtoUni = 0.            */
/*     END.                                                                                  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Limpiar-Texto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Limpiar-Texto Procedure 
PROCEDURE Limpiar-Texto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER cadenaTexto AS CHAR.
DEF INPUT PARAMETER sustituirPor AS CHAR.
DEF OUTPUT PARAMETER cadenaResultado AS CHAR.

DEF VAR tamanoCadena AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR caracteresValidos AS CHAR NO-UNDO.
DEF VAR caracterActual AS CHAR NO-UNDO.
  
tamanoCadena = LENGTH(cadenaTexto).
If tamanoCadena > 0 THEN DO:
    caracteresValidos = ' 0123456789abcdefghijklmnñopqrstuvwxyzABCDEFGHIJKLMNÑOPQRSTUVWXYZ-_.,"'.
    caracteresValidos = caracteresValidos + "/()#*+-$&%'áéíóúÁÉÍÓÚüÜº°:´Øª¿?!¡=@".
    DO i = 1 TO tamanoCadena:
        caracterActual = SUBSTRING(cadenaTexto, i, 1).
        IF INDEX(caracteresValidos, caracterActual) > 0 THEN
            cadenaResultado = cadenaResultado + caracterActual.
        ELSE 
            cadenaResultado = cadenaResultado + sustituirPor.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SemanaIso) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SemanaIso Procedure 
PROCEDURE SemanaIso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pFecha AS DATE.
DEF OUTPUT PARAMETER pSemana AS INT.
DEF OUTPUT PARAMETER pAno AS INT.

DEF VAR xDia AS INT.
DEF VAR xMes AS INT.
DEF VAR xAno AS INT.
DEF VAR xA AS INT.
DEF VAR xB AS INT.
DEF VAR xC AS INT.
DEF VAR xD AS INT.
DEF VAR xS AS INT.
DEF VAR xE AS INT.
DEF VAR xF AS INT.
DEF VAR xG AS INT.
DEF VAR xN AS INT.
DEF VAR xSemana AS INT.

ASSIGN
    xDia = DAY(pFecha)
    xMes = MONTH(pFecha)
    xAno = YEAR(pFecha).

IF xMes = 01 OR xMes = 02 THEN DO:  /* Enero o Febrero */
    ASSIGN
        xA = xAno - 1
        xB = TRUNCATE(xA / 4, 0) - TRUNCATE(xA / 100, 0) + TRUNCATE(xA / 400, 0)
        xC = TRUNCATE( (xA - 1) / 4, 0) - TRUNCATE( (xA - 1) / 100, 0) + TRUNCATE( (xA - 1) / 400, 0)
        xS = xB - xC
        xE = 0
        xF = xDia - 1 + (31 * (xMes - 1) ).
END.
ELSE DO:    /* de Marzo a Diciembre */
    ASSIGN
        xA = xAno
        xB = TRUNCATE(xA / 4, 0 ) - TRUNCATE(xA / 100, 0) + TRUNCATE(xA / 400, 0)
        xC = TRUNCATE( (xA - 1) / 4, 0 ) - TRUNCATE( (xA - 1) / 100, 0) + TRUNCATE( (xA - 1) / 400, 0)
        xS = xB - xC
        xE = xS + 1
        xF = xDia + TRUNCATE( ( ( 153 * (xMes - 3) ) + 2 ) / 5, 0) + 58 + xS.
END.
/* Adicionalmente sumándole 1 a la variable xF 
    se obtiene numero ordinal del dia de la fecha ingresada con referencia al año actual
    */
/* Estos cálculos se aplican a cualquier mes */
ASSIGN
    xG = (xA + xB) MODULO 7
    xD = (xF + xG - xE) MODULO 7 /* Adicionalmente esta variable nos indica el dia de la semana 0=Lunes, ... , 6=Domingo */
    xN = xF + 3 - xD.
IF xN < 0 THEN DO:
    /* Si la variable n es menor a 0 se trata de una semana perteneciente al año anterior */
    ASSIGN
        xSemana = 53 - TRUNCATE( (xG - xS) / 5, 0)
        xAno = xAno - 1.
END.
ELSE IF xN > (364 + xS) THEN DO:
    /* Si n es mayor a 364 + $s entonces la fecha corresponde a la primera semana del año siguiente.*/
    ASSIGN
        xSemana = 1
        xAno = xAno + 1.
END.
ELSE DO:
    /* En cualquier otro caso es una semana del año actual */
    xSemana = TRUNCATE(xN / 7, 0) + 1.
END.

ASSIGN
    pAno = xAno
    pSemana = xSemana.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

