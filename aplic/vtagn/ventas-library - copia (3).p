&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-ADocu FOR CcbADocu.
DEFINE BUFFER B-ALM FOR Almacen.
DEFINE BUFFER B-CPEDI FOR FacCPedi.
DEFINE BUFFER B-DPEDI FOR FacDPedi.
DEFINE BUFFER B-FacTabla FOR FacTabla.
DEFINE BUFFER COTIZACION FOR FacCPedi.
DEFINE TEMP-TABLE ITEM LIKE FacDPedi.
DEFINE TEMP-TABLE ITEM-1 LIKE FacDPedi.
DEFINE BUFFER PCO FOR FacCPedi.
DEFINE TEMP-TABLE PEDI NO-UNDO LIKE FacDPedi.
DEFINE BUFFER PEDIDO FOR FacCPedi.
DEFINE NEW SHARED TEMP-TABLE T-LogTabla NO-UNDO LIKE logtabla.



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
&SCOPED-DEFINE precio-venta-general vtagn/PrecioVentaMayorCredito.p

DEF SHARED VAR s-codcia AS INT.  
DEF SHARED VAR cl-codcia AS INT.  
DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR s-coddiv AS CHAR.   

/* PRODUCTOS POR ROTACION */
DEFINE SHARED VAR s-FlgRotacion LIKE gn-divi.flgrotacion.
DEFINE SHARED VAR s-FlgEmpaque  LIKE GN-DIVI.FlgEmpaque.
DEFINE SHARED VAR s-FlgMinVenta LIKE GN-DIVI.FlgMinVenta.
DEFINE SHARED VAR s-VentaMayorista LIKE GN-DIVI.VentaMayorista.
DEFINE SHARED VAR s-FlgTipoVenta LIKE GN-DIVI.FlgPreVta.
DEFINE SHARED VAR s-DiasVtoPed LIKE GN-DIVI.DiasVtoPed.
DEFINE SHARED VAR s-TpoPed AS CHAR.

DEF TEMP-TABLE ResumenxLinea
    FIELD codmat LIKE almmmatg.codmat
    FIELD codfam LIKE almmmatg.codfam
    FIELD subfam LIKE almmmatg.subfam
    FIELD canped LIKE facdpedi.canped
    INDEX Llave01 AS PRIMARY codmat codfam subfam.

DEF TEMP-TABLE ErroresxLinea LIKE ResumenxLinea.

DEFINE BUFFER y-factabla FOR factabla.
DEFINE BUFFER x-vtatabla FOR vtatabla.
DEFINE BUFFER y-vtatabla FOR vtatabla.
DEFINE BUFFER x-ccbcdocu FOR ccbcdocu.
DEFINE BUFFER x-ccbddocu FOR ccbddocu.

DEFINE TEMP-TABLE tccbcdocu LIKE ccbcdocu.
DEFINE TEMP-TABLE tw-report LIKE w-report.      /* Ic */

DEFINE VAR x-codcli-vip AS CHAR.

/* ICBPER */ 
DEFINE VAR x-articulo-ICBPER AS CHAR.

x-articulo-ICBPER = '099268'.


/* Ic - 11Ago2020 */
DEFINE VAR x-dias-tolerables-deuda-vencida AS INT.
DEFINE VAR x-tabla AS CHAR INIT "".

x-dias-tolerables-deuda-vencida = 8.        /* Segun Sr. Rodolfo Salas 8 Dias correo del  */


/* COMISION B2C - WEB */
DEFINE VAR x-Proveedor AS CHAR.                                       
DEFINE VAR x-Importe AS DEC.
DEFINE VAR x-OrigenTarjeta AS CHAR.
DEFINE VAR x-TipoTarjeta AS CHAR.
DEFINE VAR x-ComisionCalculada AS DEC.
DEFINE VAR x-PorcentajeComision AS DEC.

DEFINE VAR x-porigv AS DEC INIT -1.

/* IGV */
FIND FIRST facCfgGn WHERE facCfgGn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE facCfgGn THEN DO:
    /*
    MESSAGE "No se pudo ubicar el % de IGV en la tabla FacCfgGn"
        VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
    */
END.
ELSE DO:
    x-porigv = FacCfgGn.porigv.
END.


/* Ic - 11Ago2020 FIN */


/*DEFINE SHARED VAR s-DiasDtoPed LIKE GN-DIVI.DiasVtoPed.*/

/* Sintaxis:

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN <libreria> PERSISTENT SET hProc.

RUN <libreria>.rutina_interna IN hProc (input  buffer tt-excel:handle,
                        /*input  session:temp-directory + "file"*/ c-xls-file,
                        output c-csv-file) .

DELETE PROCEDURE hProc.

*/

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
      TABLE: B-ADocu B "?" ? INTEGRAL CcbADocu
      TABLE: B-ALM B "?" ? INTEGRAL Almacen
      TABLE: B-CPEDI B "?" ? INTEGRAL FacCPedi
      TABLE: B-DPEDI B "?" ? INTEGRAL FacDPedi
      TABLE: B-FacTabla B "?" ? INTEGRAL FacTabla
      TABLE: COTIZACION B "?" ? INTEGRAL FacCPedi
      TABLE: ITEM T "?" ? INTEGRAL FacDPedi
      TABLE: ITEM-1 T "?" ? INTEGRAL FacDPedi
      TABLE: PCO B "?" ? INTEGRAL FacCPedi
      TABLE: PEDI T "?" NO-UNDO INTEGRAL FacDPedi
      TABLE: PEDIDO B "?" ? INTEGRAL FacCPedi
      TABLE: T-LogTabla T "NEW SHARED" NO-UNDO INTEGRAL logtabla
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 25.46
         WIDTH              = 77.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-COMISION-B2C-WEB) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COMISION-B2C-WEB Procedure 
PROCEDURE COMISION-B2C-WEB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pProveedor AS CHAR.                                       
DEFINE INPUT PARAMETER pImporte AS DEC.
DEFINE INPUT PARAMETER pOrigenTarjeta AS CHAR.      /* NACIONAL/INTERNACIONAL */
DEFINE INPUT PARAMETER pTipoTarjeta AS CHAR.        /*  */
DEFINE OUTPUT PARAMETER pComisionCalculada AS DEC.
DEFINE OUTPUT PARAMETER pPorcentajeComision AS DEC.

x-Proveedor = pProveedor. 
x-Importe = pImporte.
x-OrigenTarjeta = pOrigenTarjeta.
x-TipoTarjeta = pTipoTarjeta.
x-ComisionCalculada = 0.
x-PorcentajeComision = 0. 

IF x-porigv = -1 THEN DO:
    MESSAGE "No se pudo ubicar el % de IGV en la tabla FacCfgGn"
        VIEW-AS ALERT-BOX INFORMATION.
    RETURN.
END.

IF pProveedor = 'NIUBIZ' THEN DO:

    /* pTipoTarjeta : DEBITO, CREDITO */

    RUN COMISION-B2C-WEB-NIUBIZ.
    pComisionCalculada = x-ComisionCalculada.
    pPorcentajeComision = x-PorcentajeComision.

    RETURN.
END.

IF pProveedor = "PAGOEFECTIVO" THEN DO:

    /* pTipoTarjeta : AGENTE,AGENCIA,INTERNET */

    RUN COMISION-B2C-WEB-PAGOEFECTIVO.
    
    pComisionCalculada = x-ComisionCalculada.
    pPorcentajeComision = x-PorcentajeComision.

    RETURN.

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-COMISION-B2C-WEB-NIUBIZ) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COMISION-B2C-WEB-NIUBIZ Procedure 
PROCEDURE COMISION-B2C-WEB-NIUBIZ :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Escalas */

IF x-OrigenTarjeta = "NACIONAL" THEN DO:
    IF (x-Importe >= 0.00 AND x-Importe <= 99.99) THEN DO:
        IF LOOKUP(x-TipoTarjeta,"d�bito,debito") > 0 THEN x-PorcentajeComision = 2.99.
        IF LOOKUP(x-TipoTarjeta,"cr�dito,credito") > 0 THEN x-PorcentajeComision = 3.99.        
    END.
    IF (x-Importe >= 100.00 AND x-Importe <= 249.99) THEN DO:
        IF LOOKUP(x-TipoTarjeta,"d�bito,debito") > 0 THEN x-PorcentajeComision = 2.99.
        IF LOOKUP(x-TipoTarjeta,"cr�dito,credito") > 0 THEN x-PorcentajeComision = 3.75.        
    END.
    IF (x-Importe >= 250.00 AND x-Importe <= 499.99) THEN DO:
        IF LOOKUP(x-TipoTarjeta,"d�bito,debito") > 0 THEN x-PorcentajeComision = 2.75.
        IF LOOKUP(x-TipoTarjeta,"cr�dito,credito") > 0 THEN x-PorcentajeComision = 3.25.        
    END.
    IF (x-Importe >= 500.00 AND x-Importe <= 999.99) THEN DO:
        IF LOOKUP(x-TipoTarjeta,"d�bito,debito") > 0 THEN x-PorcentajeComision = 2.0.
        IF LOOKUP(x-TipoTarjeta,"cr�dito,credito") > 0 THEN x-PorcentajeComision = 2.50.        
    END.
    IF (x-Importe >= 1000.00 AND x-Importe <= 2999.99) THEN DO:
        IF LOOKUP(x-TipoTarjeta,"d�bito,debito") > 0 THEN x-PorcentajeComision = 1.50.
        IF LOOKUP(x-TipoTarjeta,"cr�dito,credito") > 0 THEN x-PorcentajeComision = 2.0.        
    END.
    IF x-Importe >= 3000.00  THEN DO:
        IF LOOKUP(x-TipoTarjeta,"d�bito,debito") > 0 THEN x-PorcentajeComision = 1.25.
        IF LOOKUP(x-TipoTarjeta,"cr�dito,credito") > 0 THEN x-PorcentajeComision = 1.75.        
    END.
END.
ELSE DO:
    IF (x-Importe >= 0.00 AND x-Importe <= 99.99) THEN DO:
        x-PorcentajeComision = 3.99.
    END.
    IF (x-Importe >= 100.00 AND x-Importe <= 249.99) THEN DO:
        x-PorcentajeComision = 3.99.
    END.
    IF (x-Importe >= 250.00 AND x-Importe <= 499.99) THEN DO:
        x-PorcentajeComision = 3.99.
    END.
    IF (x-Importe >= 500.00 AND x-Importe <= 999.99) THEN DO:
        x-PorcentajeComision = 3.75.
    END.
    IF (x-Importe >= 1000.00 AND x-Importe <= 2999.99) THEN DO:
        x-PorcentajeComision = 3.75.
    END.
    IF x-Importe >= 3000.00  THEN DO:
        x-PorcentajeComision = 3.50.
    END.
END.

x-ComisionCalculada = ROUND(x-Importe * (x-PorcentajeComision / 100),2).

END PROCEDURE.

/*
Buenas tardes, Estimado Hiroshi

Tal como nos comunicamos, respidiendo a tu primera pregunta, las 
comisiones que se mantienen bajo el c�digo de comercio electronico,
son las siguientes donde la tasa escalonada depende del monto que 
se realice y no del tipo de tarjeta.

Importe     Importe             % Com.          % Com.
Desde       Hasta               D�bito. Nac.    Cr�dito. Nac.
                         
0.01        99.99               2.99            3.99
100.00      249.99              2.99            3.75
250.00      499.99              2.75            3.25
500.00      999.99              2.00            2.50
1,000.00    2,999.99            1.50            2.00
3,000.00    99,999,999.99       1.25            1.75
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-COMISION-B2C-WEB-PAGOEFECTIVO) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COMISION-B2C-WEB-PAGOEFECTIVO Procedure 
PROCEDURE COMISION-B2C-WEB-PAGOEFECTIVO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VAR x-imp-comision AS DEC.
    DEFINE VAR x-factor-comision AS DEC.
    DEFINE VAR x-comision-calculada AS DEC.


    IF LOOKUP(x-TipoTarjeta,"AGENTE,AGENCIA,INTERNET") = 0 THEN RETURN.

    IF x-TipoTarjeta = "AGENCIA" THEN DO:
        x-imp-comision = 4.50.
        x-factor-comision = 2.5.
    END.
    IF x-TipoTarjeta = "AGENTE" THEN DO:
        x-imp-comision = 3.00.
        x-factor-comision = 2.5.
    END.
    IF x-TipoTarjeta = "INTERNET" THEN DO:
        x-imp-comision = 2.75.
        x-factor-comision = 2.5.
    END.

    /* Le adicionamos el IGV */
    x-imp-comision = ROUND(x-imp-comision * ( 1 + (x-porigv / 100)),2).

    x-comision-calculada = x-Importe * (x-factor-comision / 100).
    x-comision-calculada = ROUND(x-comision-calculada * ( 1 + (x-porigv / 100)),2).

    IF x-comision-calculada <= x-imp-comision THEN DO:
        x-comision-calculada = x-imp-comision.
        x-factor-comision = 0.
    END.

    x-ComisionCalculada = x-comision-calculada.
    x-PorcentajeComision = x-factor-comision.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-COT_Importar-Excel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COT_Importar-Excel Procedure 
PROCEDURE COT_Importar-Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT-OUTPUT PARAMETER TABLE FOR ITEM.

DEFINE VAR p-lfilexls AS CHAR INIT "".
DEFINE VAR p-lFileXlsProcesado AS CHAR INIT "".

DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.
DEFINE VARIABLE pMensaje AS CHAR NO-UNDO.
DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.
DEFINE VARIABLE k               AS INTEGER NO-UNDO.
DEFINE VARIABLE j               AS INTEGER NO-UNDO.
DEFINE VARIABLE I-NroItm        AS INTEGER NO-UNDO.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls,*.xlsx,*.xlsm)" "*.xls,*.xlsx,*.xlsm", "Todos (*.*)" "*.*"
    TITLE "IMPORTAR EXCEL DE PEDIDOS"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN 'ADM-ERROR'.

DEF VAR x-canped AS DEC NO-UNDO.
DEF VAR x-CodMat AS CHAR NO-UNDO.
DEFINE VARIABLE lFileXlsUsado            AS CHAR.

DEFINE VARIABLE lFileXls                 AS CHAR.
DEFINE VARIABLE lNuevoFile               AS LOG.

lFileXls = "".          /* Nombre el archivo a abrir o crear, vacio es valido solo para nuevos */
lNuevoFile = NO.        /* YES : Si va crear un nuevo archivo o abrir */

lFileXls = FILL-IN-Archivo.

{lib\excel-open-file.i}
chExcelApplication:Visible = FALSE.

lMensajeAlTerminar = NO. /*  */
lCerrarAlTerminar = YES.        /* Si permanece abierto el Excel luego de concluir el proceso */

chWorkSheet = chExcelApplication:Sheets("NotaPedido").

pMensaje = ''.

/* NO borramos si hay algo ya digitado */
EMPTY TEMP-TABLE ITEM-1.

I-NroItm = 1.
FOR EACH ITEM NO-LOCK:
    i-NroItm = i-NroItm + 1.
END.
REPEAT iColumn = 9 TO 65000:
    cColumn = STRING(iColumn).
    cRange = "D" + cColumn.
    cValue = chWorkSheet:Range(cRange):Value.
    IF cValue = "" OR cValue = ? THEN LEAVE.
    x-CodMat = cValue.

    cRange = "H" + cColumn.
    cValue = trim(chWorkSheet:Range(cRange):Value).
    x-canped = DEC(cValue).

    IF x-CodMat = "" OR x-CodMat = ? OR x-canped = 0 OR x-canped = ? THEN NEXT.
    
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = x-codmat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN NEXT.

    CREATE ITEM-1.
    ASSIGN
        ITEM-1.nroitm = I-NroItm
        ITEM-1.codcia = s-codcia
        ITEM-1.codmat = x-codmat
        ITEM-1.canped = x-canped.

    I-NroItm = I-NroItm + 1.
END.
{lib\excel-close-file.i}

FOR EACH ITEM-1 BY ITEM-1.NroItm:
    FIND FIRST ITEM WHERE ITEM.codmat = ITEM-1.codmat NO-LOCK NO-ERROR.
    IF AVAILABLE ITEM THEN NEXT.
    CREATE ITEM.
    BUFFER-COPY ITEM-1 TO ITEM.
END.
/* Renombramos el Excel usado  */
lFileXlsUsado =  STRING(NOW,'99-99-9999 hh:mm:ss').
lFileXlsUsado = replace(lFileXlsUsado,":","").
lFileXlsUsado = replace(lFileXlsUsado," ","").
lFileXlsUsado = replace(lFileXlsUsado,"-","").
lFileXlsUsado = TRIM(lFileXls) + "." + lFileXlsUsado.
p-lfilexls = lFileXls.
p-lFileXlsProcesado = lFileXlsUsado.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-COT_Importar-Excel-Provincias) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COT_Importar-Excel-Provincias Procedure 
PROCEDURE COT_Importar-Excel-Provincias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT-OUTPUT PARAMETER TABLE FOR ITEM.

/* VARIABLES PARA EL EXCEL */
DEFINE VARIABLE chExcelApplication  AS COM-HANDLE.
DEFINE VARIABLE chWorkbook          AS COM-HANDLE.
DEFINE VARIABLE chWorksheet         AS COM-HANDLE.
DEFINE VARIABLE cRange          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCountLine      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iTotalColumn    AS INTEGER      NO-UNDO.
DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.

DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.
                                          
SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN 'ADM-ERROR'.

/* CREAMOS LA HOJA EXCEL */
CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

DEF VAR I-NITEM AS INT NO-UNDO.
DEFINE VAR cCodArt AS CHAR.
DEFINE VAR cQty AS CHAR.

/* Cargamos detalle */
ASSIGN
    t-Row = 21   /* 8 */
    I-NITEM = 0.
EMPTY TEMP-TABLE ITEM.
REPEAT:
    ASSIGN
        t-Row  = t-Row + 1
        I-NITEM = I-NITEM + 1.
    cCodArt = chWorkSheet:Cells(t-Row, 2):VALUE.
    IF cCodArt = "" OR cCodArt = ? THEN LEAVE.    /* FIN DE DATOS */ 
    cValue = chWorkSheet:Cells(t-Row, 3):VALUE.
    IF cValue = ? OR cValue = "" THEN cValue = "0" .
    IF cValue <> "0" THEN DO:
        CREATE ITEM.
        ASSIGN
            ITEM.NroItm = I-NITEM
            ITEM.codcia = s-codcia
            ITEM.codmat = cCodArt.
        ASSIGN
            ITEM.canped = DECIMAL(cValue)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "Error en la linea " + TRIM(STRING(t-Row, '>>>9')) + ":"  SKIP
                "Cantidad Pedida" VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END.
    END.
END.
/* Borramos cantidades en cero */
I-NITEM = I-NITEM - 1.
FOR EACH ITEM WHERE ITEM.CanPed <= 0:
    I-NITEM = I-NITEM - 1.
    DELETE ITEM.
END.
/* Renumeramos */
FOR EACH ITEM BY ITEM.nroitm DESC:
    ITEM.nroitm = I-NITEM.
    I-NITEM = I-NITEM - 1.
END.

/* CERRAMOS EL EXCEL */
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-COT_Importar-Supermercados) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COT_Importar-Supermercados Procedure 
PROCEDURE COT_Importar-Supermercados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodAlm AS CHAR.
DEF OUTPUT PARAMETER pCodCli AS CHAR.
DEF OUTPUT PARAMETER pOrdCmp AS CHAR.
DEF OUTPUT PARAMETER pSede AS CHAR.
DEF INPUT-OUTPUT PARAMETER pGlosa AS CHAR.
DEF OUTPUT PARAMETER TABLE FOR ITEM.

DEF VAR x-Archivo AS CHAR NO-UNDO.
DEF VAR x-Linea   AS CHAR FORMAT 'x(200)' NO-UNDO.
DEF VAR x-CodMat LIKE ITEM.codmat NO-UNDO.
DEF VAR x-CanPed LIKE ITEM.canped NO-UNDO.
DEF VAR x-ImpLin LIKE ITEM.implin NO-UNDO.
DEF VAR x-ImpIgv LIKE ITEM.impigv NO-UNDO.
DEF VAR x-Encabezado AS LOG INIT FALSE.
DEF VAR x-Detalle    AS LOG INIT FALSE.
DEF VAR x-NroItm AS INT INIT 0.
DEF VAR x-Ok AS LOG.
DEF VAR x-Item AS CHAR NO-UNDO.
  
DEFINE VARIABLE cSede   AS CHAR NO-UNDO.
DEFINE VARIABLE cCodCli AS CHAR NO-UNDO.

SYSTEM-DIALOG GET-FILE x-Archivo
    FILTERS 'Archivo texto (.txt)' '*.txt'
    RETURN-TO-START-DIR
    TITLE 'Selecciona al archivo texto'
    MUST-EXIST
    USE-FILENAME
    UPDATE x-Ok.
IF x-Ok = NO THEN RETURN "ADM-ERROR".

EMPTY TEMP-TABLE ITEM.

/* Para Makro */
DEFINE VAR x-len AS INT.
DEFINE VAR x-pos AS INT.
DEFINE VAR x-len-oc AS INT INIT 12.         /* Caracteres de la O/C */
    
INPUT FROM VALUE(x-Archivo).
TEXTO:
REPEAT:
    IMPORT UNFORMATTED x-Linea.
    IF x-Linea BEGINS 'ENC' THEN DO:
        ASSIGN
            x-Encabezado = YES
            x-Detalle    = NO
            x-CodMat = ''
            x-CanPed = 0
            x-ImpLin = 0
            x-ImpIgv = 0.
        x-Item = TRIM(ENTRY(6,x-Linea)).
        
        /* Ic - 26Set2018, correo pilar vega CASO 65407, 12 penultimos digitos (el ultimo NO) */
        /*FacCPedi.ordcmp:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(x-Item,11,10).*/
        IF LENGTH(x-item) > x-len-oc THEN DO:
            x-len = LENGTH(x-item) - 1.
            x-pos = (x-len - x-len-oc) + 1.
            x-item = SUBSTRING(x-item,x-pos,x-len-oc).
        END.
        pOrdCmp = x-Item.
    END.
    /* Sede y Lugar de Entrega */
    IF x-Linea BEGINS 'DPGR' AND NUM-ENTRIES(x-Linea) > 1 THEN DO:
        cSede = TRIM(ENTRY(2,x-Linea)).
    END.
    /* Cliente */
    IF x-Linea BEGINS 'IVAD' AND NUM-ENTRIES(x-Linea) > 1 THEN DO:
        cCodCli = TRIM(ENTRY(2,x-Linea)).
        /* PINTAMOS INFORMACION */
        FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia
            AND gn-clie.codibc = cCodCli
            AND gn-clie.flgsit = 'A'
            NO-LOCK NO-ERROR.
        IF AVAILABLE gn-clie THEN DO:
            ASSIGN
                pCodCli = gn-clie.codcli.
            FIND FIRST gn-clied OF gn-clie WHERE Gn-ClieD.Libre_c01 = cSede NO-LOCK NO-ERROR.
            IF AVAILABLE gn-clied THEN DO:
                ASSIGN
                    pSede = Gn-ClieD.Sede.
                FIND gn-clied WHERE gn-clied.codcia = cl-codcia
                    AND gn-clied.codcli = pCodCli
                    AND gn-clied.sede   = pSede
                    NO-LOCK NO-ERROR.
                IF AVAILABLE gn-clied
                THEN ASSIGN 
                      pGlosa = (IF pGlosa = '' THEN Gn-ClieD.DirCli ELSE pGlosa).
            END.
        END.
    END.

    /* DETALLE */
    IF x-Linea BEGINS 'LIN' 
    THEN ASSIGN
            x-Encabezado = FALSE
            x-Detalle = YES.
    IF x-Detalle = YES THEN DO:
        IF x-Linea BEGINS 'LIN' 
        THEN DO:
            x-Item = ENTRY(2,x-Linea).
            IF NUM-ENTRIES(x-Linea) = 6
            THEN ASSIGN x-CodMat = STRING(INTEGER(ENTRY(6,x-Linea)), '999999') NO-ERROR.
            ELSE ASSIGN x-CodMat = ENTRY(3,x-Linea) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN x-CodMat = ENTRY(3,x-Linea).
        END.
        FIND Almmmatg WHERE almmmatg.codcia = s-codcia
            AND Almmmatg.codmat = x-codmat
            AND Almmmatg.tpoart <> 'D'
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almmmatg
        THEN DO:
            FIND Almmmatg WHERE almmmatg.codcia = s-codcia
                AND almmmatg.codbrr = x-codmat
                AND Almmmatg.tpoart <> 'D'
                NO-LOCK NO-ERROR.
            IF AVAILABLE Almmmatg THEN x-codmat = almmmatg.codmat.
        END.
        IF NOT AVAILABLE Almmmatg
        THEN DO:
            MESSAGE "El Item" x-Item x-codmat "no esta registrado en el catalogo"
                    VIEW-AS ALERT-BOX ERROR.
            NEXT TEXTO.
        END.

        IF x-Linea BEGINS 'QTY' THEN x-CanPed = DECIMAL(ENTRY(2,x-Linea)).
        IF x-Linea BEGINS 'MOA' THEN x-ImpLin = DECIMAL(ENTRY(2,x-Linea)).
        IF x-Linea BEGINS 'TAX' 
        THEN DO:
            ASSIGN
                x-ImpIgv = DECIMAL(ENTRY(3,x-Linea))
                x-NroItm = x-NroItm + 1.
            /* consistencia de duplicidad */
            FIND FIRST ITEM WHERE ITEM.codmat = x-CodMat NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ITEM THEN DO:
                CREATE ITEM.
                ASSIGN 
                    ITEM.CodCia = s-codcia
                    ITEM.codmat = x-CodMat
                    ITEM.Factor = 1 
                    ITEM.CanPed = x-CanPed
                    ITEM.NroItm = x-NroItm 
                    ITEM.UndVta = (IF AVAILABLE Almmmatg THEN Almmmatg.Chr__01 ELSE '')
                    ITEM.ALMDES = pCodAlm
                    ITEM.AftIgv = (IF x-ImpIgv > 0 THEN YES ELSE NO).
                /* RHC 09.08.06 IGV de acuerdo al cliente */
                IF LOOKUP(TRIM(pCodCli), '20100070970,20109072177,20100106915,20504912851') > 0
                THEN ASSIGN
                        ITEM.ImpIgv = x-ImpIgv 
                        ITEM.ImpLin = x-ImpLin
                        ITEM.PreUni = (ITEM.ImpLin / ITEM.CanPed).
                ELSE ASSIGN
                        ITEM.ImpIgv = x-ImpIgv 
                        ITEM.ImpLin = x-ImpLin + x-ImpIgv
                        ITEM.PreUni = (ITEM.ImpLin / ITEM.CanPed).
            END.    /* fin de grabacion del detalle */
        END.
    END.
END.
INPUT CLOSE.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-COT_Importa_PreCotizacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COT_Importa_PreCotizacion Procedure 
PROCEDURE COT_Importa_PreCotizacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodPed AS CHAR.
DEF INPUT PARAMETER pNroPed AS CHAR.
DEF INPUT PARAMETER pCodDiv AS CHAR.    /* Lista de Precios */
DEF INPUT PARAMETER s-NroSer AS INT.
DEF INPUT PARAMETER s-CodVen AS CHAR.
DEF OUTPUT PARAMETER pNroCot AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEFINE VAR x-Articulo-ICBPer AS CHAR INIT '099268'.

FIND Vtacdocu WHERE VtaCDocu.CodCia = s-CodCia AND
    VtaCDocu.CodPed = pCodPed AND
    VtaCDocu.NroPed = pNroPed
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Vtacdocu THEN DO:
    pMensaje = 'No se encontr� la pre-cotizaci�n'.
    RETURN "ADM-ERROR".
END.

DEF VAR s-coddoc AS CHAR INIT "COT" NO-UNDO.
DEF VAR s-tpoped AS CHAR NO-UNDO.
DEF VAR s-codmon LIKE faccpedi.codmon NO-UNDO.
DEF VAR s-nrodec AS INT NO-UNDO.
DEF VAR s-tpocmb AS DEC NO-UNDO.
DEF VAR s-porigv AS DEC NO-UNDO.
DEF VAR s-FmaPgo AS CHAR NO-UNDO.

FIND FIRST FacCorre WHERE Faccorre.codcia = s-codcia
    AND Faccorre.coddiv = s-coddiv
    AND Faccorre.coddoc = s-coddoc
    AND FacCorre.nroser = s-nroser
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    pMensaje = 'NO est� configurado el correlativo para el doc. ' + s-coddoc + 
        ' la serie ' + STRING(s-nroser).
    RETURN "ADM-ERROR".
END.

DEF VAR s-Import-IBC AS LOG INIT NO NO-UNDO.
DEF VAR s-Import-B2B AS LOG INIT NO NO-UNDO.
DEF VAR s-Import-Cissac AS LOG INIT NO NO-UNDO.
DEF VAR s-CodCli AS CHAR NO-UNDO.
DEF VAR s-CndVta AS CHAR NO-UNDO.
DEF VAR s-Cmpbnte AS CHAR NO-UNDO.

ASSIGN
    s-CodCli = Vtacdocu.codcli
    s-CndVta = Vtacdocu.fmapgo
    s-Cmpbnte = Vtacdocu.cmpbnte.

EMPTY TEMP-TABLE ITEM.
EMPTY TEMP-TABLE ITEM-1.

DEFINE VARIABLE I-NroItm AS INTEGER INIT 1 NO-UNDO.

&SCOPED-DEFINE precio-venta-general vtagn/PrecioVentaMayorCredito.p

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
    /* Bloqueamos la Pre-Cotizacion */
    FIND CURRENT Vtacdocu EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE Vtacdocu THEN DO:
        {lib/mensaje-de-error.i &MensajeError="pMensaje"}
        UNDO, RETURN "ADM-ERROR".
    END.
    /* Volvemos a verificar validez de la PRE-COTIZACION */
    IF NOT (VtaCDocu.FlgEst = 'P' AND 
            VtaCDocu.FlgSit = 'X' AND 
            VtaCDocu.Libre_c01 = pCodDiv AND 
            VtaCDocu.Libre_c05 = '*' AND
            VtaCDocu.CodVen = s-CodVen)
        THEN DO:
        pMensaje = "La PRE-COTIZACION ya no est� disponible".
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* Cargamos Digitados */
    I-NroItm = 1.
    FOR EACH Vtaddocu OF Vtacdocu NO-LOCK:
        FIND FIRST ITEM WHERE ITEM.codmat = Vtaddocu.codmat NO-LOCK NO-ERROR.
        IF AVAILABLE ITEM THEN NEXT.
        CREATE ITEM.
        BUFFER-COPY Vtaddocu TO ITEM ASSIGN ITEM.NroItm = I-NroItm.
        I-NroItm = I-NroItm + 1.
    END.
    /* Correlativo */
    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}

    CREATE Faccpedi.
    BUFFER-COPY Vtacdocu 
        EXCEPT VtaCDocu.Libre_C02 VtaCDocu.Libre_C03 Vtacdocu.Libre_c05
        TO Faccpedi
        ASSIGN 
        FacCPedi.CodCia = S-CODCIA
        FacCPedi.CodDiv = S-CODDIV
        FacCPedi.CodDoc = s-coddoc 
        FacCPedi.FchPed = TODAY 
        FacCPedi.Hora   = STRING(TIME,"HH:MM:SS")
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.TpoPed = VtaCDocu.Libre_c02
        FacCPedi.Atencion = VtaCDocu.DniCli
        FacCPedi.FlgEst = "P"     /* APROBADO */
        FacCPedi.Libre_C02 = VtaCDocu.Libre_C03.    /* �ltima atenci�n */
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN
        pNroCot  = FacCPedi.NroPed      /* OJO */
        s-TpoPed = FacCPedi.TpoPed
        s-CodMon = FacCPedi.CodMon
        s-NroDec = FacCPedi.Libre_d01
        s-TpoCmb = FacCPedi.TpoCmb
        s-PorIgv = FacCPedi.PorIgv
        s-FmaPgo = FacCPedi.FmaPgo.
    ASSIGN 
        FacCPedi.Usuario = S-USER-ID
        FacCPedi.Libre_c01 = VtaCDocu.Libre_c01.    /* Lista de Precios */
    /* Recalculamos Precios de Venta */
    {vtagn/recalcular-cotizacion-general-v2.i &pTpoPed=s-TpoPed}
    /* Generamos Detalle */
    FOR EACH ITEM NO-LOCK, 
        FIRST Almmmatg OF ITEM NO-LOCK
        BY ITEM.NroItm:
        CREATE Facdpedi.
        BUFFER-COPY ITEM TO Facdpedi
            ASSIGN
                FacDPedi.CodCia = FacCPedi.CodCia
                FacDPedi.CodDiv = FacCPedi.CodDiv
                FacDPedi.coddoc = FacCPedi.coddoc
                FacDPedi.NroPed = FacCPedi.NroPed
                FacDPedi.FchPed = FacCPedi.FchPed
                FacDPedi.Hora   = FacCPedi.Hora 
                FacDPedi.FlgEst = FacCPedi.FlgEst.
    END.
    /* ************************************************************************************** */
    /* DESCUENTOS APLICADOS A TODA LA COTIZACION */
    /* ************************************************************************************** */
    RUN DCTO_VOL_LINEA (INPUT ROWID(Faccpedi),
                        INPUT s-TpoPed,
                        INPUT pCodDiv,
                        OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    RUN DCTO_VOL_SALDO (INPUT ROWID(Faccpedi),
                        INPUT s-TpoPed,
                        INPUT pCodDiv,
                        OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    RUN DCTO_VOL_SALDO_EVENTO (INPUT ROWID(Faccpedi),
                               INPUT s-TpoPed,
                               INPUT pCodDiv,
                               OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    /* ****************************************************************************************** */
    {vta2/graba-totales-cotizacion-cred.i}
    /* ****************************************************************************************** */
    /* Renumeramos items */
    I-NroItm = 1.
    FOR EACH Facdpedi OF Faccpedi BY Facdpedi.nroitm:
        ASSIGN
            Facdpedi.NroItm = I-NroItm.
        I-NroItm = I-NroItm + 1.
    END.
    /* Referencias cruzadas */
    ASSIGN
        VtaCDocu.CodRef = FacCPedi.CodDoc
        VtaCDocu.NroRef = FacCPedi.NroPed
        FacCPedi.CodRef = VtaCDocu.CodPed
        FacCPedi.NroRef = VtaCDocu.NroPed
        Vtacdocu.FlgSit = "T".
    /* ******************** */
END.
RELEASE Vtacdocu.
RELEASE FacCorre.
RELEASE Faccpedi.
RELEASE Facdpedi.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-COT_Transferir-Saldo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COT_Transferir-Saldo Procedure 
PROCEDURE COT_Transferir-Saldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT  PARAMETER pRowidI AS ROWID.
DEF INPUT  PARAMETER pCodDiv AS CHAR.
DEF OUTPUT PARAMETER pRowidS AS ROWID.

FIND COTIZACION WHERE ROWID(COTIZACION) = pRowidI NO-LOCK NO-ERROR.
IF NOT AVAILABLE COTIZACION THEN RETURN.

/* CONSISTENCIA */
IF COTIZACION.FlgEst <> "P" THEN DO:
    MESSAGE 'La' COTIZACION.coddoc COTIZACION.nroped 'NO est� pendiente'
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
FIND FIRST Facdpedi OF COTIZACION WHERE facdpedi.CanAte > 0 NO-LOCK NO-ERROR.
IF NOT AVAILABLE Facdpedi THEN DO:
    MESSAGE 'La' COTIZACION.coddoc COTIZACION.nroped 'NO tiene NINGUNA atenci�n'
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
/* BLOQUEAR SI SE HA TRABAJADO CON OTRA LISTA DE PRECIOS */
IF pCodDiv <> COTIZACION.Libre_c01 THEN DO:
    MESSAGE 'NO se puede TRANSFERIR SALDO de una Cotizaci�n generada con otra lista de precios'
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
pRowidS = ?.
MESSAGE '�Procedemos a TRANSFERIR SALDOS?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE rpta AS LOG.
IF rpta = NO THEN RETURN.

DEF VAR s-coddoc LIKE Faccpedi.coddoc NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.

DEF VAR s-codmon AS INT.
DEF VAR s-CodCli AS CHAR.
DEF VAR s-porigv AS DEC.
DEF VAR s-cndvta AS CHAR.
DEF VAR s-fmapgo AS CHAR.
DEF VAR s-tpocmb AS DEC.
DEF VAR s-nrodec AS INT.
DEF VAR s-tpoped AS CHAR.
DEF VAR s-flgigv AS LOG.
DEF VAR s-codalm AS CHAR.
DEF VAR S-NROPED AS CHAR.
DEF VAR S-CMPBNTE  AS CHAR.
DEF VAR s-import-ibc AS LOG.
DEF VAR s-import-cissac AS LOG.

DEF VAR pMensaje AS CHAR NO-UNDO.

ASSIGN
    s-TpoPed = COTIZACION.TpoPed
    s-CodAlm = COTIZACION.CodAlm
    S-CODMON = COTIZACION.CodMon
    S-CODCLI = COTIZACION.CodCli
    S-TPOCMB = COTIZACION.TpoCmb
    S-CNDVTA = COTIZACION.FmaPgo
    s-FmaPgo = COTIZACION.FmaPgo
    s-PorIgv = COTIZACION.porigv
    s-NroDec = (IF COTIZACION.Libre_d01 <= 0 THEN 2 ELSE COTIZACION.Libre_d01)
    s-FlgIgv = COTIZACION.FlgIgv
    s-nroped = COTIZACION.nroped
    S-CMPBNTE = COTIZACION.Cmpbnte.

DEF VAR x-Articulo-ICBPer AS CHAR INIT '099268'.


DEF VAR I-NITEM  AS INT NO-UNDO INIT 0.

ASSIGN
    s-coddoc = COTIZACION.coddoc
    s-nroser = INTEGER(SUBSTRING(COTIZACION.nroped,1,3))
    s-coddiv = COTIZACION.coddiv.

DEFINE VAR lFechEnt AS DATE.
DEFINE VAR lFechVen AS DATE.

DEFINE BUFFER ic-vtatabla FOR vtatabla.

lFechEnt = TODAY + (COTIZACION.FchEnt - COTIZACION.FchPed).
lFechVen = TODAY + (COTIZACION.FchVen - COTIZACION.FchPed).

/* RHC 27/01/2020 Rutina antigua */
/* FIND FIRST ic-vtatabla WHERE ic-vtatabla.codcia = s-codcia AND */
/*     ic-vtatabla.tabla = 'DSTRB' AND                            */
/*     ic-vtatabla.llave_c1 = '2016' NO-LOCK NO-ERROR.            */
/* IF AVAILABLE ic-vtatabla THEN DO:                              */
/*     IF CAPS(cotizacion.libre_c01) = 'PROCESADO' THEN DO:       */
/*         lFechEnt = ic-vtatabla.rango_fecha[2].                 */
/*         lFechVen = ic-vtatabla.rango_fecha[2] + 7.             */
/*     END.                                                       */
/*     ELSE DO:                                                   */
/*         lFechEnt = ic-vtatabla.rango_fecha[2] + 7.             */
/*         lFechVen = ic-vtatabla.rango_fecha[2] + 15.            */
/*     END.                                                       */
/* END.                                                           */
/* Ic - 19Set2017, la fecha de entrega */
DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    FIND CURRENT COTIZACION EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        {lib/mensaje-de-error.i &MensajeError="pMensaje"}
        MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN.
    END.
    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}
    CREATE Faccpedi.
    BUFFER-COPY COTIZACION 
        TO Faccpedi
        ASSIGN 
        FacCPedi.FchPed = TODAY 
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.FchEnt = lFechEnt                  /* TODAY + (COTIZACION.FchEnt - COTIZACION.FchPed) */
        FacCPedi.FchVen = lFechven                  /* TODAY + (COTIZACION.FchVen - COTIZACION.FchPed) */
        FacCPedi.FlgEst = "P"       /* APROBADO */
        FacCPedi.CodRef = COTIZACION.CodDoc
        FacCPedi.Nroref = COTIZACION.NroPed.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN 
        FacCPedi.Hora = STRING(TIME,"HH:MM").
    ASSIGN
        COTIZACION.FlgEst = "ST"
        COTIZACION.Glosa  = "SALDOS TRANSFERIDOS A LA " + Faccpedi.coddoc + ' ' + Faccpedi.nroped
        pRowidS = ROWID(FacCPedi).

    FOR EACH B-DPEDI OF COTIZACION NO-LOCK WHERE B-DPEDI.CanPed - B-DPEDI.CanAte > 0
        BY B-DPEDI.NroItm:
        I-NITEM = I-NITEM + 1.
        CREATE Facdpedi.
        BUFFER-COPY B-DPEDI
            TO Facdpedi
            ASSIGN
            FacDPedi.coddoc = FacCPedi.coddoc
            FacDPedi.NroPed = FacCPedi.NroPed
            FacDPedi.FchPed = FacCPedi.FchPed
            FacDPedi.Hora   = FacCPedi.Hora 
            FacDPedi.FlgEst = FacCPedi.FlgEst
            FacDPedi.NroItm = I-NITEM
            Facdpedi.Canped = B-DPEDI.CanPed - B-DPEDI.CanAte
            Facdpedi.CanAte = 0.
    END.
    /* Recargamos y Recalculamos */
    EMPTY TEMP-TABLE ITEM.
    FOR EACH Facdpedi OF Faccpedi EXCLUSIVE-LOCK:
        CREATE ITEM.
        BUFFER-COPY Facdpedi TO ITEM.
        DELETE Facdpedi.
    END.
    IF s-TpoPed <> "LF" THEN DO:
        {vtagn/recalcular-cotizacion-general-v2.i &pTpoPed=s-TpoPed}
        FOR EACH ITEM:
            CREATE Facdpedi.
            BUFFER-COPY ITEM TO Facdpedi.
        END.
    END.
    {vta2/graba-totales-cotizacion-cred.i}
    FIND CURRENT COTIZACION NO-LOCK.
END.
IF AVAILABLE(COTIZACION) THEN RELEASE COTIZACION.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-COT_Transferir-Saldo-Sunat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COT_Transferir-Saldo-Sunat Procedure 
PROCEDURE COT_Transferir-Saldo-Sunat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT  PARAMETER pRowidI AS ROWID.
DEF INPUT  PARAMETER pCodDiv AS CHAR.
DEF OUTPUT PARAMETER pRowidS AS ROWID.

FIND COTIZACION WHERE ROWID(COTIZACION) = pRowidI NO-LOCK NO-ERROR.
IF NOT AVAILABLE COTIZACION THEN RETURN.

/* CONSISTENCIA */
IF COTIZACION.FlgEst <> "P" THEN DO:
    MESSAGE 'La' COTIZACION.coddoc COTIZACION.nroped 'NO est� pendiente'
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
FIND FIRST Facdpedi OF COTIZACION WHERE facdpedi.CanAte > 0 NO-LOCK NO-ERROR.
IF NOT AVAILABLE Facdpedi THEN DO:
    MESSAGE 'La' COTIZACION.coddoc COTIZACION.nroped 'NO tiene NINGUNA atenci�n'
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
/* BLOQUEAR SI SE HA TRABAJADO CON OTRA LISTA DE PRECIOS */
IF pCodDiv <> COTIZACION.Libre_c01 THEN DO:
    MESSAGE 'NO se puede TRANSFERIR SALDO de una Cotizaci�n generada con otra lista de precios'
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
pRowidS = ?.
MESSAGE '�Procedemos a TRANSFERIR SALDOS?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE rpta AS LOG.
IF rpta = NO THEN RETURN.

DEF VAR s-coddoc LIKE Faccpedi.coddoc NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.

DEF VAR s-codmon AS INT.
DEF VAR s-CodCli AS CHAR.
DEF VAR s-porigv AS DEC.
DEF VAR s-cndvta AS CHAR.
DEF VAR s-fmapgo AS CHAR.
DEF VAR s-tpocmb AS DEC.
DEF VAR s-nrodec AS INT.
DEF VAR s-tpoped AS CHAR.
DEF VAR s-flgigv AS LOG.
DEF VAR s-codalm AS CHAR.
DEF VAR S-NROPED AS CHAR.
DEF VAR S-CMPBNTE  AS CHAR.
DEF VAR s-import-ibc AS LOG.
DEF VAR s-import-cissac AS LOG.

DEF VAR pMensaje AS CHAR NO-UNDO.

ASSIGN
    s-TpoPed = COTIZACION.TpoPed
    s-CodAlm = COTIZACION.CodAlm
    S-CODMON = COTIZACION.CodMon
    S-CODCLI = COTIZACION.CodCli
    S-TPOCMB = COTIZACION.TpoCmb
    S-CNDVTA = COTIZACION.FmaPgo
    s-FmaPgo = COTIZACION.FmaPgo
    s-PorIgv = COTIZACION.porigv
    s-NroDec = (IF COTIZACION.Libre_d01 <= 0 THEN 2 ELSE COTIZACION.Libre_d01)
    s-FlgIgv = COTIZACION.FlgIgv
    s-nroped = COTIZACION.nroped
    S-CMPBNTE = COTIZACION.Cmpbnte.

DEF VAR x-Articulo-ICBPer AS CHAR INIT '099268'.


DEF VAR I-NITEM  AS INT NO-UNDO INIT 0.

ASSIGN
    s-coddoc = COTIZACION.coddoc
    s-nroser = INTEGER(SUBSTRING(COTIZACION.nroped,1,3))
    s-coddiv = COTIZACION.coddiv.

DEFINE VAR lFechEnt AS DATE.
DEFINE VAR lFechVen AS DATE.

DEFINE BUFFER ic-vtatabla FOR vtatabla.

lFechEnt = TODAY + (COTIZACION.FchEnt - COTIZACION.FchPed).
lFechVen = TODAY + (COTIZACION.FchVen - COTIZACION.FchPed).

/* RHC 27/01/2020 Rutina antigua */
/* FIND FIRST ic-vtatabla WHERE ic-vtatabla.codcia = s-codcia AND */
/*     ic-vtatabla.tabla = 'DSTRB' AND                            */
/*     ic-vtatabla.llave_c1 = '2016' NO-LOCK NO-ERROR.            */
/* IF AVAILABLE ic-vtatabla THEN DO:                              */
/*     IF CAPS(cotizacion.libre_c01) = 'PROCESADO' THEN DO:       */
/*         lFechEnt = ic-vtatabla.rango_fecha[2].                 */
/*         lFechVen = ic-vtatabla.rango_fecha[2] + 7.             */
/*     END.                                                       */
/*     ELSE DO:                                                   */
/*         lFechEnt = ic-vtatabla.rango_fecha[2] + 7.             */
/*         lFechVen = ic-vtatabla.rango_fecha[2] + 15.            */
/*     END.                                                       */
/* END.                                                           */
/* Ic - 19Set2017, la fecha de entrega */
DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    FIND CURRENT COTIZACION EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        {lib/mensaje-de-error.i &MensajeError="pMensaje"}
        MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN.
    END.
    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}
    CREATE Faccpedi.
    BUFFER-COPY COTIZACION 
        TO Faccpedi
        ASSIGN 
        FacCPedi.FchPed = TODAY 
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.FchEnt = lFechEnt                  /* TODAY + (COTIZACION.FchEnt - COTIZACION.FchPed) */
        FacCPedi.FchVen = lFechven                  /* TODAY + (COTIZACION.FchVen - COTIZACION.FchPed) */
        FacCPedi.FlgEst = "P"       /* APROBADO */
        FacCPedi.CodRef = COTIZACION.CodDoc
        FacCPedi.Nroref = COTIZACION.NroPed.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    ASSIGN 
        FacCPedi.Hora = STRING(TIME,"HH:MM").
    ASSIGN
        COTIZACION.FlgEst = "ST"
        COTIZACION.Glosa  = "SALDOS TRANSFERIDOS A LA " + Faccpedi.coddoc + ' ' + Faccpedi.nroped
        pRowidS = ROWID(FacCPedi).

    FOR EACH B-DPEDI OF COTIZACION NO-LOCK WHERE B-DPEDI.CanPed - B-DPEDI.CanAte > 0
        BY B-DPEDI.NroItm:
        I-NITEM = I-NITEM + 1.
        CREATE Facdpedi.
        BUFFER-COPY B-DPEDI
            TO Facdpedi
            ASSIGN
            FacDPedi.coddoc = FacCPedi.coddoc
            FacDPedi.NroPed = FacCPedi.NroPed
            FacDPedi.FchPed = FacCPedi.FchPed
            FacDPedi.Hora   = FacCPedi.Hora 
            FacDPedi.FlgEst = FacCPedi.FlgEst
            FacDPedi.NroItm = I-NITEM
            Facdpedi.Canped = B-DPEDI.CanPed - B-DPEDI.CanAte
            Facdpedi.CanAte = 0.
    END.
    /* Recargamos y Recalculamos */
    EMPTY TEMP-TABLE ITEM.
    FOR EACH Facdpedi OF Faccpedi EXCLUSIVE-LOCK:
        CREATE ITEM.
        BUFFER-COPY Facdpedi TO ITEM.
        DELETE Facdpedi.
    END.
    IF s-TpoPed <> "LF" THEN DO:
        {vtagn/recalcular-cotizacion-general-v2.i &pTpoPed=s-TpoPed}
        FOR EACH ITEM:
            CREATE Facdpedi.
            BUFFER-COPY ITEM TO Facdpedi.
        END.
    END.
    /* ****************************************************************************************** */
    /* Importes SUNAT */
    /* ****************************************************************************************** */
    {vtagn/totales-cotizacion-sunat.i &Cabecera="FacCPedi" &Detalle="FacDPedi"}
      /* ****************************************************************************************** */
    DEF VAR hProc AS HANDLE NO-UNDO.
    RUN sunat/sunat-calculo-importes PERSISTENT SET hProc.
    RUN tabla-faccpedi IN hProc (INPUT Faccpedi.CodDiv,
                                 INPUT Faccpedi.CodDoc,
                                 INPUT Faccpedi.NroPed,
                                 OUTPUT pMensaje).
    IF RETURN-VALUE = "ADM-ERROR" THEN DO:
        MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN.
    END.
    DELETE PROCEDURE hProc.
    /* ****************************************************************************************** */
    FIND CURRENT COTIZACION NO-LOCK.
END.
IF AVAILABLE(COTIZACION) THEN RELEASE COTIZACION.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DCTO_PRONTO_DESPACHO) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DCTO_PRONTO_DESPACHO Procedure 
PROCEDURE DCTO_PRONTO_DESPACHO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.    
DEF INPUT PARAMETER pCodDiv AS CHAR.        /* Lista de Precios o Divisi�n */
DEF OUTPUT PARAMETER pMensaje AS CHAR.

DEF VAR s-NroDec AS INTE NO-UNDO.
DEF VAR s-PorIgv AS DECI NO-UNDO.
DEF VAR s-CodMon AS DECI NO-UNDO.
DEF VAR s-CodCli AS CHAR NO-UNDO.
DEF VAR s-Cmpbnte AS CHAR NO-UNDO.

DEF VAR x-DtoDespacho AS DECI DECIMALS 4 INIT 0 NO-UNDO.

FOR EACH VtaTabla NO-LOCK WHERE VtaTabla.CodCia = s-CodCia AND
    VtaTabla.Tabla = 'COT_DCTO_DESPACHO' AND
    VtaTabla.Llave_c1 = pCodDiv AND
    VtaTabla.Llave_c2 = "FECHA" 
    BY VtaTabla.Rango_fecha[1]:
    IF TODAY <= VtaTabla.Rango_fecha[1] THEN DO:
        x-DtoDespacho = VtaTabla.Rango_Valor[1].
        LEAVE.
    END.
END.

IF x-DtoDespacho <= 0 THEN RETURN 'OK'.

FIND B-CPEDI WHERE ROWID(B-CPEDI) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CPEDI THEN RETURN 'ADM-ERROR'.

ASSIGN
    s-NroDec = B-CPEDI.Libre_d01
    s-PorIgv = B-CPEDI.PorIgv
    s-CodMon = B-CPEDI.CodMon
    s-CodCli = B-CPEDI.CodCli
    s-Cmpbnte = B-CPEDI.Cmpbnte
    .

DEF VAR j AS INT NO-UNDO.
DEF VAR x-Canti AS DEC NO-UNDO.
DEF VAR x-Rango AS DEC NO-UNDO.
DEF VAR x-DctoxVolumen AS DECIMAL DECIMALS 4 NO-UNDO.
DEF VAR F-FACTOR AS DECI NO-UNDO.

/* ************************************** */
DEF BUFFER B-TABLA FOR VtaTabla.
PRINCIPAL:
/* ************************************** */
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FOR EACH B-DPEDI OF B-CPEDI NO-LOCK,
        FIRST Almmmatg OF B-DPEDI NO-LOCK,
        FIRST Almtfami OF Almmmatg NO-LOCK,
        FIRST Almsfami OF Almmmatg NO-LOCK,
        FIRST B-TABLA NO-LOCK WHERE B-TABLA.CodCia = s-CodCia AND 
        B-TABLA.Tabla = 'COT_DCTO_DESPACHO' AND 
        B-TABLA.Llave_c1 = pCodDiv AND 
        B-TABLA.Llave_c2 = "LINEA" AND
        B-TABLA.Llave_c3 = Almmmatg.CodFam:
        /* ************************************************************ */
        /* SE VA A RECALCULAR EL PRECIO DE LA COTIZACION */
        /* ************************************************************ */
        FIND Facdpedi WHERE ROWID(Facdpedi) = ROWID(B-DPEDI) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            {lib/mensaje-de-error.i &MensajeError="pMensaje"}
                UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
        END.
        ASSIGN 
            Facdpedi.Por_Dsctos[1] = x-DtoDespacho
            Facdpedi.Libre_c04 = 'COT_DCTO_DESPACHO'.
        /* ***************************************************************** */
        {vtagn/CalculoDetalleMayorCredito.i &Tabla="Facdpedi"}
        /* ***************************************************************** */
    END.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DCTO_VOL_LINEA) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DCTO_VOL_LINEA Procedure 
PROCEDURE DCTO_VOL_LINEA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Descuento por volumen por l�nea y sublinea
  B�sicamente l�nea 013 y 017
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.
DEF INPUT PARAMETER s-TpoPed AS CHAR.
DEF INPUT PARAMETER pCodDiv AS CHAR.    /* Lista de Precio o Divisi�n */
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEF VAR s-PorIgv AS DEC NO-UNDO.
DEF VAR s-CodMon AS INT NO-UNDO.
DEF VAR s-NroDec AS INT NO-UNDO.
DEF VAR s-CodCli AS CHAR NO-UNDO.
DEF VAR s-Cmpbnte AS CHAR NO-UNDO.

FIND B-CPEDI WHERE ROWID(B-CPEDI) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CPEDI THEN RETURN 'ADM-ERROR'.

ASSIGN
    s-PorIgv = B-CPEDI.PorIgv
    s-CodMon = B-CPEDI.CodMon
    s-NroDec = B-CPEDI.Libre_d01
    s-CodCli = B-CPEDI.CodCli
    s-Cmpbnte = B-CPEDI.Cmpbnte
    .

DEF VAR pCuenta AS INT NO-UNDO.

/* ************************************************************* */
/* RHC 14/10/2013 Descuentos por Volumen de Compra Acumulados    */
/* ************************************************************* */
/* SOLO VENTA:
NORMAL LIMA (N)
EXPOLIBRERIA (E)
PROVINCIAS (P) 
(I) INSTITUCIONALES 
*/
/* ************************************************************* */
IF LOOKUP(s-TpoPed, "N,E,P,I,CO") = 0 THEN RETURN 'OK'.
/* ************************************************************* */

DEF VAR j AS INT NO-UNDO.
DEF VAR x-Canti AS DEC NO-UNDO.
DEF VAR x-Rango AS DEC NO-UNDO.
DEF VAR x-DctoxVolumen AS DECIMAL DECIMALS 4 NO-UNDO.
DEF VAR x-DctoPromocional AS DECIMAL DECIMALS 4 NO-UNDO.
DEF VAR F-PREBAS AS DEC DECIMALS 4 NO-UNDO.
DEF VAR F-PREVTA AS DEC DECIMALS 4 NO-UNDO.
DEF VAR Y-DSCTOS AS DEC NO-UNDO.                    /* Descuento por Volumen y/o Promocional */
DEF VAR X-TIPDTO AS CHAR INIT "DVXDSF" NO-UNDO.     /* Tipo de descuento aplicado (PROM, VOL) */ 
DEF VAR F-FACTOR AS DECI NO-UNDO.

/* Variables para definir la lista de precios */
/* CONTROL POR DIVISION */
DEF VAR x-FlgDtoVol LIKE GN-DIVI.FlgDtoVol NO-UNDO.
/* ****************************************** */
/* CONFIGURACIONES DE LA DIVISION */
/* ****************************************** */
FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = pCodDiv NO-LOCK.
ASSIGN
    x-FlgDtoVol = GN-DIVI.FlgDtoVol             /* Descuento por Volumen */
    .
/* ****************************************** */
/* RHC 06/09/2018 CONTROL POR LISTA DE PRECIO */
/* ****************************************** */
FIND VtaCTabla WHERE VtaCTabla.CodCia = s-CodCia AND
    VtaCTabla.Tabla = 'CFGLP' AND
    VtaCTabla.Llave = pCodDiv NO-LOCK NO-ERROR.
IF AVAILABLE VtaCTabla THEN x-FlgDtoVol = VtaCTabla.Libre_l01.
/* ****************************************** */
IF x-FlgDtoVol = NO THEN RETURN 'OK'.    /* <<< OJO: La Lista debe estar configurada <<< */

EMPTY TEMP-TABLE ResumenxLinea.
EMPTY TEMP-TABLE ErroresxLinea.

/* ****************************** */
/* 1ro. Acumulamos las cantidades */
/* ****************************** */
FOR EACH B-DPEDI OF B-CPEDI NO-LOCK,
    FIRST Almmmatg OF B-DPEDI NO-LOCK,
    FIRST Almsfami OF Almmmatg NO-LOCK,
    FIRST FacTabla NO-LOCK WHERE FacTabla.codcia = s-codcia
        AND FacTabla.Tabla = X-TIPDTO
        AND FacTabla.Codigo = TRIM(pCodDiv) + '|' + TRIM(Almsfami.codfam) + '|' + TRIM(Almsfami.subfam)
        AND FacTabla.Nombre > "":
    /* Transformamos la cantidad en unidad base a cantidad en unidad de dcto x volumen */
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
        AND Almtconv.Codalter = FacTabla.Nombre
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
        MESSAGE 'NO est� configurado el factor de equivalencia para el producto' Almmmatg.codmat SKIP
            '        Unidad Base:' Almmmatg.UndBas SKIP
            'Unidad de Sub-Linea:' FacTabla.Nombre SKIP(1)
            'SE CONTINUAR� CON OTRO ART�CULO' SKIP(2)
            '*** Avisar a Sistemas ***'
            VIEW-AS ALERT-BOX WARNING TITLE "DESCUENTO POR VOLUMEN POR LINEA".
        FIND FIRST ErroresxLinea WHERE ErroresxLinea.codfam = Almmmatg.codfam
            AND ErroresxLinea.subfam = Almmmatg.subfam NO-ERROR.
        IF NOT AVAILABLE ErroresxLinea THEN CREATE ErroresxLinea.
        ASSIGN
            ErroresxLinea.codfam = Almmmatg.codfam
            ErroresxLinea.subfam = Almmmatg.subfam.
        NEXT.
    END.
    ASSIGN
        F-FACTOR = Almtconv.Equival.
    /* ******************************************************************************* */
    FIND FIRST ResumenxLinea WHERE ResumenxLinea.codfam = Almmmatg.codfam
        AND ResumenxLinea.subfam = Almmmatg.subfam NO-ERROR.
    IF NOT AVAILABLE ResumenxLinea THEN CREATE ResumenxLinea.
    ASSIGN
        ResumenxLinea.codfam = Almmmatg.codfam
        ResumenxLinea.subfam = Almmmatg.subfam
        ResumenxLinea.canped = ResumenxLinea.canped + (B-DPEDI.canped * B-DPEDI.factor / f-Factor).
END.
/* ************************************** */
/* 2do. Eliminamos las lineas con errores */
/* ************************************** */
FOR EACH ErroresxLinea:
    FIND ResumenxLinea WHERE ResumenxLinea.codfam = ErroresxLinea.codfam
        AND ResumenxLinea.subfam = ErroresxLinea.subfam
        NO-ERROR.
    IF AVAILABLE ResumenxLinea THEN DELETE ResumenxLinea.
END.

/* ************************************** */
PRINCIPAL:
/* ************************************** */
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FOR EACH ResumenxLinea NO-LOCK, 
        FIRST Almsfami NO-LOCK WHERE Almsfami.codcia = s-codcia
            AND Almsfami.codfam = ResumenxLinea.codfam
            AND Almsfami.subfam = ResumenxLinea.subfam,
        FIRST FacTabla NO-LOCK WHERE FacTabla.codcia = s-codcia
            AND FacTabla.Tabla = X-TIPDTO
            AND FacTabla.Codigo = TRIM(pCodDiv) + '|' + TRIM(Almsfami.codfam) + '|' + TRIM(Almsfami.subfam):
        /* Buscamos descuento promocional y volumen */
        ASSIGN
            x-DctoPromocional = 0
            x-DctoxVolumen = 0
            x-Rango = 0
            X-CANTI = ResumenxLinea.canped.
        DO J = 1 TO 10:
            IF X-CANTI >= FacTabla.Valor[j] AND FacTabla.Valor[j + 10] > 0  THEN DO:
                IF X-RANGO  = 0 THEN X-RANGO = FacTabla.Valor[j].
                IF X-RANGO <= FacTabla.Valor[j] THEN DO:
                    ASSIGN
                        X-RANGO  = FacTabla.Valor[j]
                        x-DctoxVolumen = FacTabla.Valor[j + 10].
                END.   
            END.   
        END.
        /* ********************************************************** */
        IF x-DctoxVolumen <= 0 THEN NEXT.
        /* ********************************************************** */
        DEF VAR f-Dsctos AS DEC NO-UNDO.
        DEF VAR z-Dsctos AS DEC NO-UNDO.
        DEF VAR f-FleteUnitario AS DEC NO-UNDO.
        DEF VAR x-AlmDes AS CHAR NO-UNDO.
        DEF VAR s-UndVta AS CHAR NO-UNDO.

        FOR EACH B-DPEDI OF B-CPEDI NO-LOCK,
            FIRST Almmmatg OF B-DPEDI NO-LOCK WHERE Almmmatg.codfam = ResumenxLinea.codfam 
                AND Almmmatg.subfam = ResumenxLinea.subfam:
            FIND Facdpedi WHERE ROWID(Facdpedi) = ROWID(B-DPEDI) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF ERROR-STATUS:ERROR = YES THEN DO:
                {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
                UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
            END.
            /* ********************************************************************************************** */
            /* Recalculamos todos los Items */
            /* Se actualiza el precio base */
            /* ********************************************************************************************** */
            ASSIGN
                x-DctoPromocional = 0.
            ASSIGN
                Facdpedi.PreUni = Facdpedi.PreBas   /* OJO <<<<<<<<<<<<<<<< */
                Y-DSCTOS = ( 1 - (1 - x-DctoxVolumen / 100) * (1 - x-DctoPromocional / 100) ) * 100     /* <<<<<<<<<< OJO <<<<<< */
                .
            ASSIGN 
                Facdpedi.PorDto  = 0
                Facdpedi.PorDto2 = 0            /* el precio unitario */
                Facdpedi.Por_Dsctos[1] = 0      /* del administrador */
                Facdpedi.Por_Dsctos[2] = 0      /* del evento */
                Facdpedi.Por_Dsctos[3] = Y-DSCTOS 
                Facdpedi.ImpIsc = 0
                Facdpedi.ImpIgv = 0
                Facdpedi.Libre_c04 = x-TipDto.
            /* ***************************************************************** */
            {vtagn/CalculoDetalleMayorCredito.i &Tabla="Facdpedi"}
            /* ***************************************************************** */
        END.
    END.
END.
IF AVAILABLE(Facdpedi) THEN RELEASE Facdpedi.
RETURN 'OK'.

END PROCEDURE.

/*
            WHEN "E" THEN DO:   /* EVENTOS */
                /* ********************************************************************************************** */
                /* Recalculamos todos los Items */
                /* ********************************************************************************************** */
                FOR EACH B-DPEDI OF B-CPEDI NO-LOCK, 
                    FIRST Almmmatg OF B-DPEDI NO-LOCK WHERE Almmmatg.codfam = ResumenxLinea.codfam 
                        AND Almmmatg.subfam = ResumenxLinea.subfam:
                    FIND Facdpedi WHERE ROWID(Facdpedi) = ROWID(B-DPEDI) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                    IF ERROR-STATUS:ERROR = YES THEN DO:
                        {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
                        UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
                    END.
                    ASSIGN
                        Y-DSCTOS = x-DctoxVolumen.
                    ASSIGN 
                        Facdpedi.PorDto  = 0
                        Facdpedi.PorDto2 = 0            /* el precio unitario */
                        Facdpedi.Por_Dsctos[1] = 0      /* del administrador */
                        Facdpedi.Por_Dsctos[2] = 0      /* del evento */
                        Facdpedi.Por_Dsctos[3] = Y-DSCTOS 
                        Facdpedi.ImpIsc = 0
                        Facdpedi.ImpIgv = 0
                        Facdpedi.Libre_c04 = x-TipDto.
                    {vtagn/calcula-linea-detalle-mayorista.i &Tabla="Facdpedi" }
                END.
            END.
            OTHERWISE DO:
                FOR EACH B-DPEDI OF B-CPEDI NO-LOCK,
                    FIRST Almmmatg OF B-DPEDI NO-LOCK WHERE Almmmatg.codfam = ResumenxLinea.codfam 
                        AND Almmmatg.subfam = ResumenxLinea.subfam:
                    FIND Facdpedi WHERE ROWID(Facdpedi) = ROWID(B-DPEDI) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                    IF ERROR-STATUS:ERROR = YES THEN DO:
                        {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
                        UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
                    END.
                    /* ********************************************************************************************** */
                    /* Recalculamos todos los Items */
                    /* Se actualiza el precio base */
                    /* ********************************************************************************************** */
                    ASSIGN
                        x-DctoPromocional = 0.
                    ASSIGN
                        F-PREBAS = Almmmatg.PreVta[1]       /* OJO => Se cambia Precio Base */
                        Y-DSCTOS = ( 1 - (1 - x-DctoxVolumen / 100) * (1 - x-DctoPromocional / 100) ) * 100     /* <<<<<<<<<< OJO <<<<<< */
                        .
                    IF Gn-Divi.VentaMayorista = 2 THEN DO:  /* OJO: Lista por Divisi�n */
                        FIND FIRST VtaListaMay OF Almmmatg WHERE VtaListaMay.CodDiv = pCodDiv NO-LOCK NO-ERROR.
                        F-PreBas = VtaListaMay.PreOfi.
                    END.
                    /* ******************************** */
                    /* PRECIO BASE A LA MONEDA DE VENTA */
                    /* ******************************** */
                    IF S-CODMON = 1 THEN DO:
                        IF Almmmatg.MonVta = 1 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                        ELSE ASSIGN F-PREBAS = F-PREBAS * Almmmatg.TpoCmb.
                    END.
                    IF S-CODMON = 2 THEN DO:
                        IF Almmmatg.MonVta = 2 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                        ELSE ASSIGN F-PREBAS = (F-PREBAS / Almmmatg.TpoCmb).
                    END.
                    F-PREVTA = F-PREBAS.
                    /************************************************/
                    RUN BIN/_ROUND1(F-PREVTA,s-NRODEC,OUTPUT F-PREVTA).
                    /************************************************/
                    ASSIGN 
                        Facdpedi.PorDto  = 0
                        Facdpedi.PorDto2 = 0            /* el precio unitario */
                        Facdpedi.Por_Dsctos[1] = 0      /* del administrador */
                        Facdpedi.Por_Dsctos[2] = 0      /* del evento */
                        Facdpedi.Por_Dsctos[3] = Y-DSCTOS 
                        Facdpedi.ImpIsc = 0
                        Facdpedi.ImpIgv = 0
                        Facdpedi.Libre_c04 = x-TipDto.
                    /*{vtagn/calcula-linea-detalle-mayorista.i &Tabla="Facdpedi" }*/
                    /* ***************************************************************** */
                    {vtagn/CalculoDetalleMayorCredito.i &Tabla="Facdpedi"}
                    /* ***************************************************************** */
                END.
            END.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DCTO_VOL_SALDO) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DCTO_VOL_SALDO Procedure 
PROCEDURE DCTO_VOL_SALDO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.    
DEF INPUT PARAMETER s-TpoPed AS CHAR.
DEF INPUT PARAMETER pCodDiv AS CHAR.
DEF OUTPUT PARAMETER pMensaje AS CHAR.

DEF VAR s-NroDec AS INTE  NO-UNDO.
DEF VAR s-PorIgv AS DECI  NO-UNDO.
DEF VAR s-CodMon AS DECI  NO-UNDO.
DEF VAR s-CodCli AS CHAR  NO-UNDO.
DEF VAR s-Cmpbnte AS CHAR NO-UNDO.

/* ************************************************************* */
/* RHC 14/10/2013 Descuentos por Volumen de Compra Acumulados    */
/* SOLO VENTA NORMAL LIMA (N, CO) */
/* ************************************************************* */
IF LOOKUP(s-TpoPed, "N,CO") = 0 THEN RETURN.

/* ************************************************************* */
/* CONTROL POR DIVISION */
/* ************************************************************* */
DEF VAR x-FlgDtoVol LIKE GN-DIVI.FlgDtoVol NO-UNDO.
/* ****************************************** */
/* CONFIGURACIONES DE LA DIVISION */
/* ****************************************** */
FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = pCodDiv NO-LOCK.
ASSIGN
    x-FlgDtoVol = GN-DIVI.FlgDtoVol             /* Descuento por Volumen */
    .
/* ****************************************** */
/* RHC 06/09/2018 CONTROL POR LISTA DE PRECIO */
/* ****************************************** */
FIND VtaCTabla WHERE VtaCTabla.CodCia = s-CodCia AND
    VtaCTabla.Tabla = 'CFGLP' AND
    VtaCTabla.Llave = pCodDiv NO-LOCK NO-ERROR.
IF AVAILABLE VtaCTabla THEN x-FlgDtoVol = VtaCTabla.Libre_l01.

IF x-FlgDtoVol = NO THEN RETURN 'OK'.    /* <<< OJO: La Lista debe estar configurada <<< */
/* ************************************************************* */
/* ************************************************************* */

DEF VAR pCuenta AS INT NO-UNDO.

DEF VAR j AS INT NO-UNDO.
DEF VAR x-Canti AS DEC NO-UNDO.
DEF VAR x-Rango AS DEC NO-UNDO.
DEF VAR x-DctoxVolumen AS DECIMAL DECIMALS 4 NO-UNDO.
DEF VAR X-TIPDTO AS CHAR INIT "DVXSALDOC" NO-UNDO.               /* Tipo de descuento aplicado (PROM, VOL) */ 
DEF VAR F-FACTOR AS DECI NO-UNDO.

/* BARREMOS TODAS LAS PROMOCIONES POR SALDOS */
FIND B-CPEDI WHERE ROWID(B-CPEDI) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CPEDI THEN RETURN 'ADM-ERROR'.

ASSIGN
    s-NroDec = B-CPEDI.Libre_d01
    s-PorIgv = B-CPEDI.PorIgv
    s-CodMon = B-CPEDI.CodMon
    s-CodCli = B-CPEDI.CodCli
    s-Cmpbnte = B-CPEDI.Cmpbnte.

/* ************************************** */
PRINCIPAL:
/* ************************************** */
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Barremos Promoci�n por Promoci�n ACTIVA */
    FOR EACH FacTabla NO-LOCK WHERE FacTabla.codcia = s-codcia AND FacTabla.Tabla = X-TIPDTO
        AND TODAY >= FacTabla.Campo-D[1] 
        AND TODAY <= FacTabla.Campo-D[2] :
        /* POR CADA PROMOCION UN CALCULO NUEVO */
        EMPTY TEMP-TABLE ResumenxLinea.
        EMPTY TEMP-TABLE ErroresxLinea.
        /* *********************************** */
        FOR EACH B-FacTabla NO-LOCK WHERE B-FacTabla.codcia = s-codcia
            AND B-FacTabla.Tabla = "DVXSALDOD"
            AND B-FacTabla.Codigo BEGINS FacTabla.Codigo,
            FIRST Facdpedi OF B-CPEDI NO-LOCK WHERE Facdpedi.codmat = B-FacTabla.Campo-C[1],
            FIRST Almmmatg OF Facdpedi NO-LOCK:
            /* Transformamos la cantidad en unidad base a cantidad en unidad de dcto x volumen */
            FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
                AND Almtconv.Codalter = FacTabla.Campo-C[1]
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almtconv THEN DO:
                MESSAGE 'NO est� configurado el factor de equivalencia para el producto' Almmmatg.codmat SKIP
                    '        Unidad Base:' Almmmatg.UndBas SKIP
                    'Unidad de Sub-Linea:' FacTabla.Campo-C[1] SKIP(1)
                    'SE CONTINUAR� CON OTRO ART�CULO' SKIP(2)
                    '*** Avisar a Sistemas ***'
                    VIEW-AS ALERT-BOX WARNING TITLE "DESCUENTO POR VOLUMEN POR SALDOS".
                FIND FIRST ErroresxLinea WHERE ErroresxLinea.codmat = Almmmatg.codmat NO-ERROR.
                IF NOT AVAILABLE ErroresxLinea THEN CREATE ErroresxLinea.
                ASSIGN
                    ErroresxLinea.codmat = Almmmatg.codmat.
                NEXT.
            END.
            ASSIGN
                F-FACTOR = Almtconv.Equival.
            /* ******************************************************************************* */
            FIND FIRST ResumenxLinea WHERE ResumenxLinea.codmat = Almmmatg.codmat NO-ERROR.
            IF NOT AVAILABLE ResumenxLinea THEN CREATE ResumenxLinea.
            ASSIGN
                ResumenxLinea.codmat = Almmmatg.codmat
                ResumenxLinea.canped = ResumenxLinea.canped + (facdpedi.canped * facdpedi.factor / f-Factor).
        END.
        X-CANTI = 0.
        FOR EACH ResumenxLinea:
            X-CANTI = X-CANTI + ResumenxLinea.canped.
        END.
        /* AHORA S� APLICAMOS DESCUENTOS */
        ASSIGN
            x-DctoxVolumen = 0
            x-Rango = 0.
        DO J = 1 TO 10:
            IF X-CANTI >= FacTabla.Valor[j] AND FacTabla.Valor[j + 10] > 0  THEN DO:
                IF X-RANGO  = 0 THEN X-RANGO = FacTabla.Valor[j].
                IF X-RANGO <= FacTabla.Valor[j] THEN DO:
                    ASSIGN
                        X-RANGO  = FacTabla.Valor[j]
                        x-DctoxVolumen = FacTabla.Valor[j + 10].
                END.   
            END.   
        END.
        /* ************************************************************ */
        /* RHC 28/10/2013 SE VA A RECALCULAR EL PRECIO DE LA COTIZACION */
        /* ************************************************************ */
        IF x-DctoxVolumen <= 0 THEN NEXT.
        /* ************************************************************ */
        FOR EACH ResumenxLinea NO-LOCK,
            FIRST B-DPEDI OF B-CPEDI WHERE B-DPEDI.codmat = ResumenxLinea.codmat NO-LOCK,
            FIRST Almmmatg OF B-DPEDI NO-LOCK,
            FIRST Almsfami OF Almmmatg NO-LOCK:
            FIND Facdpedi WHERE ROWID(Facdpedi) = ROWID(B-DPEDI) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF ERROR-STATUS:ERROR = YES THEN DO:
                {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
                UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
            END.
            ASSIGN 
                /* Se mantiene el precio unitario */
                Facdpedi.Por_Dsctos[1] = x-DctoxVolumen     /* Dcto Administrador */
                /* Se mantienen los otros descuentos */
                Facdpedi.Libre_c04 = x-TipDto.
/*             ASSIGN                                                             */
/*                 Facdpedi.PreUni = Facdpedi.PreBas   /* OJO <<<<<<<<<<<<<<<< */ */
/*                 Facdpedi.PreUni = Facdpedi.PreBas   /* OJO <<<<<<<<<<<<<<<< */ */
/*                 Facdpedi.Por_Dsctos[1] = 0     /* Dcto Administrador */        */
/*                 Facdpedi.Por_Dsctos[2] = 0     /* Dcto Evento */               */
/*                 Facdpedi.Por_Dsctos[3] = x-DctoxVolumen     /* Dcto VOL */     */
/*                 Facdpedi.Libre_c04 = x-TipDto.                                 */
            /* ***************************************************************** */
            /*{vtagn/CalculoDetalleMayorCredito.i &Tabla="Facdpedi"}*/
            {vtagn/CalculoDetalleMayorCreditoFinal.i &Tabla="Facdpedi"}
            /* ***************************************************************** */
        END.
    END.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-DCTO_VOL_SALDO_EVENTO) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DCTO_VOL_SALDO_EVENTO Procedure 
PROCEDURE DCTO_VOL_SALDO_EVENTO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.    
DEF INPUT PARAMETER s-TpoPed AS CHAR.
DEF INPUT PARAMETER pCodDiv AS CHAR.
DEF OUTPUT PARAMETER pMensaje AS CHAR.

DEF VAR s-NroDec AS INTE NO-UNDO.
DEF VAR s-PorIgv AS DECI NO-UNDO.
DEF VAR s-CodMon AS DECI NO-UNDO.
DEF VAR s-CodCli AS CHAR NO-UNDO.
DEF VAR s-Cmpbnte AS CHAR NO-UNDO.

/* ************************************************************* */
/* RHC 14/10/2013 Descuentos por Volumen de Compra Acumulados    */
/* SOLO EXPOLIBRERIA (E)                                         */
/* ************************************************************* */
IF LOOKUP(s-TpoPed, "E") = 0 THEN RETURN.

/* ************************************************************* */
/* CONTROL POR DIVISION */
/* ************************************************************* */
DEF VAR x-FlgDtoVol LIKE GN-DIVI.FlgDtoVol NO-UNDO.
/* ****************************************** */
/* CONFIGURACIONES DE LA DIVISION */
/* ****************************************** */
FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = pCodDiv NO-LOCK.
ASSIGN
    x-FlgDtoVol = GN-DIVI.FlgDtoVol             /* Descuento por Volumen */
    .
/* ****************************************** */
/* RHC 06/09/2018 CONTROL POR LISTA DE PRECIO */
/* ****************************************** */
FIND VtaCTabla WHERE VtaCTabla.CodCia = s-CodCia AND
    VtaCTabla.Tabla = 'CFGLP' AND
    VtaCTabla.Llave = pCodDiv NO-LOCK NO-ERROR.
IF AVAILABLE VtaCTabla THEN x-FlgDtoVol = VtaCTabla.Libre_l01.

IF x-FlgDtoVol = NO THEN RETURN 'OK'.    /* <<< OJO: La Lista debe estar configurada <<< */

/* ************************************************************* */
/* ************************************************************* */

FIND B-CPEDI WHERE ROWID(B-CPEDI) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CPEDI THEN RETURN 'ADM-ERROR'.

ASSIGN
    s-NroDec = B-CPEDI.Libre_d01
    s-PorIgv = B-CPEDI.PorIgv
    s-CodMon = B-CPEDI.CodMon
    s-CodCli = B-CPEDI.CodCli
    s-Cmpbnte = B-CPEDI.Cmpbnte
    .

DEF VAR j AS INT NO-UNDO.
DEF VAR x-Canti AS DEC NO-UNDO.
DEF VAR x-Rango AS DEC NO-UNDO.
DEF VAR x-DctoxVolumen AS DECIMAL DECIMALS 4 NO-UNDO.
DEF VAR X-TIPDTO AS CHAR INIT 'EDVXSALDOC' NO-UNDO.     /* Tipo de descuento aplicado */ 
DEF VAR F-FACTOR AS DECI NO-UNDO.

/* ************************************** */
PRINCIPAL:
/* ************************************** */
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* BARREMOS TODAS LAS PROMOCIONES POR SALDOS */
    FOR EACH FacTabla NO-LOCK WHERE FacTabla.codcia = s-codcia AND 
        FacTabla.Tabla = X-TIPDTO AND
        FacTabla.Codigo BEGINS pCodDiv AND
        TODAY >= FacTabla.Campo-D[1] AND
        TODAY <= FacTabla.Campo-D[2]:
        /* POR CADA PROMOCION UN CALCULO NUEVO */
        EMPTY TEMP-TABLE ResumenxLinea.
        EMPTY TEMP-TABLE ErroresxLinea.
        /* *********************************** */
        FOR EACH B-FacTabla NO-LOCK WHERE B-FacTabla.codcia = s-codcia
            AND B-FacTabla.Tabla = "EDVXSALDOD"
            AND B-FacTabla.Codigo BEGINS FacTabla.Codigo,
            FIRST Facdpedi OF B-CPEDI NO-LOCK WHERE Facdpedi.codmat = B-FacTabla.Campo-C[1],
            FIRST Almmmatg OF Facdpedi NO-LOCK:
            /* Transformamos la cantidad en unidad base a cantidad en unidad de dcto x volumen */
            FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
                AND Almtconv.Codalter = FacTabla.Campo-C[1]
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almtconv THEN DO:
                MESSAGE 'NO est� configurado el factor de equivalencia para el producto' Almmmatg.codmat SKIP
                    '        Unidad Base:' Almmmatg.UndBas SKIP
                    'Unidad de Sub-Linea:' FacTabla.Campo-C[1] SKIP(1)
                    'SE CONTINUAR� CON OTRO ART�CULO' SKIP(2)
                    '*** Avisar a Sistemas ***'
                    VIEW-AS ALERT-BOX WARNING TITLE "DESCUENTO POR VOLUMEN POR SALDOS".
                FIND FIRST ErroresxLinea WHERE ErroresxLinea.codmat = Almmmatg.codmat NO-ERROR.
                IF NOT AVAILABLE ErroresxLinea THEN CREATE ErroresxLinea.
                ASSIGN
                    ErroresxLinea.codmat = Almmmatg.codmat.
                NEXT.
            END.
            ASSIGN
                F-FACTOR = Almtconv.Equival.
            /* ******************************************************************************* */
            FIND FIRST ResumenxLinea WHERE ResumenxLinea.codmat = Almmmatg.codmat NO-ERROR.
            IF NOT AVAILABLE ResumenxLinea THEN CREATE ResumenxLinea.
            ASSIGN
                ResumenxLinea.codmat = Almmmatg.codmat
                ResumenxLinea.canped = ResumenxLinea.canped + (facdpedi.canped * facdpedi.factor / f-Factor).
        END.
        X-CANTI = 0.
        FOR EACH ResumenxLinea:
            X-CANTI = X-CANTI + ResumenxLinea.canped.
        END.
        /* AHORA S� APLICAMOS DESCUENTOS */
        ASSIGN
            x-DctoxVolumen = 0
            x-Rango = 0.
        DO J = 1 TO 10:
            IF X-CANTI >= FacTabla.Valor[j] AND FacTabla.Valor[j + 10] > 0  THEN DO:
                IF X-RANGO  = 0 THEN X-RANGO = FacTabla.Valor[j].
                IF X-RANGO <= FacTabla.Valor[j] THEN DO:
                    ASSIGN
                        X-RANGO  = FacTabla.Valor[j]
                        x-DctoxVolumen = FacTabla.Valor[j + 10].
                END.   
            END.   
        END.
        
        IF x-DctoxVolumen <= 0 THEN NEXT.
        /* ************************************************************ */
        /* RHC 28/10/2013 SE VA A RECALCULAR EL PRECIO DE LA COTIZACION */
        /* RHC 27/12/2018 SE VA A TOMAR EL PRECIO BASE                  */
        /* ************************************************************ */
        FOR EACH ResumenxLinea NO-LOCK,
            FIRST B-DPEDI OF B-CPEDI WHERE B-DPEDI.codmat = ResumenxLinea.codmat NO-LOCK,
            FIRST Almmmatg OF B-DPEDI NO-LOCK:
            FIND Facdpedi WHERE ROWID(Facdpedi) = ROWID(B-DPEDI) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF ERROR-STATUS:ERROR = YES THEN DO:
                {lib/mensaje-de-error.i &MensajeError="pMensaje"}
                UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
            END.
            ASSIGN 
                Facdpedi.PreUni        = Facdpedi.PreBas    /* Se cambia al precio de oficina (lista) */
                Facdpedi.Por_Dsctos[1] = 0                  /* Todos los dem�s descuentos */
                Facdpedi.Por_Dsctos[2] = 0                  /* NO se toman en cuenta */
                Facdpedi.Por_Dsctos[3] = x-DctoxVolumen     /* Dcto VOL o PROM */
                Facdpedi.Libre_c04 = x-TipDto.
            /* ***************************************************************** */
            {vtagn/CalculoDetalleMayorCredito.i &Tabla="Facdpedi"}
            /* ***************************************************************** */
        END.
    END.
END.
RETURN 'OK'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PED_Add-Record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PED_Add-Record Procedure 
PROCEDURE PED_Add-Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER s-PorIgv AS DEC.
DEF OUTPUT PARAMETER pFchEnt AS DATE.
DEF OUTPUT PARAMETER xNroPCO AS CHAR.
DEF OUTPUT PARAMETER s-NroCot AS CHAR.
DEF OUTPUT PARAMETER s-CodAlm AS CHAR.
DEF OUTPUT PARAMETER s-Tipo-Abastecimiento AS CHAR.
DEF OUTPUT PARAMETER TABLE FOR PEDI. 

EMPTY TEMP-TABLE PEDI.

DEF VAR s-CodRef AS CHAR INIT "COT" NO-UNDO.

ASSIGN
    xNroPCO = ''
    s-NroCot = ''
    pFchEnt = TODAY.
/* ************************************************************************************** */
/* 1ro. Solicitamos la COTIZACION o PCO */
/* ************************************************************************************** */

RUN logis/d-cot-pco-pendientes.r(OUTPUT s-CodRef, OUTPUT s-NroCot).
IF TRUE <> (s-NroCot > "") THEN RETURN "ADM-ERROR".
/* ************************************************************************************** */
/* 2do. Solicitamos los ITEMS de la COTIZACION */
/* ************************************************************************************** */
CASE TRUE:
  WHEN s-CodRef = "PCO" THEN DO:
      /* POR PRE-COTIZACIONES */
      s-CodAlm = ''.
      xNroPCO = s-NroCot.
      FOR EACH Vtaddocu NO-LOCK  WHERE VtaDDocu.CodCia = s-CodCia AND
          VtaDDocu.CodPed = s-CodRef AND
          VtaDDocu.NroPed = xNroPCO
          BREAK BY VtaDDocu.AlmDes:
          IF FIRST-OF(VtaDDocu.AlmDes) THEN DO:
              s-CodAlm = s-CodAlm + (IF TRUE <> (s-CodAlm > '') THEN '' ELSE ',') +
                  VtaDDocu.AlmDes.
          END.
      END.
      FIND FIRST PCO WHERE PCO.CodCia = s-CodCia AND 
          PCO.CodDoc = "PCO" AND 
          PCO.NroPed = xNroPCO
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE PCO THEN DO:
          MESSAGE 'No se pudo ubicar la PRE-COTIZACION' xNroPco VIEW-AS ALERT-BOX ERROR.
          RETURN 'ADM-ERROR'.
      END.
      ASSIGN 
          s-NroCot = ENTRY(1, xNroPCO, '-')   /* <<< OJO <<< */
          pFchEnt = PCO.FchEnt
          s-Tipo-Abastecimiento = "PCO".
  END.
  OTHERWISE DO:
      /* PEDIDOS NORMALES */
      /*RUN vta2/dpedidotablet-v2 (INPUT s-CodRef, INPUT s-NroCot, OUTPUT s-CodAlm).*/
      RUN logis/d-pedido-tablet.r (INPUT s-CodRef, INPUT s-NroCot, OUTPUT s-CodAlm).
      IF TRUE <> (s-CodAlm > "") THEN RETURN "ADM-ERROR".
      /* *********************************** */
      FIND FIRST COTIZACION WHERE COTIZACION.codcia = s-codcia
              AND COTIZACION.coddiv = s-CodDiv
              AND COTIZACION.coddoc = s-CodRef
              AND COTIZACION.nroped = s-NroCot
              NO-LOCK NO-ERROR.
      IF NOT AVAILABLE COTIZACION THEN DO:
          MESSAGE 'Cotizaci�n' s-NroCot 'NO se pudo ubicar' VIEW-AS ALERT-BOX ERROR.
          RETURN 'ADM-ERROR'.
      END.
      ASSIGN
          pFchEnt = COTIZACION.FchEnt
          s-Tipo-Abastecimiento = "NORMAL".
  END.
END CASE.
FIND FIRST COTIZACION WHERE COTIZACION.codcia = s-CodCia AND
    COTIZACION.coddiv = s-CodDiv AND
    COTIZACION.coddoc = "COT" AND
    COTIZACION.nroped = s-NroCot NO-LOCK NO-ERROR.
IF NOT AVAILABLE COTIZACION THEN DO:
    MESSAGE 'Cotizaci�n' s-NroCot 'NO se pudo ubicar' VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
END.

/* DISTRIBUYE LOS PRODUCTOS POR ORDEN DE ALMACENES */
CASE s-Tipo-Abastecimiento:
    WHEN "PCO" THEN DO:
        RUN PED_Asigna-PCO (xNroPCO, s-PorIgv).
        IF RETURN-VALUE = "ADM-ERROR" THEN RETURN 'ADM-ERROR'.
    END.
    OTHERWISE DO:
        RUN PED_Asigna-Cotizacion (s-CodAlm, s-PorIgv).
        IF RETURN-VALUE = "ADM-ERROR" THEN RETURN 'ADM-ERROR'.
    END.
END CASE.

RETURN 'OK'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PED_Asigna-Cotizacion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PED_Asigna-Cotizacion Procedure 
PROCEDURE PED_Asigna-Cotizacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pCodAlm AS CHAR.
  DEFINE INPUT PARAMETER s-PorIgv AS DEC.

  DEFINE VARIABLE I-NPEDI AS INTEGER NO-UNDO.
  DEFINE VARIABLE f-Factor AS DEC NO-UNDO.
  DEFINE VARIABLE x-CanPed AS DEC NO-UNDO.
  DEFINE VARIABLE s-StkComprometido AS DEC.
  DEFINE VARIABLE s-StkDis AS DEC NO-UNDO.
  DEFINE VARIABLE F-CANPED AS DECIMAL NO-UNDO.
  DEFINE VARIABLE x-StkAct AS DEC NO-UNDO.
  DEFINE VARIABLE x-CodAlm AS CHAR NO-UNDO.
  DEFINE VARIABLE i AS INT NO-UNDO.

  /* **************************************************** */
  /* RHC 31/01/2018 PARAMETROS DE ACUERDO A LA COTIZACION */
  /* **************************************************** */
  DEF BUFFER B-DIVI FOR gn-divi.
  FIND B-DIVI WHERE B-DIVI.codcia = s-codcia AND B-DIVI.coddiv = COTIZACION.Libre_c01 NO-LOCK NO-ERROR.
  IF AVAILABLE B-DIVI THEN DO:
      ASSIGN
          s-DiasVtoPed = B-DIVI.DiasVtoPed
          s-FlgEmpaque = B-DIVI.FlgEmpaque
          s-VentaMayorista = B-DIVI.VentaMayorista.
  END.
  /* **************************************************** */
  /* **************************************************** */
  DEFINE FRAME F-Mensaje
    'Procesando: ' Facdpedi.codmat SKIP(1)
    'Espere un momento por favor ...' SKIP
    WITH CENTERED NO-LABELS OVERLAY VIEW-AS DIALOG-BOX TITLE 'TRASLADANDO COTIZACION'.

  i-NPedi = 0.
  /* VERIFICACION DE LOS SALDOS DE LA COTIZACION */
  FOR EACH Facdpedi OF COTIZACION NO-LOCK WHERE (Facdpedi.CanPed - Facdpedi.CanAte) > 0:
      IF Facdpedi.CanAte < 0 THEN DO:   /* HAY UN NEGATIVO */
          MESSAGE 'Hay una incosistencia el el producto:' Facdpedi.codmat SKIP
              'Proceso abortado'
              VIEW-AS ALERT-BOX WARNING.
          RETURN 'ADM-ERROR'.
      END.
  END.
  /* CARGAMOS STOCK DISPONIBLE */
  /* s-CodAlm: Tiene uno solo almacenes configurados */
  DEF VAR t-AlmDes AS CHAR NO-UNDO.
  DEF VAR t-CanPed AS DEC NO-UNDO.
  DEF VAR pSugerido AS DEC NO-UNDO.
  DEF VAR pEmpaque AS DEC NO-UNDO.

  /* ********************************************* */
  /* LIMPIAMOS LA TABLA DE LOG DE TOPE DE DESPACHO */
  /* ********************************************* */
  EMPTY TEMP-TABLE PEDI.
  EMPTY TEMP-TABLE T-LogTabla.
  /* ********************************************* */
  ALMACENES:
  FOR EACH Facdpedi OF COTIZACION NO-LOCK WHERE (Facdpedi.CanPed - Facdpedi.CanAte) > 0,
      FIRST Almmmatg OF Facdpedi NO-LOCK,
      FIRST Almtfami OF Almmmatg NO-LOCK
      BY Facdpedi.CodMat:
      DISPLAY Facdpedi.codmat WITH FRAME F-Mensaje.
      ASSIGN
          f-Factor = Facdpedi.Factor
          t-AlmDes = ''
          t-CanPed = 0.
      x-CodAlm = ENTRY(1, pCodAlm).   /* Por si acaso, aunque solo deber�a tener un almac�n */
      F-CANPED = (FacDPedi.CanPed - FacDPedi.CanAte).
      /* FILTROS */
      FIND FIRST Almmmate WHERE Almmmate.codcia = s-codcia
          AND Almmmate.codalm = x-CodAlm  /* *** OJO *** */
          AND Almmmate.codmat = Facdpedi.CodMat
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Almmmate THEN DO:
          MESSAGE 'Producto' Facdpedi.codmat 'NO asignado al almac�n' x-CodAlm SKIP(1)
              'Comunicarse con el Gestor de Almacenes'
              VIEW-AS ALERT-BOX INFORMATION
              TITLE 'VERIFICACION DE ARTICULOS'.
          IF COTIZACION.TpoPed = "LF" THEN DO:
              RETURN 'ADM-ERROR'.
          END.
          NEXT ALMACENES.
      END.
      /* ******************************************************************** */
      /* RHC 23/07/2018 SOLAMENTE CARGAMOS T-LOGTABLA CON EL TOPE DE DESPACHO */
      /* ******************************************************************** */
      DEF VAR pAlmSug AS CHAR NO-UNDO.
      DEF VAR pRetirar AS LOG NO-UNDO.
      RUN alm/p-tope-pedido (Facdpedi.CodMat,
                             x-CodAlm,
                             f-CanPed * f-Factor,
                             YES,
                             INPUT-OUTPUT TABLE T-LogTabla,
                             OUTPUT pAlmSug,
                             OUTPUT pRetirar).
      /*IF pRetirar = YES THEN NEXT ALMACENES.*/
      /* ******************************************************************** */
      /* RHC 09/07/2020 NO verifica stock disponible en los siguientes casos:
      - Impuesto a la bolsa pl�stica
      - Servicios 
      - Drop Shipping
      */
      /* ******************************************************************** */
      /* Stock Disponible */
      x-StkAct = 0.
      s-StkDis = F-CANPED * f-Factor.  /* OJO -> Artificio en caso que no se verifique el stock disponible */
      DISPONIBLE:
      DO:
          IF x-articulo-ICBPER = Facdpedi.CodMat THEN LEAVE DISPONIBLE.
          IF Almtfami.Libre_c01 = "SV" THEN LEAVE DISPONIBLE.
          FIND FIRST VtaTabla WHERE VtaTabla.CodCia = s-CodCia AND
              VtaTabla.Tabla = "DROPSHIPPING" AND
              VtaTabla.Llave_c1 = Facdpedi.CodMat 
              NO-LOCK NO-ERROR.
          IF AVAILABLE VtaTabla THEN LEAVE DISPONIBLE.
          /* ******************************************************************** */
          /* RUTINA PRINCIPAL */
          /* ******************************************************************** */
          x-StkAct = Almmmate.StkAct.
          /* RHC 10/03/2021 Mucha vuelta: se comenta el stock comprometido */
/*           RUN gn/Stock-Comprometido-v2.r (Facdpedi.CodMat, x-CodAlm, YES, OUTPUT s-StkComprometido).           */
/*           /* ******************************************************************** */                           */
/*           /* RHC 29/04/2020 Tener cuidado, las COT tambi�n comprometen mercader�a */                           */
/*           /* ******************************************************************** */                           */
/*           FIND FIRST FacTabla WHERE FacTabla.CodCia = COTIZACION.CodCia AND                                    */
/*               FacTabla.Tabla = "GN-DIVI" AND                                                                   */
/*               FacTabla.Codigo = COTIZACION.CodDiv AND                                                          */
/*               FacTabla.Campo-L[2] = YES AND   /* Reserva Stock? */                                             */
/*               FacTabla.Valor[1] > 0           /* Horas de reserva */                                           */
/*               NO-LOCK NO-ERROR.                                                                                */
/*           IF AVAILABLE FacTabla THEN DO:                                                                       */
/*               /* Si ha llegado hasta ac� es que est� dentro de las horas de reserva */                         */
/*               /* Afectamos lo comprometido: extornamos el comprometido */                                      */
/*               s-StkComprometido = s-StkComprometido - (Facdpedi.Factor * (Facdpedi.CanPed - Facdpedi.CanAte)). */
/*           END.                                                                                                 */
          /* ******************************************************************** */
          /* ******************************************************************** */
          s-StkDis = x-StkAct - s-StkComprometido.
          IF s-StkDis <= 0 THEN DO:
              /* RHC 04/02/2016 Solo en caso de LISTA EXPRESS */
              IF COTIZACION.TpoPed = "LF" THEN DO:
                  MESSAGE 'Producto ' Facdpedi.codmat 'NO tiene Stock en el almac�n ' x-CodAlm SKIP
                      'Abortamos la generaci�n del Pedido?'
                      VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE rpta AS LOG.
                  IF rpta = YES THEN RETURN "ADM-ERROR".
              END.
              NEXT ALMACENES.
          END.
      END.
      /* DEFINIMOS LA CANTIDAD */
      x-CanPed = f-CanPed * f-Factor.   /* En unidades de stock */
      IF s-StkDis < x-CanPed THEN DO:
          /* Se ajusta la Cantidad Pedida al Saldo Disponible del Almac�n */
          f-CanPed = ((S-STKDIS - (S-STKDIS MODULO f-Factor)) / f-Factor).
      END.
      f-CanPed = f-CanPed * f-Factor.   /* En unidades de Stock */
      /* EMPAQUE SUPERMERCADOS */
      FIND FIRST supmmatg WHERE supmmatg.codcia = COTIZACION.CodCia
        AND supmmatg.codcli = COTIZACION.CodCli
        AND supmmatg.codmat = FacDPedi.codmat 
        NO-LOCK NO-ERROR.
      IF AVAILABLE supmmatg AND supmmatg.Libre_d01 <> 0 THEN DO:
          f-CanPed = (TRUNCATE((f-CanPed / supmmatg.Libre_d01),0) * supmmatg.Libre_d01).
      END.
      ELSE DO:    /* EMPAQUE OTROS */
          IF s-FlgEmpaque = YES THEN DO:
              RUN vtagn/p-cantidad-sugerida.p (COTIZACION.TpoPed,   /*s-TpoPed,*/
                                               Facdpedi.CodMat, 
                                               f-CanPed, 
                                               OUTPUT pSugerido, 
                                               OUTPUT pEmpaque).
              /*MESSAGE s-tpoped SKIP facdpedi.codmat SKIP f-canped SKIP psugerido SKIP pempaque.*/
              f-CanPed = pSugerido.
          END.
      END.
      f-CanPed = ((f-CanPed - (f-CanPed MODULO f-Factor)) / f-Factor).  /* En unidades de venta */
      IF f-CanPed <= 0 THEN NEXT ALMACENES.
      IF f-CanPed > t-CanPed THEN DO:
          t-CanPed = f-CanPed.
          t-AlmDes = x-CodAlm.
      END.

      IF t-CanPed > 0 THEN DO:
          /* ******************************* */
          /* GRABACION */
          I-NPEDI = I-NPEDI + 1.
          CREATE PEDI.
          BUFFER-COPY FacDPedi 
              EXCEPT Facdpedi.CanSol Facdpedi.CanApr
              TO PEDI
              ASSIGN 
                  PEDI.CodCia = s-codcia
                  PEDI.CodDiv = s-coddiv
                  PEDI.CodDoc = "PED"
                  PEDI.NroPed = ''
                  PEDI.CodCli = COTIZACION.CodCli
                  PEDI.ALMDES = t-AlmDes  /* *** OJO *** */
                  PEDI.NroItm = I-NPEDI
                  PEDI.CanPed = t-CanPed    /* << OJO << */
                  PEDI.CanAte = 0.
          ASSIGN
              PEDI.Libre_d01 = (FacDPedi.CanPed - FacDPedi.CanAte)
              PEDI.Libre_d02 = t-CanPed
              PEDI.Libre_c01 = '*'.
          /* RHC 28/04/2016 Caso extra�o */
          IF PEDI.CanPed > PEDI.Libre_d01 
              THEN ASSIGN PEDI.CanPed = PEDI.Libre_d01 PEDI.Libre_d02 = PEDI.Libre_d01.
          /* *************************** */
          IF PEDI.CanPed <> facdPedi.CanPed THEN DO:
              {vta2/calcula-linea-detalle.i &Tabla="PEDI"}.
          END.
          /* FIN DE CARGA */
      END.
  END.
  HIDE FRAME F-Mensaje.
  /* ******************************************************************** */
  /* ******************************************************************** */
  /* SI HUBIERA UN PRODUCTO CON UNA OBSERVACION => SE PRESENTA UNA VENTANA*/
  RUN vta2/d-ped-tope-despacho.
  /* ******************************************************************** */
  /* ******************************************************************** */
  RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PED_Asigna-PCO) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PED_Asigna-PCO Procedure 
PROCEDURE PED_Asigna-PCO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pNroPCO AS CHAR.
  DEFINE INPUT PARAMETER s-PorIgv AS DEC.
                        
  DEFINE VARIABLE I-NPEDI AS INTEGER NO-UNDO.
  DEFINE VARIABLE f-Factor AS DEC NO-UNDO.
  DEFINE VARIABLE x-CanPed AS DEC NO-UNDO.
  DEFINE VARIABLE s-StkComprometido AS DEC.
  DEFINE VARIABLE s-StkDis AS DEC NO-UNDO.
  DEFINE VARIABLE F-CANPED AS DECIMAL NO-UNDO.
  DEFINE VARIABLE x-StkAct AS DEC NO-UNDO.
  DEFINE VARIABLE x-CodAlm AS CHAR NO-UNDO.
  DEFINE VARIABLE i AS INT NO-UNDO.

  /* ************************************************************************** */
  /* RHC 31/01/2018 PARAMETROS DE ACUERDO A LA LISTA DE PRECIO DE LA COTIZACION */
  /* ************************************************************************** */
  DEF BUFFER B-DIVI FOR gn-divi.
  FIND FIRST B-DIVI WHERE B-DIVI.codcia = s-codcia AND B-DIVI.coddiv = COTIZACION.Libre_c01 NO-LOCK NO-ERROR.
  IF AVAILABLE B-DIVI THEN DO:
      ASSIGN
          s-DiasVtoPed = B-DIVI.DiasVtoPed
          s-FlgEmpaque = B-DIVI.FlgEmpaque
          s-VentaMayorista = B-DIVI.VentaMayorista.
  END.
  /* **************************************************** */
  /* **************************************************** */
  DEFINE FRAME F-Mensaje
    'Procesando: ' Facdpedi.codmat SKIP(1)
    'Espere un momento por favor ...' SKIP
    WITH CENTERED NO-LABELS OVERLAY VIEW-AS DIALOG-BOX TITLE 'TRASLADANDO COTIZACION'.

  i-NPedi = 0.
  /* VERIFICACION DE LOS SALDOS DE LA COTIZACION */
  FOR EACH Facdpedi OF COTIZACION NO-LOCK WHERE (Facdpedi.CanPed - Facdpedi.CanAte) > 0:
      IF Facdpedi.CanAte < 0 THEN DO:   /* HAY UN NEGATIVO */
          MESSAGE 'Hay una incosistencia el el producto:' Facdpedi.codmat SKIP
              'Avisar a sistemas' SKIP(2)
              'Proceso abortado'
              VIEW-AS ALERT-BOX WARNING.
          RETURN 'ADM-ERROR'.
      END.
  END.
  /* CARGAMOS STOCK DISPONIBLE */
  /* s-CodAlm: Tiene uno mas almacenes configurados */
  DEF VAR t-AlmDes AS CHAR NO-UNDO.
  DEF VAR t-CanPed AS DEC NO-UNDO.
  DEF VAR pSugerido AS DEC NO-UNDO.
  DEF VAR pEmpaque AS DEC NO-UNDO.

  /* ********************************************* */
  /* LIMPIAMOS LA TABLA DE LOG DE TOPE DE DESPACHO */
  /* ********************************************* */
  EMPTY TEMP-TABLE T-LogTabla.
  EMPTY TEMP-TABLE PEDI.
  /* ********************************************* */
  ALMACENES:
  FOR EACH Facdpedi OF COTIZACION NO-LOCK,
      EACH Vtaddocu NO-LOCK WHERE Vtaddocu.codcia = s-CodCia AND
      Vtaddocu.codped = "PCO" AND
      Vtaddocu.nroped = pNroPCO AND
      Vtaddocu.codmat = Facdpedi.codmat AND
      Vtaddocu.canped > 0,
      FIRST Almmmatg OF Facdpedi NO-LOCK
      BY Facdpedi.NroItm:
      DISPLAY Facdpedi.codmat WITH FRAME F-Mensaje.
      ASSIGN
          f-Factor = Facdpedi.Factor
          t-AlmDes = ''
          t-CanPed = 0.
      /* ************************************************* */
      /* INFORMACION DE LA PROGRAMACION DE ABASTECIMIENTOS */
      x-CodAlm = VtaDDocu.AlmDes.   /* <<< OJO <<< */
      F-CANPED = VtaDDocu.CanPed.   /* <<< OJO <<< */
      /* ************************************************* */
      /* FILTROS */
      FIND FIRST Almmmate WHERE Almmmate.codcia = s-codcia
          AND Almmmate.codalm = x-CodAlm  /* *** OJO *** */
          AND Almmmate.codmat = Facdpedi.CodMat
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Almmmate THEN DO:
          MESSAGE 'Producto' Facdpedi.codmat 'NO asignado al almac�n' x-CodAlm
              VIEW-AS ALERT-BOX INFORMATION.
          NEXT ALMACENES.
      END.
      /* ******************************************************************** */
      /* RHC 23/07/2018 SOLAMENTE CARGAMOS T-LOGTABLA CON EL TOPE DE DESPACHO */
      /* ******************************************************************** */
      DEF VAR pAlmSug AS CHAR NO-UNDO.
      DEF VAR pRetirar AS LOG NO-UNDO.
      RUN alm/p-tope-pedido (Facdpedi.CodMat,
                             x-CodAlm,
                             f-CanPed * f-Factor,
                             YES,
                             INPUT-OUTPUT TABLE T-LogTabla,
                             OUTPUT pAlmSug,
                             OUTPUT pRetirar).
      /* ******************************************************************** */
      /* ******************************************************************** */
      /* Stock Disponible */
      x-StkAct = Almmmate.StkAct.
      RUN gn/Stock-Comprometido-v2 (Facdpedi.CodMat, x-CodAlm, YES, OUTPUT s-StkComprometido).
      /* ******************************************************************** */
      /* RHC 29/04/2020 Tener cuidado, las COT tambi�n comprometen mercader�a */
      /* ******************************************************************** */
      FIND FIRST FacTabla WHERE FacTabla.CodCia = COTIZACION.CodCia AND
          FacTabla.Tabla = "GN-DIVI" AND
          FacTabla.Codigo = COTIZACION.CodDiv AND
          FacTabla.Campo-L[2] = YES AND   /* Reserva Stock? */
          FacTabla.Valor[1] > 0           /* Horas de reserva */
          NO-LOCK NO-ERROR.
      IF AVAILABLE FacTabla THEN DO:
          /* Si ha llegado hasta ac� es que est� dentro de las horas de reserva */
          /* Afectamos lo comprometido: extornamos el comprometido */
          s-StkComprometido = s-StkComprometido - (Facdpedi.Factor * (Facdpedi.CanPed - Facdpedi.CanAte)).
      END.
      /* ******************************************************************** */
      /* ******************************************************************** */
      s-StkDis = x-StkAct - s-StkComprometido.
      IF s-StkDis <= 0 THEN DO:
          NEXT ALMACENES.
      END.
      /* DEFINIMOS LA CANTIDAD */
      x-CanPed = f-CanPed * f-Factor.   /* En unidades de stock */
      IF s-StkDis < x-CanPed THEN DO:
          /* Se ajusta la Cantidad Pedida al Saldo Disponible del Almac�n */
          f-CanPed = ((S-STKDIS - (S-STKDIS MODULO f-Factor)) / f-Factor).
      END.
      /* ********************************************************************************************** */
      /* EMPAQUE OTROS */
      /* ********************************************************************************************** */
      f-CanPed = f-CanPed * f-Factor.   /* En unidades de Stock */
      IF s-FlgEmpaque = YES THEN DO:
          RUN vtagn/p-cantidad-sugerida-pco.p (Facdpedi.CodMat, 
                                               f-CanPed, 
                                               OUTPUT pSugerido, 
                                               OUTPUT pEmpaque).
          f-CanPed = pSugerido.
      END.
      f-CanPed = ((f-CanPed - (f-CanPed MODULO f-Factor)) / f-Factor).  /* En unidades de venta */
      IF f-CanPed <= 0 THEN NEXT ALMACENES.
      /* ********************************************************************************************** */
      /* ********************************************************************************************** */
      IF f-CanPed > t-CanPed THEN DO:
          t-CanPed = f-CanPed.
          t-AlmDes = x-CodAlm.
      END.
      IF t-CanPed > 0 THEN DO:
          /* CONSISTENCIA ANTES DE GRABAR */
          IF CAN-FIND(FIRST PEDI WHERE PEDI.codmat = Facdpedi.codmat AND PEDI.almdes = t-AlmDes NO-LOCK)
              THEN NEXT ALMACENES.
          /* ******************************* */
          /* GRABACION */
          I-NPEDI = I-NPEDI + 1.
          CREATE PEDI.
          BUFFER-COPY FacDPedi 
              EXCEPT Facdpedi.CanSol Facdpedi.CanApr
              TO PEDI
              ASSIGN 
                  PEDI.CodCia = s-codcia
                  PEDI.CodDiv = s-coddiv
                  PEDI.CodDoc = "PED"
                  PEDI.NroPed = ''
                  PEDI.CodCli = COTIZACION.CodCli
                  PEDI.ALMDES = t-AlmDes  /* *** OJO *** */
                  PEDI.NroItm = I-NPEDI
                  PEDI.CanPed = t-CanPed    /* << OJO << */
                  PEDI.CanAte = 0.
          ASSIGN
              PEDI.Libre_d01 = VtaDDocu.CanPed
              PEDI.Libre_d02 = t-CanPed
              PEDI.Libre_c01 = '*'.
          /* RHC 28/04/2016 Caso extra�o */
          IF PEDI.CanPed > PEDI.Libre_d01 
              THEN ASSIGN PEDI.CanPed = PEDI.Libre_d01 PEDI.Libre_d02 = PEDI.Libre_d01.
          /* *************************** */
          {vta2/calcula-linea-detalle.i &Tabla="PEDI"}.
          /* FIN DE CARGA */
      END.
  END.
  HIDE FRAME F-Mensaje.
  /* ******************************************************************** */
  /* ******************************************************************** */
  /* SI HUBIERA UN PRODUCTO CON UNA OBSERVACION => SE PRESENTA UNA VENTANA*/
  RUN vta2/d-ped-tope-despacho.
  /* ******************************************************************** */
  /* ******************************************************************** */
  RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PED_Despachar-Pedido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PED_Despachar-Pedido Procedure 
PROCEDURE PED_Despachar-Pedido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ROWID del PEDIDO */
DEF INPUT PARAMETER pRowid AS ROWID.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEF VAR x-Cuenta AS INT NO-UNDO. 

pMensaje = ''.
FIND FIRST Faccpedi WHERE ROWID(Faccpedi) = pRowid NO-LOCK.
/* Solo PEDidos GENERADOS */  
IF Faccpedi.flgest <> "G" THEN DO:
    pMensaje = "El pedido ya NO est� por aprobar". 
    RETURN "ADM-ERROR".
END. 

/* 
    Ic - 23Jun2020, para casos de pedido de 002 - CONTADO ANTICIPADO
    que aun no haya actualizado la BD
*/

DEFINE VAR x-fmapago AS CHAR.
DEFINE VAR x-boleta-deposito AS CHAR.
DEFINE VAR x-retval AS CHAR.  

x-fmapago = faccpedi.fmapgo.
x-boleta-deposito = faccpedi.libre_c03.

IF x-fmapago = '002' THEN DO:
    IF TRUE <> (x-boleta-deposito > "") THEN DO:
        pMensaje =  "Imposible realizar despacho" + CHR(10) +
                "para CONTADO ANTICIPADO, debe ingresar" + CHR(10) +
                "el numero de documento del deposito" .
        RETURN "ADM-ERROR".
    END.

    /* Verificamos que la condicion de venta este asignado a esa division  */
    RUN VTA_cond-vta_division(INPUT x-fmapago, INPUT FacCpedi.coddiv, OUTPUT x-retval).
    IF x-retval = 'NO' THEN DO:
        pMensaje =  "La condicion de venta (" + x-fmapago + ")" + CHR(10) +
                "No esta asignado a la divsion de venta (" + FacCpedi.coddiv + ")".
        RETURN "ADM-ERROR".
    END.
END.

/* Ic - 23Jun2020 - FIN */

/* Ic - 11Ago2020 */
x-dias-tolerables-deuda-vencida = 0.
x-tabla = "APR.PED|DOCMNTOS".
/*  */

/* LISTA EXPRESS */
FIND FIRST COTIZACION WHERE COTIZACION.codcia = Faccpedi.codcia
    AND COTIZACION.coddiv = Faccpedi.coddiv 
    AND COTIZACION.coddoc = Faccpedi.codref
    AND COTIZACION.nroped = Faccpedi.nroref
    NO-LOCK NO-ERROR.
IF AVAILABLE COTIZACION AND COTIZACION.TpoPed = "LF" AND COTIZACION.FlgEst <> "C" 
    THEN DO:
    pMensaje = 'PEDIDO LISTA EXPRESS' + chr(10) + 'NO se puede despachar parciales'. 
    RETURN 'ADM-ERROR'.
END.
/* ****************************************************************************** */
/* RHC 04/02/2018 Ciclo de Cross Docking Clientes */
/* ****************************************************************************** */
DEF VAR pCrossDocking AS LOG NO-UNDO.
DEF VAR pAlmacenXD AS CHAR NO-UNDO.
DEF VAR k AS INT NO-UNDO.
/* DEF BUFFER B-DESTINO FOR Almacen.                                     */
/* DEF BUFFER B-ORIGEN  FOR Almacen.                                     */
/* RUN alm/d-crossdocking-v2 (OUTPUT pCrossDocking, OUTPUT pAlmacenXD).  */
/* IF pAlmacenXD = "ERROR" THEN RETURN 'ADM-ERROR'.                      */
/* IF pCrossDocking = YES THEN DO k = 1 TO NUM-ENTRIES(Faccpedi.CodAlm): */
/*     FIND B-DESTINO WHERE B-DESTINO.codcia = s-codcia                  */
/*         AND B-DESTINO.codalm = ENTRY(k, Faccpedi.CodAlm)              */
/*         NO-LOCK.                                                      */
/*     FIND B-ORIGEN WHERE B-ORIGEN.codcia = s-codcia                    */
/*         AND B-ORIGEN.codalm = pAlmacenXD                              */
/*         NO-LOCK.                                                      */
/*     IF B-DESTINO.CodDiv = B-ORIGEN.CodDiv THEN DO:                    */
/*         pMensaje = 'NO se puede generar para el mismo CD'.            */
/*         RETURN 'ADM-ERROR'.                                           */
/*     END.                                                              */
/* END.                                                                  */
/* ****************************************************************************** */
/* ****************************************************************************** */
/* RHC 23/07/20196 Ciclo de Dejado en Tienda */
/* ****************************************************************************** */
DEF VAR pDT AS LOG NO-UNDO.
DEF VAR pAlmacenDT AS CHAR NO-UNDO.
DEF BUFFER B-DESTINO FOR Almacen.
DEF BUFFER B-ORIGEN  FOR Almacen.

RUN logis/d-dejado-en-tienda (OUTPUT pDT, OUTPUT pAlmacenDT).
IF pAlmacenDT = "ERROR" THEN RETURN 'ADM-ERROR'.

IF pDT = YES THEN DO k = 1 TO NUM-ENTRIES(Faccpedi.CodAlm):
    FIND FIRST B-DESTINO WHERE B-DESTINO.codcia = s-codcia
        AND B-DESTINO.codalm = ENTRY(k, Faccpedi.CodAlm)
        NO-LOCK.
    FIND FIRST B-ORIGEN WHERE B-ORIGEN.codcia = s-codcia
        AND B-ORIGEN.codalm = pAlmacenDT
        NO-LOCK.
    IF B-DESTINO.CodDiv = B-ORIGEN.CodDiv THEN DO:
        pMensaje = 'NO se puede generar para el mismo CD'.
        RETURN 'ADM-ERROR'.
    END.
END.
/* ****************************************************************************** */
/* ****************************************************************************** */
MESSAGE 'El Pedido est� listo para ser despachado' SKIP 'Continuamos?'
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE rpta AS LOG.
IF rpta = NO THEN RETURN 'ADM-ERROR'.
/* Cliente */
FIND FIRST Gn-clie WHERE Gn-clie.codcia = cl-codcia AND Gn-clie.codcli = Faccpedi.codcli
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Gn-clie THEN DO:
    pMensaje = 'Cliente NO registrado'.
    RETURN 'ADM-ERROR'.
END.
/* Revisar datos del Transportista */
FIND FIRST Ccbadocu WHERE Ccbadocu.codcia = Faccpedi.codcia
    AND Ccbadocu.coddiv = Faccpedi.coddiv
    AND Ccbadocu.coddoc = Faccpedi.coddoc
    AND Ccbadocu.nrodoc = Faccpedi.nroped
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Ccbadocu THEN DO:
    MESSAGE 'A�n NO ha ingresado los datos del transportista' SKIP 'Continuamos?'
        VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE rpta-1 AS LOG.
    IF rpta-1 = NO THEN RETURN 'ADM-ERROR'.
END.

/* ********************************* */
RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    {lib/lock-genericov3.i ~
        &Tabla="Faccpedi" ~
        &Condicion="ROWID(Faccpedi) = pRowid" ~
        &Bloqueo="EXCLUSIVE-LOCK" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &txtMensaje="pMensaje" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'" }
    IF Faccpedi.flgest <> "G" THEN DO:
        pMensaje = 'El Pedido ya fue aprobado por:' + CHR(10) +
            'Usuario: ' + FacCPedi.UsrAprobacion + CHR(10) +
            'Fecha: ' + STRING(FacCPedi.FchAprobacion) + CHR(10) + CHR(10) +
            'Proceso abortado'.
        UNDO, RETURN 'ADM-ERROR'.  
    END.

    /* *********************************************************************** */
    /* Ic 08/12/2020 Condicion de Venta 901 - Obligado aaprobacion CyC  */
    /* Susana Leon  */
    /* *********************************************************************** */
    IF x-fmapago = '901' THEN DO:
        ASSIGN            
            FacCPedi.Flgest = "X"
            FacCPedi.Glosa  = TRIM (FacCPedi.Glosa) 
            FacCPedi.Libre_c05 = 'CONDICION DE VENTA - OBLIGADO APROB. CyC'
            FacCPedi.Libre_c04 = '- PASA POR EVALUACION DE CREDITO'.
    END.

    /* 
        En este procedimiento hay validaciones de control.
        
        VERIFICAMOS DEUDA DEL CLIENTE, DEUDAS CON VENCIMIENTO
    */
    IF x-fmapago <> '901' THEN DO:
        {vta2/verifica-cliente-01.i &VarMensaje = pMensaje}
    END.
     
    /* *********************************************************************** */
    /* RHC 27/09/2019 TRAMITE DOCUMENTARIO: Forzamos la aprobar por COMERCIAL  */
    /* RHC 23/10/2019 TRAMITE DOCUMENTARIO: Forzamos la aprobar por CC y CC  */
    /* Faccpedi.TipVta = "Si" = Tramite documentario */
    /* *********************************************************************** */
    IF Faccpedi.TipVta = "Si" AND Faccpedi.FlgEst = "P" THEN DO:
        ASSIGN
            /*FacCPedi.Flgest = "WC"*/
            FacCPedi.Flgest = "X"
            FacCPedi.Glosa  = TRIM (FacCPedi.Glosa) 
            FacCPedi.Libre_c05 = 'TRAMITE DOCUMENTARIO'
            .
    END.
    /* *********************************************************************** */
    /* *********************************************************************** */
    /*  
        PARA CONTADO ANTICIPADO
        01Oct2019, Susana Leon converso con Daniel Llican y en coordinacion
                    con Julisa calderon, cuando la boleta de deposito (BD) es inferior 
                    al monto del pedido se debe derivar a la bandeja de CyC
    */

    DEFINE VAR x-doc-deposito AS CHAR.

    x-doc-deposito = TRIM(faccpedi.usrchq).
    IF TRUE <> (faccpedi.usrchq > "") THEN x-doc-deposito = "BD".

    IF Faccpedi.fmapgo = '002' THEN DO:
        FIND Ccbcdocu WHERE Ccbcdocu.codcia = s-codcia
            AND Ccbcdocu.coddoc = x-doc-deposito        /* BD o A/R */
            AND Ccbcdocu.nrodoc = FacCPedi.Libre_c03
            AND Ccbcdocu.codcli = FacCPedi.CodCli
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Ccbcdocu OR Ccbcdocu.flgest <> 'P' OR Ccbcdocu.SdoAct <= 0
            THEN DO:
            pMensaje = Faccpedi.usrchq + " " + FacCPedi.Libre_c03 + " NO existe o no pertenece al cliente".
            UNDO, RETURN 'ADM-ERROR'.
        END.
        IF Ccbcdocu.FmaPgo <> faccpedi.fmapgo THEN DO:
            pMensaje =  'La condici�n de venta del " + Faccpedi.usrchq + " " + FacCPedi.Libre_c03 + " NO es CONTADO ANTICIPADO'.
            UNDO, RETURN 'ADM-ERROR'.
        END.
        /*
            01Oct2019, Susana Leon converso con Daniel Llican y en coordinacion
                        con Julisa calderon, cuando la boleta de deposito (BD) es inferior 
                        al monto del pedido se debe derivar a la bandeja de CyC
        */

            
        IF FacCPedi.CodMon = Ccbcdocu.CodMon THEN DO:
            IF Ccbcdocu.sdoact < faccpedi.imptot THEN DO:
                ASSIGN faccpedi.flgest = 'X'
                    FacCPedi.Libre_c05 = 'EL MONTO DEL " + Faccpedi.usrchq + " " + FacCPedi.Libre_c03 +" ES INFERIOR AL MONTO DEL PEDIDO'
                    FacCPedi.Libre_c04 = '- PASA POR EVALUACION DE CREDITO'.

            END.
        END.
        ELSE DO:
            IF Ccbcdocu.codmon = 1 THEN DO:
                IF Ccbcdocu.sdoact < (faccpedi.imptot * FaccPedi.Tpocmb) THEN DO:
                    ASSIGN faccpedi.flgest = 'X'
                        FacCPedi.Libre_c05 = 'EL MONTO DEL " + Faccpedi.usrchq + " " + FacCPedi.Libre_c03 + " ES INFERIOR AL MONTO DEL PEDIDO'
                        FacCPedi.Libre_c04 = '- PASA POR EVALUACION DE CREDITO'.

                END.
            END.
            ELSE DO:
                IF Ccbcdocu.sdoact < (faccpedi.imptot / FaccPedi.Tpocmb) THEN DO:
                    ASSIGN faccpedi.flgest = 'X'
                        FacCPedi.Libre_c05 = 'EL MONTO DEL " + Faccpedi.usrchq + " " + FacCPedi.Libre_c03 + " ES INFERIOR AL MONTO DEL PEDIDO'
                        FacCPedi.Libre_c04 = '- PASA POR EVALUACION DE CREDITO'.

                END.
            END.
        END.
        /* RHC 21/05/2020 SLM Todo pedido pasa por aprobaci�n de Cr�ditos, excepto la divisi�n 000101 */
        IF Faccpedi.FlgEst = "P" AND Faccpedi.CodDiv <> "00101" THEN DO:
            /*  Ic - 23Jun23020, Susana Leon, queda sin efecto  */
            /*
            ASSIGN
                Faccpedi.FlgEst = "X"
                Faccpedi.Libre_c04 = '- PASA POR EVALUACION DE CREDITO'.
            */
        END.
    END.   

    /* 01Oct2019 - Fin */

    /* RHC 19/02/2018 Maldito cross docking */
    ASSIGN                  
        FacCPedi.AlmacenDT      = pAlmacenDT
        FacCPedi.DT             = pDT
        Faccpedi.CrossDocking   = pCrossDocking
        Faccpedi.AlmacenXD      = pAlmacenXD.    /* Almac�n que despacha al cliente */
    /* ********************************************************************************************** */
    /* CONTROL DE SEDE Y UBIGEO: POR CLIENTE */
    /* ********************************************************************************************** */
    IF FacCPedi.DT = YES THEN DO:
        ASSIGN
            FacCPedi.Ubigeo[1] = FacCPedi.AlmacenDT
            FacCPedi.Ubigeo[2] = "@ALM"
            FacCPedi.Ubigeo[3] = FacCPedi.AlmacenDT.
    END.


    /* ********************************************************************************************** */
    IF Faccpedi.FlgEst = "X" THEN DO:
        /* ******************************* */
        /* RHC 17/08/2018 TRACKING DE CONTROL */
        /* ******************************* */
        RUN vtagn/pTracking-04 (s-CodCia,
                          s-CodDiv,
                          Faccpedi.CodDoc,
                          Faccpedi.NroPed,
                          s-User-Id,
                          'PANPX',
                          'P',
                          DATETIME(TODAY, MTIME),
                          DATETIME(TODAY, MTIME),
                          Faccpedi.CodDoc,
                          Faccpedi.NroPed,
                          Faccpedi.CodRef,
                          Faccpedi.NroRef).
        /* ******************************* */
        /* ******************************* */
        IF TRUE <> (pMensaje > '') THEN pMensaje = "NO pas� la l�nea de cr�dito".
        LEAVE RLOOP.
    END.

    /* VERIFICAMOS MARGEN DE UTILIDAD */
    /*IF LOOKUP(s-CodDiv, '00000,00017,00018') > 0 THEN DO:*/
    IF LOOKUP(FacCPedi.Lista_de_Precios, '00000,00017,00018') > 0 THEN DO:
        {vta2/i-verifica-margen-utilidad-1.i}
        IF Faccpedi.FlgEst = "W" THEN DO:
            pMensaje = FacCPedi.Libre_c05.
            LEAVE RLOOP.
        END.
    END.

    /* CREAMOS LA ORDEN */
    DEF VAR pComprobante AS CHAR NO-UNDO.
    DEF VAR pFlgEst AS CHAR NO-UNDO.

    pMensaje = "".
    pRowid = ROWID(Faccpedi).
    pFlgEst = Faccpedi.FlgEst.  /* OJO */

    CASE TRUE:
        WHEN Faccpedi.CrossDocking = NO THEN DO:
            RUN VTA_Genera-OD ( pRowid, OUTPUT pComprobante, OUTPUT pMensaje ).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
        END.
        WHEN Faccpedi.CrossDocking = YES THEN DO:
            RUN VTA_Genera-OTR ( pRowid, OUTPUT pMensaje).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
        END.
    END CASE.
    /* ********************************************************* */
    /* RHC 07/01/2020 ExpoBodega BONIFICACION amarrada al PEDIDO */
    /* Debemos regresar el puntero a la posici�n original */
    /* ********************************************************* */
    FIND Faccpedi WHERE ROWID(Faccpedi) = pRowid NO-LOCK.
    FIND FIRST PEDIDO WHERE PEDIDO.CodCia = Faccpedi.codcia AND
        PEDIDO.CodDiv = Faccpedi.coddiv AND
        PEDIDO.CodDoc = Faccpedi.coddoc AND
        PEDIDO.CodRef = Faccpedi.codref AND
        PEDIDO.NroRef = Faccpedi.nroref AND
        PEDIDO.CodOrigen = Faccpedi.coddoc AND
        PEDIDO.NroOrigen = Faccpedi.nroped AND
        PEDIDO.FlgEst = "G" NO-LOCK NO-ERROR.
    IF AVAILABLE PEDIDO THEN DO:
        FIND CURRENT PEDIDO EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            {lib/mensaje-de-error.i &MensajeError="pMensaje" &CuentaError="x-Cuenta"}
            UNDO RLOOP, RETURN 'ADM-ERROR'.
        END.
        ASSIGN
            PEDIDO.FlgEst = pFlgEst     /* OJO */
            PEDIDO.UsrAprobacion = Faccpedi.UsrAprobacion
            PEDIDO.FchAprobacion = Faccpedi.FchAprobacion.
        /* RHC 19/02/2018 Maldito cross docking */
        ASSIGN                  
            PEDIDO.AlmacenDT = FacCPedi.AlmacenDT
            PEDIDO.DT        = FacCPedi.DT       
            PEDIDO.CrossDocking = Faccpedi.CrossDocking
            PEDIDO.ALmacenXD    = Faccpedi.AlmacenXD
            .
        /* ********************************************************************************************** */
        /* CONTROL DE SEDE Y UBIGEO: POR CLIENTE */
        /* ********************************************************************************************** */
        ASSIGN
            PEDIDO.Ubigeo[1] = FacCPedi.Ubigeo[1]
            PEDIDO.Ubigeo[2] = FacCPedi.Ubigeo[2]
            PEDIDO.Ubigeo[3] = FacCPedi.Ubigeo[3]
            .
        /* ******************************************************************** */
        /* COPIAMOS DATOS DEL TRANSPORTISTA */
        /* ******************************************************************** */
        FIND FIRST Ccbadocu WHERE Ccbadocu.codcia = Faccpedi.codcia
            AND Ccbadocu.coddiv = Faccpedi.coddiv
            AND Ccbadocu.coddoc = Faccpedi.coddoc
            AND Ccbadocu.nrodoc = Faccpedi.nroped
            NO-LOCK NO-ERROR.
        IF AVAILABLE Ccbadocu THEN DO:
            FIND FIRST B-ADOCU WHERE B-ADOCU.codcia = PEDIDO.codcia
                AND B-ADOCU.coddiv = PEDIDO.coddiv
                AND B-ADOCU.coddoc = PEDIDO.coddoc
                AND B-ADOCU.nrodoc = PEDIDO.nroped
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE B-ADOCU THEN CREATE B-ADOCU.
            BUFFER-COPY Ccbadocu TO B-ADOCU
                ASSIGN
                    B-ADOCU.CodDiv = PEDIDO.CodDiv
                    B-ADOCU.CodDoc = PEDIDO.CodDoc
                    B-ADOCU.NroDoc = PEDIDO.NroPed
                NO-ERROR.
        END.
        /* ******************************************************************** */
        pMensaje = "".
        CASE TRUE:
            WHEN PEDIDO.CrossDocking = NO THEN DO:
                RUN VTA_Genera-OD (ROWID(PEDIDO), OUTPUT pComprobante, OUTPUT pMensaje).
                IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
            END.
            WHEN PEDIDO.CrossDocking = YES THEN DO:
                RUN VTA_Genera-OTR (ROWID(PEDIDO), OUTPUT pMensaje).
                IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
            END.
        END CASE.
    END.
    /* ********************************************************* */
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PED_saldo_anticipos_campana) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PED_saldo_anticipos_campana Procedure 
PROCEDURE PED_saldo_anticipos_campana :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
        Devuelve el saldo de los Anticipos de Campa�a (A/C) en 
        ambas monedas, tomando como tipo de cambio el que ingresa
        el area contable
        
------------------------------------------------------------------------------*/

/*
    WHEN 'G' THEN pEstado = "GENERADO".
    WHEN 'X' THEN pEstado = "POR APROBAR X CRED. & COB.".
    WHEN 'T' THEN pEstado = "POR APROBAR X TESORERIA".
    WHEN 'W' THEN pEstado = "POR APROBAR POR SECR. GG.".
    WHEN 'WX' THEN pEstado = "POR APROBAR POR GG.".
    WHEN 'WL' THEN pEstado = "POR APROBAR POR LOGISTICA".
    WHEN 'WC' THEN pEstado = "POR APROBAR POR COMERCIAL".
    WHEN 'P' THEN pEstado = "APROBADO".
*/

DEFINE INPUT PARAMETER pCodCliente AS CHAR NO-UNDO.
DEFINE OUTPUT PARAMETER pImpteMN AS DEC NO-UNDO.
DEFINE OUTPUT PARAMETER pImpteME AS DEC NO-UNDO.

DEFINE VAR x-estados AS CHAR.       /* Estados validos para considerar */
DEFINE VAR x-flgest AS CHAR.
DEFINE VAR x-sec AS INT.

DEFINE VAR x-ImpteMN AS DEC.
DEFINE VAR x-ImpteME AS DEC.

pImpteMN = 0.
pImpteME = 0.

x-ImpteMN = 0.
x-ImpteME = 0.

/* Estados del pedido */
x-estados = "G,X,T,W,WX,WL,WC,P".

/* Saldos de los anticipos de campa�a */
FOR EACH Ccbcdocu USE-INDEX Llave06 NO-LOCK WHERE Ccbcdocu.codcia = Faccpedi.codcia
                        AND Ccbcdocu.codcli = pCodCliente AND Ccbcdocu.flgest = "P" 
                        AND Ccbcdocu.coddoc = "A/C":

    FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= Ccbcdocu.fchdoc NO-LOCK NO-ERROR.
    IF Ccbcdocu.CodMon = 2 THEN DO:        
        x-ImpteMN = Ccbcdocu.SdoAct * gn-tcmb.compra.
        x-ImpteME = Ccbcdocu.SdoAct.
    END.
    ELSE DO:
        x-ImpteMN = Ccbcdocu.SdoAct .
        x-ImpteME = Ccbcdocu.SdoAct / gn-tcmb.venta.
    END.

    pImpteMN = pImpteMN + x-ImpteMN.
    pImpteME = pImpteME + x-ImpteME.
END.

/* PED Pedidos generados o por aprobar */
FOR EACH gn-divi WHERE gn-divi.codcia = s-codcia NO-LOCK:

    REPEAT x-sec = 1 TO NUM-ENTRIES(x-estados,","):
        x-flgest = ENTRY(x-sec,x-estados,",").
        FOR EACH faccpedi WHERE faccpedi.codcia = s-codcia AND
                                faccpedi.coddiv = gn-divi.coddiv AND
                                faccpedi.coddoc = 'PED' AND
                                faccpedi.flgest = x-flgest NO-LOCK:
            IF faccpedi.codcli = pCodCliente AND faccpedi.tpolic = YES THEN DO:
                IF faccpedi.codmon = 2 THEN DO:
                    x-ImpteMN = faccpedi.imptot * gn-tcmb.compra.
                    x-ImpteME = faccpedi.imptot.
                END.
                ELSE DO:
                    x-ImpteMN = faccpedi.imptot.
                    x-ImpteME = faccpedi.imptot / gn-tcmb.venta.
                END.
                /* ----- */
                pImpteMN = pImpteMN - x-ImpteMN.
                pImpteME = pImpteME - x-ImpteME.
            END.
        END.

    END.                        
END.

/* Estados de la O/D */
x-estados = "P".

/* O/D pendientes de facturar */
FOR EACH gn-divi WHERE gn-divi.codcia = s-codcia NO-LOCK:

    REPEAT x-sec = 1 TO NUM-ENTRIES(x-estados,","):
        x-flgest = ENTRY(x-sec,x-estados,",").
        FOR EACH faccpedi WHERE faccpedi.codcia = s-codcia AND
                                faccpedi.coddiv = gn-divi.coddiv AND
                                faccpedi.coddoc = 'O/D' AND
                                faccpedi.flgest = x-flgest NO-LOCK:
            IF faccpedi.codcli = pCodCliente AND faccpedi.tpolic = YES THEN DO:
                IF faccpedi.codmon = 2 THEN DO:
                    x-ImpteMN = faccpedi.imptot * gn-tcmb.compra.
                    x-ImpteME = faccpedi.imptot.
                END.
                ELSE DO:
                    x-ImpteMN = faccpedi.imptot.
                    x-ImpteME = faccpedi.imptot / gn-tcmb.venta.
                END.
                /* ----- */
                pImpteMN = pImpteMN - x-ImpteMN.
                pImpteME = pImpteME - x-ImpteME.
            END.
        END.
    END.                        
END.

/*  */
IF pImpteMN < 0 THEN pImpteMN = 0.
IF pImpteME < 0 THEN pImpteME = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PED_Sync_Record) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PED_Sync_Record Procedure 
PROCEDURE PED_Sync_Record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF CAN-FIND(FIRST Facdpedi OF Faccpedi WHERE Facdpedi.canate < 0) THEN DO:
    DEF BUFFER B-DPEDI FOR Facdpedi.
    FOR EACH B-DPEDI OF Faccpedi NO-LOCK WHERE B-DPEDI.CanAte < 0:
        {lib/lock-genericov3.i ~
            &Tabla="Facdpedi" ~
            &Condicion="ROWID(Facdpedi) = ROWID(B-DPEDI)" ~
            &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
            &Accion="RETRY" ~
            &Mensaje="NO" ~
            &TipoError="UNDO, RETURN 'ADM-ERROR'" }
        Facdpedi.CanAte = 0.
    END.
    RELEASE Facdpedi.
    DEF BUFFER COTIZACION FOR Faccpedi.
    DEF BUFFER PEDIDO FOR Faccpedi.
    DEF VAR x-CanAte AS DECI NO-UNDO.
    FIND FIRST COTIZACION WHERE COTIZACION.CodCia = Faccpedi.CodCia
        AND COTIZACION.CodDoc = Faccpedi.CodRef
        AND COTIZACION.NroPed = Faccpedi.NroPed
        NO-LOCK NO-ERROR.
    IF AVAILABLE COTIZACION THEN DO:
        FOR EACH B-DPEDI OF COTIZACION EXCLUSIVE-LOCK:
            x-canate = 0.
            FOR EACH PEDIDO NO-LOCK WHERE PEDIDO.codcia = COTIZACION.codcia
                AND PEDIDO.coddoc = 'PED'
                AND PEDIDO.coddiv = COTIZACION.coddiv
                AND PEDIDO.codref = COTIZACION.coddoc
                AND PEDIDO.nroref = COTIZACION.nroped
                AND LOOKUP(PEDIDO.flgest, 'C,G,P,X,T,W,WX,WC,WL') > 0,
                EACH Facdpedi OF PEDIDO NO-LOCK WHERE Facdpedi.codmat = B-DPEDI.codmat:
                x-CanAte = x-CanAte + Facdpedi.canped.
            END.
            IF B-DPEDI.canate <> x-CanAte THEN DO:
                B-DPEDI.canate = x-canate.
            END.
        END.
        RELEASE B-DPEDI.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PED_Validate-Delete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PED_Validate-Delete Procedure 
PROCEDURE PED_Validate-Delete :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER pRowid AS ROWID.

    FIND Faccpedi WHERE ROWID(Faccpedi) = pRowid NO-LOCK.

    /* Estado del pedido */            
    DEF VAR f-Estado AS CHAR NO-UNDO.                                    
    IF LOOKUP(FacCPedi.FlgEst, "G,P,X,T,W,WX,WC,WL") = 0 THEN DO:
        RUN vta2/p-faccpedi-flgest (Faccpedi.flgest, 
                                    Faccpedi.coddoc, 
                                    OUTPUT f-Estado).
        MESSAGE 'NO se puede anular un pedido en estado:' CAPS(f-Estado) SKIP(1)
            'Proceso Abortado' VIEW-AS ALERT-BOX INFORMATION
            TITLE 'ESTADO ACTUAL DEL PEDIDO'.
        RETURN "ADM-ERROR".
    END.
    /* VERIFICAR SI TIENE ATENCIONES PARCIALES */
    FIND FIRST FacDPedi OF FacCPedi WHERE FacDPedi.CanAte > 0 NO-LOCK NO-ERROR.
    IF AVAILABLE FacDPedi THEN DO:
        MESSAGE 'No se puede eliminar un pedido con atenciones parciales' SKIP
            'Art�culo:' Facdpedi.CodMat 'Pedido:' Facdpedi.canped 'Atendido:' Facdpedi.canate
            SKIP(1)
            'Proceso Abortado' VIEW-AS ALERT-BOX INFORMATION
            TITLE 'ESTADO ACTUAL DE ATENCIONES DEL PEDIDO'.
        RETURN 'ADM-ERROR'.
    END.
    /* Facturas */
    FIND FIRST Ccbcdocu WHERE Ccbcdocu.codcia = s-codcia
        AND Ccbcdocu.fchdoc >= Faccpedi.fchped
        AND Ccbcdocu.codped = Faccpedi.coddoc
        AND Ccbcdocu.nroped = Faccpedi.nroped
        AND LOOKUP(Ccbcdocu.coddoc, "FAC,BOL") > 0
        AND Ccbcdocu.flgest <> "A"
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbcdocu THEN DO:
        MESSAGE 'No se puede eliminar un pedido con comprobantes emitidos' SKIP 
            'Comprobante:' Ccbcdocu.coddoc Ccbcdocu.nrodoc
            SKIP(1)
            'Proceso Abortado' VIEW-AS ALERT-BOX INFORMATION
            TITLE 'ESTADO ACTUAL DE COMPROBANTES DEL PEDIDO'.
        RETURN 'ADM-ERROR'.
    END.
    RUN PED_Sync_Record.
    RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SET_CLIENTE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SET_CLIENTE Procedure 
PROCEDURE SET_CLIENTE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pCodCli AS CHAR.

x-codcli-vip = pCodCli.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_cond-vta_division) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_cond-vta_division Procedure 
PROCEDURE VTA_cond-vta_division :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pCondVta AS CHAR.
DEFINE INPUT PARAMETER pDiviVta AS CHAR.
DEFINE OUTPUT PARAMETER pRetVal AS CHAR.

DEFINE VAR x-grupo AS CHAR.

pRetVal = "NO".

BUSQUEDA:
FOR EACH x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND
                            x-vtatabla.tabla = 'CV-AMBITO-GRUPO' AND
                            x-vtatabla.llave_c1 = pCondVta NO-LOCK:

    x-grupo = x-vtatabla.llave_c2.

    FIND FIRST y-vtatabla WHERE y-vtatabla.codcia = s-codcia AND
                                y-vtatabla.tabla = 'CV-AMBITO-APLICA' AND
                                y-vtatabla.llave_c1 = x-grupo AND 
                                y-vtatabla.llave_c2 = pDiviVta NO-LOCK NO-ERROR.
    IF AVAILABLE y-vtatabla THEN DO:
        pRetVal = "SI".
        LEAVE BUSQUEDA.
    END.
END.

/* ?????????????????????????? CONTADO ANTICIPADO - retiralo */
pRetVal = "SI".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_deuda_del_cliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_deuda_del_cliente Procedure 
PROCEDURE VTA_deuda_del_cliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pCodCli AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pDeudaGrupal AS LOG NO-UNDO.     
DEFINE INPUT PARAMETER pConTolerancia AS LOG NO-UNDO.     
DEFINE OUTPUT PARAMETER pDeudaSoles AS DEC NO-UNDO.
DEFINE OUTPUT PARAMETER pDeudaDolares AS DEC NO-UNDO.

/*
    pConTolerancia  : Falta implementar

    pDeudaGrupal =  YES : Si el cliente pertenece a un grupo, devuelve la deuda grupal
                    NO  :devuelve la deuda del cliente segun pCodCli pertenesca o no 
                        a un grupo
                        
    pDeudaSoles  : Devuelve la deuda en SOLES                        
    pDeudaDolares  : Devuelve la deuda en DOLARES
*/


DEFINE VAR x-desde AS DATE.
DEFINE VAR x-hasta AS DATE.
DEFINE VAR x-sustento AS LOG.

x-desde = ?.
x-hasta = ?.
x-sustento = NO.

RUN VTA_deuda_del_cliente_tramos(INPUT pCodCli, INPUT x-desde, INPUT x-hasta,
                                 INPUT pDeudaGrupal, INPUT pConTolerancia, 
                                 OUTPUT pDeudaSoles, OUTPUT pDeudaDolares,
                                 INPUT-OUTPUT TABLE tCCBCDOCU,
                                 INPUT x-sustento).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_deuda_del_cliente_tramos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_deuda_del_cliente_tramos Procedure 
PROCEDURE VTA_deuda_del_cliente_tramos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Como recibir */
DEFINE INPUT PARAMETER pCodCli AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pDesde AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pHasta AS DATE NO-UNDO.
DEFINE INPUT PARAMETER pDeudaGrupal AS LOG NO-UNDO.     
DEFINE INPUT PARAMETER pConTolerancia AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER pDeudaSoles AS DEC NO-UNDO.
DEFINE OUTPUT PARAMETER pDeudaDolares AS DEC NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tccbcdocu.
DEFINE INPUT PARAMETER pSustento AS LOG NO-UNDO.     

/*
    pConTolerancia  : Falta implementar

    pDeudaGrupal =  YES : Si el cliente pertenece a un grupo, devuelve la deuda grupal
                    NO  :devuelve la deuda del cliente segun pCodCli pertenesca o no 
                        a un grupo
                        
    pDeuda  : Devuelve la deuda en SOLES                        
    
    pDesde  : Fecha emision del documento sea mayor/igual
    pHasta  : Fecha emision del documento sea menor/igual
*/

DEFINE VAR x-Desde AS DATE NO-UNDO.
DEFINE VAR x-Hasta AS DATE NO-UNDO.

x-Desde = pDesde.
x-Hasta = pHasta.

IF x-desde = ? THEN x-desde = 01/01/1950.
IF x-hasta = ? THEN x-hasta = TODAY.

IF pDeudaGrupal = ? THEN pDeudaGrupal = YES.    /* Si no envia valor se asume que SI */
IF pConTolerancia = ? THEN pConTolerancia = NO.

IF pCodCli = "20407980566" THEN DO:
    /*MESSAGE pCodCli.*/
END.

DEF VAR pMaster AS CHAR.
DEF VAR pRelacionados AS CHAR.
DEF VAR pAgrupados AS LOG.

IF pDeudaGrupal = YES THEN DO:

    RUN ccb/p-cliente-master (pCodCli,
                              OUTPUT pMaster,
                              OUTPUT pRelacionados,
                              OUTPUT pAgrupados).
    IF pAgrupados = YES AND pRelacionados > '' THEN .
    ELSE pRelacionados = pCodCli.
END.
ELSE DO:
    pRelacionados = pCodCli.
END.


IF pCodCli = "20407980566" THEN DO:
    /*
    MESSAGE pCodCli SKIP
            pRelacionados.
            */
END.

pDeudaSoles = 0.

DEFINE VAR x-codcli AS CHAR.
DEFINE VAR x-conteo AS INT.
DEFINE VAR x-factor AS DEC.
DEFINE VAR x-impte AS DEC.
DEFINE VAR x-soles AS DEC.

FIND LAST gn-tcmb WHERE gn-tcmb.fecha <= TODAY NO-LOCK.

FOR EACH facdocum WHERE facdocum.codcia = s-codcia AND
                        facdocum.codcta[9] = "SI" NO-LOCK:

    x-factor = 1.
    IF facdocum.tpodoc = NO THEN x-factor = -1.     /* Caso N/C */

    REPEAT x-conteo = 1 TO NUM-ENTRIES(pRelacionados,","):
        x-codcli = ENTRY(x-conteo,  pRelacionados,",").

        FOR EACH CcbCDocu NO-LOCK USE-INDEX Llave06 WHERE CcbCDocu.CodCia = S-CODCIA 
            AND CcbCDocu.CodCli = x-CodCli
            AND CcbCDocu.FlgEst = 'P'
            AND CcbCDocu.coddoc = facdocum.coddoc
            AND (CcbCDocu.fchdoc >= x-desde AND CcbCDocu.fchdoc <= x-hasta) :
            x-impte = CcbCDocu.SdoAct.
            IF x-impte < 0 THEN x-impte = 0.

            IF CcbCDocu.CodMon = 2 THEN DO:
                /* Documento emitido en DOLARES */
                x-soles = ((x-impte * gn-tcmb.Compra) * x-factor).
                pDeudaSoles = pDeudaSoles + ((x-impte * gn-tcmb.Compra) * x-factor).
                pDeudaDolares = pDeudaDolares + (x-impte * x-factor).
            END.
            ELSE DO:
                x-soles = (x-impte * x-factor).
                pDeudaSoles = pDeudaSoles + (x-impte * x-factor).
                pDeudaDolares = pDeudaDolares + ((x-impte / gn-tcmb.Compra) * x-factor).
            END.

            IF pSustento = YES THEN DO:
                FIND FIRST tCCBCDOCU OF ccbcdocu EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE tCCBCDOCU THEN DO:
                    CREATE tCCBCDOCU.
                    BUFFER-COPY CCBCDOCU TO tCCBCDOCU.
                    ASSIGN tCCBCDOCU.CodAnt = pMaster.
                END.
            END.

        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_Genera-OD) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_Genera-OD Procedure 
PROCEDURE VTA_Genera-OD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.
DEF OUTPUT PARAMETER pComprobante AS CHAR.     /* O/D generada */
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.
 
pMensaje = "".
FIND B-CPEDI WHERE ROWID(B-CPEDI) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CPEDI THEN DO:
    pMensaje = "NO se ubic� el Pedido".
    RETURN 'ADM-ERROR'.
END.
/* ***************************************************************************** */
/* SOLO PARA PEDIDOS APROBADOS */
/* ***************************************************************************** */
IF B-CPEDI.FlgEst <> "P" THEN RETURN 'OK'.
/* ***************************************************************************** */
/* NO DEBE SER PARA CROSS DOCKING */
/* ***************************************************************************** */
IF B-CPEDI.CrossDocking = YES THEN DO:
    pMensaje = "Si es para Cross Docking NO debe generar una ORDEN DE DESPACHO".
    RETURN 'ADM-ERROR'.
END.
/* ***************************************************************************** */
/* NOTA: La divisi�n debe ser la de FACCPEDI */
/* ***************************************************************************** */
DEF VAR s-CodDiv AS CHAR NO-UNDO.
s-CodDiv = B-CPEDI.CodDiv.
/* ***************************************************************************** */
DEFINE VARIABLE s-FlgPicking LIKE GN-DIVI.FlgPicking.
DEFINE VARIABLE s-FlgBarras LIKE GN-DIVI.FlgBarras.
DEFINE VARIABLE s-DiasVtoO_D LIKE GN-DIVI.DiasVtoO_D.
DEFINE VARIABLE s-NroSer AS INTEGER.
/* ***************************************************************************** */
/* PARAMETROS DE COTIZACION PARA LA DIVISION */
/* ***************************************************************************** */
FIND FIRST gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = s-coddiv
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-divi THEN DO:
    pMensaje = 'Divisi�n ' + s-coddiv + ' NO configurada'.
    RETURN 'ADM-ERROR'.
END.
DEF VAR s-coddoc AS CHAR INIT "O/D" NO-UNDO.
CASE B-CPEDI.CodDoc:
    WHEN "PED" THEN s-CodDoc = "O/D".
    WHEN "P/M" THEN s-CodDoc = "O/M".
END CASE.
FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
    FacCorre.CodDoc = S-CODDOC AND
    FacCorre.CodDiv = S-CODDIV AND
    FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
   pMensaje = "Correlativo del Documento no configurado: " + s-coddoc.
   RETURN 'ADM-ERROR'.
END.
ASSIGN
    s-nroser = FacCorre.NroSer.
ASSIGN
    s-FlgPicking = GN-DIVI.FlgPicking
    s-FlgBarras  = GN-DIVI.FlgBarras
    s-DiasVtoO_D = GN-DIVI.DiasVtoO_D.
FIND FIRST FacCfgGn WHERE FacCfgGn.codcia = s-codcia NO-LOCK.
/* ***************************************************************************** */
DEFINE VAR cEsValeUtilex AS LOG INIT NO.
DEFINE VAR cNroVales AS CHAR.

DEFINE VAR hMaster AS HANDLE NO-UNDO.
RUN gn/master-library PERSISTENT SET hMaster.

pMensaje = "".
SESSION:SET-WAIT-STATE('GENERAL').
LoopGrabarData:
DO TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE :
    FIND CURRENT B-CPEDI EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE B-CPEDI THEN DO:
        pMensaje = "No se pudo Bloquear B-CPEDI".
        UNDO LoopGrabarData, LEAVE.
    END.
    {lib/lock-genericov3.i
        &Tabla="FacCorre"
        &Condicion=" FacCorre.CodCia = s-codcia
        AND FacCorre.CodDoc = s-coddoc
        AND FacCorre.NroSer = s-nroser"
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR"
        &Accion="RETRY"
        &Intentos=10
        &Mensaje="NO"
        &txtMensaje="pMensaje"
        &TipoError="UNDO LoopGrabarData, LEAVE LoopGrabarData"
        }
    /* **************************** */
    /* NOTA: el campo Importe[1] e Importe[2] sirven para determinar el redondeo
        Importe[1]: Importe original del pedido
        Importe[2]: Importe del redondeo
        NOTA. El campo TpoLic sirve para controlar cuando se aplica o no un adelanto de campa�a
    */
    /* Ic - 29Ene2016, para vales utilex (B-CPEDI : PEDIDO)*/
    /* Busco la cotizacion */
    FIND FIRST COTIZACION WHERE COTIZACION.codcia = s-codcia 
        AND COTIZACION.coddoc = 'COT' 
        AND COTIZACION.nroped = b-cpedi.nroref NO-LOCK NO-ERROR.
    IF AVAILABLE COTIZACION THEN DO:
        IF COTIZACION.tpoped = 'VU' THEN cEsValeUtilex = YES.
    END.

    CREATE FacCPedi.
    BUFFER-COPY B-CPEDI 
        EXCEPT 
        B-CPEDI.TpoPed
        B-CPEDI.FlgEst
        B-CPEDI.FlgSit
        /*B-CPEDI.TipVta*/
        TO FacCPedi
        ASSIGN 
            FacCPedi.CodCia = S-CODCIA
            FacCPedi.CodDiv = S-CODDIV
            FacCPedi.CodDoc = s-coddoc 
            FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
            FacCPedi.CodRef = B-CPEDI.CodDoc
            FacCPedi.NroRef = B-CPEDI.NroPed
            FacCPedi.TpoCmb = FacCfgGn.TpoCmb[1] 
            FacCPedi.FchPed = TODAY
            FacCPedi.FchVen = TODAY + s-DiasVtoO_D
            FacCPedi.Hora = STRING(TIME,"HH:MM")
            FacCPedi.FlgEst = 'X'   /* PROCESANDO: revisar rutina Genera-Pedido */
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        pMensaje = "Correlativo de la Orden mal registrado o duplicado".
        UNDO LoopGrabarData, LEAVE.
    END.
    /* ************************************************************************** */
    /* RHC 22/11/17 se va a volver a calcular la fecha de entrega                 */
    /* La O/D hereda la FchEnt y Libre_f02 del PED                                */
    /* ************************************************************************** */
    DEF VAR pFchEnt AS DATE NO-UNDO.
    RUN logis/p-fecha-de-entrega (INPUT Faccpedi.CodDoc,
                                  INPUT Faccpedi.NroPed,
                                  INPUT-OUTPUT pFchEnt,
                                  OUTPUT pMensaje).
    IF pMensaje > '' THEN DO:
        UNDO LoopGrabarData, LEAVE.
    END.
    ASSIGN
        FacCPedi.FchEnt = pFchEnt.  /* OJO */
    /* ************************************************************************** */
    /* TRACKING */
    /* ******************************************************************** */
    RUN vtagn/pTracking-04 (Faccpedi.CodCia,
                            Faccpedi.CodDiv,
                            Faccpedi.CodRef,
                            Faccpedi.NroRef,
                            s-User-Id,
                            'GOD',
                            'P',
                            DATETIME(TODAY, MTIME),
                            DATETIME(TODAY, MTIME),
                            Faccpedi.CodDoc,
                            Faccpedi.NroPed,
                            Faccpedi.CodRef,
                            Faccpedi.NroRef)
        NO-ERROR.
    /* ******************************************************************** */
    /* COPIAMOS DATOS DEL TRANSPORTISTA */
    /* ******************************************************************** */
    FIND FIRST Ccbadocu WHERE Ccbadocu.codcia = B-CPEDI.codcia
        AND Ccbadocu.coddiv = B-CPEDI.coddiv
        AND Ccbadocu.coddoc = B-CPEDI.coddoc
        AND Ccbadocu.nrodoc = B-CPEDI.nroped
        NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbadocu THEN DO:
        FIND FIRST B-ADOCU WHERE B-ADOCU.codcia = Faccpedi.codcia
            AND B-ADOCU.coddiv = Faccpedi.coddiv
            AND B-ADOCU.coddoc = Faccpedi.coddoc
            AND B-ADOCU.nrodoc = Faccpedi.nroped
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE B-ADOCU THEN CREATE B-ADOCU.
        BUFFER-COPY Ccbadocu TO B-ADOCU
            ASSIGN
                B-ADOCU.CodDiv = FacCPedi.CodDiv
                B-ADOCU.CodDoc = FacCPedi.CodDoc
                B-ADOCU.NroDoc = FacCPedi.NroPed
            NO-ERROR.
    END.
    /* ******************************************************************** */
    ASSIGN 
        FacCPedi.UsrAprobacion = S-USER-ID
        FacCPedi.FchAprobacion = TODAY.
    /* ******************************************************************** */
    /* Detalle de la OD */
    /* ******************************************************************** */
    
    DEFINE VARIABLE I-NITEM AS INTEGER NO-UNDO INIT 0.
    pMensaje = "NO se pudo generar el detalle de la Orden".
    FOR EACH B-DPEDI OF B-CPEDI NO-LOCK BY B-DPEDI.NroItm ON ERROR UNDO, THROW:
        /* GRABAMOS LA DIVISION Y EL ALMACEN DESTINO EN LA CABECERA */
        I-NITEM = I-NITEM + 1.
        CREATE FacDPedi. 
        BUFFER-COPY B-DPEDI 
            TO FacDPedi
            ASSIGN  
            FacDPedi.CodCia  = FacCPedi.CodCia 
            FacDPedi.coddiv  = FacCPedi.coddiv 
            FacDPedi.coddoc  = FacCPedi.coddoc 
            FacDPedi.NroPed  = FacCPedi.NroPed 
            FacDPedi.FchPed  = FacCPedi.FchPed
            FacDPedi.Hora    = FacCPedi.Hora 
            FacDPedi.FlgEst  = 'P'       /*FacCPedi.FlgEst*/
            FacDPedi.NroItm  = I-NITEM
            FacDPedi.CanAte  = 0                     /* <<< OJO <<< */
            FacDPedi.CanSol  = FacDPedi.CanPed       /* <<< OJO <<< */
            FacDPedi.CanPick = FacDPedi.CanPed      /* <<< OJO <<< */
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            pMensaje = "Error al grabar el producto " + B-DPEDI.codmat.
            UNDO LoopGrabarData, LEAVE LoopGrabarData.
        END.
    END.
    pMensaje = "".
    /* ******************************************************************** */
    /* RHC 26/12/2016 Consistencia final: La O/D y el PED deben ser iguales */
    /* ******************************************************************** */
    DEF VAR x-ItmPed AS INT NO-UNDO.
    DEF VAR x-ItmOD  AS INT NO-UNDO.
    DEF VAR x-ImpPed AS DEC NO-UNDO.
    DEF VAR x-ImpOD  AS DEC NO-UNDO.
    ASSIGN
        x-ItmPed = 0
        x-ItmOD  = 0
        x-ImpPed = 0
        x-ImpOD  = 0.
    FOR EACH Facdpedi OF Faccpedi NO-LOCK:
        x-ItmPed = x-ItmPed + 1.
        x-ImpPed = x-ImpPed + Facdpedi.ImpLin.
    END.
    FOR EACH B-DPEDI OF B-CPEDI NO-LOCK:
        x-ItmOD = x-ItmOD + 1.
        x-ImpOD = x-ImpOD + B-DPEDI.ImpLin.
    END.
    IF x-ItmPed <> x-ItmOD OR x-ImpPed <> x-ImpOD THEN DO:
        pMensaje = "Se encontr� una inconsistencia entre el Pedido y la Orden de Despacho" +  CHR(10) +
            "Proceso Abortado".        
        UNDO LoopGrabarData, LEAVE.
    END.
    /* ******************************************************************** */
    /* CONTROL DE OTROS PROCESOS POR DIVISION */
    /* ******************************************************************** */
    FIND FIRST gn-divi WHERE gn-divi.codcia = Faccpedi.codcia
        AND gn-divi.coddiv = Faccpedi.divdes
        NO-LOCK.
    ASSIGN
        s-FlgPicking = GN-DIVI.FlgPicking
        s-FlgBarras  = GN-DIVI.FlgBarras.
    IF s-FlgPicking = YES THEN FacCPedi.FlgSit = "T".    /* Por Pickear en Almac�n */
    IF s-FlgPicking = NO AND s-FlgBarras = NO THEN FacCPedi.FlgSit = "C".    /* Picking OK */
    IF s-FlgPicking = NO AND s-FlgBarras = YES THEN FacCPedi.FlgSit = "P".   /* Pre-Picking OK */
    /* Ic - 29Ene2016, para vales utilex (B-CPEDI : PEDIDO)*/
    IF (cEsValeUtilex = YES) THEN DO:
        ASSIGN 
            FacCPedi.FlgEst = 'P'
            FacCPedi.FlgSit = 'C' .
    END.
    /* Caso Tr�mite Documentario */
    IF Faccpedi.TipVta = "Si" THEN DO:
        /* Listo para Facturar */
        ASSIGN 
            FacCPedi.FlgEst = 'P'
            FacCPedi.FlgSit = 'C' .
    END.
    
    /* ******************************************************************** */
    /* Genera Sub-Pedidos si fuera el caso */
    /* ******************************************************************** */
    /* RHC 01/10/2019 Caso especial CANAL MODERNO */
    IF FacCPedi.FlgSit = "T" THEN DO:
        RUN Genera-SubOrden IN hMaster (INPUT ROWID(Faccpedi),
                                        INPUT Faccpedi.DivDes,     /* Divisi�n de Despacho */
                                        OUTPUT pMensaje).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            IF TRUE <> (pMensaje > '') THEN pMensaje = 'NO se pudo generar la sub-orden'.
            UNDO LoopGrabarData, LEAVE.        
        END.
/*         RUN Genera-SubPedidos.                                                          */
/*         IF RETURN-VALUE = 'ADM-ERROR' THEN DO:                                          */
/*             IF TRUE <> (pMensaje > '') THEN                                             */
/*                 pMensaje = "No se pudo generar el detalle de la Sub-Orden de Despacho". */
/*             UNDO LoopGrabarData, LEAVE.                                                 */
/*         END.                                                                            */
    END.
    /* *************************************************************************** */
    /* Actualiza Cantidad Atendida del Pedido */
    /* *************************************************************************** */
/*     RUN Actualiza-Pedido (+1). */
    
    RUN Actualiza-Saldo-Referencia IN hMaster (ROWID(Faccpedi), "C", OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = "ERROR: No se pudo actualizar el saldo del Pedido".
        UNDO LoopGrabarData, LEAVE.        
    END.
    /* *************************************************************************** */
    /* RHC 08/04/2016 Ahora s� actualizamos el estado */
    /* *************************************************************************** */
    ASSIGN
        FacCPedi.FlgEst = 'P'.  /* APROBADO */
    ASSIGN 
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    RELEASE FacCorre.
    pComprobante = Faccpedi.coddoc + ' ' + Faccpedi.nroped.
    /* *************************************************************************** */
    /* RHC 22/09/2020 Control por M.R. */
    /* *************************************************************************** */
    RUN ML_Genera-OD-Control IN hMaster (INPUT ROWID(Faccpedi),    /* O/D */
                                         OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = "ERROR: No se pudo generar el registro de control".
        UNDO LoopGrabarData, LEAVE.        
    END.
    /* 03/02/2022 Generaci�n de PHR */
    RUN VTA_Genera-PHR_y_HPK (INPUT ROWID(Faccpedi), OUTPUT pMensaje).     /* Dato: O/D */
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = "ERROR: No se pudo generar la PHR ni la HPK".
        UNDO LoopGrabarData, LEAVE.        
    END.
END.
DELETE PROCEDURE hMaster.
SESSION:SET-WAIT-STATE('').
IF TRUE <> (pMensaje > "") THEN RETURN 'OK'.
ELSE RETURN 'ADM-ERROR'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_Genera-OTR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_Genera-OTR Procedure 
PROCEDURE VTA_Genera-OTR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

pMensaje = ''.

FIND B-CPEDI WHERE ROWID(B-CPEDI) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CPEDI THEN DO:
    pMensaje = "NO se pudo ubicar el pedido de ventas".
    RETURN 'ADM-ERROR'.
END.
IF B-CPEDI.FlgEst <> "P" THEN RETURN "OK".
/* DEBE SER PARA CROSS DOCKING */
IF B-CPEDI.CrossDocking = NO THEN DO:
    pMensaje = "Si NO es para Cross Docking NO debe generar una ORDEN DE TRANSFERENCIA".
    RETURN 'ADM-ERROR'.
END.

DEF VAR lDivDespacho AS CHAR NO-UNDO.
DEF VAR lAlmDespacho AS CHAR NO-UNDO.

DEF VAR s-coddoc     AS CHAR INITIAL "OTR" NO-UNDO.    /* Orden de Transferencia */
DEF VAR s-NroSer     AS INT  NO-UNDO.
DEF VAR s-TpoPed     AS CHAR INITIAL "" NO-UNDO.

/* PARAMETROS DE PEDIDOS PARA LA DIVISION */
DEF VAR s-FlgEmpaque LIKE GN-DIVI.FlgEmpaque.
DEF VAR s-DiasVtoPed LIKE GN-DIVI.DiasVtoPed.
DEF VAR s-VentaMayorista LIKE GN-DIVI.VentaMayorista.
DEF VAR s-FlgPicking LIKE GN-DIVI.FlgPicking.
DEF VAR s-FlgBarras LIKE GN-DIVI.FlgBarras.

DEF VAR iNroSKU     AS INT NO-UNDO.
DEF VAR iPeso       AS DEC NO-UNDO.
DEF VAR cUbigeo     AS CHAR NO-UNDO.
DEF VAR lHora       AS CHAR NO-UNDO.
DEF VAR lDias       AS INT NO-UNDO.
DEF VAR lHoraTope   AS CHAR NO-UNDO.
DEF VAR lFechaPedido AS DATE NO-UNDO.

/* El Almac�n Destino (El que va a hacer el despacho final al cliente) */
FIND FIRST Almacen WHERE Almacen.codcia = s-codcia
      AND Almacen.codalm = B-CPEDI.AlmacenXD NO-LOCK NO-ERROR.
IF NOT AVAILABLE almacen THEN DO:
    pMensaje = 'Almac�n ' + B-CPEDI.AlmacenXD + ' NO existe'.
    RETURN "ADM-ERROR".
END.
FIND gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = Almacen.CodDiv
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-divi THEN DO:
    pMensaje = 'Divisi�n ' + Almacen.CodDiv + ' NO configurada'.
    RETURN "ADM-ERROR".
END.
cUbigeo = TRIM(gn-divi.Campo-Char[3]) + TRIM(gn-divi.Campo-Char[4]) + TRIM(gn-divi.Campo-Char[5]).
/* El almac�n de Despacho (El que deber�a despachar al cliente) */
FIND FIRST Almacen WHERE Almacen.codcia = s-codcia
      AND Almacen.codalm = B-CPEDI.CodAlm NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almacen THEN DO:
    pMensaje = 'Almacen de despacho ' + B-CPEDI.CodAlm + ' No existe'.
    RETURN "ADM-ERROR".
END.
lDivDespacho = Almacen.CodDiv.
lAlmDespacho = Almacen.CodAlm.
/* Control del correlativo */
FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
    FacCorre.CodDiv = lDivDespacho AND
    FacCorre.CodDoc = S-CODDOC AND
    FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
   pMensaje = "Codigo de Documento " + s-coddoc + " No configurado para la division " + lDivDespacho.
   RETURN "ADM-ERROR".
END.
/* La serie segun el almac�n de donde se desea despachar */
s-NroSer = FacCorre.NroSer.
/* Datos de la Divisi�n de Despacho */
FIND gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = lDivDespacho
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-divi THEN DO:
    pMensaje = 'Divisi�n ' + lDivDespacho + ' NO configurada'.
    RETURN "ADM-ERROR".
END.
ASSIGN
    s-DiasVtoPed = GN-DIVI.DiasVtoPed
    s-FlgEmpaque = GN-DIVI.FlgEmpaque
    s-VentaMayorista = GN-DIVI.VentaMayorista.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DEFINE VAR hMaster AS HANDLE NO-UNDO.
RUN gn/master-library PERSISTENT SET hMaster.

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FIND CURRENT B-CPEDI EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CPEDI THEN UNDO, RETURN 'ADM-ERROR'.
    /* ******************************************************************************** */
    /* Paso 01 : Llenar el Temporal con el detalle de los Articulos */
    /* ******************************************************************************** */
    EMPTY TEMP-TABLE PEDI.
    DEFINE VARIABLE I-NPEDI AS INTEGER NO-UNDO.
    DEFINE VARIABLE f-Factor AS DEC NO-UNDO.
    DEFINE VARIABLE F-CANPED AS DECIMAL NO-UNDO.
    DEFINE VARIABLE x-CodAlm AS CHAR NO-UNDO.
    DEFINE VARIABLE x-CanPed AS DEC NO-UNDO.
    i-NPedi = 0.
    DEF VAR t-AlmDes AS CHAR NO-UNDO.
    DEF VAR t-CanPed AS DEC NO-UNDO.
    FOR EACH B-DPEDI OF B-CPEDI NO-LOCK, FIRST Almmmatg OF B-DPEDI NO-LOCK:
        f-Factor = 1.
        t-AlmDes = ''.
        t-CanPed = 0.
        F-CANPED = (B-DPEDI.CanPed - B-DPEDI.CanAte).     /* OJO */
        x-CodAlm = lAlmDespacho.
        /* DEFINIMOS LA CANTIDAD */
        x-CanPed = f-CanPed * f-Factor.
        IF f-CanPed <= 0 THEN NEXT.
        IF f-CanPed > t-CanPed THEN DO:
            t-CanPed = f-CanPed.
            t-AlmDes = x-CodAlm.
        END.
        I-NPEDI = I-NPEDI + 1.
        CREATE PEDI.
        BUFFER-COPY B-DPEDI 
            TO PEDI
            ASSIGN 
            PEDI.CodCia = s-codcia
            PEDI.CodDiv = lDivDespacho
            PEDI.CodDoc = s-coddoc
            PEDI.NroPed = ''
            PEDI.ALMDES = t-AlmDes  /* *** OJO *** */
            PEDI.NroItm = I-NPEDI
            PEDI.CanPed = t-CanPed            /* << OJO << */
            PEDI.CanAte = 0.
        ASSIGN
            PEDI.Libre_d01 = (B-DPEDI.CanPed - B-DPEDI.CanAte)
            PEDI.Libre_d02 = t-CanPed
            PEDI.Libre_c01 = '*'.
        ASSIGN
            PEDI.UndVta = Almmmatg.UndBas.
    END.
    /* ******************************************************************************** */
    /* Paso 02 : Adiciono el Registro en la Cabecera */
    /* ******************************************************************************** */
    {lib/lock-genericov3.i
        &Tabla="FacCorre"
        &Condicion="Faccorre.codcia = s-codcia
        AND Faccorre.coddoc = s-coddoc
        AND Faccorre.nroser = s-nroser"
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR"
        &Accion="RETRY"
        &Mensaje="NO"
        &txtMensaje="pMensaje"
        &TipoError="UNDO, RETURN 'ADM-ERROR'"
        }
    /* ***************************************************************** */
    /* Determino la fecha de entrega del pedido */
    /* ***************************************************************** */
    ASSIGN
        iNroSKU = 0
        iPeso = 0.
    FOR EACH PEDI NO-LOCK, FIRST Almmmatg OF PEDI NO-LOCK:
        iNroSKU = iNroSKU + 1.
        iPeso = iPeso + (PEDI.CanPed * PEDI.Factor) * Almmmatg.PesMat.
    END.
    lFechaPedido = MAXIMUM(TODAY, B-CPEDI.FchEnt).
    FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = B-CPEDI.codalm NO-LOCK NO-ERROR.
    CREATE Faccpedi.
    BUFFER-COPY B-CPEDI TO Faccpedi
        ASSIGN 
        Faccpedi.CodCia = S-CODCIA
        Faccpedi.CodDiv = lDivDespacho
        Faccpedi.CodDoc = s-coddoc      /* OTR */
        Faccpedi.TpoPed = s-tpoped      /* "" */
        Faccpedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.CodAlm = lAlmDespacho
        Faccpedi.FchPed = TODAY
        Faccpedi.FlgEst = "X"       /* EN PROCESO */
        FacCPedi.FlgEnv = YES
        FacCPedi.Libre_c01 = s-User-id + '|' + STRING(DATETIME(TODAY, MTIME), '99/99/9999 HH:MM')
        FacCPedi.FchEnt = lFechaPedido
        Faccpedi.FchVen = lFechaPedido + 7
        Faccpedi.CodRef = B-CPEDI.CodDoc    /* PED */
        FacCPedi.NroRef = B-CPEDI.NroPed
        FacCPedi.Glosa = B-CPEDI.Glosa
        NO-ERROR.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        pMensaje = 'NO se pudo grabar la ' + s-coddoc.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    /* RHC 20/12/17 Cross Docking */
    ASSIGN
        Faccpedi.CrossDocking = YES
        Faccpedi.AlmacenXD    = Faccpedi.CodCli.            /* Destino Final (Cliente) */
    ASSIGN
        Faccpedi.CodCli       = B-CPEDI.AlmacenXD.          /* Almac�n de Tr�nsito */
    FIND Almacen WHERE Almacen.codcia = s-codcia AND Almacen.codalm = B-CPEDI.AlmacenXD NO-LOCK NO-ERROR.
    ASSIGN
        Faccpedi.NomCli = Almacen.Descripcion
        Faccpedi.Dircli = Almacen.DirAlm.
    /* ************************** */
    ASSIGN 
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    /* Actualizamos la hora cuando lo vuelve a modificar */
    ASSIGN
        Faccpedi.Usuario = S-USER-ID
        Faccpedi.Hora   = STRING(TIME,"HH:MM").
    /* Division destino */
    FIND Almacen OF Faccpedi NO-LOCK NO-ERROR.
    IF AVAILABLE Almacen THEN FacCPedi.DivDes = Almacen.CodDiv.
    /* CONTROL DE OTROS PROCESOS POR DIVISION */
    FIND gn-divi WHERE gn-divi.codcia = Faccpedi.codcia
        AND gn-divi.coddiv = Faccpedi.divdes
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-divi THEN
        ASSIGN
            s-FlgPicking = GN-DIVI.FlgPicking
            s-FlgBarras  = GN-DIVI.FlgBarras.
    IF s-FlgPicking = YES THEN FacCPedi.FlgSit = "T".    /* Por Pre-Pickear */
    IF s-FlgPicking = NO AND s-FlgBarras = NO THEN FacCPedi.FlgSit = "C".    /* Barras OK */
    IF s-FlgPicking = NO AND s-FlgBarras = YES THEN FacCPedi.FlgSit = "P".   /* Pre-Picking OK */
    /* ***************************************************************** */
    /* DETALLE DE LA OTR */
    /* ***************************************************************** */
    /* Borramos data sobrante */
    FOR EACH PEDI WHERE PEDI.CanPed <= 0:
        DELETE PEDI.
    END.
    /* AHORA S� GRABAMOS EL PEDIDO */
    I-NPEDI = 0.
    FOR EACH PEDI, FIRST Almmmatg OF PEDI NO-LOCK BY PEDI.NroItm: 
        I-NPEDI = I-NPEDI + 1.
        CREATE Facdpedi.
        BUFFER-COPY PEDI 
            TO Facdpedi
            ASSIGN
            Facdpedi.CodCia = Faccpedi.CodCia
            Facdpedi.CodDiv = Faccpedi.CodDiv
            Facdpedi.coddoc = Faccpedi.coddoc
            Facdpedi.NroPed = Faccpedi.NroPed
            Facdpedi.AlmDes = Faccpedi.CodAlm
            Facdpedi.FchPed = Faccpedi.FchPed
            Facdpedi.Hora   = Faccpedi.Hora 
            Facdpedi.FlgEst = Faccpedi.FlgEst
            FacDPedi.CanPick = FacDPedi.CanPed
            Facdpedi.NroItm = I-NPEDI.
        DELETE PEDI.
    END.
    /* verificamos que al menos exista 1 item grabado */
    FIND FIRST Facdpedi OF Faccpedi NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Facdpedi 
        THEN RETURN 'ADM-ERROR'.
    ELSE RETURN 'OK'.
    /* ***************************************************************** */
    /* FECHA DE ENTREGA */
    /* ***************************************************************** */
    RUN logis/p-fecha-de-entrega (FacCPedi.CodDoc,              /* Documento actual */
                                  FacCPedi.NroPed,
                                  INPUT-OUTPUT lFechaPedido,
                                  OUTPUT pMensaje).
    IF pMensaje > '' THEN DO:
        UNDO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        FacCPedi.FchEnt = lFechaPedido
        Faccpedi.FchVen = lFechaPedido + 7
        Faccpedi.FlgEst = "P".                  /* OJO >>> APROBADO */
    /* *********************************************************** */
    /* *********************************************************** */
    /* Actualizamos la cotizacion */
/*     RUN Actualiza-Pedido (+1). */
    RUN Actualiza-Saldo-Referencia IN hMaster ("C", OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        pMensaje = "ERROR: No se pudo actualizar el saldo del Pedido".
        UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
    END.
    /* *********************************************************** */
    /* TRACKING */
    RUN vtagn/pTracking-04 (Faccpedi.CodCia,
                            Faccpedi.CodDiv,
                            Faccpedi.CodRef,
                            Faccpedi.NroRef,
                            s-User-Id,
                            'GOT',    /* Generaci�n OTR */
                            'P',
                            DATETIME(TODAY, MTIME),
                            DATETIME(TODAY, MTIME),
                            Faccpedi.CodDoc,
                            Faccpedi.NroPed,
                            Faccpedi.CodRef,
                            Faccpedi.NroRef)
        NO-ERROR.
    /* ******************************************************************** */
    /* Genera Sub-Pedidos si fuera el caso */
    /* ******************************************************************** */
    IF FacCPedi.FlgSit = "T" THEN DO:
        RUN Genera-SubOrden (INPUT Faccpedi.CodDiv,     /* Divisi�n de Despacho */
                             OUTPUT pMensaje).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            pMensaje = 'NO se pudo generar la sub-orden'.
            UNDO, RETURN 'ADM-ERROR'.
        END.
    END.
    /* *************************************************************************** */
    /* RHC 22/09/2020 Control por M.R. */
    /* *************************************************************************** */
    RUN ML_Genera-OD-Control IN hMaster (INPUT ROWID(Faccpedi),    /* O/D */
                                         OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        IF TRUE <> (pMensaje > '') THEN pMensaje = "ERROR: No se pudo generar el registro de control".
        UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
    END.
    /* *************************************************************************** */
    /* ************ CREAMOS VARIAS OTRs SI FUERA NECESARIO ************* */
    /* *************************************************************************** */
    REPEAT ON ERROR UNDO PRINCIPAL, RETURN 'ADM-ERROR' ON STOP UNDO PRINCIPAL, RETURN 'ADM-ERROR':
        FIND FIRST PEDI NO-ERROR.
        IF NOT AVAILABLE PEDI THEN LEAVE.
        CREATE B-CPEDI.
        BUFFER-COPY FacCPedi TO B-CPEDI ASSIGN B-CPEDI.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999").
        ASSIGN FacCorre.Correlativo = FacCorre.Correlativo + 1.
        FIND FacCPedi WHERE ROWID(Faccpedi) = ROWID(B-CPEDI).
        RUN Genera-Pedido.    /* Detalle del pedido */
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            pMensaje = 'NO se pudo generar el pedido' + CHR(10) + 'NO hay stock suficiente en los almacenes'.
            UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
        END.
        /* Actualizamos la cotizacion */
        /*     RUN Actualiza-Pedido (+1). */
        RUN Actualiza-Saldo-Referencia IN hMaster ("C", OUTPUT pMensaje).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            pMensaje = "ERROR: No se pudo actualizar el saldo del Pedido".
            UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
        END.
        /* *********************************************************** */
        /* ******************************************************************** */
        /* Genera Sub-Pedidos si fuera el caso */
        /* ******************************************************************** */
        IF FacCPedi.FlgSit = "T" THEN DO:
            RUN Genera-SubOrden (INPUT Faccpedi.CodDiv,     /* Divisi�n de Despacho */
                                 OUTPUT pMensaje).
            IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
                pMensaje = 'NO se pudo generar la sub-orden'.
                UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
            END.
        END.
        /* *************************************************************************** */
        /* RHC 22/09/2020 Control por M.R. */
        /* *************************************************************************** */
        RUN ML_Genera-OD-Control IN hMaster (INPUT ROWID(Faccpedi),    /* O/D */
                                             OUTPUT pMensaje).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            IF TRUE <> (pMensaje > '') THEN pMensaje = "ERROR: No se pudo generar el registro de control".
            UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
        END.
    END.
END.
DELETE PROCEDURE hMaster.
IF AVAILABLE(FacCorre) THEN RELEASE FacCorre.
IF AVAILABLE(Facdpedi) THEN RELEASE Facdpedi.
IF AVAILABLE(Faccpedi) THEN RELEASE Faccpedi.
IF AVAILABLE(B-CPEDI)  THEN RELEASE B-CPEDI.
IF AVAILABLE(B-CPEDI)  THEN RELEASE B-CPEDI.
IF pMensaje > '' THEN RETURN 'ADM-ERROR'.
ELSE RETURN 'OK'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_Genera-PHR_y_HPK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_Genera-PHR_y_HPK Procedure 
PROCEDURE VTA_Genera-PHR_y_HPK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.        /* O/D */
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

/* 1ro Generamos la PHR siempre y cuando se despache en un CD por Picking por rutas */
/* 2do Generamos la HPK */
DEF BUFFER ORDENES FOR Faccpedi.

FIND ORDENES WHERE ROWID(ORDENES) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE ORDENES THEN DO:
    pMensaje = "NO se pudo ubicar el registro de la O/D u OTR".
    RETURN 'ADM-ERROR'.
END.
/* La divisi�n despacho debe ser PICKING POR RUTAS */
FIND gn-divi WHERE gn-divi.codcia = s-codcia AND
    gn-divi.coddiv = ORDENES.DivDes NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-divi OR gn-divi.Campo-Char[6] <> "PR" THEN RETURN 'OK'.

/* La divisi�n origen del ser PHR AUTOMATICA */
FIND VtaTabla WHERE VtaTabla.CodCia = s-CodCia AND
    VtaTabla.Tabla = "CFGDIV_PHR" AND
    VtaTabla.Llave_c1 = ORDENES.DivDes AND
    VtaTabla.Llave_c2 = ORDENES.CodDiv
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN RETURN 'OK'.

DEF VAR s-CodDoc AS CHAR NO-UNDO INIT 'PHR'.
DEF VAR s-adm-new-record AS LOG NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.
DEF VAR s-nroser AS INTE NO-UNDO.
DEF VAR pCuenta AS INTE NO-UNDO.

/* ARtificio */
ASSIGN
    s-coddiv = ORDENES.DivDes.
FIND FIRST FacCorre WHERE FacCorre.CodCia = s-codcia AND
    FacCorre.CodDiv = s-coddiv AND
    FacCorre.CodDoc = s-coddoc AND
    FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    pMensaje = "Correlativo no configurado para la divisi�n " + s-coddiv +
        " y el documento " + s-coddoc.
    RETURN 'ADM-ERROR'.
END.
ASSIGN
    s-nroser = FacCorre.NroSer.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* 1ro. Generamos la PHR */
    IF ORDENES.Cliente_Recoge = YES THEN DO:
        FIND FIRST Di-RutaC WHERE DI-RutaC.CodCia = s-codcia AND
            DI-RutaC.CodDiv = s-coddiv AND
            DI-RutaC.CodDoc = s-coddoc AND
            DI-RutaC.FchDoc = TODAY AND
            DI-RutaC.FlgEst = "PK" AND
            DI-RutaC.Libre_l05 = YES AND
            DI-RutaC.Libre_c05 = "AUTOMATICO" AND
            DI-RutaC.Observ BEGINS ORDENES.CodDiv   /* Origen */
            NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        FIND FIRST Di-RutaC WHERE DI-RutaC.CodCia = s-codcia AND
            DI-RutaC.CodDiv = s-coddiv AND
            DI-RutaC.CodDoc = s-coddoc AND
            DI-RutaC.FchDoc = TODAY AND
            DI-RutaC.FlgEst = "PK" AND
            DI-RutaC.Libre_l05 = NO AND
            DI-RutaC.Libre_c05 = "AUTOMATICO" AND
            DI-RutaC.Observ BEGINS ORDENES.CodDiv   /* Origen */
            NO-LOCK NO-ERROR.
    END.
    s-adm-new-record = YES.
    IF AVAILABLE Di-RutaC THEN s-adm-new-record = NO.
    IF s-adm-new-record = YES THEN DO:
        {lib/lock-genericov3.i ~
            &Tabla="FacCorre" ~
            &Condicion="FacCorre.CodCia = s-codcia AND ~
            FacCorre.CodDiv = s-coddiv AND ~
            FacCorre.CodDoc = s-coddoc AND ~
            FacCorre.NroSer = s-nroser" ~
            Bloqueo="EXCLUSIVE-LOCK NO-ERROR NO-WAIT" ~
            &Accion="RETRY" ~
            &Mensaje="NO" ~
            &txtMensaje="pMensaje" ~
            &TipoError="UNDO, RETURN 'ADM-ERROR'" }
        CREATE DI-RutaC.
        ASSIGN
            DI-RutaC.CodCia = s-codcia
            DI-RutaC.CodDiv = s-coddiv
            DI-RutaC.CodDoc = s-coddoc
            DI-RutaC.FchDoc = TODAY
            DI-RutaC.NroDoc = STRING(FacCorre.NroSer, '999') + STRING(FacCorre.Correlativo, '999999')
            DI-RutaC.Libre_c05 = "AUTOMATICO"   
            DI-RutaC.Libre_L05 = ORDENES.Cliente_Recoge
            DI-RutaC.usuario = s-user-id
            DI-RutaC.flgest  = "PK"     /* Con Hoja de Ruta */
            NO-ERROR.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
            UNDO, RETURN 'ADM-ERROR'.
        END.
        ASSIGN
            FacCorre.Correlativo = FacCorre.Correlativo + 1.
        RELEASE FacCorre.
        ASSIGN
            DI-RutaC.Observ = ORDENES.CodDiv + " ".
        FIND gn-divi WHERE gn-divi.codcia = s-codcia AND
            gn-divi.coddiv = ORDENES.CodDiv NO-LOCK NO-ERROR.
        IF AVAILABLE gn-divi THEN DI-RutaC.Observ = DI-RutaC.Observ + GN-DIVI.DesDiv + " ".
        DI-RutaC.Observ = DI-RutaC.Observ + (IF ORDENES.Cliente_Recoge = YES 
                                             THEN "CLIENTE RECOGE"
                                             ELSE "CLIENTE NO RECOGE").
    END.
    ELSE DO:
        FIND CURRENT DI-RutaC EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            {lib/mensaje-de-error.i &CuentaError="pCuenta" &MensajeError="pMensaje"}
            UNDO, RETURN 'ADM-ERROR'.
        END.
    END.
    CREATE DI-RutaD.
    ASSIGN
        DI-RutaD.CodCia = DI-RutaC.CodCia 
        DI-RutaD.CodDiv = DI-RutaC.CodDiv 
        DI-RutaD.CodDoc = DI-RutaC.CodDoc 
        DI-RutaD.NroDoc = DI-RutaC.NroDoc
        DI-RutaD.CodRef = ORDENES.CodDoc
        DI-RutaD.NroRef = ORDENES.NroPed.
    /* Generamos la HPK */
    DEFINE VAR hLibLogis AS HANDLE NO-UNDO.
    RUN logis/p-genera-hpk-library.p PERSISTENT SET hLibLogis.
    RUN HPK_Genera-HPK-Master IN hLibLogis (INPUT ROWID(DI-RutaC),
                                            INPUT ROWID(ORDENES),
                                            OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    DELETE PROCEDURE hLibLogis.

END.
IF AVAILABLE DI-RutaC THEN RELEASE DI-RutaC.
IF AVAILABLE DI-RutaD THEN RELEASE DI-RutaD.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_tolerancia-dias-vctos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_tolerancia-dias-vctos Procedure 
PROCEDURE VTA_tolerancia-dias-vctos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pTipoDoc AS CHAR.
DEFINE INPUT PARAMETER pDiviVta AS CHAR.
DEFINE INPUT PARAMETER pCodCli AS CHAR.
DEFINE INPUT PARAMETER pUbiCli AS CHAR.                 /* Vacio indica que hay que buscar la ubicacion del Cliente */
DEFINE OUTPUT PARAMETER pDiasTolerables AS INT INIT 0.

DEFINE VAR x-ubiclie AS CHAR.

x-ubiclie = pUbiCli.

IF pUbiCli = "" THEN DO:
    /* Se debe buscar si el cliente es de LIMA o PROVINCIA */

   /* Cliente de LIMA o PROVINCIA */
   RUN VTA_ubicacion-cliente(INPUT pCodcli, OUTPUT pUbiCli).

END.
FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND
                            x-vtatabla.llave_c1 = pTipoDoc AND
                            x-vtatabla.llave_c2 = pDiviVta AND
                            x-vtatabla.llave_c3 = x-ubiclie AND
                            x-vtatabla.libre_c01 = "ACTIVO" NO-LOCK NO-ERROR.
IF AVAILABLE x-vtatabla THEN DO:
    pDiasTolerables = x-vtatabla.rango_valor[1].
    RETURN.
END.
FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND
                            x-vtatabla.llave_c1 = pTipoDoc AND
                            x-vtatabla.llave_c2 = pDiviVta AND
                            x-vtatabla.llave_c3 = "*" AND           /* Cualquier ubicacion del cliente */
                            x-vtatabla.libre_c01 = "ACTIVO" NO-LOCK NO-ERROR.
IF AVAILABLE x-vtatabla THEN DO:
    pDiasTolerables = x-vtatabla.rango_valor[1].
    RETURN.
END.
FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND
                            x-vtatabla.llave_c1 = pTipoDoc AND
                            x-vtatabla.llave_c2 = "*" AND           /* Cualquier DIVISION */
                            x-vtatabla.llave_c3 = x-ubiclie AND
                            x-vtatabla.libre_c01 = "ACTIVO" NO-LOCK NO-ERROR.
IF AVAILABLE x-vtatabla THEN DO:
    pDiasTolerables = x-vtatabla.rango_valor[1].
    RETURN.
END.
FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND
                            x-vtatabla.llave_c1 = pTipoDoc AND
                            x-vtatabla.llave_c2 = "*" AND           /* Cualquier DIVISION */
                            x-vtatabla.llave_c3 = "*" AND           /* Cualquier UBICACION del cliente */
                            x-vtatabla.libre_c01 = "ACTIVO" NO-LOCK NO-ERROR.
IF AVAILABLE x-vtatabla THEN DO:
    pDiasTolerables = x-vtatabla.rango_valor[1].
    RETURN.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_ubicacion-cliente) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_ubicacion-cliente Procedure 
PROCEDURE VTA_ubicacion-cliente :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCodCli AS CHAR.
DEFINE OUTPUT PARAMETER pUbiClie AS CHAR.

pUbiClie = ''.
/* Cliente es de LIMA o PROVINCIA siempre y cuando le hayan registrado direccion fiscal */
FIND FIRST gn-clieD WHERE gn-clieD.codcia = 0 AND
                            gn-clieD.codcli = pCodCli AND 
                            gn-clieD.sede = "@@@" NO-LOCK NO-ERROR.     /* @@@ : Direccion Fiscal */

IF AVAILABLE gn-clieD THEN DO:
    pUbiClie = 'LIMA'.
    IF gn-clieD.codpos = 'P0' THEN pUbiClie = 'PROVINCIA'.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_Valida-Cantidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_Valida-Cantidad Procedure 
PROCEDURE VTA_Valida-Cantidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pCodMat AS CHAR.
  DEF INPUT PARAMETER pUndVta AS CHAR.
  DEF INPUT-OUTPUT PARAMETER pCanPed AS DEC.
  DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

  DEFINE VAR x-es-cliente-vip AS LOG.
  DEFINE VAR x-tabla AS CHAR.

  x-tabla = 'CLIENTE_VIP_CONTI'.

  x-es-cliente-vip = NO.

  /* Ic - 31Ene2020, cliente VIP no validar minimo de venta */
  FIND FIRST vtatabla WHERE vtatabla.codcia = s-codcia AND
                              vtatabla.tabla = x-tabla AND
                              vtatabla.llave_c1 = x-codcli-vip NO-LOCK NO-ERROR.
  IF AVAILABLE vtatabla THEN DO:
       x-es-cliente-vip = YES.
  END.

  /* ************************************************************************** */
  /* Verificaci�n de la Unidad de Venta */
  /* ************************************************************************** */
  IF TRUE <> (pUndVta > "") THEN DO:
       pMensaje = "La unidad est� en blanco".
       RETURN "ADM-ERROR".
  END.
  FIND FIRST Unidades WHERE Unidades.Codunid = pUndVta NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Unidades THEN DO:
      pMensaje = 'Unidad NO v�lida o inactiva'.
      RETURN "ADM-ERROR".
  END.
/*   IF NOT AVAILABLE Unidades OR Unidades.Libre_l01 = YES THEN DO: */
/*       pMensaje = 'Unidad NO v�lida o inactiva'.                  */
/*       RETURN "ADM-ERROR".                                        */
/*   END.                                                           */
  FIND FIRST Almmmatg WHERE Almmmatg.codcia = s-codcia AND
      Almmmatg.codmat = pCodMat NO-LOCK.
  FIND FIRST Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
      AND Almtconv.Codalter = pUndVta NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Almtconv THEN DO:
      pMensaje = 'NO est� configurado el factor de equivalencia para el producto ' + 
          Almmmatg.codmat + CHR(10) +
          '    Unidad base: ' + Almmmatg.UndBas + CHR(10) +
          'Unidad de venta: ' + pUndVta.
      RETURN "ADM-ERROR".
  END.
  /* ************************************************************************** */
  /* Verificaci�n de la cantidad vendida en base a la configuraci�n de unidades */
  /* ************************************************************************** */
  DEF VAR x-CanPed AS DEC NO-UNDO.
  DEF VAR i-CanPed AS INT NO-UNDO.

  /* IMP. BOLSA PLASTICA - no validar Stock */
  IF x-articulo-ICBPER <> pCodMat THEN DO:
      x-CanPed = DECIMAL(pCanPed).
      i-CanPed = INTEGER(pCanPed).
      IF Unidades.Libre_L02 = YES THEN DO:
          /* Unidad indivisible */
          IF x-CanPed <> i-CanPed THEN DO:
              pMensaje = 'Solo se puede vender en valores enteros'.
              pCanPed = i-CanPed.
              RETURN 'ADM-ERROR'.
          END.
      END.
      ELSE DO:
          /* Por m�ltiplos entonces */
          IF Almtconv.Multiplos <> 0 THEN DO:
              /* Cliente VIP */
              IF x-es-cliente-vip = NO THEN DO:
                  IF (TRUNCATE(x-CanPed / Almtconv.Multiplos, 0) * Almtconv.Multiplos) <> x-CanPed THEN DO:
                      pMensaje = 'Solo se puede vender en m�ltiplos de ' + STRING(Almtconv.Multiplos).
                      pCanPed = TRUNCATE(x-CanPed / Almtconv.Multiplos, 0) * Almtconv.Multiplos.
                      RETURN 'ADM-ERROR'.
                  END.
              END.
          END.
      END.

  END.

  RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_Valida-Empaque) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_Valida-Empaque Procedure 
PROCEDURE VTA_Valida-Empaque :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodCli AS CHAR.
DEF INPUT PARAMETER pCodMat AS CHAR.                                      
DEF INPUT-OUTPUT PARAMETER pCanPed AS DEC.
DEF INPUT PARAMETER pFactor AS DEC.
DEF INPUT PARAMETER pTpoPed AS CHAR.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

/* ************************************************************************** */
/* Control de Empaque */
/* ************************************************************************** */
DEF VAR pSugerido AS DEC NO-UNDO.
DEF VAR pEmpaque  AS DEC NO-UNDO.
DEF VAR f-CanPed  AS DEC NO-UNDO.
DEF VAR x-CanPed  AS DEC NO-UNDO.

x-CanPed = pCanPed.     /* Valor original */

FIND FIRST Almmmatg WHERE Almmmatg.codcia = s-codcia AND Almmmatg.codmat = pCodMat NO-LOCK.
f-CanPed = pCanPed * pFactor.   /* En unidades de stock */
RUN vtagn/p-cantidad-sugerida.p (INPUT pTpoPed, 
                                 INPUT pCodMat, 
                                 INPUT f-CanPed, 
                                 OUTPUT pSugerido,  /* Redondeado al EMPAQUE o a f-CanPed */
                                 OUTPUT pEmpaque).
pMensaje = ''.
CASE TRUE:
    WHEN pTpoPed = "S" THEN DO:     /* CANAL MODERNO (SUPERMERCADOS) */
        /* EMPAQUE SUPERMERCADOS */
        FIND FIRST supmmatg WHERE supmmatg.codcia = s-CodCia
            AND supmmatg.codcli = pCodCli
            AND supmmatg.codmat = pCodMat
            NO-LOCK NO-ERROR.
        IF AVAILABLE supmmatg AND supmmatg.Libre_d01 <> 0 THEN DO:
            f-CanPed = (TRUNCATE((f-CanPed / supmmatg.Libre_d01),0) * supmmatg.Libre_d01).
        END.
        /* Redondeamos al empaque */
        pCanPed = ( f-CanPed - ( f-CanPed MODULO pFactor ) ) / pFactor.
    END.
    WHEN s-FlgEmpaque = YES THEN DO:
        /* En caso se determine el EMPAQUE */
        IF pEmpaque > 0 THEN DO:
            /* Devolvemos el sugerido */
            pCanPed = pSugerido / pFactor.
            /* Solo para un control de errores (si se desea) */
            f-CanPed = TRUNCATE((f-CanPed / pEmpaque),0) * pEmpaque.
            IF f-CanPed <> (x-CanPed * pFactor) THEN DO:
                pMensaje = 'Solo puede despachar en empaques de ' + STRING(pEmpaque) + ' ' + Almmmatg.UndBas.
                RETURN 'ADM-ERROR'.
            END.
        END.
    END.
END CASE.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_Valida-Margen-Utilidad-Total) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_Valida-Margen-Utilidad-Total Procedure 
PROCEDURE VTA_Valida-Margen-Utilidad-Total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Validamos producto por producto
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pRowid AS ROWID.
DEF OUTPUT PARAMETER pError AS CHAR NO-UNDO.

pError = "".

/* 19Ene2021 - Provisional *** OJO ***************????????????????????????? */
/*RETURN "OK".*/
/* 19Ene2021 - Provisional *** OJO ***************????????????????????????? */

FIND B-CPEDI WHERE ROWID(B-CPEDI) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CPEDI THEN RETURN 'OK'.

/* ********************************* MARGEN DE UTILIDAD ******************************* */
/* CONTRATO MARCO, REMATES, EXPOLIBRERIA, LISTA EXPRESS: NO TIENE MINIMO NI MARGEN DE UTILIDAD */
IF LOOKUP(B-CPEDI.TpoPed, "M,R") > 0 THEN RETURN "OK".   
IF B-CPEDI.Libre_C04 = "SI" THEN RETURN "OK".       /* (s-TpoMarco) */
IF LOOKUP(B-CPEDI.FMAPGO, "899,900") > 0 THEN RETURN "OK".
/* ******************************************************************************* */

DEF VAR pMargen AS DEC NO-UNDO.
DEF VAR pLimite AS DEC NO-UNDO.

FOR EACH Facdpedi OF B-CPEDI NO-LOCK WHERE Facdpedi.Libre_c05 <> "OF",
    FIRST Almmmatg OF Facdpedi NO-LOCK WHERE Almmmatg.CatConta[1] <> "SV": 
    
    RUN vtagn/p-margen-utilidad-v11.r (INPUT B-CPEDI.Lista_de_Precios,
                                     INPUT Facdpedi.codmat,
                                     INPUT (Facdpedi.ImpLin / Facdpedi.CanPed),
                                     INPUT Facdpedi.UndVta,
                                     INPUT B-CPEDI.CodMon,
                                     INPUT B-CPEDI.TpoCmb,
                                     INPUT NO,
                                     INPUT Facdpedi.AlmDes,
                                     OUTPUT pMargen,
                                     OUTPUT pLimite,
                                     OUTPUT pError).
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN 'ADM-ERROR'.
END.
RETURN 'OK'.

END PROCEDURE.

/*
DEF INPUT PARAMETER pRowid AS ROWID.
DEF OUTPUT PARAMETER pError AS CHAR NO-UNDO.

pError = "".
FIND B-CPEDI WHERE ROWID(B-CPEDI) = pRowid NO-LOCK NO-ERROR.
IF NOT AVAILABLE B-CPEDI THEN RETURN 'OK'.

IF LOOKUP(B-CPEDI.TpoPed, "M,R,E,LF") > 0 THEN RETURN "OK".   

DEF VAR pMargen AS DEC NO-UNDO.
DEF VAR pLimite AS DEC NO-UNDO.

FOR EACH Facdpedi OF B-CPEDI NO-LOCK WHERE Facdpedi.Libre_c05 <> "OF":
    RUN vtagn/p-margen-utilidad-v2 (INPUT B-CPEDI.Lista_de_Precios,   /* INPUT B-CPEDI.CodDiv,*/
                                    INPUT Facdpedi.codmat,
                                    INPUT (Facdpedi.ImpLin / Facdpedi.CanPed),
                                    INPUT Facdpedi.UndVta,
                                    INPUT B-CPEDI.CodMon,
                                    INPUT B-CPEDI.TpoCmb,
                                    INPUT NO,
                                    INPUT Facdpedi.AlmDes,
                                    OUTPUT pMargen,
                                    OUTPUT pLimite,
                                    OUTPUT pError).
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN 'ADM-ERROR'.
END.
RETURN 'OK'.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_Valida-Minimo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_Valida-Minimo Procedure 
PROCEDURE VTA_Valida-Minimo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.                                      
DEF INPUT-OUTPUT PARAMETER pCanPed AS DEC.
DEF INPUT PARAMETER pFactor AS DEC.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

/* ************************************************************************** */
/* Control de M�nimo de Ventas */
/* ************************************************************************** */
DEF VAR f-CanPed AS DEC NO-UNDO.
DEF VAR f-Minimo AS DEC NO-UNDO.

FIND FIRST Almmmatg WHERE Almmmatg.codcia = s-codcia AND Almmmatg.codmat = pCodMat NO-LOCK.
IF s-FlgMinVenta = YES THEN DO:
    CASE s-CodDiv:
        WHEN "00065" THEN DO:       /* CHICLAYO */
            IF Almmmatg.PesoBruto > 0 THEN DO:
                f-CanPed = pCanPed * pFactor.
                f-Minimo = Almmmatg.PesoBruto.
                IF f-Minimo > 0 THEN DO:
                    IF f-CanPed < f-Minimo THEN DO:
                        pMensaje = "ERROR el el art�culo " + Almmmatg.codmat + CHR(10) +
                            "No se puede despachar menos de " + STRING(f-Minimo) + ' ' + Almmmatg.UndStk.
                        RETURN 'ADM-ERROR'.
                    END.
                    IF f-CanPed > f-Minimo AND Almmmatg.Paquete > 0 THEN DO:
                        IF (f-CanPed - f-Minimo) MODULO Almmmatg.Paquete > 0 THEN DO:
                            pMensaje = "ERROR el el art�culo " + Almmmatg.codmat + CHR(10) +
                                  "No se puede despachar menos de " + STRING(f-Minimo) + ' ' + Almmmatg.UndStk + CHR(10) +
                                   "el incrementos de " + STRING(Almmmatg.Paquete) + ' ' + Almmmatg.UndStk.
                            RETURN 'ADM-ERROR'.
                        END.
                    END.
                END.
                ELSE IF Almmmatg.Paquete > 0 AND f-CanPed <> 1 THEN DO:
                    IF f-CanPed MODULO Almmmatg.Paquete > 0 THEN DO:
                        pMensaje = "ERROR el el art�culo " + Almmmatg.codmat + CHR(10) +
                            "Solo se puede despachar en m�ltiplos de " + STRING(Almmmatg.Paquete) + ' ' + Almmmatg.UndStk.
                        RETURN 'ADM-ERROR'.
                    END.
                END.
            END.
        END.
        OTHERWISE DO:
            f-CanPed = pCanPed * pFactor.
            f-Minimo = Almmmatg.DEC__03.
            IF s-VentaMayorista = 2 THEN DO:  /* LISTA POR DIVISION */
                FIND FIRST VtaListaMay OF Almmmatg WHERE Vtalistamay.coddiv = s-coddiv NO-LOCK NO-ERROR.
                IF AVAILABLE VtaListaMay AND Vtalistamay.CanEmp > 0 THEN DO:
                    IF f-CanPed < Vtalistamay.CanEmp THEN DO:
                        pMensaje = 'Solo puede vender como m�nimo ' + STRING(Vtalistamay.CanEmp) + ' ' + Almmmatg.UndBas.
                        RETURN "ADM-ERROR".
                    END.
                END.
            END.
            ELSE DO:      /* LISTA GENERAL */
                IF f-Minimo > 0 THEN DO:
                    IF f-CanPed < f-Minimo THEN DO:
                        pMensaje = 'Solo puede vender como m�nimo ' + STRING(f-Minimo) + ' ' + Almmmatg.UndBas.
                        RETURN "ADM-ERROR".
                    END.
                END.
            END.
            IF s-TpoPed = "E" THEN DO:    /* Expolibreria */
                IF f-Minimo > 0 THEN DO:
                    f-CanPed = TRUNCATE((f-CanPed / f-Minimo),0) * f-Minimo.
                    IF f-CanPed <> pCanPed * pFactor THEN DO:
                        pMensaje = 'Solo puede vender en m�ltiplos de ' + STRING(f-Minimo) + ' ' + Almmmatg.UndBas.
                        RETURN "ADM-ERROR".
                    END.
                END.
            END.
        END.
    END CASE.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VTA_Valida-Unidad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VTA_Valida-Unidad Procedure 
PROCEDURE VTA_Valida-Unidad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER pCodMat AS CHAR.
  DEF INPUT PARAMETER pUndVta AS CHAR.
  DEF INPUT-OUTPUT PARAMETER pCanPed AS DEC.
  DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

  /* ************************************************************************** */
  /* Verificaci�n de la Unidad de Venta */
  /* ************************************************************************** */
  IF TRUE <> (pUndVta > "") THEN DO:
       pMensaje = "La unidad est� en blanco".
       RETURN "ADM-ERROR".
  END.
  FIND Unidades WHERE Unidades.Codunid = pUndVta NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Unidades OR Unidades.Libre_l01 = YES THEN DO:
      pMensaje = 'Unidad NO v�lida o inactiva'.
      RETURN "ADM-ERROR".
  END.
  FIND FIRST Almmmatg WHERE Almmmatg.codcia = s-codcia NO-LOCK.
  FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
      AND Almtconv.Codalter = pUndVta
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Almtconv THEN DO:
      pMensaje = 'NO est� configurado el factor de equivalencia para el producto ' + 
          Almmmatg.codmat + CHR(10) +
          '    Unidad base: ' + Almmmatg.UndBas + CHR(10) +
          'Unidad de venta: ' + pUndVta.
      RETURN "ADM-ERROR".
  END.
  /* ************************************************************************** */
  /* Verificaci�n de la cantidad vendida en base a la configuraci�n de unidades */
  /* ************************************************************************** */
/*   DEF VAR x-CanPed AS DEC NO-UNDO.                                                                  */
/*   DEF VAR i-CanPed AS INT NO-UNDO.                                                                  */
/*                                                                                                     */
/*   x-CanPed = DECIMAL(pCanPed).                                                                      */
/*   i-CanPed = INTEGER(pCanPed).                                                                      */
/*   IF Unidades.Libre_L02 = YES THEN DO:                                                              */
/*       /* Unidad indivisible */                                                                      */
/*       IF x-CanPed <> i-CanPed THEN DO:                                                              */
/*           pMensaje = 'Solo se puede vender en valores enteros'.                                     */
/*           pCanPed = i-CanPed.                                                                       */
/*           RETURN 'ADM-ERROR'.                                                                       */
/*       END.                                                                                          */
/*   END.                                                                                              */
/*   ELSE DO:                                                                                          */
/*       /* Por m�ltiplos entonces */                                                                  */
/*       IF Almtconv.Multiplos <> 0 THEN DO:                                                           */
/*           IF (TRUNCATE(x-CanPed / Almtconv.Multiplos, 0) * Almtconv.Multiplos) <> x-CanPed THEN DO: */
/*               pMensaje = 'Solo se puede vender en m�ltiplos de ' + STRING(Almtconv.Multiplos).      */
/*               pCanPed = TRUNCATE(x-CanPed / Almtconv.Multiplos, 0) * Almtconv.Multiplos.            */
/*               RETURN 'ADM-ERROR'.                                                                   */
/*           END.                                                                                      */
/*       END.                                                                                          */
/*   END.                                                                                              */
  RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

