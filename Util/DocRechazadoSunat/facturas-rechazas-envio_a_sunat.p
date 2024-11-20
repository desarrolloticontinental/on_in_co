	
    DEFINE VARIABLE lFileXls                 AS CHARACTER.
	DEFINE VARIABLE lNuevoFile               AS LOG.

    DEFINE VAR xCaso AS CHAR.
    DEFINE VAR xFamilia AS CHAR.
    DEFINE VAR xSubFamilia AS CHAR.
    DEFINE VAR lLinea AS INT.
    DEFINE VAR dValor AS DEC.
    DEFINE VAR lTpoCmb AS DEC.

	lFileXls = "D:\241\FacturasRechazadas\FACTURAS RECHAZADAS - DETALLE v3.xlsx".		/* Nombre el archivo a abrir o crear, vacio solo para nuevos */
    lFileXls = "D:\241\FacturasRechazadas\FACTURAS RECHAZADAS 20.07 AL 22.07 VOL2.xls".		/* Nombre el archivo a abrir o crear, vacio solo para nuevos */
    lFileXls = "D:\241\FacturasRechazadas\FACTURAS RECHAZADAS VOL3.xls".		/* Nombre el archivo a abrir o crear, vacio solo para nuevos */

	lNuevoFile = NO.	                    /* Si va crear un nuevo archivo o abrir */

	{lib\excel-open-file.i}

    lMensajeAlTerminar = YES. /*  */
    lCerrarAlTerminar = NO.	/* Si permanece abierto el Excel luego de concluir el proceso */
    
    chWorkbook:Worksheets(1):Activate.
    chWorksheet = chWorkbook:Worksheets(1).

    DEFINE VAR zLinea AS INT INIT 1.
    DEFINE VAR oLinea AS INT INIT 1.

    DEFINE VAR xNewCodDoc AS CHAR.
    DEFINE VAR xNewNroDoc AS CHAR.
    DEFINE VAR xRetVal AS CHAR.
    DEFINE VAR xCmpte AS CHAR.
    DEFINE VAR xFiler AS CHAR.

    DEFINE VAR xCadaCuanto AS INT.
    DEFINE VAR xConteo AS INT.

    xCadaCuanto = 15.

    cColumn = STRING(lLinea).
    REPEAT iColumn = 4 TO 65000 :
        cColumn = STRING(iColumn).

        chWorkbook:Worksheets(1):Activate.
        chWorksheet = chWorkbook:Worksheets(1).

        cRange = "AD" + cColumn.
        xCmpte = TRIM(chWorkSheet:Range(cRange):TEXT).

        IF xCmpte <> "" THEN NEXT.

        cRange = "D" + cColumn.
        xCaso = chWorkSheet:Range(cRange):TEXT.

        IF xCaso = "" OR xCaso = ? THEN LEAVE.    /* FIN DE DATOS */        

        xCaso = REPLACE(xCaso,"-","").
        xCaso = REPLACE(xCaso,"F","").
        xCaso = REPLACE(xCaso," ","").

        /* Validacoiones */
        FIND FIRST ccbcdocu WHERE ccbcdocu.codcia = 1 AND
                                    ccbcdocu.coddoc = 'FAC' AND
                                    ccbcdocu.nrodoc = xCaso NO-LOCK NO-ERROR.
        IF AVAILABLE ccbcdocu THEN DO:

            xFiler = ccbcdocu.codcob.
            IF TRUE <> (xFiler > "") THEN DO:            
    
                RUN D:\newsie\on_in_co\Util\DocRechazadoSunat\clonar_comprobante.r (INPUT ROWID(ccbcdocu), OUTPUT xRetVal).
    
                IF TRUE <> (xRetVal > "") THEN DO:
    
                    FIND CURRENT ccbcdocu NO-LOCK NO-ERROR.
                    IF AVAILABLE ccbcdocu THEN DO:
                        xNewCodDoc = ENTRY(1,ccbcdocu.codcob,",").
                        xNewNroDoc = ENTRY(2,ccbcdocu.codcob,",").  /*FAC,25100128559*/
                        
                        cRange = "AD" + cColumn.
                        chWorkSheet:Range(cRange):VALUE = "'" + xNewNroDoc.
                        cRange = "AE" + cColumn.
                        chWorkSheet:Range(cRange):VALUE = "".
    
                        xConteo = xConteo + 1.
                    END.
                END.
                ELSE DO:
                    cRange = "AE" + cColumn.
                    chWorkSheet:Range(cRange):VALUE = "ERROR clonar Cmpte:" + xRetVal.
                END.
            END.
            ELSE DO:
                cRange = "AE" + cColumn.
                chWorkSheet:Range(cRange):VALUE = "ERROR el campo CodCob tiene valor " + xFiler.
            END.
        END.
        ELSE DO:
            cRange = "AE" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "Comprobante no existe".
        END.

        IF xConteo >= xCadaCuanto THEN LEAVE.
    END.


	{lib\excel-close-file.i}
