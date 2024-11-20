	
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

    DEFINE VAR xOldCodDoc AS CHAR.
    DEFINE VAR xOldNroDoc AS CHAR.
    DEFINE VAR xNewCodDoc AS CHAR.
    DEFINE VAR xNewNroDoc AS CHAR.
    DEFINE VAR xRetVal AS CHAR.
    DEFINE VAR xCmpte AS CHAR.
    DEFINE VAR xFiler AS CHAR.
    DEFINE VAR xNuevoIC AS CHAR.
    DEFINE VAR xNuevoCJE AS CHAR.

    DEFINE VAR xCadaCuanto AS INT.
    DEFINE VAR xConteo AS INT.
    DEFINE VAR xConteo2 AS INT.

    DEFINE VAR rowOrigen AS ROWID.
    DEFINE VAR rowDestino AS ROWID.


    DEFINE BUFFER b-ccbcdocu FOR ccbcdocu.
    
    xCadaCuanto = 15.

    cColumn = STRING(lLinea).
    REPEAT iColumn = 4 TO 65000 :
        cColumn = STRING(iColumn).

        chWorkbook:Worksheets(1):Activate.
        chWorksheet = chWorkbook:Worksheets(1).

        cRange = "D" + cColumn.
        xCaso = chWorkSheet:Range(cRange):TEXT.

        IF xCaso = "" OR xCaso = ? THEN LEAVE.    /* FIN DE DATOS */      

        cRange = "AF" + cColumn.
        xFiler = chWorkSheet:Range(cRange):TEXT.

        IF xFiler <> "ACEPTADO POR SUNAT" THEN NEXT.

        xCaso = REPLACE(xCaso,"-","").
        xCaso = REPLACE(xCaso,"F","").
        xCaso = REPLACE(xCaso," ","").

        xOldCodDoc = "FAC".
        xOldNroDoc = xCaso.

        cRange = "AD" + cColumn.        
        xNewCodDoc = "FAC".
        xNewNroDoc = chWorkSheet:Range(cRange):TEXT.

        IF xNewNroDoc = "" OR xNewNroDoc = ? THEN NEXT.

        FIND FIRST ccbcdocu WHERE ccbcdocu.codcia = 1 AND
                                    ccbcdocu.coddoc = 'FAC' AND
                                    ccbcdocu.nrodoc = xOldNroDoc NO-LOCK NO-ERROR.
        IF AVAILABLE ccbcdocu THEN DO:
            xConteo2 = 0.
            rowOrigen = ROWID(ccbcdocu).            

            FIND FIRST b-ccbcdocu WHERE b-ccbcdocu.codcia = 1 AND 
                                    b-ccbcdocu.coddoc = xNewCodDoc AND
                                    b-ccbcdocu.nrodoc = xNewNroDoc NO-LOCK NO-ERROR.
            IF AVAILABLE b-ccbcdocu THEN DO:
                rowDestino = ROWID(b-ccbcdocu).
                cRange = "AG" + cColumn.
                xFiler = chWorkSheet:Range(cRange):TEXT.
                IF TRUE <> (xFiler > "") THEN DO:
                    /* Kardex */
                    RUN D:\newsie\on_in_co\Util\DocRechazadoSunat\ADD_kardex_clon.r (INPUT rowDestino, OUTPUT xRetVal).
                    IF TRUE <> (xRetVal > "") THEN DO:
                        cRange = "AG" + cColumn.
                        chWorkSheet:Range(cRange):VALUE = "OK".
                    END.
                    ELSE DO:
                        cRange = "AG" + cColumn.
                        chWorkSheet:Range(cRange):VALUE = xRetval.
                    END.
                    xConteo2 = xConteo2 + 1.
                END.

                /* Liquidaciones I/C */
                xRetVal = "".
                xNuevoIC = "".
                cRange = "AH" + cColumn.
                xFiler = chWorkSheet:Range(cRange):TEXT.

                IF NOT xFiler BEGINS "OK" THEN DO:
                    RUN D:\newsie\on_in_co\Util\DocRechazadoSunat\clonar_IC.r (INPUT rowOrigen, INPUT rowDestino, OUTPUT xRetVal, OUTPUT xNuevoIC).
                    IF TRUE <> (xRetVal > "") THEN DO:
                        cRange = "AH" + cColumn.
                        chWorkSheet:Range(cRange):VALUE = "OK :" + xNuevoIC.
                    END.
                    ELSE DO:
                        cRange = "AH" + cColumn.
                        chWorkSheet:Range(cRange):VALUE = "ERROR I/C :" + xRetval.
                    END.
                    xConteo2 = xConteo2 + 1.
                END.

                /* Canje CJE */
                xRetVal = "".
                xNuevoCJE = "".
                cRange = "AI" + cColumn.
                xFiler = chWorkSheet:Range(cRange):TEXT.

                IF NOT xFiler BEGINS "OK" THEN DO:
                    RUN D:\newsie\on_in_co\Util\DocRechazadoSunat\clonar_cje_let.r (INPUT rowOrigen, INPUT rowDestino, OUTPUT xRetVal,OUTPUT xNuevoCJE).
                    IF TRUE <> (xRetVal > "") THEN DO:
                        cRange = "AI" + cColumn.
                        chWorkSheet:Range(cRange):VALUE = "OK :" + xNuevoCJE.
                    END.
                    ELSE DO:
                        cRange = "AI" + cColumn.
                        chWorkSheet:Range(cRange):VALUE = "ERROR CANJE :" + xRetval.
                    END.
                    xConteo2 = xConteo2 + 1.
                END.

                /* Anulaciones */
                xRetVal = "".
                cRange = "AJ" + cColumn.
                xFiler = chWorkSheet:Range(cRange):TEXT.

                IF NOT xFiler BEGINS "OK" THEN DO:
                    IF TRUE <> (xRetVal > "") THEN DO:
                        RUN D:\newsie\on_in_co\Util\DocRechazadoSunat\anular_comprobante.r (INPUT rowOrigen, OUTPUT xRetVal).
                        IF TRUE <> (xRetVal > "") THEN DO:
                            cRange = "AJ" + cColumn.
                            chWorkSheet:Range(cRange):VALUE = "OK :".
                        END.
                        ELSE DO:
                            cRange = "AJ" + cColumn.
                            chWorkSheet:Range(cRange):VALUE = "ERROR ANULACION :" + xRetval.
                        END.
                        xConteo2 = xConteo2 + 1.
                    END.
                END.
                IF xConteo2 > 0 THEN xConteo = xConteo + 1.
            END.
        END.

        IF xConteo >= xCadaCuanto THEN LEAVE.
    END.


	{lib\excel-close-file.i}
