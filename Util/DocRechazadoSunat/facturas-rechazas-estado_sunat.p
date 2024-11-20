	
    DEFINE VARIABLE lFileXls                 AS CHARACTER.
	DEFINE VARIABLE lNuevoFile               AS LOG.

    DEFINE VAR xCaso AS CHAR.
    DEFINE VAR xFamilia AS CHAR.
    DEFINE VAR xSubFamilia AS CHAR.
    DEFINE VAR lLinea AS INT.
    DEFINE VAR dValor AS DEC.
    DEFINE VAR lTpoCmb AS DEC.

	lFileXls = "D:\241\FacturasRechazadas\FACTURAS RECHAZADAS - DETALLE v3.xlsx".		/* Nombre el archivo a abrir o crear, vacio solo para nuevos */    
    /*
    lFileXls = "D:\241\FacturasRechazadas\FACTURAS RECHAZADAS 20.07 AL 22.07 VOL2.xls".		/* Nombre el archivo a abrir o crear, vacio solo para nuevos */
    
    lFileXls = "D:\241\FacturasRechazadas\FACTURAS RECHAZADAS VOL3.xls".		/* Nombre el archivo a abrir o crear, vacio solo para nuevos */
    */
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

    DEFINE VAR x-estado-bizlinks AS CHAR.
    DEFINE VAR x-estado-sunat AS CHAR.
    DEFINE VAR x-estado-doc AS CHAR.
    DEFINE VAR x-motivo-rechazo AS CHAR.

    xCadaCuanto = 15.

    cColumn = STRING(lLinea).
    REPEAT iColumn = 4 TO 65000 :
        cColumn = STRING(iColumn).

        chWorkbook:Worksheets(1):Activate.
        chWorksheet = chWorkbook:Worksheets(1).

        cRange = "D" + cColumn.
        xCaso = chWorkSheet:Range(cRange):TEXT.

        IF xCaso = "" OR xCaso = ? THEN LEAVE.    /* FIN DE DATOS */        

        cRange = "AD" + cColumn.        
        xNewCodDoc = "FAC".
        xNewNroDoc = chWorkSheet:Range(cRange):TEXT.

        IF xNewNroDoc = "" OR xNewNroDoc = ? THEN NEXT.

        x-estado-bizlinks = "".
        x-estado-sunat = "".
        x-estado-doc = "".
        x-motivo-rechazo = "".

        RUN gn/p-estado-documento-electronico-v2.r(INPUT xNewCodDoc,
                            INPUT xNewNroDoc,
                            INPUT "",
                            OUTPUT x-estado-bizlinks,
                            OUTPUT x-estado-sunat,
                            OUTPUT x-estado-doc,
                            INPUT-OUTPUT x-motivo-rechazo).      

        x-estado-sunat = replace(x-estado-sunat,"|","").

        IF x-estado-sunat = "AC_03" OR x-estado-sunat = "RC_05" THEN DO:
            cRange = "AF" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "ACEPTADO POR SUNAT".
            IF x-estado-sunat = "RC_05" THEN DO:
                chWorkSheet:Range(cRange):VALUE = "RECHAZADO POR SUNAT".
            END.
        END.

        /*IF xConteo >= xCadaCuanto THEN LEAVE.*/
    END.


	{lib\excel-close-file.i}
