

/* GENERA UN MOVIMIENTO DE SALIDA O INGRESO POR TRANSFERENCIA
    ENTRE EL ALMACEN 11T 
    
    Valor (-) : Es un faltante en el almacen inventariado.
    Valor (+) : Es un sobrante en el almacen inventariado.
*/
    
DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almmmate.
DISABLE TRIGGERS FOR LOAD OF almmmatg.
DISABLE TRIGGERS FOR LOAD OF faccorre.
DISABLE TRIGGERS FOR LOAD OF almacen.

/*  10  : Utilex Surquillo 
    27  : Utilex Chorrillos
    503 : Utilex La Molina 
    502 : Utilex San Borja 
    501 : Utilex Plaza Norte
    28  : San Juan
*/
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-CodAlm AS CHAR INIT "90" NO-UNDO.     /* ALMACEN INVENTARIADO ???????????????? */
DEF VAR s-CodMov AS INT INIT 03 NO-UNDO.        /* MOVIMIENTO DE TRANSFERENCIA */
DEF VAR s-User-Id AS CHAR INIT "SISTEMAS" NO-UNDO.
DEF VAR x-Archivo AS CHAR NO-UNDO.      /* ARCHIVO DE INVENTARIOS */
DEF VAR x-Llave AS CHAR NO-UNDO.
DEF VAR x-CodMat AS CHAR NO-UNDO.
DEF VAR x-CanDes AS DEC NO-UNDO.
DEF VAR x-FchDoc AS DATE NO-UNDO.       /* FECHA DE INVENTARIO */
DEF VAR x-NroDoc AS INT NO-UNDO.
DEF VAR x-Item AS INT INIT 1 NO-UNDO.
DEF VAR r-Rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
	DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
	DEFINE VARIABLE chWorksheet             AS COM-HANDLE.

	DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.

	DEFINE VARIABLE iCount                  AS INTEGER init 1.
	DEFINE VARIABLE iIndex                  AS INTEGER.
	DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 1.
	DEFINE VARIABLE cColumn                 AS CHARACTER.
	DEFINE VARIABLE cRange                  AS CHARACTER.
	DEFINE VARIABLE x-signo                 AS DECI.

    DEFINE VARIABLE lFileXls                 AS CHARACTER.
    DEFINE VARIABLE lValidaOk  AS LOGICAL .

    s-CodAlm = "38".
    x-FchDoc  = 05/11/2017. /* Fecha (mm/dd/aaaa) del Ajuste  ????????????  TODAY  */
    ASSIGN
        lFileXls = "d:\tmp\Ajuste almac�n Belaunde 38.xlsx".

	/* create a new Excel Application object */
	CREATE "Excel.Application" chExcelApplication.

	/* launch Excel so it is visible to the user */
	chExcelApplication:Visible = TRUE.

    /* OPEN XLS */
	chWorkbook = chExcelApplication:Workbooks:OPEN(lFIleXls).

	/* get the active Worksheet */
	chWorkSheet = chExcelApplication:Sheets:Item(1).

    DEF TEMP-TABLE t-almdmov LIKE almdmov.    

	/* Iterate through the salesrep table and populate
	   the Worksheet appropriately */
	iColumn = 2.
    lValidaOk = YES.

    /* Verifica si los Articulos existen */
    REPEAT iColumn = 2 TO 65000 :
        /*iColumn = iColumn + 1.*/
        cColumn = STRING(iColumn).
    
        cRange = "A" + cColumn.
        x-CodMat = chWorkSheet:Range(cRange):Value.
    
        IF x-CodMat = "" OR x-CodMat = ? THEN LEAVE.
    
        cRange = "D" + cColumn.
        x-CanDes = chWorkSheet:Range(cRange):Value.
        IF x-CanDes <> 0  THEN DO:       

            DISPLAY x-CodMat.
            PAUSE 0.

            FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
                AND Almmmatg.codmat = x-codmat
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almmmatg THEN DO:
                lValidaOk = NO.
                MESSAGE 'Cdigo no registrado' x-codmat.
                UNDO, LEAVE.
            END.
        END.
    END.

    IF lValidaOk = NO THEN RETURN.

    iColumn = 2.

    REPEAT iColumn = 2 TO 65000 :
        /*iColumn = iColumn + 1.*/
	    cColumn = STRING(iColumn).

	    cRange = "A" + cColumn.
        x-CodMat = chWorkSheet:Range(cRange):Value.

        IF x-CodMat = "" OR x-CodMat = ? THEN LEAVE.

        cRange = "D" + cColumn.
        x-CanDes = chWorkSheet:Range(cRange):Value.
        IF x-CanDes <> 0  THEN DO:       
            FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
                AND Almmmatg.codmat = x-codmat
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almmmatg THEN DO:
                MESSAGE 'Cdigo no registrado' x-codmat.
                UNDO, LEAVE.
            END.
            
            CREATE t-almdmov.
            ASSIGN
                t-Almdmov.CodCia = s-codcia
                t-Almdmov.TipMov = "S"
                t-Almdmov.CodMov = s-codmov
                t-Almdmov.CodAlm = (IF x-CanDes > 0 THEN "11T" ELSE s-CodAlm)
                t-Almdmov.AlmOri = (IF x-CanDes > 0 THEN s-CodAlm ELSE "11T")
                t-Almdmov.FchDoc = x-FchDoc
                t-Almdmov.CanDes = ABSOLUTE(x-CanDes)
                t-Almdmov.codmat = x-CodMat
                t-Almdmov.CodMon = 1
                t-Almdmov.CodUnd = Almmmatg.UndStk
                t-Almdmov.Factor = 1.
        END.         
    END.


/* GENERAMOS EL MOVIMIENTO DE SALIDA POR TRANSFERENCIA */
FOR EACH t-Almdmov NO-LOCK BREAK 
    BY t-Almdmov.codalm
    BY t-Almdmov.tipmov 
    BY t-Almdmov.codmat:
    DISPLAY t-Almdmov.codmat.
    PAUSE 0.
    IF FIRST-OF(t-Almdmov.CodAlm) OR FIRST-OF(t-Almdmov.TipMov)
        THEN DO:
        DISPLAY t-almdmov.codalm t-almdmov.tipmov.
        PAUSE 0. 
        /* BLOQUEAMOS CORRELATIVO */
        /* Buscamos el correlativo de almacenes */
        FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
            AND Almacen.CodAlm = t-Almdmov.codalm 
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Almacen THEN DO: 
            MESSAGE 'NO se pudo bloquer el correlativo por almacen' VIEW-AS ALERT-BOX ERROR.
            UNDO, LEAVE.
        END.
        ASSIGN 
            x-Nrodoc  = Almacen.CorrSal
            Almacen.CorrSal = Almacen.CorrSal + 1.
        CREATE Almcmov.
        ASSIGN 
            Almcmov.CodCia = s-CodCia 
            Almcmov.CodAlm = t-Almdmov.CodAlm 
            Almcmov.AlmDes = t-Almdmov.AlmOri
            Almcmov.TipMov = t-Almdmov.TipMov
            Almcmov.CodMov = s-CodMov
            Almcmov.NroSer = 000    /* CORRELATIVO INTERNO */
            Almcmov.NroDoc = x-NroDoc
            Almcmov.FlgSit = "T"      /* Transferido pero no Recepcionado */
            Almcmov.FchDoc = t-Almdmov.FchDoc
            Almcmov.HorSal = STRING(TIME,"HH:MM")
            Almcmov.HraDoc = STRING(TIME,"HH:MM")
            Almcmov.CodMon = 1
            Almcmov.usuario = S-USER-ID.

        /* RHC 24.03.2011 Generamos un movimiento de ingreso */
        /*RUN alm/ing-trf-vir (ROWID(Almcmov), '999').*/
        /* ************************************************* */

        x-Item = 1.
    END.
    CREATE almdmov.
    ASSIGN 
        Almdmov.CodCia = s-CodCia 
        Almdmov.CodAlm = t-Almdmov.CodAlm 
        Almdmov.AlmOri = t-Almdmov.AlmOri
        Almdmov.TipMov = t-Almdmov.TipMov 
        Almdmov.CodMov = s-CodMov 
        Almdmov.NroSer = 000
        Almdmov.NroDoc = x-NroDoc 
        Almdmov.CodMon = 1
        Almdmov.FchDoc = t-Almdmov.FchDoc 
        Almdmov.HraDoc = STRING(TIME,"HH:MM")
        Almdmov.codmat = t-Almdmov.codmat
        Almdmov.CanDes = t-Almdmov.CanDes
        Almdmov.CodUnd = t-Almdmov.CodUnd
        Almdmov.Factor = t-Almdmov.Factor
        Almdmov.ImpCto = t-Almdmov.ImpCto
        Almdmov.PreUni = t-Almdmov.PreUni
        Almdmov.CodAjt = ''
        Almdmov.HraDoc = STRING(TIME,"HH:MM")
        Almdmov.NroItm = x-Item
        R-ROWID = ROWID(Almdmov).
    x-Item = x-Item + 1.
    RUN alm/almdcstk (R-ROWID).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN.
    RUN alm/almacpr1 (R-ROWID, "U").
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN.
END.


/* DESBLOQUEAMOS LOS CORRELATIVOS */
IF AVAILABLE(Faccorre) THEN RELEASE Faccorre.
IF AVAILABLE(Almacen) THEN RELEASE Almacen.
IF AVAILABLE(Almdmov) THEN RELEASE Almdmov.
IF AVAILABLE(Almmmate) THEN RELEASE Almmmate.

	chExcelApplication:DisplayAlerts = False.
	chExcelApplication:Quit().


	/* release com-handles */
	RELEASE OBJECT chExcelApplication NO-ERROR.      
	RELEASE OBJECT chWorkbook NO-ERROR.
	RELEASE OBJECT chWorksheet NO-ERROR.
	RELEASE OBJECT chWorksheetRange NO-ERROR. 

	MESSAGE 'Proceso Terminado' VIEW-AS ALERT-BOX INFORMATION.

