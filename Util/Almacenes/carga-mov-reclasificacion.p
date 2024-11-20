DEFINE NEW SHARED VARIABLE s-codcia AS INT.
DEFINE NEW SHARED VARIABLE s-codalm AS CHAR.

DEFINE VAR S-FCHDOC AS DATE.
DEF VAR f-Factor-1 AS DEC NO-UNDO.
DEF VAR f-Factor-2 AS DEC NO-UNDO.

DEFINE VAR F-TPOCMB AS DECIMAL NO-UNDO.
DEFINE BUFFER B-CMOV FOR Almcmov.
DEFINE TEMP-TABLE ITEM LIKE Almdmov.
DEFINE TEMP-TABLE DETALLE LIKE Almdmov
    INDEX llave01 AS PRIMARY CodAlm CodMat.

RUN Carga-Excel.

ASSIGN
    s-codcia = 001
    s-codalm = ''
    s-fchdoc = TODAY.

DEFINE NEW SHARED VARIABLE S-TPOMOV AS CHAR INIT 'S'.   /* Salidas */
DEFINE NEW SHARED VARIABLE C-CODMOV AS CHAR INIT '14'.  /* Reclasificación */
DEFINE NEW SHARED VARIABLE S-NROSER  AS INTEGER.
DEFINE NEW SHARED VARIABLE L-NROSER AS CHAR.    /* Nº Serie Válidos */

/* Cargamos DETALLE */
FOR EACH DETALLE BREAK BY DETALLE.CodAlm TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    IF FIRST-OF(DETALLE.CodAlm) THEN DO:
        EMPTY TEMP-TABLE ITEM.
        ASSIGN s-CodAlm = DETALLE.CodAlm.
        /*MESSAGE 'Almacén:' s-codalm VIEW-AS ALERT-BOX.*/
    END.
    CREATE ITEM.
    BUFFER-COPY DETALLE TO ITEM.
    
    /* CONSISTENCIA */
    /* SALIDAS */
    FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
        AND  Almmmatg.CodMat = ITEM.codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE "Producto NO registrado en el catálogo" ITEM.codmat VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN ERROR.
    END.
    FIND Almmmate WHERE Almmmate.CodCia = S-CODCIA 
        AND  Almmmate.CodAlm = S-CODALM 
        AND  Almmmate.CodMat = ITEM.codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmate THEN DO:
       MESSAGE "Producto NO asignado a este Almacén" s-codalm ITEM.codmat
           VIEW-AS ALERT-BOX ERROR.
       UNDO, RETURN ERROR.
    END.
    FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
        AND  Almtconv.Codalter = Almmmatg.UndStk
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
       MESSAGE "Codigo de unidad no existe" ITEM.codmat VIEW-AS ALERT-BOX ERROR.
       UNDO, RETURN ERROR.
    END.

    /* INGRESOS */
    FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA 
        AND  Almmmatg.CodMat = ITEM.codant
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE "Producto NO registrado en el catálogo" ITEM.codant VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN ERROR.
    END.
    FIND Almmmate WHERE Almmmate.CodCia = S-CODCIA 
        AND  Almmmate.CodAlm = S-CODALM 
        AND  Almmmate.CodMat = ITEM.codant
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmate THEN DO:
       MESSAGE "Producto NO asignado a este Almacén" s-codalm item.codant VIEW-AS ALERT-BOX ERROR.
       UNDO, RETURN ERROR.
    END.
    FIND Almtconv WHERE Almtconv.CodUnid = Almmmatg.UndBas 
        AND  Almtconv.Codalter = Almmmatg.UndStk
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
       MESSAGE "Codigo de unidad no existe" item.codant VIEW-AS ALERT-BOX ERROR.
       UNDO, RETURN ERROR.
    END.
    F-Factor-1 = Almtconv.Equival.

    /* VALORIZACION DEL INGRESO */
    FIND LAST Almstkge WHERE Almstkge.codcia = s-codcia
        AND Almstkge.codmat = ITEM.CodAnt
        AND Almstkge.fecha <= s-FchDoc
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almstkge 
    THEN ASSIGN
        ITEM.PreUni = AlmStkge.CtoUni * f-Factor-1
        ITEM.ImpCto = ITEM.CanDes * ITEM.PreUni
        ITEM.CodMon = 1.

    IF LAST-OF(DETALLE.CodAlm) THEN DO:
        RUN Genera-Cabecera NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE 'ERROR en al almacén' s-codalm VIEW-AS ALERT-BOX WARNING.
            UNDO, RETURN ERROR.
        END.
        /*MESSAGE 'Mov. registrado:' s-codalm VIEW-AS ALERT-BOX WARNING.*/
    END.
END.

PROCEDURE Genera-Cabecera:

DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
    /* Code placed here will execute PRIOR to standard behavior. */
    /* CONSISTENCIA DE MOVIMIENTOS */
    FIND Almtdocm WHERE Almtdocm.CodCia = s-codcia
        AND Almtdocm.CodAlm = s-codalm
        AND Almtdocm.TipMov = 'I'
        AND Almtdocm.CodMov = INTEGER (c-codmov)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtdocm THEN DO:
        MESSAGE 'Movimiento de entrada' c-codmov 'NO configurado en el almacén' s-codalm
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.
    FIND Almtdocm WHERE Almtdocm.CodCia = s-codcia
        AND Almtdocm.CodAlm = s-codalm
        AND Almtdocm.TipMov = 'S'
        AND Almtdocm.CodMov = INTEGER (c-codmov)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtdocm THEN DO:
        MESSAGE 'Movimiento de salida' c-codmov 'NO configurado en el almacén' s-codalm
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
    END.

    /* BLOQUEAMOS Almacen hasta 5 intentos */
    {lib/lock-generico.i &Tabla=Almacen &Condicion="Almacen.CodCia = s-CodCia ~
    AND Almacen.CodAlm = s-CodAlm" ~
    &Bloqueo=EXCLUSIVE-LOCK NO-WAIT ~
    &Accion=RETRY ~
    &Mensaje=YES ~
    &TipoError="ERROR"}

      /* Dispatch standard ADM method.                             */
    CREATE Almcmov.
    ASSIGN
        Almcmov.FchDoc = s-fchdoc
        Almcmov.Usuario = "SISTEMAS".

    /* Buscamos un correlativo válido */
    DEF VAR x-CorrSal LIKE Almacen.CorrSal NO-UNDO.
    DEF VAR x-CorrIng LIKE Almacen.CorrIng NO-UNDO.

    SALIDAS:
    REPEAT:
        ASSIGN
            x-CorrSal = Almacen.CorrSal.
        IF NOT CAN-FIND(FIRST Almcmov WHERE Almcmov.codcia = Almtdocm.CodCia 
                        AND Almcmov.codalm = Almtdocm.CodAlm
                        AND Almcmov.tipmov = "S"
                        AND Almcmov.codmov = Almtdocm.CodMov
                        AND Almcmov.nroser = s-NroSer
                        AND Almcmov.nrodoc = x-CorrSal
                        NO-LOCK)
            THEN LEAVE.
        ASSIGN
            Almacen.CorrSal = Almacen.CorrSal + 1.
    END.
    INGRESOS:
    REPEAT:
        ASSIGN
            x-CorrIng = Almacen.CorrIng.
        IF NOT CAN-FIND(FIRST Almcmov WHERE Almcmov.codcia = Almtdocm.CodCia 
                        AND Almcmov.codalm = Almtdocm.CodAlm
                        AND Almcmov.tipmov = "I"
                        AND Almcmov.codmov = Almtdocm.CodMov
                        AND Almcmov.nroser = s-NroSer
                        AND Almcmov.nrodoc = x-CorrIng
                        NO-LOCK)
            THEN LEAVE.
        ASSIGN
            Almacen.CorrIng = Almacen.CorrIng + 1.
    END.
    /* MOVIMIENTO DE SALIDA */
    ASSIGN 
        Almcmov.CodCia = Almtdocm.CodCia 
        Almcmov.CodAlm = Almtdocm.CodAlm 
        Almcmov.TipMov = Almtdocm.TipMov
        Almcmov.CodMov = Almtdocm.CodMov
        Almcmov.NroSer = S-NROSER
        Almcmov.Nrodoc  = Almacen.CorrSal
        Almcmov.HorSal = STRING(TIME,"HH:MM:SS")
        Almcmov.TpoCmb  = F-TPOCMB.
    /* MOVIMIENTO DE ENTRADA */
    CREATE B-CMOV.
    BUFFER-COPY Almcmov 
        TO B-CMOV
        ASSIGN
        B-CMOV.TipMov = "I"
        B-CMOV.Nrodoc = Almacen.CorrIng
        B-CMOV.NroRef = STRING(Almcmov.nroser, '999') + STRING(Almcmov.nrodoc, '9999999').
    ASSIGN 
        Almcmov.NroRef = STRING(B-CMOV.nroser, '999') + STRING(B-CMOV.nrodoc, '9999999')
        Almacen.CorrSal = Almacen.CorrSal + 1
        Almacen.CorrIng = Almacen.CorrIng + 1.

    RUN Genera-Detalle.
    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
        MESSAGE 'Error al grabar el detalle' VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN ERROR.
    END.

END.

END PROCEDURE.

PROCEDURE Genera-Detalle:

    DEFINE VARIABLE N-Itm AS INTEGER NO-UNDO.
    DEF VAR r-Rowid AS ROWID NO-UNDO.
    DEF VAR pComprometido AS DEC.

    DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
        /* SALIDAS */
        N-Itm = 0.
        FOR EACH ITEM WHERE ITEM.codmat <> "" BY ITEM.NroItm:
            N-Itm = N-Itm + 1.
            CREATE almdmov.
            ASSIGN 
                Almdmov.CodCia = Almcmov.CodCia 
                Almdmov.CodAlm = Almcmov.CodAlm 
                Almdmov.TipMov = Almcmov.TipMov 
                Almdmov.CodMov = Almcmov.CodMov 
                Almdmov.NroSer = Almcmov.NroSer 
                Almdmov.NroDoc = Almcmov.NroDoc 
                Almdmov.CodMon = Almcmov.CodMon 
                Almdmov.FchDoc = Almcmov.FchDoc 
                Almdmov.TpoCmb = Almcmov.TpoCmb
                Almdmov.codmat = ITEM.codmat
                Almdmov.codant = ITEM.codant      /* OJO */
                Almdmov.CanDes = ITEM.CanDes
                Almdmov.CodUnd = ITEM.CodUnd
                Almdmov.Factor = ITEM.Factor
                Almdmov.PreBas = ITEM.PreBas
                Almdmov.ImpCto = ITEM.ImpCto
                Almdmov.PreUni = ITEM.PreUni
                Almdmov.NroItm = N-Itm
                Almdmov.CodAjt = ''
                Almdmov.HraDoc = almcmov.HorSal
                R-ROWID = ROWID(Almdmov).
            FIND Almmmatg OF Almdmov NO-LOCK.
            ASSIGN
                Almdmov.CodUnd = Almmmatg.UndStk.
            RUN alm/almdcstk (R-ROWID).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

            RUN ALM\ALMACPR1 (R-ROWID,"U").
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
        END.
        /* ENTRADAS */
        /* RHC 28.01.10 El codigo destino se puede repetir => acumular por codigo */
        N-Itm = 0.
        FOR EACH ITEM WHERE ITEM.codant <> "" BY ITEM.NroItm:
            FIND Almdmov WHERE Almdmov.codcia = B-CMOV.codcia
                AND Almdmov.codalm = B-CMOV.codalm
                AND Almdmov.tipmov = B-CMOV.tipmov
                AND Almdmov.codmov = B-CMOV.codmov
                AND Almdmov.nroser = B-CMOV.nroser
                AND Almdmov.nrodoc = B-CMOV.nrodoc
                AND Almdmov.codmat = ITEM.codant
                NO-ERROR.
            IF NOT AVAILABLE Almdmov THEN DO:
                N-Itm = N-Itm + 1.
                CREATE almdmov.
                ASSIGN 
                    Almdmov.CodCia = B-CMOV.CodCia 
                    Almdmov.CodAlm = B-CMOV.CodAlm 
                    Almdmov.TipMov = B-CMOV.TipMov 
                    Almdmov.CodMov = B-CMOV.CodMov 
                    Almdmov.NroSer = B-CMOV.NroSer 
                    Almdmov.NroDoc = B-CMOV.NroDoc 
                    Almdmov.CodMon = B-CMOV.CodMon 
                    Almdmov.FchDoc = B-CMOV.FchDoc 
                    Almdmov.TpoCmb = B-CMOV.TpoCmb
                    Almdmov.codmat = ITEM.codant      /* OJO */
                    Almdmov.codant = ITEM.codmat      /* OJO */
                    Almdmov.CodUnd = ITEM.CodUnd
                    Almdmov.Factor = ITEM.PreBas      /* OJO */
                    Almdmov.NroItm = N-Itm
                    Almdmov.CodAjt = 'A'
                    Almdmov.HraDoc = B-CMOV.HorRcp
                    R-ROWID = ROWID(Almdmov).
            END.
            /* RHC 28.01.11 Verificamos en las tablas de reclasificaciones */
            FIND Almdrecl WHERE Almdrecl.codcia = s-codcia
                AND Almdrecl.codmatr = ITEM.codant
                AND Almdrecl.codmat  = ITEM.codmat
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almdrecl THEN DO:
                ASSIGN 
                    Almdmov.CanDes = Almdmov.candes + ITEM.CanDes
                    Almdmov.ImpCto = ITEM.ImpCto
                    Almdmov.PreUni = (ITEM.ImpCto / ITEM.CanDes).
            END.
            ELSE DO:
                ASSIGN 
                    Almdmov.CanDes = Almdmov.candes + ITEM.CanDes / Almdrecl.Factor
                    Almdmov.ImpCto = Almdmov.ImpCto + ITEM.ImpCto
                    Almdmov.PreUni = Almdmov.ImpCto / Almdmov.CanDes.
            END.
            /* fin de valorizaciones */
            FIND Almmmatg OF Almdmov NO-LOCK.
            ASSIGN
                Almdmov.CodUnd = Almmmatg.UndStk.
            FIND FIRST Almtmovm WHERE Almtmovm.CodCia = B-CMOV.CodCia 
                AND  Almtmovm.Tipmov = B-CMOV.TipMov 
                AND  Almtmovm.Codmov = B-CMOV.CodMov 
                NO-LOCK NO-ERROR.
            IF AVAILABLE Almtmovm AND Almtmovm.PidPCo 
                THEN ASSIGN Almdmov.CodAjt = "A".
            ELSE ASSIGN Almdmov.CodAjt = ''.
            RUN ALM\ALMACSTK (R-ROWID).
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
            RUN ALM\ALMACPR1 (R-ROWID,"U").
            IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'. 
        END.
    END.


END PROCEDURE.



PROCEDURE Carga-Excel:

    DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
    DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

    DEFINE VARIABLE chExcelApplication  AS COM-HANDLE.
    DEFINE VARIABLE chWorkbook          AS COM-HANDLE.
    DEFINE VARIABLE chWorksheet         AS COM-HANDLE.
    DEFINE VARIABLE cRange          AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE iCountLine      AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iTotalColumn    AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cValueX          AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
    DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
    DEFINE VARIABLE t-Row           AS INTEGER INIT 1.
    DEFINE VARIABLE i-Column        AS INTEGER     NO-UNDO.

    SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    /*RETURN-TO-START-DIR */
    USE-FILENAME
    UPDATE OKpressed.
    IF OKpressed = FALSE THEN RETURN.

    CREATE "Excel.Application" chExcelApplication.
    chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
    chWorkSheet = chExcelApplication:Sheets:ITEM(1).

    /* CARGAMOS EL TEMPORAL */
    DEF VAR x-CodMat AS CHAR.
    DEF VAR x-CanDes AS DEC.
    ASSIGN
        t-Column = 0
        t-Row = 1.     /* Saltamos el encabezado de los campos */
    REPEAT:
        ASSIGN
            t-column = 0
            t-Row    = t-Row + 1.
        t-column = t-column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
        /* ALMACEN */
        CREATE DETALLE.
        ASSIGN
            DETALLE.codalm = cValue.
        /* ARTICULO */
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            x-CodMat = cValue.
        /* CANTIDAD */
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            x-CanDes = DECIMAL(cValue)
            DETALLE.candes = ABSOLUTE(x-CanDes).
        IF x-CanDes > 0 THEN ASSIGN DETALLE.CodAnt = x-CodMat. ELSE DETALLE.CodMat = x-CodMat.
        /* ARTICULO */
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            x-CodMat = cValue.
        /* CANTIDAD */
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            x-CanDes = DECIMAL(cValue).
        IF x-CanDes > 0 THEN ASSIGN DETALLE.CodAnt = x-CodMat. ELSE DETALLE.CodMat = x-CodMat.
    END.
    chExcelApplication:QUIT().
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet. 


END PROCEDURE.
