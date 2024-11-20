DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-fchdoc AS DATE NO-UNDO.
DEF VAR s-tipmov AS CHAR NO-UNDO.
DEF VAR s-codmov AS INT INIT 98 NO-UNDO.
DEF VAR s-candes AS DEC NO-UNDO.
DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR s-nrodoc AS INT NO-UNDO.

ASSIGN 
    s-fchdoc = DATE(12,31,2013)
    s-nroser = 000
    s-nrodoc = 000002.

DEF TEMP-TABLE t-mate LIKE almmmate.
DEF TEMP-TABLE t-alma LIKE almacen
    FIELD stkact LIKE almmmate.stkact.
DEF TEMP-TABLE t-cmov LIKE almcmov.
DEF TEMP-TABLE t-dmov LIKE almdmov.

/* Cargamos temporal */
RUN Carga-Temporal.

/* barremos todos los productos */
FOR EACH t-mate, FIRST almmmatg OF t-mate NO-LOCK:
    /* Definimos los almacenes a trabajar y sus saldos al corte */
    RUN Cargamos-Saldos.

    /* Generamos movimientos hasta consumir el ajuste contable */
    s-tipmov = "I". /* por defecto */
    IF t-mate.stkact < 0 THEN s-tipmov = "S".
    t-mate.stkact = ABSOLUTE(t-mate.stkact).    /* Valor positivo */

    FOR EACH t-alma WHERE t-alma.codcia = s-codcia
        BREAK BY t-alma.codcia BY t-alma.stkact DESC:
        s-candes = MINIMUM(t-mate.stkact,t-alma.stkact).
        CREATE t-dmov.
        ASSIGN
            t-dmov.NroItm = 1
            t-dmov.CodCia = s-codcia
            t-dmov.CodAlm = t-alma.codalm
            t-dmov.TipMov = s-tipmov
            t-dmov.CodMov = s-codmov
            t-dmov.FchDoc = s-fchdoc
            t-dmov.NroSer = s-nroser
            t-dmov.NroDoc = s-nrodoc
            t-dmov.CodMon = 1
            t-dmov.codmat = t-mate.codmat
            t-dmov.CodUnd = almmmatg.undstk
            t-dmov.CanDes = s-candes
            /*t-dmov.ImpCto = t-mate.VCtMn1*/
            t-dmov.PreUni = t-mate.VCtMn1     /* OJO <<< UNITARIO */
            t-dmov.Factor = 1
            t-dmov.ImpLin = t-dmov.candes * t-dmov.preuni
            .
        ASSIGN t-mate.stkact = t-mate.stkact - t-dmov.candes.
        IF t-mate.stkact <= 0 THEN LEAVE.
        IF LAST-OF(t-alma.codcia) AND t-mate.stkact > 0 
            THEN ASSIGN 
            t-dmov.candes = t-dmov.candes + t-mate.stkact
            t-dmov.implin = t-dmov.candes * t-dmov.preuni.
    END.
END.
/* generamos cabeceras */
FOR EACH t-dmov NO-LOCK:
    FIND FIRST t-cmov WHERE t-cmov.codcia = t-dmov.codcia
        AND t-cmov.codalm = t-dmov.codalm
        AND t-cmov.tipmov = t-dmov.tipmov
        AND t-cmov.codmov = t-dmov.codmov
        AND t-cmov.nroser = t-dmov.nroser
        AND t-cmov.nrodoc = t-dmov.nrodoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE t-cmov THEN DO:
        CREATE t-cmov.
        BUFFER-COPY t-dmov TO t-cmov ASSIGN t-cmov.usuario = 'ADMIN'.
    END.
END.

/* Ahora sí grabamos las tablas */
FOR EACH t-cmov NO-LOCK:
    CREATE almcmov.
    BUFFER-COPY t-cmov TO almcmov.
END.
FOR EACH t-dmov NO-LOCK:
    CREATE almdmov.
    BUFFER-COPY t-dmov TO almdmov.
END.
/* Actualizamos kardex */
DEF VAR x-fchini AS DATE NO-UNDO.
x-fchini = s-fchdoc - 30.
FOR EACH t-mate NO-LOCK WHERE t-mate.codcia = s-codcia AND t-mate.codmat <> '':
    DISPLAY 'recalculando:' t-mate.codmat WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RUN alm/calc-costo-promedio (t-mate.codmat, x-fchini ).
END.


PROCEDURE Carga-Temporal:

    DEFINE VARIABLE FILL-IN-Archivo AS CHAR         NO-UNDO.
    DEFINE VARIABLE OKpressed       AS LOG          NO-UNDO.
    DEFINE VARIABLE chExcelApplication          AS COM-HANDLE.
    DEFINE VARIABLE chWorkbook                  AS COM-HANDLE.
    DEFINE VARIABLE chWorksheet                 AS COM-HANDLE.
    DEFINE VARIABLE cRange          AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE iCountLine      AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iTotalColumn    AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
    DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
    DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
    DEFINE VARIABLE t-Row           AS INTEGER INIT 1.

    /* RUTINA GENERAL */
    SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
        FILTERS "Archivos Excel (*.xls,*.xlsx)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
        TITLE "Archivo(s) de Carga..."
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
    IF OKpressed = FALSE THEN RETURN NO-APPLY.

    /* CREAMOS LA HOJA EXCEL */
    CREATE "Excel.Application" chExcelApplication.
    chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
    chWorkSheet = chExcelApplication:Sheets:ITEM(1).

    ASSIGN
        t-Row = 1.     /* Saltamos el encabezado de los campos */
    REPEAT:
        ASSIGN
            t-column = 0
            t-Row    = t-Row + 1.
        t-column = t-column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
        /* CODIGO */
        CREATE t-mate.
        ASSIGN
            t-mate.codcia = s-codcia
            t-mate.codmat = cValue.
        /* ALMACEN */
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            t-mate.codalm = cValue.
        /* DIFERENCIA */        
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            t-mate.stkact = DECIMAL(cValue)
            NO-ERROR.
        /* PU */        
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            t-mate.VCtMn1  = ABSOLUTE(DECIMAL(cValue))
            NO-ERROR.
    END.

    /* CERRAMOS EL EXCEL */
    chExcelApplication:QUIT().
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet. 

END PROCEDURE.

PROCEDURE Cargamos-Saldos:

    EMPTY TEMP-TABLE t-alma.

    /* almacén definido? */
    IF t-mate.codalm <> 'DEL QUE TENGA MAYOR STOCK' THEN DO:
        CREATE t-alma.
        ASSIGN
            t-alma.codcia = s-codcia
            t-alma.codalm = t-mate.codalm
            t-alma.stkact = ABSOLUTE(t-mate.stkact).
    END.
    ELSE DO:
        FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia:
            FIND LAST almstkal WHERE almstkal.codcia = s-codcia
                AND almstkal.codalm = almacen.codalm
                AND almstkal.codmat = t-mate.codmat
                AND almstkal.fecha <= s-fchdoc
                NO-LOCK NO-ERROR.
            IF AVAILABLE almstkal AND AlmStkal.StkAct > 0 THEN DO:
                CREATE t-alma.
                BUFFER-COPY almacen
                    TO t-alma
                    ASSIGN t-alma.stkact = almstkal.stkact.
            END.
        END.
        FIND FIRST t-alma NO-ERROR.
        IF NOT AVAILABLE t-alma THEN DO:
            /* Almacén por defecto */
            CREATE t-alma.
            ASSIGN
                t-alma.codcia = s-codcia
                t-alma.codalm = '11'
                t-alma.stkact = ABSOLUTE(t-mate.stkact).
        END.
    END.

END PROCEDURE.
