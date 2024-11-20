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

DEF TEMP-TABLE t-dmov LIKE almdmov
    INDEX Llave01 AS PRIMARY codcia codmat codalm tipmov codmov.

/* Cargamos temporal */
RUN Carga-Temporal.

FOR EACH t-dmov NO-LOCK:
    FIND almdmov WHERE almdmov.codcia = s-codcia
        AND almdmov.codalm = t-dmov.codalm
        AND almdmov.tipmov = t-dmov.tipmov
        AND almdmov.codmov = t-dmov.codmov
        AND almdmov.fchdoc = s-fchdoc
        AND almdmov.codmat = t-dmov.codmat
        AND almdmov.candes = t-dmov.candes
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        DISPLAY t-dmov.codmat t-dmov.codalm t-dmov.tipmov t-dmov.codmov
        t-dmov.candes WITH STREAM-IO NO-BOX WIDTH 320.
    ASSIGN
        Almdmov.CanDev = Almdmov.CanDes .
    ASSIGN
        Almdmov.CanDes = t-dmov.candev.
END.

/* Actualizamos kardex */

DEF VAR x-fchini AS DATE NO-UNDO.
x-fchini = s-fchdoc - 30.
FOR EACH t-dmov NO-LOCK WHERE t-dmov.codcia = s-codcia AND t-dmov.codmat <> ''
    BREAK BY t-dmov.codmat:
    IF FIRST-OF(t-dmov.codmat) THEN DO:
        DISPLAY 'recalculando:' t-dmov.codmat WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
        RUN alm/calc-costo-promedio (t-dmov.codmat, x-fchini ).
    END.
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
        CREATE t-dmov.
        ASSIGN
            t-dmov.codcia = s-codcia
            t-dmov.codmat = cValue.
        /* ALMACEN */
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            t-dmov.codalm = cValue.
        /* MOVIMIENTO */
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            t-dmov.tipmov = ENTRY(1,cValue,'-')
            t-dmov.codmov = INTEGER(ENTRY(2,cValue,'-')).
        /* DICE */        
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            t-dmov.candes = DECIMAL(cValue)
            NO-ERROR.
        /* DEBE DECIR */        
        t-Column = t-Column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            t-dmov.candev = DECIMAL(cValue)
            NO-ERROR.
    END.

    /* CERRAMOS EL EXCEL */
    chExcelApplication:QUIT().
    RELEASE OBJECT chExcelApplication.      
    RELEASE OBJECT chWorkbook.
    RELEASE OBJECT chWorksheet. 

END PROCEDURE.

