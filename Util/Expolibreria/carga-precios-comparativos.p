DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    /*RETURN-TO-START-DIR */
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN.

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

DEF VAR s-codcia    AS INT  NO-UNDO.
DEF TEMP-TABLE Detalle 
    FIELD Descripcion AS CHAR
    FIELD codigo AS CHAR
    FIELD nombre AS CHAR
    FIELD unidad AS CHAR
    FIELD precio AS DEC
    FIELD discount AS DEC
    FIELD discount2 AS DEC
    FIELD preofi LIKE Almmmatg.PreOfi
    FIELD CHR__01 LIKE Almmmatg.CHR__01.
ASSIGN
    s-codcia = 001.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

/* CARGAMOS EL TEMPORAL */
ASSIGN
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 1
        t-Row    = t-Row + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    /* CODMAT */
    CREATE Detalle.
    ASSIGN
        Detalle.descripcion = cValue.
    /* CODMAT */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.codigo = cValue.
    /* CODMAT */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.nombre = cValue.
    /* CODMAT */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.unidad = cValue.
    /* PRECIO */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.Precio = DECIMAL(cValue) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    /* PROMOCION */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.discount = DECIMAL(cValue) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
    /* PROMOCION */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.discount2 = DECIMAL(cValue) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, NEXT.
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

OUTPUT TO c:\tmp\xyz.txt.
PUT UNFORMATTED
    'Description|Column_1|Name|Unit|Price|Discount|Discount2|PreOfi|UnitOfi'
    SKIP.
FOR EACH detalle, FIRST almmmatg NO-LOCK WHERE almmmatg.codcia = 001
    AND almmmatg.codmat = detalle.codigo:
    ASSIGN
        detalle.preofi = almmmatg.preofi
        detalle.CHR__01 = almmmatg.CHR__01.
    IF almmmatg.monvta = 2 THEN detalle.preofi = almmmatg.preofi * almmmatg.tpocmb.
    PUT UNFORMATTED
        detalle.Descripcion '|'
        detalle.codigo '|'
        detalle.nombre '|'
        detalle.unidad '|'
        detalle.precio '|'
        detalle.discount '|'
        detalle.discount2 '|'
        detalle.preofi '|'
        detalle.CHR__01 
        SKIP.
END.
OUTPUT CLOSE.
