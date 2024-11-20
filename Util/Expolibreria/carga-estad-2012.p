DEF VAR x-linea AS CHAR.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '10015'.
DEF VAR x-CToTot AS DEC NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.


DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    RETURN-TO-START-DIR 
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN.

FOR EACH w-report WHERE task-no = 666:
    DELETE w-report.
END.

DEFINE VARIABLE chExcelApplication AS COM-HANDLE.
DEFINE VARIABLE chWorkbook AS COM-HANDLE.
DEFINE VARIABLE chWorksheet AS COM-HANDLE.

DEFINE VARIABLE cRange          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCountLine      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iTotalColumn    AS INTEGER      NO-UNDO.
DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 11.

DEFINE VARIABLE i-Column        AS INTEGER     NO-UNDO.

CREATE "Excel.Application" chExcelApplication.

chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 

    /* Cliente */
    cValue = chWorkSheet:Cells(t-Row, 1):VALUE.
    CREATE w-report.
    ASSIGN
        w-report.task-no = 666
        w-report.Llave-C = SUBSTRING(cValue,1,11)
        w-report.Llave-I = INTEGER(chWorkSheet:Cells(t-Row, 2):VALUE)
        w-report.Campo-C[1] = chWorkSheet:Cells(t-Row, 4):VALUE
        w-report.Campo-F[1] = DECIMAL(chWorkSheet:Cells(t-Row, 5):VALUE)
        w-report.Campo-F[2] = DECIMAL(chWorkSheet:Cells(t-Row, 6):VALUE)
        w-report.Campo-F[3] = DECIMAL(chWorkSheet:Cells(t-Row, 7):VALUE)
        w-report.Campo-F[4] = DECIMAL(chWorkSheet:Cells(t-Row, 8):VALUE)
        w-report.Campo-F[5] = DECIMAL(chWorkSheet:Cells(t-Row, 9):VALUE).
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

