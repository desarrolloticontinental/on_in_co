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

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-periodo AS INT INIT 2013.
DEF VAR s-nromes AS INT INIT 04.
DEF VAR s-codope AS CHAR INIT '076'.
DEF VAR s-nroast AS CHAR INIT '040035'.  /*'020032'*/
DEF VAR s-tpocmb AS DEC INIT 2.581.
DEF VAR s-nroitm AS INT.

FIND cb-cmov WHERE cb-cmov.codcia = s-codcia
    AND cb-cmov.periodo = s-periodo
    AND cb-cmov.nromes = s-nromes
    AND cb-cmov.codope = s-codope
    AND cb-cmov.nroast = s-nroast
    NO-ERROR.
IF NOT AVAILABLE cb-cmov THEN RETURN.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

ASSIGN
    s-nroitm = 0
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        s-nroitm = s-nroitm + 1
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 

    cValueX = chWorkSheet:Cells( t-Column,t-Row):VALUE.

    DISPLAY t-Row t-column cValue cValueX.

    CREATE cb-dmov.
    BUFFER-COPY cb-cmov TO cb-dmov.
    ASSIGN
        cb-dmov.NroItm = s-nroitm
        cb-dmov.tpocmb = s-tpocmb
        cb-dmov.codcta = STRING(DECIMAL(cValue), '99999999').

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.coddiv = (IF cValue <> ? THEN cValue ELSE "")

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.clfaux = (IF cValue <> ? THEN cValue ELSE "").

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.codaux = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.NroRuc = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.coddoc = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.nrodoc = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.GloDoc = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.CodMon = (IF cValue = "S/." THEN 1 ELSE 2).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.TpoMov = (IF cValue = "DEBE" THEN NO ELSE YES).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.ImpMn2 = DECIMAL(cValue).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.ImpMn1 = DECIMAL(cValue).
    
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.FchDoc = DATE(cValue).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        cb-dmov.FchVto = DATE(cValue).

END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

