DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
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
DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.
DEFINE VARIABLE i-Column        AS INTEGER     NO-UNDO.

DEF VAR s-codcia AS INT INIT 001.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

DEF VAR x-codalm AS CHAR FORMAT 'x(3)' NO-UNDO.
DEF VAR x-tipmov LIKE almcmov.tipmov NO-UNDO.
DEF VAR x-codmov LIKE almcmov.codmov NO-UNDO.
DEF VAR x-nrodoc LIKE almcmov.nrodoc NO-UNDO.
DEF VAR x-codmat LIKE almdmov.codmat NO-UNDO.
DEF VAR x-preunimn LIKE almdmov.preuni NO-UNDO.
DEF VAR x-preunime LIKE almdmov.preuni NO-UNDO.

ASSIGN
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 0
        t-Row    = t-Row + 1.
    ASSIGN
        t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    ASSIGN
        x-codalm = cValue.
    IF INDEX(cValue, '.') > 0 THEN x-codalm = TRIM(STRING(INTEGER(cValue))).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-tipmov = SUBSTRING(cValue,1,1)
        x-codmov = INTEGER(SUBSTRING(cValue,2)).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-nrodoc = INTEGER(cValue).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-codmat = cValue.

    t-Column = t-Column + 4.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-preunimn = DECIMAL(cValue).
    t-Column = t-Column + 2.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-preunime = DECIMAL(cValue).

    DISPLAY x-codalm x-tipmov x-codmov x-nrodoc x-codmat x-preunimn x-preunime
        WITH STREAM-IO NO-BOX WIDTH 320.
    PAUSE 0.
    FIND almdmov WHERE codcia = 1
        AND codalm = x-codalm
        AND tipmov = x-tipmov
        AND codmov = x-codmov
        AND nrodoc = x-nrodoc
        AND codmat = x-codmat.
/*     DISPLAY codalm tipmov codmov nrodoc codmat candes codmon preuni impcto x-preunimn x-preunime */
/*         WITH STREAM-IO NO-BOX WIDTH 320.                                                         */
/*     PAUSE 0.                                                                                     */
    IF almdmov.codmon = 2 THEN
        ASSIGN
        almdmov.preuni = x-preunime
        almdmov.impcto = almdmov.candes * almdmov.preuni.
    IF almdmov.codmon = 1 THEN
        ASSIGN
        almdmov.preuni = x-preunimn
        almdmov.impcto = almdmov.candes * almdmov.preuni.
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 


