DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.

/*
    El Excel debe tener esta estructu
    Almacen	TipoMov	CodMoviento	SerieNroDcto	FechaDcto
    21F     S       50          00000485        28/06/2013
    21F     I       55          00008452        28/06/2013
*/

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

DEF VAR x-codalm LIKE almcmov.codalm NO-UNDO.
DEF VAR x-tipmov LIKE almcmov.TipMov NO-UNDO.
DEF VAR x-codmov LIKE almcmov.codmov NO-UNDO.
DEF VAR x-numero AS CHAR NO-UNDO.
DEF VAR x-nroser LIKE almcmov.nroser NO-UNDO.
DEF VAR x-nrodoc LIKE almcmov.nrodoc NO-UNDO.
DEF VAR x-fchdoc LIKE almcmov.fchdoc NO-UNDO.
DEF VAR x-nrorf1 LIKE almcmov.nrorf1 NO-UNDO.

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

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-tipmov = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-codmov = INTEGER(cValue).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-numero = cValue
        x-nroser = INTEGER(SUBSTRING(x-numero,1,3))
        x-nrodoc = INTEGER(SUBSTRING(x-numero,4)).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-fchdoc = DATE(cValue).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-nrorf1 = cValue.
    FIND almcmov WHERE codcia = 1
        AND codalm = x-codalm
        AND tipmov = x-tipmov
        AND codmov = x-codmov
        AND nroser = x-nroser
        AND nrodoc = x-nrodoc EXCLUSIVE NO-ERROR.
    IF AVAILABLE almcmov THEN DO: 
        /*
        DISPLAY almcmov.nrodoc almcmov.tipmov almcmov.fchdoc x-fchdoc almcmov.nrorf1 x-nrorf1
            WITH STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
        */
        
        FOR EACH almdmov OF almcmov:
            almdmov.fchdoc = x-fchdoc.
        END.
        almcmov.fchdoc = x-fchdoc.
          
    END.
    ELSE DO:
        /*
        DISPLAY X-codalm X-tipmov X-codmov X-nroser X-nrodoc.
        PAUSE 0.
        */
    END.
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 


