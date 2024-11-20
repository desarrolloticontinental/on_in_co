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

DEF VAR s-codcia    AS INT  NO-UNDO.
DEF VAR cb-codcia   AS INT  NO-UNDO.
DEF VAR s-periodo   AS INT  NO-UNDO.
DEF VAR s-nromes    AS INT  NO-UNDO.
DEF VAR s-codope    AS CHAR NO-UNDO.
DEF VAR s-nroast    AS CHAR NO-UNDO.
DEF VAR s-tpocmb    AS DEC  NO-UNDO.
DEF VAR s-nroitm    AS INT  NO-UNDO.
DEF VAR s-user-id   AS CHAR NO-UNDO.
DEF VAR s-codmon AS INT INIT 1 NO-UNDO.

ASSIGN
    s-codcia = 001
    cb-codcia = 000
    s-periodo = 2014
    s-nromes = 12
    s-codope = '076'
    s-tpocmb = 1.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    /*RETURN-TO-START-DIR */
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN.


DEF TEMP-TABLE T-DMOV LIKE T-DMOV.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

/* CARGAMOS EL TEMPORAL */
ASSIGN
    s-nroitm = 0
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */

DEF VAR s-codcta AS CHAR.
DEF VAR s-codaux AS CHAR.
DEF VAR s-fchdoc AS DATE.
DEF VAR s-coddoc AS CHAR.
DEF VAR s-nrodoc AS CHAR.
DEF VAR s-cargo AS DEC.
DEF VAR s-abono AS DEC.
DEF VAR s-dice AS CHAR.
DEF VAR s-decir AS CHAR.


REPEAT:
    ASSIGN
        s-nroitm = s-nroitm + 1
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    /* periodo */
    s-periodo = INTEGER(cValue).
    /* mes */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-nromes = INTEGER(cvalue).
    /* asiento */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-nroast = cvalue.
    /* libro */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-codope = cvalue.
    /* cuenta */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-codcta = cvalue.
    /* auxiliar */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-codaux = cvalue.
    /* fecha */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-fchdoc = date(cvalue).
    /* doc */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-coddoc = cvalue.
    /* numero */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-nrodoc = cvalue.
    /* cargo */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-cargo = round(decimal(cvalue),2).
    /* abono */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-abono = round(decimal(cvalue),2).
    /* dice */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN s-dice = string(INT(cvalue)) NO-ERROR.
    /* debe decir */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-decir = string(INT(cvalue)).

    IF s-cargo = ? THEN s-cargo = 0.
    IF s-abono = ? THEN s-abono = 0.
    IF s-coddoc = ? THEN s-coddoc = ''.
    IF s-nrodoc = ? THEN s-nrodoc = ''.
    
    /*
    MESSAGE 'period' s-periodo SKIP 'mes' s-nromes SKIP 'asiento' s-nroast SKIP
        'libro' s-codope SKIP 'cuenta' s-codcta SKIP 'aux' s-codaux SKIP
        'doc' s-coddoc SKIP 'nro' s-nrodoc SKIP 'cco' s-dice s-decir
        SKIP 'importe' s-cargo s-abono
        VIEW-AS ALERT-BOX.
    LEAVE.
    */
    
    FIND cb-dmov WHERE codcia = 1
        AND periodo = s-periodo
        AND nromes = s-nromes
        AND nroast = s-nroast
        AND codope = s-codope
        AND codcta = s-codcta
        AND codaux = s-codaux
        AND coddoc = s-coddoc
        AND nrodoc = s-nrodoc
        AND cco = s-dice
        AND impmn1 = (s-cargo + s-abono)
        NO-ERROR.
    IF AVAILABLE cb-dmov THEN DO:
        DISPLAY codcta coddoc nrodoc cco s-decir impmn1 WITH STREAM-IO.
        /*
        pause 0.
        cb-dmov.cco = s-decir.
        */
    END.
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

