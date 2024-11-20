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
DEF VAR s-coddiv AS CHAR NO-UNDO.
DEF VAR s-clfaux AS CHAR NO-UNDO.

ASSIGN
    s-codcia = 001
    cb-codcia = 000
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
    /* periodo */
    ASSIGN
        s-periodo = 2015
        s-nromes  = 08.
    ASSIGN
        s-nroitm = s-nroitm + 1
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    /* Mes */
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-nromes = INTEGER(cvalue).
    /* cuenta */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-codcta = SUBSTRING(cvalue,1,8).
    /* asiento */
    t-Column = t-Column + 2.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-nroast = cvalue.
    /* libro */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-codope = cvalue.
    /* division */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-coddiv = cvalue.
    /* cco dice */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-dice = cvalue.
    /* cco debe decir */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-decir = cvalue.
    /* auxiliar */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-clfaux = cvalue.
    IF s-clfaux = ? THEN s-clfaux = ''.
    /* auxiliar */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-codaux = cvalue.
    IF s-codaux = ? THEN s-codaux = ''.
    /* doc */
    t-Column = t-Column + 3.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-coddoc = cvalue.
    IF s-coddoc = ? THEN s-coddoc = ''.
    /* numero */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    s-nrodoc = cvalue.
    IF s-nrodoc = ? THEN s-nrodoc = ''.
/*     MESSAGE 's-nromes' s-nromes SKIP 's-nroast' s-nroast SKIP 's-codcta' s-codcta */
/*         SKIP 's-codope' s-codope SKIP 's-coddiv' s-coddiv                         */
/*         SKIP 's-dice' s-dice 'debe' s-decir SKIP 's-clfaux' s-clfaux              */
/*         SKIP 's-codaux' s-codaux SKIP 's-coddoc' s-coddoc                         */
/*         SKIP 's-nrodoc' s-nrodoc.                                                 */
/*     NEXT.                                                                         */
    FOR EACH cb-dmov WHERE codcia = 1 
        and periodo = s-periodo 
        and nromes = s-nromes 
        and nroast = s-nroast 
        and codcta = s-codcta 
        and codope = s-codope 
        and coddiv = s-coddiv 
        and cco = s-dice 
        and clfaux = s-clfaux 
        and codaux = s-codaux 
        and coddoc = s-coddoc 
        and nrodoc = s-nrodoc:
        DISPLAY codcta nroast codope coddoc nrodoc cco s-decir WITH STREAM-IO.
        PAUSE 0.
        ASSIGN cb-dmov.cco = s-decir.
    END.
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

