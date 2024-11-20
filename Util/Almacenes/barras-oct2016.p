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
DEFINE VARIABLE iValue          AS INT64      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.
DEFINE VARIABLE i-Column        AS INTEGER     NO-UNDO.
DEFINE VAR lMsg AS LOG.

DEF VAR s-codcia    AS INT  NO-UNDO.
DEF VAR s-user-id   AS CHAR INIT 'ADMIN' NO-UNDO.
ASSIGN
    s-codcia = 001.
SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xlsx)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN.
CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

DEFINE VAR iColumn AS INT.

lmsg = NO.
REPEAT iColumn = 2 TO 65000:
    /* Descripción */
    cRange = "A" + TRIM(STRING(iColumn)).
    cValue = chWorkSheet:Range(cRange):VALUE.

    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */

    cValue = TRIM(cValue).

    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = cValue
        NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE 'Error código:' cValue VIEW-AS ALERT-BOX ERROR.
        LEAVE.
    END.
    /* Anulamos códigos EAN 13 y 14 */
    FOR EACH Almmmat1 OF Almmmatg:
        DELETE Almmmat1.
    END.
    ASSIGN Almmmatg.codbrr = "".
    /* Cargamos nuevos códigos */
    cRange = "N" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    IF cValue > '' THEN ASSIGN Almmmatg.codbrr = cValue.
    /* EAN14 a */
    cRange = "O" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    cRange = "P" + TRIM(STRING(iColumn)).
    ASSIGN dValue = DECIMAL(chWorkSheet:Range(cRange):VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'Error EAN14-A código:' Almmmatg.codmat VIEW-AS ALERT-BOX ERROR.
        UNDO, LEAVE.
    END.
    IF TRUE <> (cValue > '') THEN NEXT.
    CREATE Almmmat1.
    ASSIGN
        Almmmat1.CodCia = Almmmatg.codcia
        Almmmat1.codmat = Almmmatg.codmat.
    ASSIGN
        Almmmat1.Barras[1] = cValue
        Almmmat1.Equival[1] = dValue.
    /* EAN14 b */
    cRange = "Q" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    cRange = "R" + TRIM(STRING(iColumn)).
    ASSIGN dValue = DECIMAL(chWorkSheet:Range(cRange):VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'Error EAN14-B código:' Almmmatg.codmat VIEW-AS ALERT-BOX ERROR.
        UNDO, LEAVE.
    END.
    IF TRUE <> (cValue > '') THEN NEXT.
    ASSIGN
        Almmmat1.Barras[2] = cValue
        Almmmat1.Equival[2] = dValue.
    /* EAN14 c */
    cRange = "S" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    cRange = "T" + TRIM(STRING(iColumn)).
    ASSIGN dValue = DECIMAL(chWorkSheet:Range(cRange):VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'Error EAN14-C código:' Almmmatg.codmat VIEW-AS ALERT-BOX ERROR.
        UNDO, LEAVE.
    END.
    IF TRUE <> (cValue > '') THEN NEXT.
    ASSIGN
        Almmmat1.Barras[3] = cValue
        Almmmat1.Equival[3] = dValue.
    /* EAN14 d */
    cRange = "U" + TRIM(STRING(iColumn)).
    cValue = TRIM(chWorkSheet:Range(cRange):VALUE).
    cRange = "V" + TRIM(STRING(iColumn)).
    ASSIGN dValue = DECIMAL(chWorkSheet:Range(cRange):VALUE) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'Error EAN14-C código:' Almmmatg.codmat VIEW-AS ALERT-BOX ERROR.
        UNDO, LEAVE.
    END.
    IF TRUE <> (cValue > '') THEN NEXT.
    ASSIGN
        Almmmat1.Barras[4] = cValue
        Almmmat1.Equival[4] = dValue.
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 


RETURN.



