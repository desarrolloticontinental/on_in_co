DEFINE VARIABLE s-codcia AS INT INIT 001 NO-UNDO.
DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

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

DEF VAR k         AS INT NO-UNDO.
DEF VAR x-DtoVolR AS DEC NO-UNDO.
DEF VAR x-DtoVolD AS DEC NO-UNDO.
DEF VAR x-apepat LIKE gn-clie.apepat.
DEF VAR x-apemat LIKE gn-clie.apemat.
DEF VAR x-nombre LIKE gn-clie.nombre.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

/* CHEQUEAMOS LA INTEGRIDAD DEL ARCHIVO EXCEL */
ASSIGN
    t-Column = 0
    t-Row = 1.     /* Saltamos 1ra linea */

cValue = chWorkSheet:Cells(1,1):VALUE.
IF cValue = "" OR cValue = ? THEN DO:
    MESSAGE 'Formato del archivo Excel errado' VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

/* Cargamos temporal */
DEF TEMP-TABLE T-CLIE
    FIELD codcia LIKE gn-clie.codcia
    FIELD codcli LIKE gn-clie.codcli
    FIELD ruc    LIKE gn-clie.ruc
    FIELD apepat LIKE gn-clie.apepat
    FIELD apemat LIKE gn-clie.apemat
    FIELD nombre LIKE gn-clie.nombre
    FIELD nomcli LIKE gn-clie.nomcli
    FIELD dircli LIKE gn-clie.dircli
    FIELD coddept LIKE gn-clie.CodDept 
    FIELD codprov LIKE gn-clie.CodProv 
    FIELD coddist LIKE gn-clie.CodDist
    .

ASSIGN
    t-Column = 0
    t-Row = 0.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = TRIM(chWorkSheet:Cells(t-Row, t-Column):VALUE).
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 

    CREATE T-CLIE.
    ASSIGN
        T-CLIE.codcia = 000
        T-CLIE.codcli = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-apepat = TRIM(cValue).
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-apemat = TRIM(cValue).
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-nombre = TRIM(cValue).

    CASE TRUE:
        WHEN LOOKUP(SUBSTRING(t-clie.codcli,1,2), '20') > 0 THEN DO:
            ASSIGN
                t-clie.nombre = x-apemat
                t-clie.nomcli = x-apemat
                T-CLIE.ruc = t-clie.codcli.
        END.
        WHEN LOOKUP(SUBSTRING(t-clie.codcli,1,2), '10,15,17') > 0 THEN DO:
            ASSIGN
                t-clie.apepat = x-apepat
                t-clie.apemat = x-apemat
                t-clie.nombre = x-nombre
                t-clie.nomcli = TRIM(x-apepat) + ' ' + TRIM(x-apemat) + ', ' + x-nombre
                T-CLIE.ruc = t-clie.codcli.
        END.
        OTHERWISE DO:
            ASSIGN
                t-clie.apepat = x-apepat
                t-clie.apemat = x-apemat
                t-clie.nombre = x-nombre
                t-clie.nomcli = TRIM(x-apepat) + ' ' + TRIM(x-apemat) + ', ' + x-nombre.
        END.
    END CASE.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-CLIE.dircli = TRIM(cValue).

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-CLIE.CodDept = STRING(INTEGER(cValue), '99').

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-CLIE.CodProv = STRING(INTEGER(cValue), '99').

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-CLIE.CodDist = STRING(INTEGER(cValue), '99').
END.

chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

FOR EACH t-clie:
    FIND FIRST gn-clie OF t-clie NO-ERROR.
    IF AVAILABLE gn-clie THEN DO:
        BUFFER-COPY t-clie TO gn-clie.
/*         DISPLAY                               */
/*             t-clie.codcli                     */
/*             gn-clie.codcli                    */
/*             t-clie.apepat                     */
/*             gn-clie.apepat                    */
/*             t-clie.apemat                     */
/*             gn-clie.apemat                    */
/*             t-clie.nombre                     */
/*             gn-clie.nombre                    */
/*             t-clie.nomcli                     */
/*             gn-clie.nomcli                    */
/*             t-clie.ruc                        */
/*             gn-clie.ruc                       */
/*             t-clie.coddept                    */
/*             gn-clie.coddept                   */
/*             t-clie.codprov                    */
/*             gn-clie.codprov                   */
/*             t-clie.coddist                    */
/*             gn-clie.coddist                   */
/*             WITH 1 COL WITH STREAM-IO NO-BOX. */
    END.
END.
