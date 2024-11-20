/* ACTUALIZA LOS BUENOS CONTRIBUYENTES  DE PROVEEDORES */

DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "SELECCIONE LOS BUENOS CONTRIBUYENTES"
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
DEFINE VARIABLE cValueX          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.
DEFINE VARIABLE i-Column        AS INTEGER     NO-UNDO.

DEF VAR s-codcia  AS INT INIT 001 NO-UNDO.
DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR pv-codcia AS INT INIT 000 NO-UNDO.
DEF TEMP-TABLE t-cliente LIKE gn-clie.
DEF TEMP-TABLE t-proveedor LIKE gn-prov.


CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

RUN Proveedor-Retenedor.


chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

PROCEDURE Proveedor-Retenedor:
/* ************************** */

    EMPTY TEMP-TABLE t-proveedor.
    ASSIGN
        t-Column = 0
        t-Row = 1.     /* Saltamos el encabezado de los campos */
    REPEAT:
        ASSIGN
            t-column = 0
            t-Row    = t-Row + 1.
        t-column = t-column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
        cValue = STRING(DECIMAL(cValue), '99999999999').
        FIND FIRST t-proveedor WHERE t-proveedor.codpro = cValue NO-LOCK NO-ERROR.
        IF AVAILABLE t-proveedor THEN NEXT.

        CREATE t-proveedor.
        ASSIGN
            t-proveedor.codcia = pv-codcia
            t-proveedor.codpro = cValue
            t-proveedor.ruc    = cValue
            NO-ERROR.
        DISPLAY t-proveedor.codpro WITH STREAM-IO NO-BOX NO-LABELS.
        PAUSE 0.
    END.

    FOR EACH gn-prov WHERE gn-prov.codcia = pv-codcia:
        IF gn-prov.libre_c02 <> "No" THEN gn-prov.libre_c02 = "No".
        FIND FIRST t-proveedor WHERE t-proveedor.codcia = pv-codcia AND
            t-proveedor.ruc = gn-prov.ruc NO-LOCK NO-ERROR.
        IF AVAILABLE t-proveedor THEN DO:
            gn-prov.libre_c02 = "Si".
            DISPLAY 'PROVEEDORES:' gn-prov.codpro WITH STREAM-IO NO-LABELS.
            PAUSE 0.
        END.
    END.

END PROCEDURE.
