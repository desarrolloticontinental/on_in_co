/* ACTUALIZA LOS AGENTES DE RETENCION DE PROVEEDORES Y CLIENTES */

DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "SELECCIONE LOS AGENTES RETENEDORES"
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

/* PRIMERO LOS CLIENTES */
RUN Cliente-Retenedor.

/* SEGUNDO LOS PROVEEDORES */
RUN Proveedor-Retenedor.


chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 


PROCEDURE Cliente-Retenedor:
/* ************************ */

    EMPTY TEMP-TABLE t-cliente.
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
        FIND FIRST t-cliente WHERE t-cliente.codcli = cValue NO-LOCK NO-ERROR.
        IF AVAILABLE t-cliente THEN NEXT.

        CREATE t-cliente.
        ASSIGN
            t-cliente.codcia = cl-codcia
            t-cliente.codcli = SUBSTRING(cValue,1,11)
            NO-ERROR.
    END.
    FOR EACH gn-clie WHERE gn-clie.codcia = cl-codcia:
        IF gn-clie.rucold <> "NO" THEN gn-clie.rucold = "NO".
        FIND FIRST t-cliente OF gn-clie NO-LOCK NO-ERROR.
        IF AVAILABLE t-cliente THEN DO:
            gn-clie.rucold = "SI".
            DISPLAY 'CLIENTE:' gn-clie.codcli WITH STREAM-IO.
            PAUSE 0.
        END.
    END.

END PROCEDURE.

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
    END.

    FOR EACH gn-prov WHERE gn-prov.codcia = pv-codcia:
        IF gn-prov.libre_c01 <> "NO" THEN gn-prov.libre_c01 = "NO".
        FIND FIRST t-proveedor WHERE t-proveedor.codcia = pv-codcia AND
            t-proveedor.ruc = gn-prov.ruc NO-LOCK NO-ERROR.
        IF AVAILABLE t-proveedor THEN DO:
            gn-prov.libre_c01 = "SI".
            DISPLAY 'PROVEEDORES:' gn-prov.codpro WITH STREAM-IO NO-LABELS.
            PAUSE 0.
        END.
    END.

END PROCEDURE.
