/* CLIENTES EXCEPTUADOS DE PERCEPCION */

DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls*", "Todos (*.*)" "*.*"
    TITLE "SELECCIONE CLIENTES EXCEPTUADOS DE PERCEPCION"
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
DEF VAR s-tabla AS CHAR INIT 'CLNOPER' NO-UNDO.

DEF TEMP-TABLE t-cliente LIKE gn-clie.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

/* PRIMERO LOS CLIENTES */
RUN Cliente-No-Perceptor.


chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

    FOR EACH t-cliente:
        FIND FIRST vtatabla WHERE vtatabla.codcia = s-codcia
            AND vtatabla.tabla = s-tabla
            AND VtaTabla.Llave_c1 = t-cliente.codcli
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE vtatabla THEN DO:
            CREATE vtatabla.
            ASSIGN
                VtaTabla.CodCia = s-codcia
                VtaTabla.Tabla = s-tabla
                VtaTabla.Llave_c1 = t-cliente.codcli
                VtaTabla.Llave_c2 = t-cliente.nomcli.
/*             DISPLAY 'CLIENTE:' t-cliente.codcli t-cliente.nomcli WITH STREAM-IO NO-LABELS WIDTH 320. */
/*             PAUSE 0.                                                                                 */
        END.
    END.
    /* ahora al reves */
    FOR EACH VtaTabla WHERE VtaTabla.codcia = s-codcia 
        AND VtaTabla.tabla = s-Tabla:
        FIND FIRST t-cliente WHERE t-cliente.codcli = VtaTabla.Llave_c1
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE t-cliente THEN DELETE vtatabla.
    END.



PROCEDURE Cliente-No-Perceptor:
/* **************************** */

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
            t-cliente.codcli = cValue
            NO-ERROR.
        

        t-column = t-column + 1.
        cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
        ASSIGN
            t-cliente.nomcli = TRIM(cValue).
    END.

/*     FOR EACH VtaTabla WHERE VtaTabla.codcia = s-codcia                */
/*         AND VtaTabla.tabla = s-Tabla:                                 */
/*         DELETE VtaTabla.                                              */
/*     END.                                                              */
/*     FOR EACH t-cliente:                                               */
/*         DISPLAY t-cliente.codcli. PAUSE 0.                            */
/*         CREATE VtaTabla.                                              */
/*         ASSIGN                                                        */
/*             VtaTabla.CodCia = s-codcia                                */
/*             VtaTabla.Tabla = s-tabla                                  */
/*             VtaTabla.Llave_c1 = t-cliente.codcli                      */
/*             VtaTabla.Llave_c2 = t-cliente.nomcli.                     */
/*         DISPLAY 'CLIENTE:' t-cliente.codcli WITH STREAM-IO NO-LABELS. */
/*         PAUSE 0.                                                      */
/*     END.                                                              */

END PROCEDURE.

