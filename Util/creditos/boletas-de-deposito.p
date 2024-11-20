DEF VAR s-codcia AS INT INIT 001 NO-UNDO.

DEF TEMP-TABLE Detalle
    FIELD codcli LIKE gn-clie.codcli
    FIELD ruccli LIKE gn-clie.ruc
    FIELD nomcli LIKE gn-clie.nomcli FORMAT 'x(100)'
    FIELD coddiv LIKE gn-divi.coddiv FORMAT 'x(5)'
    FIELD desdiv LIKE GN-DIVI.DesDiv
    FIELD coddoc LIKE ccbcdocu.coddoc
    FIELD nrodoc LIKE ccbcdocu.nrodoc
    FIELD fchdoc LIKE ccbcdocu.fchdoc
    FIELD moneda AS CHAR FORMAT 'x(10)'
    FIELD imptot LIKE ccbcdocu.imptot
    FIELD codref LIKE ccbccaja.coddoc
    FIELD nroref LIKE ccbccaja.nrodoc
    FIELD fchref LIKE ccbccaja.fchdoc
    FIELD impref LIKE ccbdmov.imptot
    .


/* Archivo de Salida */
DEF VAR c-csv-file AS CHAR NO-UNDO.
DEF VAR c-xls-file AS CHAR INIT 'Archivo_Excel' NO-UNDO.
DEF VAR rpta AS LOG INIT NO NO-UNDO.

SYSTEM-DIALOG GET-FILE c-xls-file
    FILTERS 'Libro de Excel' '*.xlsx'
    INITIAL-FILTER 1
    ASK-OVERWRITE
    CREATE-TEST-FILE
    DEFAULT-EXTENSION ".xlsx"
    SAVE-AS
    TITLE "Guardar como"
    USE-FILENAME
    UPDATE rpta.
IF rpta = NO THEN RETURN.

SESSION:SET-WAIT-STATE('GENERAL').
/* Variable de memoria */
DEFINE VAR hProc AS HANDLE NO-UNDO.
/* Levantamos la libreria a memoria */
RUN lib\Tools-to-excel PERSISTENT SET hProc.

/* Cargamos la informacion al temporal */
EMPTY TEMP-TABLE Detalle.
RUN Carga-Temporal.

/* Programas que generan el Excel */
RUN pi-crea-archivo-csv IN hProc (INPUT BUFFER Detalle:HANDLE,
                                  INPUT c-xls-file,
                                  OUTPUT c-csv-file) .

RUN pi-crea-archivo-xls IN hProc (INPUT BUFFER Detalle:handle,
                                  INPUT  c-csv-file,
                                  OUTPUT c-xls-file) .

/* Borramos librerias de la memoria */
DELETE PROCEDURE hProc.
SESSION:SET-WAIT-STATE('').


PROCEDURE Carga-Temporal:
/* ********************* */
DEF VAR iCuenta AS INT NO-UNDO.
FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddoc = "BD"
    AND ccbcdocu.flgest <> "A",
    FIRST gn-divi OF ccbcdocu NO-LOCK:
    iCuenta = 0.
    CREATE Detalle.
    ASSIGN
        detalle.codcli = ccbcdocu.codcli
        detalle.ruccli = ccbcdocu.ruccli
        detalle.nomcli = ccbcdocu.nomcli
        detalle.coddiv = ccbcdocu.coddiv
        detalle.desdiv = gn-divi.desdiv
        detalle.coddoc = ccbcdocu.coddoc
        detalle.nrodoc = ccbcdocu.nrodoc
        detalle.fchdoc = ccbcdocu.fchdoc
        detalle.moneda = (IF ccbcdocu.codmon = 1 THEN 'SOLES' ELSE 'DOLARES')
        detalle.imptot = ccbcdocu.imptot.
    FOR EACH ccbdmov NO-LOCK WHERE ccbdmov.codcia = ccbcdocu.codcia
        AND ccbdmov.coddoc = ccbcdocu.coddoc
        AND ccbdmov.nrodoc = ccbcdocu.nrodoc,
        FIRST ccbccaja NO-LOCK WHERE ccbccaja.codcia = ccbdmov.codcia
        AND ccbccaja.coddoc = ccbdmov.codref
        AND ccbccaja.nrodoc = ccbdmov.nroref:
        IF iCuenta = 0 THEN DO:
        END.
        ELSE DO:
            CREATE Detalle.
            ASSIGN
                detalle.codcli = ccbcdocu.codcli
                detalle.ruccli = ccbcdocu.ruccli
                detalle.nomcli = ccbcdocu.nomcli
                detalle.coddiv = ccbcdocu.coddiv
                detalle.desdiv = gn-divi.desdiv
                detalle.coddoc = ccbcdocu.coddoc
                detalle.nrodoc = ccbcdocu.nrodoc
                detalle.fchdoc = ccbcdocu.fchdoc
                detalle.moneda = (IF ccbcdocu.codmon = 1 THEN 'SOLES' ELSE 'DOLARES')
                detalle.imptot = ccbcdocu.imptot.
        END.
        ASSIGN
            detalle.codref = ccbccaja.coddoc
            detalle.nroref = ccbccaja.nrodoc
            detalle.fchref = ccbccaja.fchdoc
            detalle.impref = ccbdmov.imptot
            .
        iCuenta = iCuenta + 1.
    END.
END.

END PROCEDURE.
