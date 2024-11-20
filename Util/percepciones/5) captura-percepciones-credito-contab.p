DEFINE VARIABLE s-codcia AS INT INIT 001 NO-UNDO.
DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.
DEFINE VARIABLE FechaPercepcion AS DATE NO-UNDO.

/* Datos del Comprobante de Percepción */
DEF VAR s-nroser AS INT INIT 008 NO-UNDO.
DEF VAR s-nrodoc AS INT INIT 000492 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'PER' NO-UNDO.

ASSIGN
    FechaPercepcion = 02/28/2015.

DEFINE BUFFER b-cdocu FOR ccbcdocu.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls,*xlsx", "Todos (*.*)" "*.*"
    TITLE "*** PERCEPCIONES AL CREDITO ***"
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
DEF TEMP-TABLE T-CMVTO LIKE ccbcmvto.
DEF TEMP-TABLE T-DMVTO LIKE ccbdmvto.

DEF BUFFER BT-DMVTO FOR T-DMVTO.

ASSIGN
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 0
        t-Row    = t-Row + 1.
    /* DIVISION */
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 

    CREATE T-DMVTO.
    ASSIGN
        T-DMVTO.codcia = s-codcia
        T-DMVTO.coddoc = 'PER'
        T-DMVTO.coddiv = cValue.
    /* CORRELATIVO */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMVTO.nrodoc = cValue.
    /* FECHA */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMVTO.fchemi = DATE(cValue).

    /* I/C */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMVTO.nrodep = cValue.
    /* CLIENTE */
    t-Column = t-Column + 2.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMVTO.codcli = cValue.
    /* REF */
    t-Column = t-Column + 2.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMVTO.codref = cValue.
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMVTO.nroref = cValue.
    /* PRECIO VENTA */
    t-Column = t-Column + 3.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMVTO.impdoc = DECIMAL(cValue).
    /* % PERCEPCION */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMVTO.impint = DECIMAL(cValue).
    /* PERCEPCION */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMVTO.imptot = DECIMAL(cValue).
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

/* PARCHE: LAS NOTAS DE CREDITO DE PASAN A LA DIVISION DEL COMPROBANTE DE REFERENCIA */
FOR EACH T-DMVTO WHERE T-DMVTO.codref = "N/C":
    FIND FIRST BT-DMVTO WHERE BT-DMVTO.codcli = T-DMVTO.codcli
        AND BT-DMVTO.codref <> "N/C"
        NO-LOCK NO-ERROR.
    IF AVAILABLE BT-DMVTO THEN T-DMVTO.coddiv = BT-DMVTO.coddiv.
END.
/* ********************************************************************************** */
DEF VAR s-rowid  AS ROWID NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF ccbcmvto.
DISABLE TRIGGERS FOR LOAD OF ccbdmvto.

DEF VAR x-CuentaItems AS INT INIT 0 NO-UNDO.
DEF VAR x-CambiaSerie AS LOG INIT YES NO-UNDO.
FOR EACH T-DMVTO BREAK BY T-DMVTO.coddiv BY T-DMVTO.codcli BY T-DMVTO.nrodoc:
    IF FIRST-OF(T-DMVTO.coddiv) OR FIRST-OF(T-DMVTO.codcli) OR x-CuentaItems > 11
        THEN DO:
        CREATE Ccbcmvto.
        ASSIGN
            CcbCMvto.CodCia = s-codcia
            CcbCMvto.CodDiv = T-DMVTO.coddiv
            CcbCMvto.CodDoc = s-coddoc
            CcbCMvto.CodCli = T-DMVTO.codcli
            CcbCMvto.CodMon = 1
            CcbCMvto.FchDoc = FechaPercepcion
            CcbCMvto.FlgEst = "P"
            CcbCMvto.Glosa = ""
            CcbCMvto.ImpTot = 0
            CcbCMvto.NroDoc = STRING(s-nroser, '999') + STRING(s-nrodoc, '999999')
            CcbCMvto.Libre_chr[1] = "I/C"
            CcbCMvto.Libre_chr[2] = T-DMVTO.nrodep
            CcbCMvto.Libre_chr[3] = "CANCELACION"
            CcbCMvto.Usuario = "ADMIN".
        s-nrodoc = s-nrodoc + 1.
        s-rowid = ROWID(Ccbcmvto).
        x-CuentaItems = 0.
/*         IF x-CambiaSerie = YES AND s-nroser = 914 */
/*             AND s-nrodoc > 10000 THEN DO:         */
/*             x-CambiaSerie = NO.                   */
/*             s-nroser = 008.                       */
/*             s-nrodoc = 000001.                    */
/*         END.                                      */
    END.
    FIND Ccbcmvto WHERE ROWID(Ccbcmvto) = s-rowid NO-LOCK.
    CREATE Ccbdmvto.
    BUFFER-COPY Ccbcmvto
        TO Ccbdmvto
        ASSIGN
        /*Ccbdmvto.codope = T-DMvto.CodOpe
        Ccbdmvto.nroast = T-DMvto.NroAst*/
        Ccbdmvto.codref = T-DMvto.CodRef
        Ccbdmvto.nroref = T-DMvto.NroRef
        Ccbdmvto.imptot = T-DMvto.ImpTot
        Ccbdmvto.impdoc = T-DMvto.ImpDoc
        Ccbdmvto.ImpInt = T-Dmvto.ImpInt.
    x-CuentaItems = x-CuentaItems + 1.
END.
