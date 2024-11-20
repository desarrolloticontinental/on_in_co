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

PROMPT-FOR 'Periodo:' s-periodo FORMAT '9999' SKIP
    'Mes:' s-nromes FORMAT '99' SKIP
    'Libro:' s-codope FORMAT 'x(3)' SKIP
    'Moneda:' s-codmon 
    WITH FRAME f-frame NO-LABELS.
ASSIGN FRAME f-frame s-periodo s-nromes s-codope s-codmon .
FIND cb-oper WHERE cb-oper.codcia = cb-codcia
    AND cb-oper.codope = s-codope
    NO-LOCK NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.
/*MESSAGE 'Ingrese la moneda (1 o 2)' UPDATE s-codmon s-periodo.*/

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
REPEAT:
    ASSIGN
        s-nroitm = s-nroitm + 1
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    /* CUENTA */
    CREATE T-DMOV.
    ASSIGN
        T-DMOV.NroItm = s-nroitm
        T-DMOV.tpocmb = s-tpocmb
        T-DMOV.codcta = STRING(DECIMAL(cValue), '99999999').
    /* AUXILIAR */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMOV.codaux = cValue.
    /* FECHA */
    t-Column = t-Column + 2.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMOV.FchDoc = DATE(cValue).
    /* DIVISION */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMOV.coddiv = (IF cValue <> ? THEN cValue ELSE "").
    /* CCO */
/*     t-Column = t-Column + 1.                               */
/*     cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.     */
/*     ASSIGN                                                 */
/*         T-DMOV.cco = (IF cValue <> ? THEN cValue ELSE ""). */
    /* COD DOC */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMOV.coddoc = cValue.
    /* NRO DOCU */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMOV.nrodoc = cValue.
    /* VENCIMIENTO */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMOV.FchVto = DATE(cValue).
    /* REFERENCIA */
/*     t-Column = t-Column + 1.                           */
/*     cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE. */
/*     ASSIGN                                             */
/*         T-DMOV.nroref = cValue.                        */
    /* GLOSA */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        T-DMOV.GloDoc = cValue.
    /* MONEDA */
    ASSIGN
        T-DMOV.CodMon = s-codmon.
    /* CLASIFICACION AUXILIAR */
    FIND cb-ctas WHERE cb-ctas.codcia = cb-codcia
        AND cb-ctas.codcta = T-DMOV.codcta
        NO-LOCK NO-ERROR.
    IF AVAILABLE cb-ctas THEN T-DMOV.clfaux = cb-ctas.ClfAux.
    /* IMPORTES */
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        dValue = DECIMAL(cValue).
    IF dValue > 0 THEN DO:
        IF s-codmon = 1 THEN
            ASSIGN
            T-DMOV.ImpMn1 = dValue
            T-DMOV.TpoMov = NO.
        ELSE ASSIGN
            T-DMOV.ImpMn2 = dValue
            T-DMOV.TpoMov = NO.
    END.
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        dValue = DECIMAL(cValue).
    IF dValue > 0 THEN DO:
        IF s-codmon = 1 THEN
            ASSIGN
            T-DMOV.ImpMn1 = dValue
            T-DMOV.TpoMov = YES.
        ELSE ASSIGN
            T-DMOV.ImpMn2 = dValue
            T-DMOV.TpoMov = YES.
    END.
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

RUN cbd/cbdnast (
    cb-codcia, 
    s-codcia, 
    s-periodo, 
    s-NroMes, 
    s-codope, 
    OUTPUT s-nroast). 

CREATE cb-cmov.
ASSIGN 
    cb-cmov.CodCia  = s-codcia
    cb-cmov.Periodo = s-periodo
    cb-cmov.NroMes  = s-NroMes
    cb-cmov.CodOpe  = s-CodOpe
    cb-cmov.NroAst  = STRING(s-NroAst,"999999")
    cb-cmov.FchAst  = TODAY
    cb-cmov.Usuario = s-user-id
    cb-cmov.CodMon = s-codmon.

/* Voucher atrazados colocamos el ultimo dia del mes como fecha de registro */
IF s-NroMes = 12
    THEN cb-cmov.FchAst  = DATE( 1, 1, s-periodo + 1) - 1.
ELSE cb-cmov.FchAst  = DATE( s-NroMes + 1, 1, s-periodo) - 1.
/* Buscando el Tipo de Cambio que le corresponde */
FIND LAST gn-tcmb WHERE gn-tcmb.FECHA <= cb-cmov.FchAst NO-LOCK NO-ERROR.
IF AVAILABLE gn-tcmb THEN
    IF cb-oper.TpoCmb = 1 THEN
        cb-cmov.TpoCmb = gn-tcmb.Compra.
    ELSE cb-cmov.TpoCmb = gn-tcmb.Venta.
FOR EACH T-DMOV:
    CREATE cb-dmov.
    BUFFER-COPY cb-cmov
        TO cb-dmov
        ASSIGN 
        cb-dmov.nroitm = T-DMOV.nroitm
        cb-dmov.codcta = T-DMOV.codcta
        cb-dmov.codaux = T-DMOV.codaux
        cb-dmov.clfaux = T-DMOV.clfaux
        cb-dmov.fchdoc = T-DMOV.fchdoc
        cb-dmov.coddiv = T-DMOV.coddiv
        cb-dmov.cco    = T-DMOV.cco
        cb-dmov.coddoc = T-DMOV.coddoc
        cb-dmov.nrodoc = T-DMOV.nrodoc
        cb-dmov.fchvto = T-DMOV.fchvto
        cb-dmov.nroref = T-DMOV.nroref
        cb-dmov.glodoc = T-DMOV.glodoc
        cb-dmov.tpomov = T-DMOV.tpomov
        cb-dmov.impmn1 = T-DMOV.impmn1
        cb-dmov.impmn2 = T-DMOV.impmn2.
    IF cb-dmov.codmon = 2 THEN cb-dmov.impmn1 = cb-dmov.impmn2 * cb-cmov.tpocmb.
END.

ASSIGN  
    cb-cmov.HbeMn1 = 0
    cb-cmov.HbeMn2 = 0
    cb-cmov.HbeMn3 = 0
    cb-cmov.DbeMn1 = 0
    cb-cmov.DbeMn2 = 0
    cb-cmov.DbeMn3 = 0.
FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.CodCia  = cb-cmov.codcia  
    AND cb-dmov.Periodo = cb-cmov.periodo 
    AND cb-dmov.NroMes  = cb-cmov.NroMes  
    AND cb-dmov.CodOpe  = cb-cmov.CodOpe  
    AND cb-dmov.NroAst  = cb-cmov.NroAst:
    IF cb-dmov.TpoMov THEN     /* Tipo H */
        ASSIGN  cb-cmov.HbeMn1 = cb-cmov.HbeMn1 + cb-dmov.ImpMn1
                cb-cmov.HbeMn2 = cb-cmov.HbeMn2 + cb-dmov.ImpMn2
                cb-cmov.HbeMn3 = cb-cmov.HbeMn3 + cb-dmov.ImpMn3.
    ELSE 
        ASSIGN cb-cmov.DbeMn1 = cb-cmov.DbeMn1 + cb-dmov.ImpMn1
               cb-cmov.DbeMn2 = cb-cmov.DbeMn2 + cb-dmov.ImpMn2
               cb-cmov.DbeMn3 = cb-cmov.DbeMn3 + cb-dmov.ImpMn3.
END.         


