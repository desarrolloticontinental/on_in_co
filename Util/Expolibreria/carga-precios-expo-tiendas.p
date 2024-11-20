DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
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
DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.

DEF VAR s-codcia    AS INT INIT 001 NO-UNDO.
DEF VAR pCodDiv     AS CHAR INIT '10015' NO-UNDO.
DEF TEMP-TABLE detalle LIKE Vtalistamay.

DISABLE TRIGGERS FOR LOAD OF VtaListaMay.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

/* CARGAMOS EL TEMPORAL */
ASSIGN
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 2
        t-Row    = t-Row + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    CREATE Detalle.
    ASSIGN
        Detalle.codcia = s-codcia
        Detalle.coddiv = pcoddiv
        Detalle.codmat = cValue.
    t-Column = t-Column + 2.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.CHR__01 = cValue.
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.preofi = DECIMAL(cValue).
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.PromDto = DECIMAL(cValue).
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

  DEF VAR x-CtoTot AS DEC NO-UNDO.
  DEF VAR f-Factor AS DEC NO-UNDO.

FOR EACH detalle, FIRST almmmatg OF detalle NO-LOCK:
    IF detalle.promdto > 0 THEN
        ASSIGN
        detalle.promfchd = 01/01/2015
        detalle.promfchh = 03/31/2015.
    ASSIGN
        detalle.TpoCmb = Almmmatg.TpoCmb
        detalle.MonVta = Almmmatg.monvta
        detalle.FchAct = TODAY
        detalle.CHR__01 = Almmmatg.CHR__01.
    /* REGRABAMOS EN LA MONEDA DE VENTA */
    IF Almmmatg.MonVta = 2 THEN detalle.PreOfi = detalle.PreOfi / Almmmatg.TpoCmb.
    /* MARGEN */
    IF Almmmatg.monvta = detalle.monvta THEN x-CtoTot = Almmmatg.CtoTot.
    ELSE IF detalle.monvta = 1 THEN x-CtoTot = Almmmatg.ctotot *  Almmmatg.tpocmb.
    ELSE x-CtoTot = Almmmatg.ctotot /  Almmmatg.tpocmb.
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
        AND Almtconv.Codalter = detalle.Chr__01
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almtconv THEN F-FACTOR = Almtconv.Equival.
    ASSIGN
        detalle.Dec__01 = ( (detalle.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100.
/*     DISPLAY detalle.codmat detalle.monvta detalle.preofi detalle.CHR__01 detalle.promdto detalle.promfchd detalle.promfchh */
/*         WITH STREAM-IO NO-BOX WIDTH 320.                                                                                   */
END.

FOR EACH vtalistamay WHERE vtalistamay.codcia = s-codcia AND vtalistamay.coddiv = pcoddiv:
    DELETE vtalistamay.
END.
FOR EACH detalle NO-LOCK WHERE detalle.codcia = 1 AND detalle.coddiv = pcoddiv AND detalle.codmat <> '':
    CREATE vtalistamay.
    BUFFER-COPY detalle TO vtalistamay.
END.
