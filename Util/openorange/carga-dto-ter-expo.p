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
DEF VAR pCodDiv     AS CHAR INIT '10060' NO-UNDO.
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
    t-Column = t-Column + 2.
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

  FOR EACH vtatabla WHERE codcia = s-codcia AND tabla = 'DTOTEREXPO':
      DELETE vtatabla.
  END.
FOR EACH detalle NO-LOCK WHERE detalle.promdto > 0, FIRST almmmatg OF detalle NO-LOCK WHERE almmmatg.CHR__02 = "T":
    DISPLAY detalle.codmat.
    PAUSE 0.
    CREATE vtatabla.
    ASSIGN
        vtatabla.codcia = s-codcia
        vtatabla.tabla = 'DTOTEREXPO'
        vtatabla.llave_c1 = detalle.coddiv
        vtatabla.llave_c2 = detalle.codmat
        vtatabla.rango_fecha[1] = 01/01/2015
        vtatabla.rango_fecha[2] = 03/31/2015
        vtatabla.valor[1] = detalle.promdto.

END.

