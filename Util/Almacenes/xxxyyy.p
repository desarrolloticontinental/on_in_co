DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xlsx)" "*.xls,*.xlsx", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN.

DEF BUFFER B-DPEDI FOR Facdpedi.

DEF TEMP-TABLE t-cpedi LIKE faccpedi.
DEF TEMP-TABLE t-dpedi LIKE facdpedi
    FIELD codnew LIKE facdpedi.codmat
    FIELD cannew LIKE facdpedi.canped
    FIELD factornew LIKE facdpedi.factor.

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddoc AS CHAR INIT 'COT' NO-UNDO.
DEF VAR s-coddiv AS CHAR NO-UNDO.
DEF VAR s-nroped AS CHAR NO-UNDO.
DEF VAR s-codmat AS CHAR NO-UNDO.

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

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

DEFINE VAR iColumn AS INT.
REPEAT iColumn = 2 TO 65000:
    /* División */
    cRange = "A" + TRIM(STRING(iColumn)).
    cValue = chWorkSheet:Range(cRange):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */
    s-coddiv = TRIM(cValue).
    /* Número */
    cRange = "C" + TRIM(STRING(iColumn)).
    cValue = chWorkSheet:Range(cRange):VALUE.
    s-nroped = TRIM(cValue).
    FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
        AND Faccpedi.coddiv = s-coddiv
        AND Faccpedi.coddoc = s-coddoc
        AND Faccpedi.nroped = s-nroped
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccpedi THEN DO:
        DISPLAY 'no cabecera' s-codcia s-coddiv s-coddoc s-nroped.
        RETURN.
    END.
    /* Producto */
    cRange = "K" + TRIM(STRING(iColumn)).
    cValue = chWorkSheet:Range(cRange):VALUE.
    s-codmat = SUBSTRING(cValue,1,6).
    FIND Facdpedi OF Faccpedi WHERE Facdpedi.codmat = s-codmat NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Facdpedi THEN DO:
        DISPLAY 'no detalle' s-codcia s-coddiv s-coddoc s-nroped FORMAT 'x(9)'  s-codmat.
        RETURN.
    END.
    CREATE t-dpedi.
    BUFFER-COPY Facdpedi TO t-dpedi.
    /* Espejo */
    cRange = "AG" + TRIM(STRING(iColumn)).
    cValue = chWorkSheet:Range(cRange):VALUE.
    s-codmat = TRIM(cValue).
    IF TRUE <> (s-codmat > '') THEN NEXT.
    ASSIGN
        t-dpedi.codnew = s-codmat.
    cRange = "AH" + TRIM(STRING(iColumn)).
    cValue = chWorkSheet:Range(cRange):VALUE.
    ASSIGN
        t-dpedi.cannew = DECIMAL(cValue).
    cRange = "AI" + TRIM(STRING(iColumn)).
    cValue = chWorkSheet:Range(cRange):VALUE.
    ASSIGN
        t-dpedi.factornew = DECIMAL(cValue).
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

FOR EACH t-dpedi WHERE TRUE <> (t-dpedi.codnew > ''):
    DELETE t-dpedi.
END.
FOR EACH t-dpedi BREAK BY t-dpedi.coddiv BY t-dpedi.coddoc BY t-dpedi.nroped: 
    IF FIRST-OF(t-dpedi.coddiv) OR FIRST-OF(t-dpedi.coddoc) OR FIRST-OF(t-dpedi.nroped) THEN DO:
        FIND Faccpedi WHERE Faccpedi.codcia = t-dpedi.codcia
            AND Faccpedi.coddiv = t-dpedi.coddiv
            AND Faccpedi.coddoc = t-dpedi.coddoc
            AND Faccpedi.nroped = t-dpedi.nroped
            NO-LOCK.
        CREATE t-cpedi.
        BUFFER-COPY Faccpedi TO t-cpedi.
    END.
END.

DEF VAR s-nroser AS INT NO-UNDO.
DEF VAR s-codalm AS CHAR.

FOR EACH t-cpedi:
    /* Cabecera */
    ASSIGN
        s-coddiv = t-cpedi.coddiv
        s-nroser = INTEGER(SUBSTRING(t-cpedi.nroped,1,3))
        s-codalm = t-cpedi.codalm.

    CREATE Faccpedi.
    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}
    BUFFER-COPY t-cpedi TO Faccpedi
        ASSIGN 
        FacCPedi.FchPed = TODAY 
        FacCPedi.Hora   = STRING(TIME, 'HH:MM:SS')
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.CodRef = t-cpedi.coddoc
        FacCPedi.NroRef = t-cpedi.nroped.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    FOR EACH B-DPEDI OF T-CPEDI NO-LOCK:
        CREATE Facdpedi.
        BUFFER-COPY B-DPEDI TO Facdpedi
            ASSIGN
            FacDPedi.CodCia = FacCPedi.CodCia
            FacDPedi.CodDiv = FacCPedi.CodDiv
            FacDPedi.coddoc = FacCPedi.coddoc
            FacDPedi.NroPed = FacCPedi.NroPed
            FacDPedi.FchPed = FacCPedi.FchPed
            FacDPedi.Hora   = FacCPedi.Hora 
            FacDPedi.FlgEst = FacCPedi.FlgEst.
        /* Buscamos código equivalente */
        FIND T-DPEDI WHERE T-DPEDI.codcia = Facdpedi.codcia
            AND T-DPEDI.coddiv = Facdpedi.coddiv
            AND T-DPEDI.coddoc = Facdpedi.coddoc
            AND T-DPEDI.nroped = Facdpedi.nroped
            AND T-DPEDI.codmat = Facdpedi.codmat
            NO-LOCK NO-ERROR.
        IF AVAILABLE T-DPEDI THEN DO:
            ASSIGN
                Facdpedi.codmat = T-DPEDI.codnew
                Facdpedi.canped = T-DPEDI.cannew
                Facdpedi.canate = T-DPEDI.canate * T-DPEDI.factornew
                Facdpedi.preuni = T-DPEDI.preuni / T-DPEDI.factornew.
        END.
    END.
END.


FOR EACH t-cpedi:
    FIND faccpedi WHERE faccpedi.codcia = t-cpedi.codcia
        AND faccpedi.coddiv = t-cpedi.coddiv
        AND faccpedi.coddoc = t-cpedi.coddoc
        AND faccpedi.nroped = t-cpedi.nroped.
    ASSIGN faccpedi.flgest = "X".
END.


