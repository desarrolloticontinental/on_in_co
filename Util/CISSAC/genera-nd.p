DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.
DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.
DISABLE TRIGGERS FOR LOAD OF faccorre.


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
DEFINE VARIABLE i-Column        AS INTEGER     NO-UNDO.

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-nroitm AS INT.

DEF TEMP-TABLE t-cdocu LIKE ccbcdocu.

DEF BUFFER b-cdocu FOR ccbcdocu.
DEF BUFFER b-ddocu FOR ccbddocu.

DEF VAR x-coddoc LIKE ccbcdocu.coddoc NO-UNDO.
DEF VAR x-nrodoc LIKE ccbcdocu.nrodoc NO-UNDO.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

ASSIGN
    s-nroitm = 0
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        s-nroitm = s-nroitm + 1
        t-column = 0
        t-Row    = t-Row + 1.
    ASSIGN
        t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    ASSIGN
        x-CodDoc = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-NroDoc = cValue.

    FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddoc = x-coddoc
        AND ccbcdocu.nrodoc = x-nrodoc
        NO-LOCK.
    FIND t-cdocu WHERE t-cdocu.codcia = s-codcia
        AND t-cdocu.coddoc = x-coddoc
        AND t-cdocu.nrodoc = x-nrodoc
        NO-ERROR.
    IF NOT AVAILABLE t-cdocu THEN CREATE t-cdocu.
    BUFFER-COPY ccbcdocu 
        TO t-cdocu.
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        t-cdocu.imptot = DECIMAL(cValue).
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

FOR EACH t-cdocu WHERE t-cdocu.imptot <= 0:
    DELETE t-cdocu.
END.
FOR EACH t-cdocu:
    FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.coddoc = t-cdocu.coddoc
        AND ccbcdocu.nrodoc = t-cdocu.nrodoc
        NO-LOCK.
/*     DISPLAY t-cdocu.coddoc t-cdocu.nrodoc ccbcdocu.codmon ccbcdocu.imptot t-cdocu.imptot */
/*         WITH STREAM-IO NO-BOX WIDTH 320.                                                 */
    RUN Nota-de-Credito-Debito ("N/D", 110, "00003").
END.


PROCEDURE Nota-de-Credito-Debito:
/* ****************************** */

    DEF INPUT PARAMETER pCodDoc AS CHAR.
    DEF INPUT PARAMETER pNroSer AS INT.
    DEF INPUT PARAMETER pCodMat AS CHAR.

    FIND FIRST Faccfggn WHERE codcia = 1 NO-LOCK.
    FIND CcbTabla WHERE CcbTabla.CodCia = 1
        AND CcbTabla.Tabla  = pCodDoc
        AND CcbTabla.Codigo = pCodMat NO-LOCK.
    FIND Faccorre WHERE faccorre.codcia = 1
        AND Faccorre.coddoc = pCodDoc
        AND Faccorre.nroser = pNroSer
        EXCLUSIVE-LOCK.
    CREATE b-cdocu.
    ASSIGN
        b-cdocu.codcia = s-codcia
        b-cdocu.coddiv = '00000'
        b-cdocu.coddoc = pCodDoc
        b-cdocu.nrodoc = STRING(Faccorre.nroser, '999') + STRING(Faccorre.correlativo, '999999')
        b-cdocu.fchdoc = TODAY
        b-cdocu.codcli = ccbcdocu.codcli
        b-cdocu.ruccli = ccbcdocu.ruccli
        b-cdocu.nomcli = ccbcdocu.nomcli
        b-cdocu.dircli = ccbcdocu.dircli
        b-cdocu.porigv = ( IF ccbcdocu.porigv > 0 THEN ccbcdocu.porigv ELSE 18.00 )
        b-cdocu.codmon = ccbcdocu.codmon
        b-cdocu.usuario = 'ADMIN'
        b-cdocu.tpocmb = Faccfggn.tpocmb[1]
        b-cdocu.codref = ccbcdocu.coddoc
        b-cdocu.nroref = ccbcdocu.nrodoc
        b-cdocu.codven = ccbcdocu.codven
        b-cdocu.cndcre = 'N'
        b-cdocu.fmapgo = ccbcdocu.fmapgo.
    /* ACTUALIZAMOS EL CENTRO DE COSTO */
    FIND GN-VEN WHERE GN-VEN.codcia = s-codcia
        AND GN-VEN.codven = b-cdocu.codven NO-LOCK NO-ERROR.
    IF AVAILABLE GN-VEN THEN b-cdocu.cco = GN-VEN.cco.

    ASSIGN
        Faccorre.correlativo = Faccorre.correlativo + 1.

    CREATE b-ddocu.
    BUFFER-COPY b-cdocu TO b-ddocu.
    ASSIGN
        b-ddocu.codmat = CcbTabla.Codigo
        b-ddocu.factor = 1
        b-ddocu.candes = 1
        b-ddocu.preuni = t-cdocu.imptot
        b-ddocu.implin = t-cdocu.imptot.
    IF CcbTabla.Afecto THEN
        ASSIGN
        b-ddocu.AftIgv = Yes
        b-ddocu.ImpIgv = (b-ddocu.CanDes * b-ddocu.PreUni) * ((b-cdocu.PorIgv / 100) / (1 + (b-cdocu.PorIgv / 100))).
    ELSE
        ASSIGN
        b-ddocu.AftIgv = No
        b-ddocu.ImpIgv = 0.
    b-ddocu.NroItm = 1.

    ASSIGN
      b-cdocu.ImpBrt = 0
      b-cdocu.ImpExo = 0
      b-cdocu.ImpDto = 0
      b-cdocu.ImpIgv = 0
      b-cdocu.ImpTot = 0.
    FOR EACH b-ddocu OF b-cdocu NO-LOCK:
      ASSIGN
            b-cdocu.ImpBrt = b-cdocu.ImpBrt + (IF b-ddocu.AftIgv = Yes THEN b-ddocu.PreUni * b-ddocu.CanDes ELSE 0)
            b-cdocu.ImpExo = b-cdocu.ImpExo + (IF b-ddocu.AftIgv = No  THEN b-ddocu.PreUni * b-ddocu.CanDes ELSE 0)
            b-cdocu.ImpDto = b-cdocu.ImpDto + b-ddocu.ImpDto
            b-cdocu.ImpIgv = b-cdocu.ImpIgv + b-ddocu.ImpIgv
            b-cdocu.ImpTot = b-cdocu.ImpTot + b-ddocu.ImpLin.
    END.
    ASSIGN 
        b-cdocu.ImpVta = b-cdocu.ImpBrt - b-cdocu.ImpIgv
        b-cdocu.ImpBrt = b-cdocu.ImpBrt - b-cdocu.ImpIgv + b-cdocu.ImpDto
        b-cdocu.SdoAct = b-cdocu.ImpTot
        b-cdocu.FlgEst = 'P'.

END PROCEDURE.
