DEFINE VARIABLE s-ptovta  AS INT.
DEFINE VARIABLE x-coddoc  AS CHAR INITIAL "A/R" NO-UNDO.
DEF VAR s-codcia    AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv    AS CHAR INIT '10060' NO-UNDO.
DEF VAR s-coddoc    AS CHAR INIT 'I/C' NO-UNDO.
DEF VAR s-tipo      AS CHAR INITIAL "ANTREC" NO-UNDO.
DEF VAR s-codter    AS CHAR INIT 'EXPOAREQUIPA' NO-UNDO.
DEF VAR x-importe AS DEC NO-UNDO.
DEF VAR x-moneda AS CHAR NO-UNDO.
DEF VAR s-user-id AS CHAR INIT 'SYSTEM' NO-UNDO.
DEF VAR X-CTASOL AS CHAR INIT "122101".
DEF VAR X-CTADOL AS CHAR INIT "122102".
DEF VAR F-CODDOC AS CHAR INIT "A/R".
DEF VAR F-NRODOC AS CHAR.
DEF VAR Y-CODMON AS INTEGER.
DEF VAR x-FmaPgo AS CHAR.

FIND CcbDTerm WHERE 
     CcbDTerm.CodCia = s-codcia AND
     CcbDTerm.CodDiv = s-coddiv AND
     CcbDTerm.CodDoc = s-coddoc AND
     CcbDTerm.CodTer = s-codter NO-LOCK NO-ERROR.
IF NOT AVAILABLE ccbdterm THEN DO:
    MESSAGE
        "Ingreso de caja no esta configurado en este terminal"
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.
s-ptovta = ccbdterm.nroser.
  
/* Control de correlativos */
FIND FIRST FacCorre WHERE 
    faccorre.codcia = s-codcia AND 
    faccorre.coddoc = s-coddoc AND
    faccorre.nroser = s-ptovta AND
    FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    MESSAGE
        "No esta definida la serie" s-ptovta SKIP
        "para el ingreso a caja" VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

FIND FIRST FacCorre WHERE 
    faccorre.codcia = s-codcia AND 
    faccorre.coddoc = x-coddoc AND
    faccorre.coddiv = s-coddiv AND
    FacCorre.FlgEst = YES
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    MESSAGE
        "No esta definido Documento Anticipo para la Division" SKIP
        "Verifique Por Favor....."
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.


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


DEF TEMP-TABLE detalle LIKE Ccbccaja.
DISABLE TRIGGERS FOR LOAD OF Ccbccaja.

CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

/* CARGAMOS EL TEMPORAL */
ASSIGN
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 1
        t-Row    = t-Row + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    CREATE Detalle.
    ASSIGN
        Detalle.codcia = s-codcia
        Detalle.coddiv = s-coddiv
        Detalle.coddoc = s-coddoc
        Detalle.tipo   = s-tipo
        Detalle.Nroast = STRING(INTEGER(cValue),'9999999')
        Detalle.nrodoc = STRING(INTEGER(cValue),'9999999').
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.fchdoc = DATE(cValue)
        Detalle.fchcie = DATE(cValue).
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.HorCie = cValue.
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.CodCli = STRING(DECIMAL(cValue), '99999999999').
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.NomCli = cValue.
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.Glosa = cValue.
    t-Column = t-Column + 2.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-Importe = DECIMAL(cValue).
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-moneda = cValue.

    IF x-moneda = 'PEN' THEN ASSIGN Detalle.ImpNac[1] = x-importe Detalle.codmon = 1.
    ELSE ASSIGN Detalle.ImpUsa[1] = x-importe Detalle.codmon = 2.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        x-FmaPgo = cValue.
/*     CASE TRUE:                                                                                */
/*         WHEN x-FmaPgo = 'CHQ' THEN DO:                                                        */
/*             IF x-moneda = 'PEN' THEN ASSIGN Detalle.ImpNac[2] = x-importe Detalle.codmon = 1. */
/*             ELSE ASSIGN Detalle.ImpUsa[2] = x-importe Detalle.codmon = 2.                     */
/*         END.                                                                                  */
/*         WHEN x-FmaPgo = 'TRNF' THEN DO:                                                       */
/*             IF x-moneda = 'PEN' THEN ASSIGN Detalle.ImpNac[5] = x-importe Detalle.codmon = 1. */
/*             ELSE ASSIGN Detalle.ImpUsa[5] = x-importe Detalle.codmon = 2.                     */
/*         END.                                                                                  */
/*     END CASE.                                                                                 */

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.Glosa = cValue.

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF x-FmaPgo = 'CHQ' THEN Detalle.Voucher[2] = STRING(INTEGER(cValue)).
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF x-FmaPgo = 'TRNF' THEN Detalle.Voucher[5] = STRING(INTEGER(cValue)).

END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

/* CARGAMOS INGRESOS */

FOR EACH detalle NO-LOCK WHERE detalle.codcia = s-codcia
    AND detalle.coddoc = s-coddoc
    AND detalle.nrodoc <> '' BY detalle.nrodoc:
    FIND LAST gn-tccja WHERE gn-tccja.Fecha <= Detalle.fchdoc NO-LOCK NO-ERROR.

    FIND FacCorre WHERE 
        FacCorre.CodCia = s-codcia AND
        FacCorre.CodDoc = s-coddoc AND 
        FacCorre.NroSer = s-ptovta
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE FacCorre THEN UNDO, RETURN.
    CREATE Ccbccaja.
    BUFFER-COPY Detalle
        TO Ccbccaja
        ASSIGN
        CcbCCaja.NroDoc  = STRING(faccorre.nroser, "999") + STRING(faccorre.correlativo, "999999")
        CcbCCaja.usuario = s-user-id
        CcbCCaja.CodCaja = S-CODTER
        CcbCCaja.TpoCmb  = (IF AVAILABLE gn-tccja THEN gn-tccja.Compra ELSE 0)
        CcbcCaja.Flgest    = "C"
        CcbCCaja.Voucher[1] = STRING(F-Coddoc,"X(3)") + STRING(F-Nrodoc,"X(9)")
        CcbCCaja.Voucher[10] = STRING(TIME,"HH:MM:SS").
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1. 
    Y-CodMon = Detalle.CodMon.
    IF Detalle.CodMon = 2 THEN CcbCCaja.CodCta[1] = X-CTADOL.
    IF Detalle.CodMon = 1 THEN CcbCCaja.CodCta[1] = X-CTASOL.
    CREATE CcbDCaja.
    ASSIGN
        CcbDCaja.CodCia = CcbCCaja.CodCia   
        CcbDCaja.CodDoc = CcbCCaja.CodDoc
        CcbDCaja.NroDoc = CcbCCaja.NroDoc
        CcbDCaja.CodMon = Y-CodMon
        CcbDCaja.CodRef = CcbCCaja.CodDoc
        CcbDCaja.NroRef = CcbCCaja.NroDoc
        CcbDCaja.FchDoc = CcbCCaja.FchDoc
        CcbDCaja.CodDiv = CcbCCaja.CodDiv
        CcbDCaja.TpoCmb = CcbCCaja.TpoCmb.
    IF Y-CodMon = 1 THEN DO:
        CcbDCaja.ImpTot = CcbCCaja.ImpNac[1].
    END.
    IF Y-CodMon = 2 THEN DO:
        CcbDCaja.ImpTot = CcbCCaja.ImpUsa[1].
    END.
    RUN Cancelar-Documento.
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN.
    DISPLAY ccbccaja.coddiv ccbccaja.coddoc ccbccaja.nrodoc ccbccaja.fchdoc WITH STREAM-IO.
    PAUSE 0.
END.


PROCEDURE Cancelar-Documento:

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FIND FIRST FacCorre WHERE
        FacCorre.CodCia = s-codcia AND
        FacCorre.Coddiv = s-coddiv AND
        FacCorre.CodDoc = f-coddoc AND
        FacCorre.FlgEst = YES
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE FacCorre THEN RETURN 'ADM-ERROR'.
    CREATE CcbCDocu.
    ASSIGN
        CcbCDocu.Codcia = S-CODCIA
        CcbCDocu.CodDiv = S-CODDIV
        CcbCDocu.Coddoc = F-CODDOC
        CcbCDocu.NroDoc = STRING(faccorre.nroser, "999") + STRING(faccorre.correlativo, "999999")
        CcbCDocu.CodCli = CcbCcaja.Codcli
        CcbCDocu.NomCli = CcbCcaja.Nomcli
        CcbCDocu.CodRef = CcbCcaja.CodDoc
        CcbCDocu.NroRef = CcbCcaja.NroDoc
        CcbCDocu.FchDoc = TODAY
        CcbCDocu.FchVto = TODAY
        CcbCDocu.CodMon = Y-CODMON
        CcbCDocu.Usuario = S-USER-ID            
        CcbCDocu.FlgEst = "P"
        CcbCDocu.NroOrd = CcbCcaja.NroAst
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN UNDO, RETURN 'ADM-ERROR'.
    ASSIGN
        CcbCCaja.Voucher[1] = STRING(F-Coddoc,"X(3)") + CcbCDocu.NroDoc
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    IF Y-CodMon = 2 THEN DO:
        ASSIGN
            CcbCDocu.ImpTot = CcbCcaja.ImpUsa[1]
            CcbCDocu.SdoAct = CcbCcaja.ImpUsa[1].
    END.
    IF Y-CodMon = 1 THEN DO:
        ASSIGN
            CcbCDocu.ImpTot = CcbCcaja.ImpNac[1]
            CcbCDocu.SdoAct = CcbCcaja.ImpNac[1].
    END.
END.
RETURN 'OK'.

END PROCEDURE.
