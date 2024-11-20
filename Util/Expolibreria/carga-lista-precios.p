DEF VAR x-linea AS CHAR.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '10015'.
DEF VAR x-CToTot AS DEC NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.


DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.

SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xls", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    RETURN-TO-START-DIR 
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN.

DEFINE VARIABLE chExcelApplication AS COM-HANDLE.
DEFINE VARIABLE chWorkbook AS COM-HANDLE.
DEFINE VARIABLE chWorksheet AS COM-HANDLE.

DEFINE VARIABLE cRange          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCountLine      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iTotalColumn    AS INTEGER      NO-UNDO.
DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iValue          AS INTEGER      NO-UNDO.
DEFINE VARIABLE dValue          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE t-Column        AS INTEGER INIT 1.
DEFINE VARIABLE t-Row           AS INTEGER INIT 11.

DEFINE VARIABLE i-Column        AS INTEGER     NO-UNDO.

CREATE "Excel.Application" chExcelApplication.

chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).

t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 0
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 

    /* PROVEEDOR */
    cValue = chWorkSheet:Cells(t-Row, 1):VALUE.
    IF cValue <> '51135890' THEN NEXT.

    /* PRODUCTO  */ 
    cValue = chWorkSheet:Cells(t-Row, 5):VALUE.
    FIND almmmatg WHERE codcia = s-codcia
        AND codmat = cValue
        NO-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        NEXT.
    END.
    /*DISPLAY cValue.*/
    FIND VtaListaMay WHERE VtaListaMay.CodCia = s-codcia
        AND VtaListaMay.CodDiv = s-coddiv
        AND VtaListaMay.CodMat = cValue
        NO-ERROR.
    IF NOT AVAILABLE VtaListaMay THEN CREATE VtaListaMay.
    ASSIGN
        VtaListaMay.CodCia = s-codcia
        VtaListaMay.CodDiv = s-coddiv
        VtaListaMay.CodMat = cValue
        vtalistamay.desmat = almmmatg.desmat
        VtaListaMay.Chr__01 = almmmatg.CHR__01
        Vtalistamay.canemp = almmmatg.canemp
        VtaListaMay.FchIng = TODAY
        VtaListaMay.codfam = Almmmatg.codfam
        VtaListaMay.DesMar = Almmmatg.desmar
        VtaListaMay.DesMat = Almmmatg.desmat
        VtaListaMay.subfam = Almmmatg.subfam
        VtaListaMay.FchAct  = TODAY
        VtaListaMay.usuario = "ADMIN"
        VtaListaMay.MonVta = 1
        VtaListaMay.TpoCmb = Almmmatg.TpoCmb.
    dValue = DECIMAL(chWorkSheet:Cells(t-Row, 21):VALUE).
    ASSIGN
        VtaListaMay.PreOfi = dValue.
    dValue = DECIMAL(chWorkSheet:Cells(t-Row, 22):VALUE).
    ASSIGN
        VtaListaMay.PromDto = dValue * 100.
    ASSIGN
        VtaListaMay.PromFchD = TODAY 
        VtaListaMay.PromFchH = 03/31/2013.
    dValue = DECIMAL(chWorkSheet:Cells(t-Row, 12):VALUE).
    ASSIGN
        VtaListaMay.CanEmp = dValue.
    IF Almmmatg.monvta = VTalistamay.monvta THEN x-CtoTot = Almmmatg.CtoTot.
    ELSE IF Vtalistamay.monvta = 1 THEN x-CtoTot = Almmmatg.ctotot *  Almmmatg.tpocmb.
    ELSE x-CtoTot = Almmmatg.ctotot /  Almmmatg.tpocmb.
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
        AND Almtconv.Codalter = VtaListaMay.Chr__01
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almtconv THEN DO:
        F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
    END.  
    ASSIGN
        VtaListaMay.Dec__01 = ( (VtaListaMay.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100. 
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

/*
INPUT FROM c:\tmp\standford.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND almmmatg WHERE codcia = 001
        AND codmat = SUBSTRING(x-linea,1,6)
        NO-LOCK NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        DISPLAY x-linea.
        NEXT.
    END.
    FIND VtaListaMay WHERE VtaListaMay.CodCia = s-codcia
        AND VtaListaMay.CodDiv = s-coddiv
        AND VtaListaMay.CodMat = SUBSTRING(x-linea,1,6)
        NO-ERROR.
    IF NOT AVAILABLE VtaListaMay THEN CREATE VtaListaMay.
    ASSIGN
        VtaListaMay.CodCia = s-codcia
        VtaListaMay.CodDiv = s-coddiv
        VtaListaMay.CodMat = SUBSTRING(x-linea,1,6)
        vtalistamay.desmat = almmmatg.desmat
        VtaListaMay.Chr__01 = almmmatg.CHR__01
        Vtalistamay.canemp = almmmatg.canemp
        VtaListaMay.FchIng = TODAY
        VtaListaMay.codfam = Almmmatg.codfam
        VtaListaMay.DesMar = Almmmatg.desmar
        VtaListaMay.DesMat = Almmmatg.desmat
        VtaListaMay.subfam = Almmmatg.subfam
        VtaListaMay.FchAct  = TODAY
        VtaListaMay.usuario = "ADMIN"
        VtaListaMay.MonVta = 1
        VtaListaMay.TpoCmb = Almmmatg.TpoCmb
        VtaListaMay.PreOfi = DECIMAL(SUBSTRING(x-linea,21,10))
        VtaListaMay.PromDto = DECIMAL(SUBSTRING(x-linea,31))
        VtaListaMay.PromFchD = TODAY 
        VtaListaMay.PromFchH = 03/31/2013
        /*VtaListaMay.CHR__01 = SUBSTRING(x-linea,11,5)*/
        VtaListaMay.CanEmp = DECIMAL(SUBSTRING(x-linea,16,5)).
    IF ALmmmatg.monvta = VTalistamay.monvta THEN x-CtoTot = Almmmatg.CtoTot.
    ELSE IF Vtalistamay.monvta = 1 THEN x-CtoTot = Almmmatg.ctotot *  Almmmatg.tpocmb.
    ELSE x-CtoTot = Almmmatg.ctotot /  Almmmatg.tpocmb.
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
        AND Almtconv.Codalter = VtaListaMay.Chr__01
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almtconv THEN DO:
        F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
    END.  
    ASSIGN
        VtaListaMay.Dec__01 = ( (VtaListaMay.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100. 
END.
*/
