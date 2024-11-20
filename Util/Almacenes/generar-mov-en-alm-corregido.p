DEFINE VARIABLE s-codcia AS INT INIT 001 NO-UNDO.
DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.
DEFINE VARIABLE FechaPercepcion AS DATE NO-UNDO.

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

DEFINE TEMP-TABLE Detalle
    FIELD codcia AS INT
    FIELD tipmov AS CHAR
    FIELD codmov AS INT
    FIELD codalm AS CHAR
    FIELD nroord AS CHAR
    FIELD fchdoc AS DATE
    FIELD codmat AS CHAR
    FIELD codund AS CHAR
    FIELD candes AS DEC DECIMALS 4.

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

    CREATE Detalle.
    ASSIGN
        Detalle.codcia = s-codcia
        Detalle.tipmov = (IF cValue = 'Salida' THEN 'S' ELSE 'I').

    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.codmov = INTEGER(cValue) NO-ERROR.
    
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.codalm = cValue.
    
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.nroord = cValue.
    
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.fchdoc = DATE(cValue).
    
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.codmat = cValue.

    t-Column = t-Column + 2.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.codund = cValue.
    
    t-Column = t-Column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    ASSIGN
        Detalle.candes = DECIMAL(cValue).
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

DEF VAR s-nroser AS INT INIT 000 NO-UNDO.
DEF VAR C-ROWID AS ROWID NO-UNDO.
DEF VAR R-ROWID AS ROWID NO-UNDO.
DEF VAR n-Itm AS INT NO-UNDO.

FOR EACH detalle ,
    FIRST almtdocm OF detalle NO-LOCK,
    FIRST Almmmatg OF Detalle NO-LOCK
    BREAK BY detalle.codcia BY detalle.codalm 
    BY detalle.nroord
    BY detalle.tipmov BY detalle.codmov 
    BY detalle.fchdoc BY detalle.codmat:
    /* CABECERA */
    IF FIRST-OF(detalle.codalm) OR FIRST-OF(detalle.nroord) OR FIRST-OF(detalle.tipmov) OR FIRST-OF(detalle.codmov) OR FIRST-OF(detalle.fchdoc) THEN DO:
        CREATE almcmov.
        ASSIGN 
            Almcmov.CodCia = Almtdocm.CodCia 
            Almcmov.CodAlm = Almtdocm.CodAlm 
            Almcmov.TipMov = Almtdocm.TipMov
            Almcmov.CodMov = Almtdocm.CodMov
            Almcmov.NroSer = S-NROSER
            Almcmov.HorSal = STRING(TIME,"HH:MM:SS")
            Almcmov.FchDoc = Detalle.fchdoc
            Almcmov.Usuario = "ADMIN"
            Almcmov.CodRef  = "OP"
            Almcmov.NroRef = SUBSTRING(Detalle.nroord,3)
            Almcmov.NroRf1 = Detalle.nroord
            Almcmov.NroRf2 = SUBSTRING(Detalle.nroord,3).
        ASSIGN c-Rowid = ROWID(Almcmov).

        CASE detalle.tipmov:
            WHEN "S" THEN DO:
                FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
                    AND Almacen.CodAlm = Almtdocm.CodAlm 
                    EXCLUSIVE-LOCK NO-ERROR.
                ASSIGN 
                    Almcmov.Nrodoc  = Almacen.CorrSal
                    Almacen.CorrSal = Almacen.CorrSal + 1.
            END.
            WHEN "I" THEN DO:
                FIND Almacen WHERE Almacen.CodCia = S-CODCIA 
                    AND Almacen.CodAlm = Almtdocm.CodAlm 
                    EXCLUSIVE-LOCK NO-ERROR.
                ASSIGN 
                    Almcmov.Nrodoc  = Almacen.CorrIng
                    Almacen.CorrIng = Almacen.CorrIng + 1.
            END.
        END CASE.
        n-Itm = 1.
        MESSAGE almcmov.codalm almcmov.tipmov almcmov.codmov almcmov.nrodoc.
    END.
    
    
    /* DETALLE */
    FIND Almcmov WHERE ROWID(Almcmov) = c-Rowid.
    FIND Almdmov OF Almcmov WHERE Almdmov.codmat = Detalle.codmat NO-ERROR.
    IF NOT AVAILABLE Almdmov THEN CREATE Almdmov.
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
        AND Almtconv.Codalter = Detalle.CodUnd
        NO-LOCK NO-ERROR.
    ASSIGN Almdmov.CodCia = Almcmov.CodCia 
           Almdmov.CodAlm = Almcmov.CodAlm 
           Almdmov.TipMov = Almcmov.TipMov 
           Almdmov.CodMov = Almcmov.CodMov 
           Almdmov.NroSer = Almcmov.NroSer 
           Almdmov.NroDoc = Almcmov.NroDoc 
           Almdmov.CodMon = Almcmov.CodMon 
           Almdmov.FchDoc = Almcmov.FchDoc 
           Almdmov.TpoCmb = Almcmov.TpoCmb
           Almdmov.codmat = Detalle.codmat
           Almdmov.CanDes = Almdmov.CanDes + Detalle.CanDes
           Almdmov.CodUnd = Detalle.CodUnd
           Almdmov.Factor = (IF AVAILABLE Almtconv THEN Almtconv.Equival ELSE 1)
           Almdmov.NroItm = N-Itm
           Almdmov.CodAjt = ''
           Almdmov.HraDoc = almcmov.HorSal
                  R-ROWID = ROWID(Almdmov).
    

    n-Itm = n-Itm + 1.
END.
