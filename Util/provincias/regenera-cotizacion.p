DEFINE VARIABLE s-codcia AS INT INIT 001 NO-UNDO.
DEFINE VARIABLE s-coddoc AS CHAR INIT "COT" NO-UNDO.
DEFINE VARIABLE s-nrodoc AS CHAR INIT "018140220".
DEFINE VARIABLE s-porigv LIKE faccpedi.porigv NO-UNDO.
DEFINE VARIABLE s-codcli LIKE faccpedi.codcli NO-UNDO.
DEFINE VARIABLE cl-codcia AS INT INIT 000 NO-UNDO.
DEFINE VARIABLE s-cmpbnte LIKE faccpedi.cmpbnte NO-UNDO.
DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.

FIND faccpedi WHERE codcia = s-codcia
    AND coddoc = s-coddoc
    AND nroped = s-nrodoc
    NO-ERROR.
IF NOT AVAILABLE faccpedi THEN RETURN.

ASSIGN
    s-porigv = faccpedi.porigv
    s-codcli = faccpedi.codcli
    s-cmpbnte = faccpedi.cmpbnte .

DEFINE VARIABLE OKpressed AS LOG NO-UNDO.
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
DEF TEMP-TABLE ITEM LIKE facdpedi.

FOR EACH facdpedi OF faccpedi NO-LOCK:
    CREATE ITEM.
    BUFFER-COPY facdpedi
        EXCEPT preuni prevta pordto por_dsctos
        TO ITEM.
END.

ASSIGN
    t-Column = 0
    t-Row = 1.     /* Saltamos el encabezado de los campos */
REPEAT:
    ASSIGN
        t-column = 1
        t-Row    = t-Row + 1.
    t-column = t-column + 1.
    cValue = chWorkSheet:Cells(t-Row, t-Column):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 

    FIND ITEM WHERE ITEM.codmat = cValue NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT.

    ASSIGN
        ITEM.preuni = chWorkSheet:Cells(t-Row, 7):VALUE.
END.
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 

FOR EACH ITEM, FIRST almmmatg OF ITEM NO-LOCK, FIRST almsfami OF almmmatg NO-LOCK:
    ASSIGN
        ITEM.ImpLin = ROUND ( ITEM.CanPed * ITEM.PreUni * 
                      ( 1 - ITEM.Por_Dsctos[1] / 100 ) *
                      ( 1 - ITEM.Por_Dsctos[2] / 100 ) *
                      ( 1 - ITEM.Por_Dsctos[3] / 100 ), 2 ).
    IF ITEM.Por_Dsctos[1] = 0 AND ITEM.Por_Dsctos[2] = 0 AND ITEM.Por_Dsctos[3] = 0 
        THEN ITEM.ImpDto = 0.
        ELSE ITEM.ImpDto = ITEM.CanPed * ITEM.PreUni - ITEM.ImpLin.
    ASSIGN
        ITEM.ImpLin = ROUND(ITEM.ImpLin, 2)
        ITEM.ImpDto = ROUND(ITEM.ImpDto, 2).
    IF ITEM.AftIgv 
    THEN ITEM.ImpIgv = ITEM.ImpLin - ROUND( ITEM.ImpLin  / ( 1 + (s-PorIgv / 100) ), 4 ).
    ELSE ITEM.ImpIgv = 0.

    /* RHC 07/11/2013 CALCULO DE PERCEPCION */
    DEF VAR s-PorPercepcion AS DEC INIT 0 NO-UNDO.
    ASSIGN
        ITEM.CanSol = 0
        ITEM.CanApr = 0.
    FIND FIRST Vtatabla WHERE Vtatabla.codcia = s-codcia
        AND Vtatabla.tabla = 'CLNOPER'
        AND VtaTabla.Llave_c1 = s-CodCli
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Vtatabla THEN DO:
        FIND gn-clie WHERE gn-clie.codcia = cl-codcia AND gn-clie.codcli = s-codcli NO-LOCK.
        IF gn-clie.Libre_L01 = YES AND gn-clie.RucOld <> "SI" THEN s-Porpercepcion = 0.5.
        IF gn-clie.Libre_L01 = NO AND gn-clie.RucOld <> "SI" THEN s-Porpercepcion = 2.
        /* Ic 04 Julio 2013 
            gn-clie.Libre_L01   : PERCEPCTOR
            gn-clie.RucOld      : RETENEDOR
        */
        IF s-Cmpbnte = "BOL" THEN s-Porpercepcion = 2.
        IF Almsfami.Libre_c05 = "SI" THEN
            ASSIGN
            ITEM.CanSol = s-PorPercepcion
            ITEM.CanApr = ROUND(ITEM.implin * s-PorPercepcion / 100, 2).
    END.
END.

FOR EACH ITEM, FIRST facdpedi OF faccpedi WHERE facdpedi.codmat = ITEM.codmat:
    BUFFER-COPY ITEM TO facdpedi.
END.

{vta2/graba-totales-cotizacion-cred.i}


