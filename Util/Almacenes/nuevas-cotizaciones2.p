/* Crea nuevas cotizaciones en base a otras y luego las cierra manualmente */

DEF TEMP-TABLE T-CPEDI LIKE Faccpedi.
DEF TEMP-TABLE T-DPEDI LIKE Facdpedi
    FIELD CodNew LIKE Facdpedi.codmat
    FIELD CanNew LIKE Facdpedi.canped
    FIELD UndNew LIKE Facdpedi.undvta.

DEF BUFFER B-CPEDI FOR Faccpedi.
DEF BUFFER B-DPEDI FOR Facdpedi.

DEF NEW SHARED VAR s-coddoc AS CHAR INIT 'COT'.
DEF NEW SHARED VAR s-nroser AS INT INIT 000.
DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00000'.
DEF NEW SHARED VAR s-tpoped AS CHAR.

DEFINE VARIABLE FILL-IN-Archivo AS CHAR NO-UNDO.
DEFINE VARIABLE OKpressed AS LOG NO-UNDO.
                                          
SYSTEM-DIALOG GET-FILE FILL-IN-Archivo
    FILTERS "Archivos Excel (*.xls)" "*.xlsx", "Todos (*.*)" "*.*"
    TITLE "Archivo(s) de Carga..."
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
IF OKpressed = FALSE THEN RETURN 'ADM-ERROR'.

/* CREAMOS LA HOJA EXCEL */
/* VARIABLES PARA EL EXCEL */
DEFINE VARIABLE chExcelApplication  AS COM-HANDLE.
DEFINE VARIABLE chWorkbook          AS COM-HANDLE.
DEFINE VARIABLE chWorksheet         AS COM-HANDLE.
DEFINE VARIABLE cRange          AS CHARACTER    NO-UNDO.
DEFINE VARIABLE iCountLine      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iTotalColumn    AS INTEGER      NO-UNDO.
DEFINE VARIABLE cValue          AS CHARACTER    NO-UNDO.
/*DEFINE VARIABLE t-Column        AS INTEGER INIT 1.*/
DEFINE VARIABLE t-Row           AS INTEGER INIT 1.
CREATE "Excel.Application" chExcelApplication.
chWorkbook = chExcelApplication:Workbooks:OPEN(FILL-IN-Archivo).
chWorkSheet = chExcelApplication:Sheets:ITEM(1).
SESSION:SET-WAIT-STATE('GENERAL').
RUN Carga-Temporal.
SESSION:SET-WAIT-STATE('').
/* CERRAMOS EL EXCEL */
chExcelApplication:QUIT().
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet. 
/*
FOR EACH t-cpedi:
    DISPLAY t-cpedi.coddoc t-cpedi.nroped.
END.
RETURN.
*/
FOR EACH T-CPEDI:
    RUN Genera-Copia.
END.


PROCEDURE Carga-Temporal:
/* ********************* */
DEF VAR x-CodDiv AS CHAR NO-UNDO.
DEF VAR x-NroPed AS CHAR NO-UNDO.

/* Cargamos detalle */
ASSIGN
    t-Row = 1.
EMPTY TEMP-TABLE T-CPEDI.
EMPTY TEMP-TABLE T-DPEDI.
REPEAT:
    ASSIGN
        t-Row  = t-Row + 1.
    cValue = chWorkSheet:Cells(t-Row, 1):VALUE.
    IF cValue = "" OR cValue = ? THEN LEAVE.    /* FIN DE DATOS */ 
    /* Division */
    ASSIGN
        cValue = STRING(INTEGER(cValue), '99999')
        NO-ERROR.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        MESSAGE 'Error división linea' t-Row VIEW-AS ALERT-BOX.
        NEXT.
    END.
    x-CodDiv = cValue.
    /* Cotizacion */
    cValue = chWorkSheet:Cells(t-Row, 3):VALUE.
    ASSIGN
        cValue = STRING(INTEGER(cValue), '999999999')
        NO-ERROR.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        MESSAGE 'Error # de cotización línea' t-Row VIEW-AS ALERT-BOX.
        NEXT.
    END.
    x-NroPed = cValue.
    FIND Faccpedi WHERE Faccpedi.codcia = s-codcia
        AND Faccpedi.coddiv = x-coddiv
        AND Faccpedi.coddoc = s-coddoc
        AND Faccpedi.nroped = x-NroPed
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccpedi THEN DO:
        MESSAGE 'No encontró la cotizacion' x-coddiv x-nroped VIEW-AS ALERT-BOX.
        NEXT.
    END.
    CREATE T-DPEDI.
    BUFFER-COPY Faccpedi TO T-DPEDI.
    /* Producto */
    cValue = chWorkSheet:Cells(t-Row, 4):VALUE.
    ASSIGN
        T-DPEDI.codmat = SUBSTRING(cValue,1,6).
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = T-DPEDI.codmat
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE 'Producto no registrado' T-DPEDI.nroped T-DPEDI.codmat VIEW-AS ALERT-BOX.
        DELETE T-DPEDI.
        NEXT.
    END.
    /* Unidad */
    cValue = chWorkSheet:Cells(t-Row, 5):VALUE.
    ASSIGN
        T-DPEDI.UndVta = cValue.
    /* Cantidad */
    cValue = chWorkSheet:Cells(t-Row, 9):VALUE.
    ASSIGN
        T-DPEDI.CanPed = DECIMAL(cValue)
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE 'Error producto' T-DPEDI.nroped T-DPEDI.codmat 'cantidad pedida' cValue VIEW-AS ALERT-BOX.
        DELETE T-DPEDI.
        NEXT.
    END.
    /* PRODUCTO ESPEJO */
    cValue = chWorkSheet:Cells(t-Row, 6):VALUE.
    ASSIGN
        T-DPEDI.codnew = cValue.
    IF T-DPEDI.codnew BEGINS '(' THEN T-DPEDI.codnew = ''.
    IF T-DPEDI.codnew <> '' THEN DO:
        FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
            AND Almmmatg.codmat = T-DPEDI.codnew
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almmmatg THEN DO:
            MESSAGE 'Producto Espejo no registrado' T-DPEDI.codnew VIEW-AS ALERT-BOX.
            DELETE T-DPEDI.
            NEXT.
        END.
        /* Unidad */
        cValue = chWorkSheet:Cells(t-Row, 8):VALUE.
        ASSIGN
            T-DPEDI.UndNew = cValue.
        /* Cantidad New */
        cValue = chWorkSheet:Cells(t-Row, 10):VALUE.
        ASSIGN
            T-DPEDI.CanNew = DECIMAL(cValue)
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE 'Error producto' T-DPEDI.nroped T-DPEDI.codmat 'cantidad nueva' cValue VIEW-AS ALERT-BOX.
            DELETE T-DPEDI.
            NEXT.
        END.
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = T-DPEDI.UndNew
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv OR Almmmatg.undbas <> T-DPEDI.undnew THEN DO:
            MESSAGE 'Error conversión unidades' SKIP
                'Codigo' T-DPEDI.codmat SKIP
                'Unidad Base' Almmmatg.undbas SKIP
                'Unida new' T-DPEDI.undnew VIEW-AS ALERT-BOX.
            DELETE T-DPEDI.
            NEXT.
        END.
    END.
END.
/* Creamos cabeceras */
FOR EACH T-DPEDI BREAK BY T-DPEDI.coddiv BY T-DPEDI.nroped:
    IF FIRST-OF(T-DPEDI.coddiv) OR FIRST-OF(T-DPEDI.nroped) THEN DO:
        CREATE T-CPEDI.
        BUFFER-COPY T-DPEDI TO T-CPEDI.
    END.
END.
/* Borramos los que sobran */
FOR EACH T-DPEDI WHERE T-DPEDI.cannew = 0:
    DELETE T-DPEDI.
END.


END PROCEDURE.

PROCEDURE Genera-Copia:
/* ******************* */
DEF VAR x-CanPed LIKE Facdpedi.canped NO-UNDO.
DEF VAR x-CanNew LIKE Facdpedi.canped NO-UNDO.
DEF VAR x-CodMat LIKE Facdpedi.codmat NO-UNDO.
DEF VAR x-CodNew LIKE facdpedi.codmat NO-UNDO.
DEF VAR x-UndNew LIKE facdpedi.undvta NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.

RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FIND B-CPEDI OF T-CPEDI EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE B-CPEDI THEN DO:
        MESSAGE 'NO se pudo copiar la cotización' T-CPEDI.nroped VIEW-AS ALERT-BOX.
        RETURN.
    END.
    ASSIGN
        s-nroser = INTEGER(SUBSTRING(B-CPEDI.nroped,1,3))
        s-coddiv = B-CPEDI.coddiv
        s-tpoped = B-CPEDI.tpoped.
    CREATE Faccpedi.
    {vta2/icorrelativosecuencial.i &Codigo = s-coddoc &Serie = s-nroser}
    BUFFER-COPY B-CPEDI TO Faccpedi
        ASSIGN 
        FacCPedi.FchPed = TODAY 
        FacCPedi.Hora = STRING(TIME,"HH:MM")
        FacCPedi.NroPed = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
        FacCPedi.Ubigeo[2] = B-CPEDI.coddoc + '|' + B-CPEDI.nroped.
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.

    FOR EACH B-DPEDI OF B-CPEDI NO-LOCK:
        CREATE Facdpedi.
        BUFFER-COPY B-DPEDI TO Facdpedi
            ASSIGN
            Facdpedi.nroped = Faccpedi.nroped
            Facdpedi.fchped = Faccpedi.fchped
            Facdpedi.hora   = Faccpedi.hora.
        /* Buscamos si hay una variacion de código */
        FIND T-DPEDI OF T-CPEDI WHERE T-DPEDI.codmat = B-DPEDI.codmat NO-ERROR.
        IF AVAILABLE T-DPEDI THEN DO:
            ASSIGN
                x-CodMat = T-DPEDI.codmat
                x-CanPed = B-DPEDI.canped
                x-CodNew = T-DPEDI.codnew
                x-UndNew = T-DPEDI.undnew
                x-CanNew = T-DPEDI.cannew.
            /* Factor de Conversión */
            FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
                AND Almmmatg.codmat = x-CodNew
                NO-LOCK.
            FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
                AND Almtconv.Codalter = x-UndNew
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almtconv THEN DO:
                MESSAGE 'NO está configurado el factor de equivalencia para el producto' Almmmatg.codmat SKIP
                    '   Unidad Stock:' Almmmatg.UndBas SKIP
                    'Unidad de Venta:' x-UndNew
                    VIEW-AS ALERT-BOX ERROR.
                UNDO RLOOP, RETURN "ADM-ERROR".
            END.
            /* VALORES POR DEFECTO */
            ASSIGN
                F-FACTOR = Almtconv.Equival.
            /* Grabamos */
            ASSIGN
                Facdpedi.codmat = x-CodNew
                Facdpedi.factor = f-Factor
                Facdpedi.undvta = x-UndNew
                Facdpedi.canped = x-CanNew
                Facdpedi.codmatweb = x-CodMat
                Facdpedi.libre_d01 = x-CanNew.
            ASSIGN
                Facdpedi.PreUni = Facdpedi.ImpLin / Facdpedi.CanPed ~
                / ( 1 - Facdpedi.Por_Dsctos[1] / 100 ) ~
                / ( 1 - Facdpedi.Por_Dsctos[2] / 100 ) ~
                / ( 1 - Facdpedi.Por_Dsctos[3] / 100 )
                Facdpedi.PreVta[1] = Facdpedi.PreUni
                Facdpedi.PreBas    = Facdpedi.PreUni.
            IF Facdpedi.Por_Dsctos[1] = 0 AND Facdpedi.Por_Dsctos[2] = 0 AND Facdpedi.Por_Dsctos[3] = 0 
                THEN Facdpedi.ImpDto = 0.
                ELSE Facdpedi.ImpDto = Facdpedi.CanPed * Facdpedi.PreUni - Facdpedi.ImpLin.
            IF Facdpedi.AftIgv 
                THEN Facdpedi.ImpIgv = Facdpedi.ImpLin - ROUND( Facdpedi.ImpLin  / ( 1 + (Faccpedi.PorIgv / 100) ), 4 ).
                ELSE Facdpedi.ImpIgv = 0.
            ASSIGN
                Facdpedi.Libre_d02 = 0.     /* Sin Flete */
        END.
    END.
    ASSIGN
        B-CPEDI.FlgEst = "X"
        B-CPEDI.UsrAct = "ADMIN"
        B-CPEDI.FecAct = TODAY
        B-CPEDI.HorAct = STRING(TIME,'HH:MM:SS').
END.

END PROCEDURE.
