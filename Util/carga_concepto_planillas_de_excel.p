
DEFINE VARIABLE chExcelApplication AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorksheet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cRange AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCountLine AS INTEGER NO-UNDO.
DEFINE VARIABLE cNomper AS CHARACTER LABEL "Nombre" FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE iCodMov LIKE pl-mov-mes.codmov NO-UNDO.

DEFINE TEMP-TABLE tt_pl NO-UNDO
    FIELDS tt_codper LIKE pl-pers.codper
    FIELDS tt_mes LIKE pl-mov-mes.nromes
    FIELDS tt_cal LIKE pl-mov-mes.codcal
    FIELDS tt_con LIKE pl-mov-mes.codmov
    FIELDS tt_val LIKE pl-mov-mes.valcal
    INDEX tt_idx01 tt_codper tt_mes tt_con.

DEFINE BUFFER b_mov-mes FOR pl-mov-mes.

CREATE "Excel.Application" chExcelApplication.

chWorkbook = chExcelApplication:Workbooks:OPEN("D:\tmp\vacaciones.xls").
chWorkSheet = chExcelApplication:Sheets:ITEM(1).
iCountLine = 1.
iCodMov = 502.

REPEAT:

    CREATE tt_pl.

    iCountLine = iCountLine + 1.

    /* Código */
    cRange = "A" + TRIM(STRING(iCountLine)).
    tt_codper = STRING(chWorkSheet:Range(cRange):VALUE,"999999").

    /* Mes */
    cRange = "C" + TRIM(STRING(iCountLine)).
    tt_mes = chWorkSheet:Range(cRange):VALUE.

    /* Cálculo */
    cRange = "D" + TRIM(STRING(iCountLine)).
    tt_cal = chWorkSheet:Range(cRange):VALUE.

    /* Concepto */
    cRange = "E" + TRIM(STRING(iCountLine)).
    tt_con = chWorkSheet:Range(cRange):VALUE.

    /* Valor */
    cRange = "F" + TRIM(STRING(iCountLine)).
    tt_val = chWorkSheet:Range(cRange):VALUE.

    IF tt_codper = "" OR tt_codper = ? THEN LEAVE.

END.

/* Busca Vacaciones */
FOR EACH tt_pl, FIRST pl-pers WHERE
    pl-pers.codcia = 1 AND
    pl-pers.codper = tt_codper NO-LOCK:
    cNomper = PL-PERS.patper + " " + pl-pers.matper + " " + pl-pers.nomper.
    /* Busca Vacaciones */
    FIND pl-mov-mes WHERE
        pl-mov-mes.codcia = 1 AND
        pl-mov-mes.periodo = 2009 AND
        pl-mov-mes.nromes = tt_mes AND
        pl-mov-mes.codpln = 1 AND
        pl-mov-mes.codcal = tt_cal AND
        pl-mov-mes.codper = tt_codper AND
        pl-mov-mes.codmov = tt_con NO-ERROR.
    /* Crea Vacaciones No registradas */
    IF NOT AVAILABLE pl-mov-mes THEN DO:
        CREATE b_mov-mes.
        ASSIGN
            b_mOV-MES.codcal       = tt_cal
            b_mOV-MES.CodCia       = 1
            b_mOV-MES.CodMov       = tt_con
            b_mOV-MES.codper       = tt_codper
            b_mOV-MES.codpln       = 1
            b_mOV-MES.Fch-Ult-Cal  = TODAY
            b_mOV-MES.flgreg-mes   = TRUE
            b_mOV-MES.Hra-Ult-Cal  = STRING(TIME,"hh:mm:ss")
            b_mOV-MES.NroMes       = tt_mes
            b_mOV-MES.Periodo      = 2009
            b_mOV-MES.valcal-mes   = tt_val.
    END.
    ELSE DO:
        /* Modifica Registros Existentes */
        IF tt_val <> pl-mov-mes.valcal THEN
            ASSIGN pl-mov-mes.valcal-mes = tt_val.
    END.
    /* Busca Faltas */
    FIND b_mov-mes WHERE
        b_mov-mes.codcia = 1 AND
        b_mov-mes.periodo = 2009 AND
        b_mov-mes.nromes = tt_mes AND
        b_mov-mes.codpln = 1 AND
        b_mov-mes.codcal = tt_cal AND
        b_mov-mes.codper = tt_codper AND
        b_mov-mes.codmov = iCodMov NO-ERROR.
    IF AVAILABLE b_mov-mes THEN DO:
        /* Si valores son iguales Borra faltas */
        IF tt_val = b_mov-mes.valcal THEN DO:
            DELETE b_mov-mes.
        END.
        /* Guarda la diferencia */
        ELSE DO:
            b_mov-mes.valcal-mes = ABS(tt_val - b_mov-mes.valcal-mes).
        END.
    END.

END.

chExcelApplication:QUIT().

/* liberar com-handles */
RELEASE OBJECT chExcelApplication.
RELEASE OBJECT chWorkSheet.
RELEASE OBJECT chWorkbook.
