
DEFINE VARIABLE p-codpln AS INTEGER NO-UNDO.
DEFINE VARIABLE p-codcal AS INTEGER NO-UNDO.
DEFINE VARIABLE p-period AS INTEGER NO-UNDO.
DEFINE VARIABLE p-mes AS INTEGER NO-UNDO.

DEFINE VARIABLE suma AS DECIMAL NO-UNDO.
DEFINE VARIABLE resta AS DECIMAL NO-UNDO.
DEFINE VARIABLE total AS DECIMAL NO-UNDO.

p-codpln = 1.
p-codcal = 4.
p-period = 2007.
p-mes = 12.

FOR EACH PL-FLG-MES WHERE
    PL-FLG-MES.codcia  = 1 AND
    PL-FLG-MES.periodo = p-period AND
    PL-FLG-MES.nromes  = p-mes AND
    PL-FLG-MES.codpln  = p-codpln AND
    PL-FLG-MES.codper  >= "" NO-LOCK:
    total = 0.
    suma = 0.
    resta = 0.
    FOR EACH PL-MOV-MES WHERE
        PL-MOV-MES.codcia  = PL-FLG-MES.codcia AND
        PL-MOV-MES.periodo = PL-FLG-MES.periodo AND
        PL-MOV-MES.nromes  = PL-FLG-MES.nromes AND
        PL-MOV-MES.codpln  = PL-FLG-MES.codpln AND
        PL-MOV-MES.codcal  = p-codcal AND
        PL-MOV-MES.codper  = PL-FLG-MES.codper NO-LOCK:
        FIND PL-BOLE WHERE
            PL-BOLE.CodCal = PL-MOV-MES.codcal AND
            PL-BOLE.CodPln = PL-MOV-MES.codpln AND
            PL-BOLE.CodMov = PL-MOV-MES.codmov NO-LOCK NO-ERROR.
        CASE PL-BOLE.tpobol:
            /*
            WHEN "Aportes" OR
            */
            WHEN "Descuentos"     THEN resta = resta + PL-MOV-MES.valcal-mes.
            WHEN "Remuneraciones" THEN suma = suma + PL-MOV-MES.valcal-mes.
        END CASE.
    END.
    FOR EACH PL-MOV-MES WHERE
        PL-MOV-MES.codcia  = PL-FLG-MES.codcia AND
        PL-MOV-MES.periodo = PL-FLG-MES.periodo AND
        PL-MOV-MES.nromes  = PL-FLG-MES.nromes AND
        PL-MOV-MES.codpln  = PL-FLG-MES.codpln AND
        PL-MOV-MES.codcal  = p-codcal AND
        PL-MOV-MES.codmov  = 403 AND
        PL-MOV-MES.codper  = PL-FLG-MES.codper NO-LOCK:
        total = total + PL-MOV-MES.valcal-mes.
    END.
    if total <> suma - resta then
        display PL-FLG-MES.codper total(total) suma - resta(total) .
END.
