DEF VAR I AS INT.
DEF VAR x-valcal-mes AS DEC.

FOR EACH pl-pers NO-LOCK WHERE codcia = 1:
    DO I = 1 TO 12:
        x-valcal-mes = 0.
        FOR EACH PL-BOLE NO-LOCK WHERE
            PL-BOLE.CodPln = 1 AND
            PL-BOLE.CodCal = 1 AND
            PL-BOLE.TpoBol = "Remuneraciones" BY PL-BOLE.nroitm:
            IF LOOKUP(string(PL-BOLE.CodMov, '999'), '104,109,137,118,119,135,141,803') > 0 THEN NEXT.
            FIND PL-MOV-MES WHERE PL-MOV-MES.codcia = 1
                AND PL-MOV-MES.Periodo = 2010
                AND PL-MOV-MES.NroMes = I
                AND PL-MOV-MES.CodPln = 1
                AND PL-MOV-MES.CodCal = 1
                AND PL-MOV-MES.CodPer = PL-PERS.CodPer
                AND PL-MOV-MES.CodMov = PL-BOLE.CodMov
                NO-LOCK NO-ERROR.
            IF AVAILABLE PL-MOV-MES THEN x-valcal-mes = x-valcal-mes + valcal-mes.
        END.
        FIND PL-MOV-MES WHERE PL-MOV-MES.codcia = 1
            AND PL-MOV-MES.Periodo = 2010
            AND PL-MOV-MES.NroMes = I
            AND PL-MOV-MES.CodPln = 1
            AND PL-MOV-MES.CodCal = 1
            AND PL-MOV-MES.CodPer = PL-PERS.CodPer
            AND PL-MOV-MES.CodMov = 405
            NO-ERROR.
        IF AVAILABLE PL-MOV-MES AND PL-MOV-MES.valcal-mes <> x-valcal-mes
            THEN DO:
            DISPLAY PL-MOV-MES.CodPer PL-MOV-MES.NroMes PL-MOV-MES.valcal-mes x-valcal-mes SKIP.
            pl-mov-mes.valcal-mes = x-valcal-mes.
        END.
    END.
END.


