DEF VAR k AS INT.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-periodo AS INT INIT 2013.
DEF VAR FILL-IN-NRO-MES AS INT INIT 4.
DEF VAR i-month AS INT.
DEF VAR i-year AS INT.

DEF BUFFER b-mes FOR pl-mov-mes.

DEFINE TEMP-TABLE Tempo LIKE PL-MOV-MES
    FIELD Moneda-Cts LIKE Pl-Cts.Moneda
    FIELD NroDpt-Cts LIKE pl-flg-mes.nrodpt-cts
    FIELD cts AS CHAR
    FIELD Ult6Sueldos AS DEC.

FOR EACH pl-flg-mes NO-LOCK WHERE codcia = 1
        AND periodo = s-periodo
        AND nromes = FILL-IN-NRO-MES:
    FOR EACH PL-MOV-MES WHERE
            PL-MOV-MES.CodCia  = s-CodCia AND
            PL-MOV-MES.Periodo = s-Periodo AND
            PL-MOV-MES.NroMes  = FILL-IN-NRO-MES AND
            PL-MOV-MES.CodPln  = 01 AND
            PL-MOV-MES.CodCal  = 001 AND
            PL-MOV-MES.CodPer  = PL-FLG-MES.CodPer AND
            PL-MOV-MES.CodMov  = 403  NO-LOCK :
        IF PL-MOV-MES.ValCal-Mes <> 0 AND PL-FLG-Mes.Nrodpt-cts <> ""  THEN DO:
            CREATE Tempo.
            BUFFER-COPY PL-MOV-MES TO Tempo
                ASSIGN Tempo.Moneda-Cts = 1.
            FIND PL-CTS OF PL-FLG-MES NO-LOCK NO-ERROR.
            IF AVAILABLE PL-CTS THEN 
                ASSIGN
                Tempo.Moneda-Cts = PL-CTS.Moneda-Cts
                Tempo.NroDpt-Cts = PL-FLG-MES.NroDpt-Cts
                Tempo.Cts = pl-flg-mes.cts.
            /* Ultimas 6 remuneraciones */
            ASSIGN
                i-Month = PL-FLG-MES.NroMes
                i-Year  = PL-FLG-MES.Periodo.
            DO k = 1 TO 6:
                FIND B-MES WHERE B-MES.codcia = s-codcia
                    AND B-MES.nromes = i-Month
                    AND B-MES.periodo = i-Year
                    AND B-MES.CodPln  = 01
                    AND B-MES.codcal = 001      /* Sueldos */
                    AND B-MES.codmov = 401      /* Total Ingresos */
                    AND B-MES.codper = PL-FLG-MES.codper
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE B-MES THEN LEAVE.
                IF AVAILABLE B-MES THEN Tempo.Ult6Sueldos = Tempo.Ult6Sueldos + B-MES.valcal-mes.
                FOR EACH B-MES NO-LOCK WHERE B-MES.codcia = s-codcia
                    AND B-MES.nromes = i-Month
                    AND B-MES.periodo = i-Year
                    AND B-MES.CodPln  = 01
                    AND B-MES.codcal = 001      /* Sueldos */
                    AND B-MES.codper = PL-FLG-MES.codper
                    AND (B-MES.codmov = 508 OR B-MES.codmov = 509 OR B-MES.codmov = 510):   /* Descuentos */
                    Tempo.Ult6Sueldos = Tempo.Ult6Sueldos - B-MES.valcal-mes.
                END.
                i-Month = i-Month - 1.
                IF i-Month <= 0 
                    THEN ASSIGN
                    i-Month = 12
                    i-Year = i-Year - 1.
            END.
        END.
    END.
END.

OUTPUT TO c:\tmp\cts-conti.txt.
PUT UNFORMATTE
    'PERIODO|MES|CODIGO|NOMBRE|DNI|CODMOV|VALOR|MONEDACTS|CTS|CUENTA|ULT6REMU'
    SKIP.
FOR EACH tempo, FIRST pl-pers WHERE pl-pers.codper = tempo.codper NO-LOCK:
    PUT UNFORMATTED
        tempo.periodo '|'
        tempo.nromes '|'
        tempo.codper '|'
        TRIM(patper) + ' ' + TRIM(matper) + ', ' + pl-pers.nomper '|'
        pl-pers.nrodocid '|'
        tempo.codmov '|'
        tempo.valcal-mes '|'
        tempo.Moneda-Cts '|'
        tempo.CTS '|'
        tempo.nrodpt-cts '|'
        Ult6Sueldos
        SKIP.
END.
OUTPUT CLOSE.

