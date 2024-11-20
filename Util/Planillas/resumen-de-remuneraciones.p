{bin/s-global.i}
{pln/s-global.i}

    DEFINE VARIABLE x-con-reg AS INTEGER NO-UNDO.

s-periodo = 2009.

DEFINE WORK-TABLE tmp-bole
    FIELD t-nro    AS INTEGER
    FIELD t-codrem AS INTEGER
    FIELD t-refrem AS CHARACTER
    FIELD t-desrem AS CHARACTER
    FIELD t-imprem AS DECIMAL
    FIELD t-coddes AS INTEGER
    FIELD t-desdes AS CHARACTER
    FIELD t-impdes AS DECIMAL
    FIELD t-codApo AS INTEGER
    FIELD t-desApo AS CHARACTER
    FIELD t-impApo AS DECIMAL.

DEFINE TEMP-TABLE Tempo 
       FIELD Codcia AS INTEGER 
       FIELD CodPer AS CHAR FORMAT "x(6)"
       FIELD NomPer AS CHAR FORMAT "x(35)"
       FIELD CCosto AS CHAR FORMAT "x(6)"
       FIELD NomCos AS CHAR FORMAT "x(35)"
       FIELD TpoBol AS CHAR FORMAT "x(15)"
       FIELD CodMov AS INTEGER FORMAT "999"
       FIELD ValCal AS DECI EXTENT 40.


FOR EACH pl-pers NO-LOCK WHERE pl-pers.codcia = s-codcia:
    RUN busca-datos.
END.
RUN excel.

PROCEDURE busca-datos:

    DEFINE VAR x-nomper as char .
    DISPLAY PL-PERS.CodPer.
    PAUSE 0.

    /* Cargamos el temporal con los ingresos */
        FOR EACH PL-MOV-MES WHERE
            PL-MOV-MES.CodCia  =  s-CodCia AND
            PL-MOV-MES.Periodo =  s-Periodo AND
            PL-MOV-MES.NroMes  >= 6 AND
            PL-MOV-MES.NroMes  <= 12 AND
            PL-MOV-MES.CodCal  =  001 AND
            PL-MOV-MES.CodPer  =  PL-PERS.CodPer,
            FIRST PL-FLG-MES OF PL-MOV-MES NO-LOCK :
            FIND Tempo WHERE Tempo.CodPer = PL-MOV-MES.CodPer NO-ERROR.
            x-nomper = "".
            IF NOT AVAILABLE Tempo THEN DO:                 
                x-nomper = TRIM(pl-pers.patper) + " " + TRIM(pl-pers.matper) + " " + TRIM(pl-pers.nomper).
                FIND cb-auxi WHERE cb-auxi.CodCia = cb-codcia AND 
                    cb-auxi.CLFAUX = "CCO" AND 
                    cb-auxi.CodAUX = pl-flg-mes.ccosto NO-LOCK NO-ERROR.
                CREATE tempo.
                ASSIGN
                    Tempo.Codcia = S-CODCIA
                    Tempo.CodPer = PL-MOV-MES.CodPer                       
                    Tempo.Codmov = Pl-MOV-MES.CodMov 
                    Tempo.ccosto = pl-flg-mes.ccosto
                    tempo.nomper = x-nomper 
                    tempo.nomcos = cb-auxi.nomaux.
            END.
            CASE PL-MOV-MES.CodMov :
                WHEN 100 THEN Tempo.ValCal[1] =  Tempo.ValCal[1] + PL-MOV-MES.ValCal-Mes.
                WHEN 101 THEN Tempo.ValCal[2] =  Tempo.ValCal[2] + PL-MOV-MES.ValCal-Mes.
                WHEN 103 THEN Tempo.ValCal[3] =  Tempo.ValCal[3] + PL-MOV-MES.ValCal-Mes.
                WHEN 106 THEN Tempo.ValCal[4] =  Tempo.ValCal[4] + PL-MOV-MES.ValCal-Mes.
                /*WHEN 116 THEN Tempo.ValCal[5] =  Tempo.ValCal[5] + PL-MOV-MES.ValCal-Mes.*/
                WHEN 104 THEN Tempo.ValCal[5] =  Tempo.ValCal[5] + PL-MOV-MES.ValCal-Mes.
                WHEN 119 THEN Tempo.ValCal[6] =  Tempo.ValCal[6] + PL-MOV-MES.ValCal-Mes.
                WHEN 125 THEN Tempo.ValCal[7] =  Tempo.ValCal[7] + PL-MOV-MES.ValCal-Mes.
                WHEN 126 THEN Tempo.ValCal[8] =  Tempo.ValCal[8] + PL-MOV-MES.ValCal-Mes.
                WHEN 127 THEN Tempo.ValCal[9] =  Tempo.ValCal[9] + PL-MOV-MES.ValCal-Mes.
                WHEN 131 THEN Tempo.ValCal[10] =  Tempo.ValCal[10] + PL-MOV-MES.ValCal-Mes.
                WHEN 134 THEN Tempo.ValCal[11] =  Tempo.ValCal[11] + PL-MOV-MES.ValCal-Mes.
                WHEN 136 THEN Tempo.ValCal[12] =  Tempo.ValCal[12] + PL-MOV-MES.ValCal-Mes.
                WHEN 138 THEN Tempo.ValCal[13] =  Tempo.ValCal[13] + PL-MOV-MES.ValCal-Mes.
                WHEN 209 THEN Tempo.ValCal[14] =  Tempo.ValCal[14] + PL-MOV-MES.ValCal-Mes.
                WHEN 401 THEN Tempo.ValCal[15] =  Tempo.ValCal[15] + PL-MOV-MES.ValCal-Mes.
            END.
        END.
        x-con-reg = 0.

    /*Calculando para Planilla*/

    /* Cargamos el temporal con los ingresos */
        FOR EACH PL-MOV-MES WHERE
            PL-MOV-MES.CodCia  =  s-CodCia AND
            PL-MOV-MES.Periodo =  s-Periodo AND
            PL-MOV-MES.NroMes  >= 6 AND
            PL-MOV-MES.NroMes  <= 12 AND
            PL-MOV-MES.CodCal  =  005 AND
            PL-MOV-MES.CodPer  =  PL-PERS.CodPer,
            FIRST PL-FLG-MES OF PL-MOV-MES NO-LOCK :
            FIND Tempo WHERE Tempo.CodPer = PL-MOV-MES.CodPer NO-ERROR.
            x-nomper = "".
            IF NOT AVAILABLE Tempo THEN DO:
                x-nomper = TRIM(pl-pers.patper) + " " + TRIM(pl-pers.matper) + " " + TRIM(pl-pers.nomper).
                FIND cb-auxi WHERE cb-auxi.CodCia = cb-codcia AND 
                    cb-auxi.CLFAUX = "CCO" AND 
                    cb-auxi.CodAUX = pl-flg-mes.ccosto NO-LOCK NO-ERROR.
                CREATE tempo.
                ASSIGN
                    Tempo.Codcia = S-CODCIA
                    Tempo.CodPer = PL-MOV-MES.CodPer                       
                    Tempo.Codmov = Pl-MOV-MES.CodMov 
                    Tempo.ccosto = pl-flg-mes.ccosto
                    tempo.nomper = x-nomper 
                    tempo.nomcos = cb-auxi.nomaux.
            END.
            CASE PL-MOV-MES.CodMov :
                WHEN 100 THEN Tempo.ValCal[21] =  Tempo.ValCal[21] + PL-MOV-MES.ValCal-Mes.
                WHEN 101 THEN Tempo.ValCal[22] =  Tempo.ValCal[22] + PL-MOV-MES.ValCal-Mes.
                WHEN 103 THEN Tempo.ValCal[23] =  Tempo.ValCal[23] + PL-MOV-MES.ValCal-Mes.
                WHEN 106 THEN Tempo.ValCal[24] =  Tempo.ValCal[24] + PL-MOV-MES.ValCal-Mes.
                WHEN 104 THEN Tempo.ValCal[25] =  Tempo.ValCal[25] + PL-MOV-MES.ValCal-Mes.
                WHEN 119 THEN Tempo.ValCal[26] =  Tempo.ValCal[26] + PL-MOV-MES.ValCal-Mes.
                WHEN 125 THEN Tempo.ValCal[27] =  Tempo.ValCal[27] + PL-MOV-MES.ValCal-Mes.
                WHEN 126 THEN Tempo.ValCal[28] =  Tempo.ValCal[28] + PL-MOV-MES.ValCal-Mes.
                WHEN 127 THEN Tempo.ValCal[29] =  Tempo.ValCal[29] + PL-MOV-MES.ValCal-Mes.
                WHEN 131 THEN Tempo.ValCal[30] =  Tempo.ValCal[30] + PL-MOV-MES.ValCal-Mes.
                WHEN 134 THEN Tempo.ValCal[31] =  Tempo.ValCal[31] + PL-MOV-MES.ValCal-Mes.
                WHEN 136 THEN Tempo.ValCal[32] =  Tempo.ValCal[32] + PL-MOV-MES.ValCal-Mes.
                WHEN 138 THEN Tempo.ValCal[33] =  Tempo.ValCal[33] + PL-MOV-MES.ValCal-Mes.
                WHEN 209 THEN Tempo.ValCal[34] =  Tempo.ValCal[34] + PL-MOV-MES.ValCal-Mes.
                WHEN 401 THEN Tempo.ValCal[35] =  Tempo.ValCal[35] + PL-MOV-MES.ValCal-Mes.
            END.
        END.
        x-con-reg = 0.


END PROCEDURE.

PROCEDURE EXCEL:

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER init 1.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 1.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* encabezado */
t-column = t-column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = s-NomCia.
t-column = t-column + 1.
cColumn = STRING(t-Column).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = 'CALCULO : '.
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "R E P O R T E   D E   R E M U N E R A C I O N E S " + STRING(S-PERIODO,"9999") + " AL MES " + STRING(12,"9999").

t-column = t-column + 2.
cColumn = STRING(t-Column).
chWorkSheet:Range("C4"):Value = 'Dias'.
chWorkSheet:Range("E4"):Value = 'Asignacion'.
chWorkSheet:Range("G4"):Value = 'Refrig.'.
chWorkSheet:Range("H4"):Value = 'Reembolso'.
chWorkSheet:Range("I4"):Value = 'H O R A S    E X T R A S'.
chWorkSheet:Range("L4"):Value = 'Bonifica.'.
chWorkSheet:Range("M4"):Value = 'Bonifica.'.
chWorkSheet:Range("O4"):Value = 'Asignacion'.
/*Liquidaciones*/
chWorkSheet:Range("S4"):Value = 'Dias'.
chWorkSheet:Range("U4"):Value = 'Asignacion'.
chWorkSheet:Range("W4"):Value = 'Refrig.'.
chWorkSheet:Range("X4"):Value = 'Reembolso'.
chWorkSheet:Range("Y4"):Value = 'H O R A S    E X T R A S'.
chWorkSheet:Range("AB4"):Value = 'Bonifica'.
chWorkSheet:Range("AC4"):Value = 'Bonifica'.
chWorkSheet:Range("AE4"):Value = 'Bonifica'.

chWorkSheet:Range("A5"):Value = 'Codigo'.
chWorkSheet:Range("B5"):Value = 'Nombre'.
chWorkSheet:Range("C5"):Value = 'Laborados'.
chWorkSheet:Range("D5"):Value = 'Basico'.
chWorkSheet:Range("E5"):Value = 'Familiar'.
chWorkSheet:Range("F5"):Value = 'Vacaciones'.
chWorkSheet:Range("G5"):Value = 'Movilidad'.
chWorkSheet:Range("H5"):Value = 'Subsidio'.
chWorkSheet:Range("I5"):Value = '25%'.
chWorkSheet:Range("J5"):Value = '100%'.
chWorkSheet:Range("K5"):Value = '35%'.
chWorkSheet:Range("L5"):Value = 'Incentivo'.
chWorkSheet:Range("M5"):Value = 'Especial'.
chWorkSheet:Range("N5"):Value = 'Reintegro'.
chWorkSheet:Range("O5"):Value = 'Extraordin'.
chWorkSheet:Range("P5"):Value = 'Comision'.
chWorkSheet:Range("Q5"):Value = 'Otros'.
chWorkSheet:Range("R5"):Value = 'Total Sueldos'.
/*Liquidaciones*/
chWorkSheet:Range("S5"):Value = 'Laborados'.
chWorkSheet:Range("T5"):Value = 'Basico'.
chWorkSheet:Range("U5"):Value = 'Familiar'.
chWorkSheet:Range("V5"):Value = 'Vacaciones'.
chWorkSheet:Range("W5"):Value = 'Movilidad'.
chWorkSheet:Range("X5"):Value = 'Subsidio'.
chWorkSheet:Range("Y5"):Value = '25%'.
chWorkSheet:Range("Z5"):Value = '100%'.
chWorkSheet:Range("AA5"):Value = '35%'.
chWorkSheet:Range("AB5"):Value = 'Incentivo'.
chWorkSheet:Range("AC5"):Value = 'Especial'.
chWorkSheet:Range("AD5"):Value = 'Reintegro'.
chWorkSheet:Range("AE5"):Value = 'Extraordin'.
chWorkSheet:Range("AF5"):Value = 'Comision'.
chWorkSheet:Range("AG5"):Value = 'Otros'.
chWorkSheet:Range("AH5"):Value = 'Total Liquidaciones'.

FOR EACH tempo BREAK BY tempo.codcia BY tempo.ccosto BY tempo.Codper:
    t-column = t-column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + tempo.codper.
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.nomper.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[1].
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[2].
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[3].
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[4].
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[5].
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[6].
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[7].
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[8].
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[9].
    cRange = "L" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[10].
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[11].
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[12].
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[13].
    cRange = "P" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[14].
    cRange = "R" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[15].
    /*Liquidaciones*/
    cRange = "S" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[21].
    cRange = "T" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[22].
    cRange = "U" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[23].
    cRange = "V" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[24].
    cRange = "W" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[25].
    cRange = "X" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[26].
    cRange = "Y" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[27].
    cRange = "Z" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[28].
    cRange = "AA" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[29].
    cRange = "AB" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[30].
    cRange = "AC" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[31].
    cRange = "AD" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[32].
    cRange = "AE" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[33].
    cRange = "AF" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[34].
    cRange = "AH" + cColumn.
    chWorkSheet:Range(cRange):Value = tempo.ValCal[35].

       ACCUM tempo.ValCal[1] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[2] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[3] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[4] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[5] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[6] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[7] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[8] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[9] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[10] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[11] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[12] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[13] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[14] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[15] ( SUB-TOTAL BY tempo.ccosto).
       /*Liquidaciones*/
       ACCUM tempo.ValCal[21] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[22] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[23] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[24] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[25] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[26] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[27] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[28] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[29] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[30] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[31] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[32] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[33] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[34] ( SUB-TOTAL BY tempo.ccosto).
       ACCUM tempo.ValCal[35] ( SUB-TOTAL BY tempo.ccosto).


       ACCUM tempo.ValCal[1] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[2] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[3] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[4] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[5] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[6] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[7] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[8] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[9] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[10] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[11] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[12] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[13] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[14] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[15] ( TOTAL BY tempo.codcia).
       /*Liquidaciones*/
       ACCUM tempo.ValCal[21] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[22] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[23] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[24] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[25] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[26] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[27] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[28] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[29] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[30] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[31] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[32] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[33] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[34] ( TOTAL BY tempo.codcia).
       ACCUM tempo.ValCal[35] ( TOTAL BY tempo.codcia).

    IF LAST-OF(tempo.ccosto) THEN DO:
        t-column = t-column + 1.
        cColumn = STRING(t-Column).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):Value = "'" + tempo.ccosto.
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = tempo.nomcos.
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[1].
        cRange = "D" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[2].
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[3].
        cRange = "F" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[4].
        cRange = "G" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[5].
        cRange = "H" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[6].
        cRange = "I" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[7].
        cRange = "J" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[8].
        cRange = "K" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[9].
        cRange = "L" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[10].
        cRange = "M" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[11].
        cRange = "N" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[12].
        cRange = "O" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[13].
        cRange = "P" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[14].
        cRange = "R" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[15].
        /*Liquidaciones*/
        cRange = "S" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[21].
        cRange = "T" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[22].
        cRange = "U" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[23].
        cRange = "V" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[24].
        cRange = "W" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[25].
        cRange = "X" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[26].
        cRange = "Y" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[27].
        cRange = "Z" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[28].
        cRange = "AA" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[29].
        cRange = "AB" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[30].
        cRange = "AC" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[31].
        cRange = "AD" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[32].
        cRange = "AE" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[33].
        cRange = "AF" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[34].
        cRange = "AH" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM SUB-TOTAL BY Tempo.ccosto tempo.ValCal[35].

        t-column = t-column + 1.
    END.
    
    IF LAST-OF(tempo.codcia) THEN DO:            
        t-column = t-column + 1.
        cColumn = STRING(t-Column).
        cRange = "B" + cColumn.
        chWorkSheet:Range(cRange):Value = "Total Compañia ------------> ".
        cRange = "C" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[1].
        cRange = "D" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[2].
        cRange = "E" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[3].
        cRange = "F" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[4].
        cRange = "G" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[5].
        cRange = "H" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[6].
        cRange = "I" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[7].
        cRange = "J" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[8].
        cRange = "K" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[9].
        cRange = "L" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[10].
        cRange = "M" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[11].
        cRange = "N" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[12].
        cRange = "O" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[13].
        cRange = "P" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[14].
        cRange = "R" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[15].
        /*Liquidaciones*/
        cRange = "S" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[21].
        cRange = "T" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[22].
        cRange = "U" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[23].
        cRange = "V" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[24].
        cRange = "W" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[25].
        cRange = "X" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[26].
        cRange = "Y" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[27].
        cRange = "Z" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[28].
        cRange = "AA" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[29].
        cRange = "AB" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[30].
        cRange = "AC" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[31].
        cRange = "AD" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[32].
        cRange = "AE" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[33].
        cRange = "AF" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[34].
        cRange = "AH" + cColumn.
        chWorkSheet:Range(cRange):Value = ACCUM TOTAL BY Tempo.codcia tempo.ValCal[35].

        t-column = t-column + 1.
    END.

END.
/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.

END PROCEDURE.
