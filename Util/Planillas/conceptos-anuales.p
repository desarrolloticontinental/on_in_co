DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-periodo AS INT INIT 2009.
DEF VAR s-nomcia AS CHAR.
DEF VAR fill-in-nro-mes AS INT INIT 7.
DEFINE STREAM strm-boleta. /* STREAM para el reporte */
DEFINE VARIABLE x-linea   AS CHARACTER FORMAT "x(64)" NO-UNDO.

DEFINE TEMP-TABLE Tempo 
       FIELD CodPer AS CHAR FORMAT "x(6)"
       FIELD Patper LIKE PL-PERS.PatPer
       FIELD MatPer LIKE PL-PERS.MatPer
       FIELD NomPer LIKE PL-PERS.NomPer
       FIELD TpoBol AS CHAR FORMAT "x(15)"
       FIELD CodMov AS INTEGER FORMAT "999"
       FIELD valcalD AS DECI EXTENT 13.

FOR EACH PL-FLG-MES WHERE PL-FLG-MES.CodCia = s-CodCia
    AND PL-FLG-MES.Periodo = s-Periodo
    AND PL-FLG-MES.codpln = 01
    AND PL-FLG-MES.NroMes = fill-in-nro-mes
    NO-LOCK,
    EACH PL-PERS OF PL-FLG-MES NO-LOCK:
    RUN busca_datos.
END.
            
OUTPUT STREAM strm-boleta TO c:\tmp\anuales.txt.
    FOR EACH tempo NO-LOCK BY tempo.codper BY tempo.CodMov :
        PUT STREAM strm-boleta tempo.codper '|'.
        PUT STREAM strm-boleta tempo.patper '|'.
        PUT STREAM strm-boleta tempo.matper '|'.
        PUT STREAM strm-boleta tempo.nomper '|'.
        PUT STREAM strm-boleta tempo.codmov '|'.
        PUT STREAM strm-boleta tempo.valcal[1] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[2] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[3] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[4] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[5] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[6] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[7] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[8] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[9] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[10] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[11] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[12] FORMAT "->>>>9.99" '|'.
        PUT STREAM strm-boleta tempo.valcal[13] FORMAT "->>>>9.99" SKIP.
    END.

OUTPUT STREAM strm-boleta CLOSE.


PROCEDURE busca_datos:

    DEFINE  VAr I AS INTEGER.
    DEFINE VARIABLE x-con-reg AS INTEGER NO-UNDO.


    DEFINE VARIABLE saldo     AS DECIMAL.
    DEFINE VARIABLE F-valcalD  AS DECIMAL.
    DEFINE VARIABLE linea-msg AS CHARACTER EXTENT 3.
    DEFINE VARIABLE j         AS INTEGER.
    DEFINE VARIABLE ii        AS INTEGER.
    DEFINE VARIABLE x-dirdiv  AS CHAR.
    DEFINE VARIABLE x-desdiv  AS CHAR.
    DEFINE VARIABLE x-dirper  AS CHAR.
    DEFINE VARIABLE x-LEper   AS CHAR.
    DEFINE VARIABLE X-RUC     AS CHAR INIT "20100038146" format "X(11)".

    IF PL-FLG-MES.SitAct = "Inactivo" THEN RETURN. /* Si no esta Activo */

    IF NOT CAN-FIND(FIRST PL-MOV-MES WHERE
        PL-MOV-MES.CodCia  = s-CodCia        AND
        PL-MOV-MES.Periodo = s-Periodo       AND
        PL-MOV-MES.NroMes  = FILL-IN-NRO-MES AND
        PL-MOV-MES.CodPln  = 01  AND
        PL-MOV-MES.CodCal  = 004  AND
        PL-MOV-MES.CodPer  = PL-FLG-MES.CodPer) THEN RETURN. /* Si no tiene c lculo */

    /*Cargamos el temporal con los ingresos */
    FOR EACH PL-BOLE NO-LOCK WHERE
        PL-BOLE.CodPln = 01 AND
        PL-BOLE.CodCal = 004 AND
        PL-BOLE.TpoBol = "Remuneraciones" BY PL-BOLE.nroitm:
        DO I = 1 TO FILL-IN-NRO-MES:
            FIND PL-MOV-MES WHERE
                PL-MOV-MES.CodCia  = s-CodCia AND
                PL-MOV-MES.Periodo = s-Periodo AND
                PL-MOV-MES.NroMes  = I AND
                PL-MOV-MES.CodPln  = 01 AND
                PL-MOV-MES.CodCal  = PL-BOLE.CodCal AND
                PL-MOV-MES.CodPer  = PL-FLG-MES.CodPer AND
                PL-MOV-MES.CodMov  = PL-BOLE.CodMov NO-LOCK NO-ERROR.
            IF AVAILABLE PL-MOV-MES AND PL-MOV-MES.valcal-Mes <> 0 THEN DO:
                FIND Tempo WHERE Tempo.CodPer = PL-MOV-MES.CodPer AND
                                 Tempo.CodMov = Pl-MOV-MES.CodMov
                                 NO-ERROR.
                IF NOT AVAILABLE Tempo THEN DO:                 
                CREATE tempo.
                ASSIGN
                    Tempo.CodPer = PL-MOV-MES.CodPer 
                    Tempo.PatPer = PL-PERS.PatPer
                    Tempo.MatPer = PL-PERS.MatPer
                    Tempo.NomPer = PL-PERS.NomPer
                    Tempo.TpoBol = Pl-BOLE.TpoBol
                    Tempo.Codmov = Pl-MOV-MES.CodMov .
                END.
                Tempo.valcal[I] = Tempo.valcal[I] + PL-MOV-MES.valcal-Mes .    
                Tempo.valcal[13] = Tempo.valcal[13] + PL-MOV-MES.valcal-Mes .
            END.
        END.
    END.

END PROCEDURE.
