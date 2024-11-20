DEF VAR total_ingresos AS DEC.
DEF VAR total_dias AS DEC.
DEF VAR s-codcia AS INT INIT 001.

ASSIGN
    total_ingresos = 0
    total_dias = 0.

DEF TEMP-TABLE detalle
    FIELD codper LIKE pl-pers.codper
    FIELD valcal-mes AS DEC EXTENT 4000.

/* Empleados */ 
/* Boleta de Remuneraciones */
FOR EACH PL-MOV-MES NO-LOCK WHERE
    PL-MOV-MES.CodCia = s-codcia AND
    PL-MOV-MES.Periodo = 2010 AND
    (PL-MOV-MES.NroMes >= 1 AND PL-MOV-MES.NroMes <= 12) AND    /* Todo el año */
    PL-MOV-MES.CodPln = 01 AND                                  /* Planilla Empleados */
    PL-MOV-MES.CodCal = 01 AND                                  /* Cálculo */
    PL-MOV-MES.CodPer <> "" AND                                 /* Todos los Empl */
    (PL-MOV-MES.CodMov = 101 OR                                 /* Basico */
    PL-MOV-MES.CodMov = 103 OR                                  /* Asig. Familiar */
    PL-MOV-MES.CodMov = 106 OR                                  /* Vacacional */
    PL-MOV-MES.CodMov = 107 OR                                  /* Vacaciones Trabajadas*/
    PL-MOV-MES.CodMov = 108 OR                                  /* Vacaciones Truncas */
    PL-MOV-MES.CodMov = 118 OR                                  /* Descanso medico */
    PL-MOV-MES.CodMov = 125 OR                                  /* HE 25% */
    PL-MOV-MES.CodMov = 126 OR                                  /* HE 100% */
    PL-MOV-MES.CodMov = 127 OR                                  /* HE 35% */
    PL-MOV-MES.CodMov = 131 OR                                  /* Bonificacion Incentivo */
    PL-MOV-MES.CodMov = 134 OR                                  /* Bonificacion Especial */
    PL-MOV-MES.CodMov = 136 OR                                  /* Reintegro */
    PL-MOV-MES.CodMov = 139 OR                                  /* Gratificacion Trunca */
    PL-MOV-MES.CodMov = 801 OR                                  /* Bonificacion por Produccion */
    PL-MOV-MES.CodMov = 802 OR                                  /* Bonificacion Nocturna */
    PL-MOV-MES.CodMov = 803 OR                                  /* Subsidio Pre-Post Natal */
     PL-MOV-MES.CodMov = 130 OR                                  /* Otros Ingresos */
     PL-MOV-MES.CodMov = 146 OR                                  /* Riesgo de Caja */
     PL-MOV-MES.CodMov = 209 OR                                  /* Comisiones */
    PL-MOV-MES.CodMov = 099)                                    /* Dias Efectivos */
    :
    IF PL-MOV-MES.ValCal-Mes = 0 THEN NEXT.
    FIND detalle WHERE detalle.codper = pl-mov-mes.codper NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.codper = pl-mov-mes.codper
        detalle.valcal-mes[pl-mov-mes.codmov] = detalle.valcal-mes[pl-mov-mes.codmov] + pl-mov-mes.valcal-mes.
    CASE PL-MOV-MES.CodMov:
        WHEN 099 THEN DO:
            total_dias = total_dias + PL-MOV-MES.ValCal-Mes.
        END.
        OTHERWISE DO:
            total_ingresos = total_ingresos + PL-MOV-MES.ValCal-Mes.
        END.
    END CASE.
END.
/* Boleta de Gratificaciones */
DEF VAR x-codmov AS INT.

FOR EACH PL-MOV-MES WHERE
    PL-MOV-MES.CodCia = s-codcia AND
    PL-MOV-MES.Periodo = 2010 AND
    (PL-MOV-MES.NroMes >= 1 AND PL-MOV-MES.NroMes <= 12) AND    /* Todo el año */
    PL-MOV-MES.CodPln = 01 AND                                  /* Planilla Empleados*/
    PL-MOV-MES.CodCal = 04 AND                                  /* Cálculo */
    PL-MOV-MES.CodPer <> "" AND                                 /* Todos los Empl */
    PL-MOV-MES.CodMov = 212                                     /* Gratificacion */
    NO-LOCK:
    IF PL-MOV-MES.ValCal-Mes = 0 THEN NEXT.
    x-CodMov = pl-mov-mes.codmov + 1000.
    FIND detalle WHERE detalle.codper = pl-mov-mes.codper NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.codper = pl-mov-mes.codper
        detalle.valcal-mes[x-codmov] = detalle.valcal-mes[x-codmov] + pl-mov-mes.valcal-mes.
    total_ingresos = total_ingresos + PL-MOV-MES.ValCal-Mes.
END.
/* Liquidacion */
FOR EACH PL-MOV-MES WHERE
    PL-MOV-MES.CodCia = s-codcia AND
    PL-MOV-MES.Periodo = 2010 AND
    (PL-MOV-MES.NroMes >= 1 AND PL-MOV-MES.NroMes <= 12) AND    /* Todo el año */
    PL-MOV-MES.CodPln = 01 AND                                  /* Planilla Empleados*/
    PL-MOV-MES.CodCal = 05 AND                                  /* Cálculo */
    PL-MOV-MES.CodPer <> "" AND                                 /* Todos los Empl */
    (PL-MOV-MES.CodMov = 431 OR                                  /* Liq Acumulada */
    PL-MOV-MES.CodMov = 139)                                     /* Gratificacion Trunca */
    NO-LOCK:
    IF PL-MOV-MES.ValCal-Mes = 0 THEN NEXT.
    x-CodMov = pl-mov-mes.codmov + 2000.
    FIND detalle WHERE detalle.codper = pl-mov-mes.codper NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.codper = pl-mov-mes.codper
        detalle.valcal-mes[x-codmov] = detalle.valcal-mes[x-codmov] + pl-mov-mes.valcal-mes.
    total_ingresos = total_ingresos + PL-MOV-MES.ValCal-Mes.
END.
/* Liquidacion de Eventuales */
FOR EACH PL-MOV-MES WHERE
    PL-MOV-MES.CodCia = s-codcia AND
    PL-MOV-MES.Periodo = 2010 AND
    (PL-MOV-MES.NroMes >= 1 AND PL-MOV-MES.NroMes <= 12) AND    /* Todo el año */
    PL-MOV-MES.CodPln = 01 AND                                  /* Planilla Empleados*/
    PL-MOV-MES.CodCal = 08 AND                                  /* Cálculo */
    PL-MOV-MES.CodPer <> "" AND                                 /* Todos los Empl */
    (PL-MOV-MES.CodMov = 431 OR                                 /* Acumu. Vacaciones */
    PL-MOV-MES.CodMov = 611)                                    /* Gratificacion Trunca */
    NO-LOCK:
    IF PL-MOV-MES.ValCal-Mes = 0 THEN NEXT.
    x-CodMov = pl-mov-mes.codmov + 3000.
    FIND detalle WHERE detalle.codper = pl-mov-mes.codper NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.codper = pl-mov-mes.codper
        detalle.valcal-mes[x-codmov] = detalle.valcal-mes[x-codmov] + pl-mov-mes.valcal-mes.
    total_ingresos = total_ingresos + PL-MOV-MES.ValCal-Mes.
END.

DEF VAR x-linea AS CHAR FORMAT 'x(1000)'.

x-linea = 'PERSONAL|101|103|106|107|108|118|125|126|127|131|134|136|139|801|802|803|130|146|209|'.
x-linea = x-linea + '212|'.
x-linea = x-linea + '431|139|'.
x-linea = x-linea + '431|611|'.
x-linea = x-linea + 'DIAS EFECTIVOS|'.
OUTPUT TO c:\tmp\utilidades.txt.
PUT UNFORMATTED x-linea SKIP.
FOR EACH detalle, FIRST pl-pers WHERE pl-pers.codper = detalle.codper NO-LOCK:
    x-linea = detalle.codper + ' - ' +
                TRIM (pl-pers.patper) + TRIM (pl-pers.matper) + ', ' + 
                TRIM(pl-pers.nomper) + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[101], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[103], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[106], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[107], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[108], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[118], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[125], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[126], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[127], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[131], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[134], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[136], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[139], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[801], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[802], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[803], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[130], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[146], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[209], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[1212], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[2431], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[2139], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[3431], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[3611], '>>>,>>9.99') + '|'.
    x-linea = x-linea + STRING (detalle.valcal-mes[99], '>>>,>>9.99') + '|'.
    PUT UNFORMATTED x-linea SKIP.
END.
OUTPUT CLOSE.

