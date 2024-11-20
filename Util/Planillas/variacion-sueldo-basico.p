DEF VAR x-linea AS CHAR FORMAT 'x(20)'.
DEF TEMP-TABLE detalle
    FIELD codper LIKE pl-pers.codper
    FIELD nomper LIKE pl-pers.nomper
    FIELD fchini AS DATE
    FIELD glosa AS CHAR.


INPUT FROM c:\tmp\renovar.prn.        
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codper = SUBSTRING(x-linea,1,6)
            detalle.fchini = DATE(SUBSTRING(x-linea,11,10)).
    END.
END.
INPUT CLOSE.

DEF VAR x-valcal-mes AS DEC.
FOR EACH detalle, FIRST pl-pers WHERE pl-pers.codper = detalle.codper NO-LOCK:
    detalle.nomper = trim(pl-pers.patper) + ' ' + 
        TRIM(pl-pers.matper) + ', ' + pl-pers.nomper.
    x-valcal-mes = 0.
    FIND FIRST pl-mov-mes USE-INDEX idx01 WHERE pl-mov-mes.codcia = 1
        AND pl-mov-mes.codper = detalle.codper
        AND pl-mov-mes.periodo = YEAR(detalle.fchini)
        AND pl-mov-mes.nromes = MONTH(detalle.fchini)
        AND pl-mov-mes.codpln = 01
        AND pl-mov-mes.codcal = 0
        AND pl-mov-mes.codmov = 101
        NO-LOCK NO-ERROR.
    IF AVAILABLE pl-mov-mes THEN DO:
        ASSIGN
            x-valcal-mes = valcal-mes
            detalle.glosa = "PERIODO: " + STRING(Periodo,'9999') +
                            " MES: " + STRING(NroMes,'99') +
                            " BASICO: " + STRING(valcal-mes, '>>,>>9.99').
    END.
    REPEAT WHILE AVAILABLE pl-mov-mes:
        IF valcal-mes <> x-valcal-mes THEN DO:
            ASSIGN
                x-valcal-mes = valcal-mes
                detalle.glosa = detalle.glosa + " " +
                                "PERIODO: " + STRING(Periodo,'9999') +
                                " MES: " + STRING(NroMes,'99') +
                                " BASICO: " + STRING(valcal-mes, '>>,>>9.99').
        END.
        FIND NEXT pl-mov-mes USE-INDEX idx01 WHERE pl-mov-mes.codcia = 1
        AND pl-mov-mes.codper = detalle.codper
        AND pl-mov-mes.codpln = 01
        AND pl-mov-mes.codcal = 0
        AND pl-mov-mes.codmov = 101
        NO-LOCK NO-ERROR.
    END.
END.

OUTPUT TO c:\tmp\basicos.txt.
FOR EACH detalle:
    DISPLAY
        detalle.codper
        detalle.nomper FORMAT 'x(40)'
        detalle.fchini
        detalle.glosa FORMAT 'x(250)'
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

