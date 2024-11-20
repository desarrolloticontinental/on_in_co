/*
    Carga múltiplo para despacho de artículos Supermercados.
*/

DEFINE TEMP-TABLE tt_load
    FIELDS tt_codcli AS CHARACTER
    FIELDS tt_codart AS CHARACTER
    FIELDS tt_multip AS DECIMAL.

INPUT FROM VALUE("O:\rosa\Supermercados\multiplo.csv").
REPEAT:
    CREATE tt_load.
    IMPORT DELIMITER ";" tt_codcli tt_codart tt_multip.
END.
INPUT CLOSE.

FOR EACH tt_load:
    IF tt_codcli = "" THEN NEXT.

    FIND FIRST supmmatg WHERE
        supmmatg.CodCia = 1 AND
        supmmatg.CodCli = tt_codcli AND
        supmmatg.codmat = tt_codart NO-ERROR.
    IF AVAILABLE supmmatg THEN DO:
        DISPLAY tt_load.
        ASSIGN supmmatg.Libre_d01 = tt_multip.
    END.

END.

