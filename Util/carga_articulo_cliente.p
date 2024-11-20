
DEFINE VARIABLE cl-codcia AS INTEGER INITIAL 0 NO-UNDO.

DEFINE TEMP-TABLE tt_load
    FIELDS tt_codcli AS CHARACTER
    FIELDS tt_codart AS CHARACTER
    FIELDS tt_codext AS CHARACTER.

FOR Empresas FIELDS
    (Empresas.CodCia Empresas.Campo-CodCli) WHERE
    Empresas.CodCia = 1 NO-LOCK:
END.
IF NOT Empresas.Campo-CodCli THEN cl-codcia = 1.

INPUT FROM VALUE("O:\rosa\Supermercados\matenorma.csv").
REPEAT:
    CREATE tt_load.
    IMPORT DELIMITER ";" tt_codcli tt_codart tt_codext.
END.
INPUT CLOSE.

FOR EACH tt_load:
    IF tt_codcli = "" THEN NEXT.
    FOR gn-clie FIELDS
        (gn-clie.codcia gn-clie.codcli) WHERE
        gn-clie.codcia = cl-codcia AND
        gn-clie.codcli = tt_codcli NO-LOCK:
    END.
    IF NOT AVAILABLE gn-clie THEN NEXT.

    FOR almmmatg FIELDS
        (almmmatg.codcia almmmatg.codmat) WHERE
        almmmatg.codcia = 1 AND
        almmmatg.codmat = tt_codart NO-LOCK:
    END.
    IF NOT AVAILABLE almmmatg THEN NEXT.

    DISPLAY tt_load.
    CREATE supmmatg.
    supmmatg.CodCia = 1.
    supmmatg.CodCli = tt_codcli.
    supmmatg.codmat = tt_codart.
    supmmatg.codartcli = tt_codext.

END.

