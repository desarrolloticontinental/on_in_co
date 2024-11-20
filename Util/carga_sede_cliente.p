
DEFINE VARIABLE cl-codcia AS INTEGER INITIAL 0 NO-UNDO.

DEFINE TEMP-TABLE tt_load
    FIELDS tt_codcli AS CHARACTER
    FIELDS tt_sede AS CHARACTER
    FIELDS tt_sedeext AS CHARACTER.

FOR Empresas FIELDS
    (Empresas.CodCia Empresas.Campo-CodCli) WHERE
    Empresas.CodCia = 1 NO-LOCK:
END.
IF NOT Empresas.Campo-CodCli THEN cl-codcia = 1.

INPUT FROM VALUE("O:\rosa\Supermercados\sedes.csv").
REPEAT:
    CREATE tt_load.
    IMPORT DELIMITER ";" tt_codcli tt_sede tt_sedeext.
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

    FIND gn-clied WHERE
        gn-clied.codcia = cl-codcia AND
        gn-clied.codcli = tt_codcli AND
        gn-clied.sede = tt_sede NO-ERROR.
    IF NOT AVAILABLE gn-clied THEN NEXT.

    DISPLAY tt_load.
    ASSIGN gn-clied.sedeclie = tt_sedeext.

END.

