DEF VAR x-codcli AS CHAR FORMAT 'x(11)'.
DEF VAR x-implc AS DEC FORMAT '>>>,>>>,>>9.99'.
DEF VAR x-fchini AS DATE.
DEF VAR x-fchfin AS DATE.
DEF VAR x-estado AS CHAR FORMAT 'x(15)'.

INPUT FROM c:\tmp\clientes.prn.
OUTPUT TO c:\tmp\lineas.txt.
REPEAT:
    IMPORT x-codcli.
    IF x-codcli = '' THEN LEAVE.
    FIND gn-clie WHERE codcia = 000 
        AND codcli = x-codcli.
    ASSIGN
        x-implc = ?
        x-fchini = ?
        x-fchfin = ?
        x-estado = ?.
    FOR EACH gn-cliel OF gn-clie NO-LOCK BY gn-cliel.fchfin:
        ASSIGN
            x-implc = gn-cliel.implc
            x-fchini = gn-cliel.fchini
            x-fchfin = gn-cliel.fchfin.
    END.
    CASE FlagAut:
    WHEN "A" THEN x-estado = "AUTORIZADO".
    WHEN "R" THEN x-estado = "RECHAZADO".
    OTHERWISE x-estado = "SIN AUTORIZAR".
    END CASE.
    DISPLAY
        gn-clie.codcli 
        gn-clie.nomcli 
        x-ImpLC WHEN x-implc <> ? 
        x-FchIni WHEN x-fchini <> ? 
        x-FchFin WHEN x-fchfin <> ? 
        x-estado
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
INPUT CLOSE.
OUTPUT CLOSE.

