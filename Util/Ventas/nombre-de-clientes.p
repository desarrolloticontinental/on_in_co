DEF VAR x-ApePat AS CHAR NO-UNDO.
DEF VAR x-ApeMat AS CHAR NO-UNDO.
DEF VAR x-Nombre AS CHAR NO-UNDO.
DEF VAR x-nomcli AS CHAR NO-UNDO.
DEF VAR x-Tipo AS CHAR NO-UNDO.

OUTPUT TO c:\tmp\cambios.txt.
FOR EACH gn-clie WHERE codcia = 0
    AND gn-clie.flgsit = "A" BY gn-clie.codcli:
    x-tipo = if substring(gn-clie.ruc,1,1) = "2" then "02" else "01".
    IF x-Tipo = "01" THEN DO:
        ASSIGN
            x-ApePat = ''
            x-ApeMat = ''
            x-Nombre = ''
            x-NomCli = ''.
        DISPLAY gn-clie.codcli.
        PAUSE 0.
        x-NomCli = TRIM(gn-clie.NomCli).
        ASSIGN
            x-ApePat = ENTRY(1, x-NomCli, " ").
        IF INDEX(x-NomCli, ",") > 0 
        THEN DO:
            x-Nombre = TRIM( ENTRY(2, x-NomCli) ).
            x-ApeMat = TRIM ( SUBSTRING(x-NomCli, 
                                 INDEX(x-NomCli, " ") + 1, 
                                 INDEX(x-NomCli, ",") - INDEX(x-NomCli, " ") - 1) ).
        END.
        ELSE DO:
            x-ApeMat = ENTRY(2, x-NomCli, " ").
            x-Nombre = TRIM ( SUBSTRING(x-NomCli, INDEX(x-NomCli, x-ApeMat) + LENGTH(x-ApeMat) + 1) ).
        END.
        ASSIGN
            gn-clie.apepat = x-apepat
            gn-clie.apemat = x-apemat
            gn-clie.nombre = x-Nombre.
    END.
    ELSE gn-clie.nombre = TRIM (gn-clie.nomcli).
END.
OUTPUT CLOSE.