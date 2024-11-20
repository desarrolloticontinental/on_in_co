DEF VAR cl-codcia AS INT INIT 000 NO-UNDO.
DEF VAR x-canal AS CHAR NO-UNDO.
DEF VAR x-giro AS CHAR NO-UNDO.
DEF VAR x-division AS CHAR NO-UNDO.

OUTPUT TO d:\tmp\clientes.txt.
PUT UNFORMATTED
    'CODIGO|NOMBRE|DIRECCION|RUC|CANAL|GIRO|DIVISION'
    SKIP.
FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia
    AND gn-clie.flgsit = "A":
    x-division = gn-clie.coddiv.
    x-canal = gn-clie.canal.
    FIND almtabla WHERE almtabla.Tabla = 'CN' 
        AND almtabla.Codigo = gn-clie.canal NO-LOCK NO-ERROR.
    IF AVAILABLE almtabla THEN x-canal = gn-clie.canal + ' ' + almtabla.nombre.
    x-giro = gn-clie.gircli.
    FIND almtabla WHERE almtabla.Tabla = 'GN' 
        AND almtabla.Codigo = gn-clie.gircli NO-LOCK NO-ERROR.
    IF AVAILABLE almtabla THEN x-giro = gn-clie.gircli + ' ' + almtabla.nombre.
    FIND gn-divi WHERE gn-divi.codcia = 001
        AND gn-divi.coddiv = x-division
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-divi THEN x-division = gn-divi.coddiv + ' ' + gn-divi.desdiv.
    PUT UNFORMATTED
        codcli '|'
        nomcli '|'
        dircli '|'
        ruc '|'
        x-canal '|'
        x-giro '|'
        x-division
        SKIP.

END.
OUTPUT CLOSE.
