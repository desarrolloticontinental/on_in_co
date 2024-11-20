DEF VAR x-codigo AS CHAR no-undo.
INPUT FROM d:\tmp\clientes.prn.
OUTPUT TO d:\tmp\mayra.txt.
PUT UNFORMATTED 'CODIGO|NOMBRE|RUC/DNI|EMAIL' SKIP.
REPEAT :
    IMPORT UNFORMATTED x-codigo.
    IF x-codigo = '' THEN LEAVE.
    FOR EACH gn-clie NO-LOCK WHERE codcia = 0 AND ruc = x-codigo:
        PUT UNFORMATTED 
            gn-clie.codcli '|'
            gn-clie.nomcli '|'
            gn-clie.ruc '|'
            gn-clie.transporte[4]
            SKIP.
    END.
    FOR EACH gn-clie NO-LOCK WHERE codcia = 0 AND dni = x-codigo:
        PUT UNFORMATTED 
            gn-clie.codcli '|'
            gn-clie.nomcli '|'
            gn-clie.dni '|'
            gn-clie.transporte[4]
            SKIP.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.

