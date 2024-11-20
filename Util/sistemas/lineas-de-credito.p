DEF VAR ccodcli AS CHAR.

INPUT FROM d:\clientes.prn.
OUTPUT TO d:\prueba.txt.
REPEAT:
    IMPORT UNFORMATTED ccodcli.
    IF TRUE <> (ccodcli > '') THEN LEAVE.
    FOR EACH gn-cliel NO-LOCK WHERE gn-cliel.codcia = 0 AND 
        gn-cliel.codcli = ccodcli AND
        gn-cliel.fchfin >= DATE(01,01,2017):
        PUT UNFORMATTED
            codcli '|' 
            fchini '|'
            fchfin '|'
            implc 
            SKIP.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.
