DEF BUFFER b-clie FOR gn-clie.
OUTPUT TO d:\tmp\relacionados.txt.
FOR EACH VtaCTabla WHERE VtaCTabla.CodCia = 001
    AND VtaCTabla.Tabla = 'CLGRP' NO-LOCK,
    EACH gn-clie WHERE gn-clie.CodCli = VtaCTabla.Llave
    AND gn-clie.CodCia = 000 NO-LOCK,
    EACH VtaDTabla OF VtaCTabla NO-LOCK,
    EACH b-clie WHERE b-clie.CodCli = VtaDTabla.Tipo
    AND b-clie.CodCia = 000 NO-LOCK:
    PUT UNFORMATTED
        gn-clie.codcli '|'
        gn-clie.nomcli '|'
        b-clie.codcli '|'
        b-clie.nomcli
        SKIP.
END.
