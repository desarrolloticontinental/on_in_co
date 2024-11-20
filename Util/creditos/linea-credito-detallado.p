
OUTPUT TO c:\tmp\lineas-de-credito.txt.
PUT UNFORMATTED 
    'CLIENTE|NOMBRE|INICIO|FIN|IMPORTE'
    SKIP.
FOR EACH gn-clie NO-LOCK WHERE codcia = 000.
    FOR EACH gn-cliel OF gn-clie NO-LOCK 
        WHERE gn-cliel.fchini >= 11/01/2014
        AND gn-cliel.fchfin <= 03/31/2015
        BY gn-cliel.fchfin:
        PUT UNFORMATTED
            gn-clie.codcli '|'
            gn-clie.nomcli '|'
            Gn-ClieL.FchIni '|'
            Gn-ClieL.FchFin '|'
            Gn-ClieL.ImpLC
            SKIP.
    END.
END.
