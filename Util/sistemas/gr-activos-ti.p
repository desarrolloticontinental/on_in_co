OUTPUT TO d:\guias.txt.

PUT UNFORMATTED
    'COD|NRO|FECHA|CLIENTE|NOMBRE|DIRECCION|REF 1|REF 2' SKIP.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1 
    and coddoc = 'g/r' 
    and nrodoc begins '251' 
    and tpofac = 'I' 
    and codant = 'cliente' 
    and codped = 'almacen' 
    and fchdoc >= 01/01/2020 
    and flgest <> 'A':
    PUT UNFORMATTED 
        coddoc '|'
        nrodoc '|'
        fchdoc '|'
        codcli '|'
        nomcli '|'
        dircli '|'
        lugent '|'
        lugent2
        SKIP.
END.
OUTPUT CLOSE.

