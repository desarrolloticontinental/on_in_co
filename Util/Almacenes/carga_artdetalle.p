
OUTPUT TO "D:/ArtFamSFam.txt".
PUT UNFORMATTED 
    "CODMAT|"
    "DESCRIPCION|"
    "MARCA|"
    "CODFAM|"
    "FAMILIA|"
    "SUBFAM|"
    "SUBFAMILIA|"
    "UND" SKIP.

FOR EACH almmmatg WHERE almmmatg.codcia = 1
    AND almmmatg.tpoart = "A" NO-LOCK:
    FIND FIRST Almtfami WHERE  Almtfami.CodCia = almmmatg.codcia
        AND Almtfami.codfam = almmmatg.codfam NO-LOCK NO-ERROR.

    FIND FIRST AlmSfami WHERE  AlmSfami.CodCia = almmmatg.codcia
        AND AlmSfami.codfam = almmmatg.codfam 
        AND AlmSfami.subfam = almmmatg.subfam NO-LOCK NO-ERROR.

    PUT UNFORMATTED
        almmmatg.codmat "|"
        almmmatg.desmat "|"
        almmmatg.desmar "|"
        almmmatg.codfam "|".
    IF AVAIL almtfami THEN 
        PUT UNFORMATTED Almtfami.desfam.
    PUT UNFORMATTED
        "|"
        almmmatg.subfam "|".
    IF AVAIL almsfami THEN 
        PUT UNFORMATTED AlmSFami.dessub.
    PUT UNFORMATTED 
        "|"
        Almmmatg.UndBas SKIP.        
END.
OUTPUT TO CLOSE.
