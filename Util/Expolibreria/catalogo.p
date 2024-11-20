DEFINE VARIABLE cDesFam AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSubFam AS CHARACTER   NO-UNDO.

    
OUTPUT TO "c:\tmp\catalogo.txt".
PUT UNFORMATTED
    "Codigo|Nombre|Familia|SubFamilia|CodMar|Marca|Unidad|Afecto Percepcion"
    SKIP.

FOR EACH almmmatg WHERE almmmatg.codcia = 1 AND almmmatg.tpoart <> "D" NO-LOCK,
    FIRST almtfami OF almmmatg NO-LOCK,
    FIRST almsfami OF almmmatg NO-LOCK:
    PUT UNFORMATTED
        almmmatg.codmat "|"
        almmmatg.desmat "|"
        almmmatg.codfam "|"
        almmmatg.subfam "|"
        almmmatg.codmar "|"
        almmmatg.desmar "|"
        Almmmatg.UndStk "|"
        AlmSFami.Libre_c05
        SKIP.
END.
OUTPUT TO CLOSE.
