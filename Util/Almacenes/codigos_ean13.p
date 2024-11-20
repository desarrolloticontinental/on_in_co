DEFINE VARIABLE cDesFam AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSubFam AS CHARACTER   NO-UNDO.

    
OUTPUT TO "d:\tmp\EAN13-14.txt".
PUT UNFORMATTED
    "CodMat|Descripcion|CodMar|Marca|Familia|SubFamilia|Unidad|Categoria|Codigo EAN13|"
    "Codigo EN14-1|Equivalencia-1|"
    "Codigo EN14-2|Equivalencia-2|"
    "Codigo EN14-3|Equivalencia-3"
    SKIP.

FOR EACH almmmatg WHERE almmmatg.codcia = 1 NO-LOCK:
    FIND FIRST almtfam WHERE almtfam.codcia = 1
        AND almtfam.codfam = almmmatg.codfam NO-LOCK NO-ERROR.
    IF AVAIL almtfam THEN cDesFam = almmmatg.codfam + " - " + 
        almtfam.desfam.
    ELSE cDesFam = "".

    FIND FIRST almsfam WHERE almsfam.codcia = 1
        AND almsfam.codfam = almmmatg.codfam
        AND almsfam.subfam = almmmatg.subfam NO-LOCK NO-ERROR.
    IF AVAIL almsfam THEN cSubFam = almmmatg.subfam + " - " +
        almsfam.dessub.
    ELSE cSubFam = "".
    FIND Almmmat1 OF Almmmatg NO-LOCK NO-ERROR.

    PUT UNFORMATTED
        almmmatg.codmat "|"
        almmmatg.desmat "|"
        almmmatg.codmar "|"
        almmmatg.desmar "|"
        cDesFam         "|"
        cSubFam         "|"
        Almmmatg.UndStk "|"
        Almmmatg.CatConta[1] "|"
        Almmmatg.CodBrr "|"
        .
    IF AVAILABLE Almmmat1 THEN
        PUT UNFORMATTED
            almmmat1.barra[1] '|'
            almmmat1.equival[1] '|'
            almmmat1.barra[2] '|'
            almmmat1.equival[2] '|'
            almmmat1.barra[3] '|'
            almmmat1.equival[3].
    ELSE PUT '|||||'.
    PUT SKIP.
END.
OUTPUT TO CLOSE.
