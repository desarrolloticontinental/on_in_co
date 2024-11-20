DEF TEMP-TABLE detalle
    FIELD codmat LIKE almmmatg.codmat
    FIELD desmat AS CHAR FORMAT 'x(60)'
    FIELD codfam LIKE almmmatg.codfam
    FIELD subfam LIKE almmmatg.subfam
    FIELD flete_arequipa AS DEC
    FIELD flete_trujillo AS DEC.

DEF VAR x-flete AS DEC.
DEF VAR pcoddiv AS CHAR.

pcoddiv = '20060'.
FOR EACH almcatvtad NO-LOCK WHERE almcatvtad.codcia = 1
    AND almcatvtad.coddiv = pcoddiv,
    FIRST almmmatg OF almcatvtad NO-LOCK:
    FIND detalle WHERE detalle.codmat = almmmatg.codmat NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.codmat = almmmatg.codmat
        detalle.desmat = almmmatg.desmat
        detalle.codfam = almmmatg.codfam
        detalle.subfam = almmmatg.subfam.
    RUN gn/flete-unitario (almmmatg.codmat,
                          pcoddiv,
                          1,
                          1,
                          OUTPUT x-flete).
    ASSIGN
        detalle.flete_arequipa = x-flete.
END.
pcoddiv = '20067'.
FOR EACH almcatvtad NO-LOCK WHERE almcatvtad.codcia = 1
    AND almcatvtad.coddiv = pcoddiv,
    FIRST almmmatg OF almcatvtad NO-LOCK:
    FIND detalle WHERE detalle.codmat = almmmatg.codmat NO-ERROR.
    IF NOT AVAILABLE detalle THEN CREATE detalle.
    ASSIGN
        detalle.codmat = almmmatg.codmat
        detalle.desmat = almmmatg.desmat
        detalle.codfam = almmmatg.codfam
        detalle.subfam = almmmatg.subfam.
    RUN gn/flete-unitario (almmmatg.codmat,
                          pcoddiv,
                          1,
                          1,
                          OUTPUT x-flete).
    ASSIGN
        detalle.flete_trujillo = x-flete.
END.
OUTPUT TO d:\tmp\fletes.txt.
FOR EACH detalle:
    PUT UNFORMATTED
        detalle.codmat '|'
        detalle.desmat '|'
        detalle.codfam '|'
        detalle.subfam '|'
        detalle.flete_arequipa '|'
        detalle.flete_trujillo
        SKIP.
END.
OUTPUT CLOSE.

