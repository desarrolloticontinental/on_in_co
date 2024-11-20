DEF VAR x-nompro LIKE gn-prov.nompro.
DEF VAR x-nomcli LIKE gn-clie.nomcli.

DEF TEMP-TABLE detalle
    FIELD codcia LIKE faccpedi.codcia
    FIELD evento AS CHAR FORMAT 'x(10)'
    FIELD codven LIKE faccpedi.codven
    FIELD codmat LIKE facdpedi.codmat
    FIELD canped LIKE facdpedi.canped
    FIELD canate LIKE facdpedi.canate
    FIELD undvta LIKE facdpedi.undvta
    FIELD implin LIKE facdpedi.implin
    INDEX llave01 AS PRIMARY codmat codven.

/* PROVINCIAS */
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND coddoc = 'cot'
    AND nroped BEGINS '040'
    AND LOOKUP (flgest, 'P,C') > 0
    AND fchped >= 10/25/2010,
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    FIND detalle WHERE detalle.codmat = facdpedi.codmat
        AND detalle.codven = faccpedi.codven
        NO-ERROR.
    IF NOT AVAILABLE detalle THEN DO:
        CREATE detalle.
        ASSIGN
            detalle.codcia = faccpedi.codcia
            detalle.evento = "PROVINCIAS"
            detalle.codven = faccpedi.codven
            detalle.codmat = facdpedi.codmat.
    END.
    ASSIGN
        detalle.undvta = almmmatg.undbas
        detalle.canped = detalle.canped + ( facdpedi.canped * facdpedi.factor )
        detalle.canate = detalle.canate + ( facdpedi.canate * facdpedi.factor )
        detalle.implin = detalle.implin + facdpedi.implin.
END.
OUTPUT TO c:\tmp\estadisticas.txt APPEND.
FOR EACH detalle NO-LOCK,
    FIRST almmmatg OF detalle NO-LOCK,
    FIRST gn-ven OF detalle NO-LOCK:
    FIND gn-prov WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = almmmatg.codpr1 NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.
    ELSE x-nompro = ''.
    DISPLAY
        detalle.evento
        '|'
        detalle.codven     FORMAT 'x(3)'
        gn-ven.nomven       FORMAT 'x(30)'
        '|'
        almmmatg.chr__02    FORMAT 'x(1)'
        '|'
        detalle.codmat     FORMAT 'x(6)'
        almmmatg.desmat 
        '|'
        almmmatg.desmar     FORMAT 'x(15)'  
        '|'
        almmmatg.codfam 
        '|'
        almmmatg.subfam 
        '|'
        almmmatg.codpr1     FORMAT 'x(11)'
        x-nompro      FORMAT 'x(40)'
        '|'
        detalle.canped FORMAT '>>>>>9.99'
        '|'
        detalle.canate FORMAT '->>>>>9.99'
        '|'
        detalle.undvta 
        '|'
        detalle.implin FORMAT '>>>>>9.99'
        '|'
        WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE WIDTH 320.
END.
OUTPUT CLOSE.

