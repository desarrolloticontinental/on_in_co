DEF VAR x-nompro LIKE gn-prov.nompro.
DEF VAR x-nomven LIKE gn-ven.nomven.

OUTPUT TO c:\tmp\expo2011\estadisticas.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND coddiv = '00015'
    AND fchped >= 10/25/2010
    AND flgest <> 'A',
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK,
    FIRST almtfami OF almmmatg NO-LOCK,
    FIRST almsfami NO-LOCK WHERE almsfami.codcia = almmmatg.codcia
    AND almsfami.codfam = almmmatg.codfam
    AND almsfami.subfam = almmmatg.subfam:
    FIND gn-prov WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = almmmatg.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.
    ELSE x-nompro = ''.
    FIND gn-ven OF faccpedi NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ven THEN x-nomven = gn-ven.nomven.
    ELSE x-nomven = ''.
    DISPLAY
        faccpedi.codcli + ' ' + faccpedi.nomcli FORMAT 'x(50)' '|'
        faccpedi.codven + ' ' + x-nomven FORMAT 'x(30)' '|'
        facdpedi.codmat + ' ' + almmmatg.desmat FORMAT 'x(60)' '|'
        almmmatg.desmar FORMAT 'x(20)' '|'
        almmmatg.codfam + ' ' + almtfami.desfam FORMAT 'x(25)' '|'
        almmmatg.subfam + ' ' + almsfami.dessub FORMAT 'x(25)' '|'
        almmmatg.codpr1 + ' ' + x-nompro FORMAT 'x(50)' '|'
        facdpedi.undvta '|'
        facdpedi.canped '|'
        facdpedi.implin
        WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE WIDTH 600.
END.
OUTPUT CLOSE.
