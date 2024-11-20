DEF VAR x-nompro LIKE gn-prov.nompro.
DEF VAR x-nomcli LIKE gn-clie.nomcli.

/* EXPOLIBRERIA */
OUTPUT TO c:\tmp\estadisticas.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00015'
    AND coddoc = 'cot'
    AND LOOKUP (flgest, 'P,C') > 0
    AND fchped >= 10/25/2010,
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK,
    FIRST gn-ven OF faccpedi NO-LOCK:
    FIND gn-prov WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = almmmatg.codpr1 NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.
    ELSE x-nompro = ''.
    FIND gn-clie WHERE gn-clie.codcia = 0
        AND gn-clie.codcli = faccpedi.codcli NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN x-nomcli = gn-clie.nomcli.
    ELSE x-nomcli = faccpedi.nomcli.
    DISPLAY
        'EXPO2010|'
        faccpedi.fchped 
        '|'
        faccpedi.nroped 
        '|'
        faccpedi.codcli     FORMAT 'x(11)'
        x-nomcli     FORMAT 'x(30)'
        '|'
        faccpedi.codven     FORMAT 'x(3)'
        gn-ven.nomven       FORMAT 'x(25)'
        '|'
        almmmatg.chr__02    FORMAT 'x(1)'
        '|'
        facdpedi.codmat     FORMAT 'x(6)'
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
        facdpedi.preuni FORMAT '>>>9.9999'
        '|'
        facdpedi.canped FORMAT '>>>>>9.99'
        '|'
        facdpedi.canate FORMAT '->>>>>9.99'
        '|'
        facdpedi.undvta 
        '|'
        facdpedi.implin FORMAT '>>>>>9.99'
        '|'
        "1"             
        WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE WIDTH 320.
END.
OUTPUT CLOSE.

/* PROVINCIAS */
OUTPUT TO c:\tmp\estadisticas.txt APPEND.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddiv = '00000'
    AND coddoc = 'cot'
    AND nroped BEGINS '040'
    AND LOOKUP (flgest, 'P,C') > 0
    AND fchped >= 10/25/2010,
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK,
    FIRST gn-ven OF faccpedi NO-LOCK:
    FIND gn-prov WHERE gn-prov.codcia = 0
        AND gn-prov.codpro = almmmatg.codpr1 NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.
    ELSE x-nompro = ''.
    FIND gn-clie WHERE gn-clie.codcia = 0
        AND gn-clie.codcli = faccpedi.codcli NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN x-nomcli = gn-clie.nomcli.
    ELSE x-nomcli = faccpedi.nomcli.
    DISPLAY
        'PROVINCIA|'
        faccpedi.fchped 
        '|'
        faccpedi.nroped 
        '|'
        faccpedi.codcli     FORMAT 'x(11)'
        x-nomcli     FORMAT 'x(30)'
        '|'
        faccpedi.codven     FORMAT 'x(3)'
        gn-ven.nomven       FORMAT 'x(25)'
        '|'
        almmmatg.chr__02    FORMAT 'x(1)'
        '|'
        facdpedi.codmat     FORMAT 'x(6)'
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
        facdpedi.preuni FORMAT '>>>9.9999'
        '|'
        facdpedi.canped FORMAT '>>>>>9.99'
        '|'
        facdpedi.canate FORMAT '->>>>>9.99'
        '|'
        facdpedi.undvta 
        '|'
        facdpedi.implin FORMAT '>>>>>9.99'
        '|'
        "1"             
        WITH STREAM-IO NO-BOX NO-LABELS NO-UNDERLINE WIDTH 320.
END.
OUTPUT CLOSE.
