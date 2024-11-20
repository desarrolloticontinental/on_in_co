DEF BUFFER cotizacion FOR faccpedi.
DEF BUFFER pedido FOR faccpedi.
DEF BUFFER guia FOR ccbcdocu.

DEF VAR x-nrohr AS CHAR.

OUTPUT TO d:\tmp\malfacturado.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 001
    AND coddiv = '00040'
    AND LOOKUP(coddoc, 'fac,bol') > 0
    AND fchdoc >= 01/01/19
    AND flgest <> 'A',
    FIRST pedido NO-LOCK WHERE pedido.codcia = 1
    AND pedido.coddoc = ccbcdocu.codped 
    AND pedido.nroped = ccbcdocu.nroped,
    FIRST cotizacion NO-LOCK WHERE cotizacion.codcia = 1
    AND cotizacion.coddoc = pedido.codref
    AND cotizacion.nroped = pedido.nroref
    AND cotizacion.coddiv = '00015'
    AND cotizacion.libre_c01 = '10015',
    EACH ccbddocu OF ccbcdocu NO-LOCK,
    FIRST Almmmatg OF ccbddocu NO-LOCK,
    FIRST vtalistamay NO-LOCK WHERE vtalistamay.codcia = 1
    AND vtalistamay.coddiv = cotizacion.libre_c01
    AND vtalistamay.codmat = ccbddocu.codmat
    AND VtaListaMay.Chr__01 <> ccbddocu.undvta:
    x-nrohr = ''.
    FOR EACH guia NO-LOCK WHERE guia.codcia = 1
        AND guia.coddoc = 'G/R'
        AND guia.codref = ccbcdocu.coddoc
        AND guia.nroref = ccbcdocu.nrodoc
        AND guia.flgest <> 'A',
        EACH di-rutad NO-LOCK WHERE di-rutad.codcia = 1
        AND di-rutad.coddoc = 'h/r'
        AND di-rutad.codref = guia.coddoc
        AND di-rutad.nroref = guia.nrodoc:
        x-nrohr = di-rutad.nrodoc.
    END.
    PUT UNFORMATTED
        cotizacion.coddoc                   '|'
        cotizacion.nroped                   '|'
        cotizacion.fchped                   '|'
        ccbcdocu.coddoc                     '|'
        ccbcdocu.nrodoc                     '|'
        ccbcdocu.fchdoc                     '|'
        ccbcdocu.codcli                     '|'
        ccbcdocu.nomcli                     '|'
        ccbddocu.codmat                     '|'
        almmmatg.desmat                     '|'
        ccbddocu.undvta                     '|'
        vtalistamay.CHR__01                 '|'
        ccbddocu.candes                     '|'
        ccbddocu.factor                     '|'
        (ccbddocu.implin / ccbddocu.candes) '|'
        vtalistamay.preofi                  '|'
        almmmatg.codfam                     '|'
        almmmatg.subfam                     '|'
        x-nrohr                             '|'
        SKIP.
END.
OUTPUT CLOSE.

