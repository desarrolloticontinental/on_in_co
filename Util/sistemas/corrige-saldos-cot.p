DEF VAR x-canate AS DEC NO-UNDO.
DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-orden FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.

DEF VAR s-codcia AS INT INIT 001.

FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddoc = 'cot'
    AND coddiv = '00015'
    AND libre_c01 = '10015'
    AND fchped >= DATE(01,01,2019)
    AND flgest <> 'A' WITH FRAME f-Report:
    FOR EACH facdpedi OF faccpedi EXCLUSIVE-LOCK:
        x-canate = 0.
        /* por pedidos en curso */
        FOR EACH b-cpedi NO-LOCK WHERE b-cpedi.codcia = s-codcia
            AND b-cpedi.coddoc = 'ped'
            AND b-cpedi.coddiv = faccpedi.coddiv
            AND b-cpedi.codcli = faccpedi.codcli
            AND b-cpedi.codref = faccpedi.coddoc
            AND b-cpedi.nroref = faccpedi.nroped
            AND lookup(b-cpedi.flgest, 'p,x') > 0,
            EACH b-dpedi OF b-cpedi NO-LOCK WHERE b-dpedi.codmat = facdpedi.codmat:
            x-canate = x-canate + ( b-dpedi.canped - b-dpedi.canate ).
        END.
        /* por ordenes de despacho */
        FOR EACH b-cpedi NO-LOCK WHERE b-cpedi.codcia = s-codcia
            AND b-cpedi.coddoc = 'ped'
            AND b-cpedi.coddiv = faccpedi.coddiv
            AND b-cpedi.codcli = faccpedi.codcli
            AND b-cpedi.codref = faccpedi.coddoc
            AND b-cpedi.nroref = faccpedi.nroped
            AND lookup(b-cpedi.flgest, 'c,p') > 0:
            FOR EACH b-orden NO-LOCK WHERE b-orden.codcia = s-codcia
                AND b-orden.coddoc = 'o/d'
                AND b-orden.coddiv = faccpedi.coddiv
                AND b-orden.codcli = faccpedi.codcli
                AND b-orden.codref = b-cpedi.coddoc
                AND b-orden.nroref = b-cpedi.nroped
                AND LOOKUP(b-orden.flgest, 'c,p') > 0,
                EACH b-dpedi OF b-orden NO-LOCK WHERE b-dpedi.codmat = facdpedi.codmat:
                x-canate = x-canate + b-dpedi.canped.
            END.
        END.
        IF facdpedi.canate <> x-canate THEN DO:
            DISPLAY facdpedi.nroped facdpedi.codmat facdpedi.canped facdpedi.canate x-canate
                WITH STREAM-IO NO-BOX.
            /*facdpedi.canate = x-canate.*/
        END.
    END.
END.
