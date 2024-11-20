

DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.


DEF VAR x-canate AS DEC NO-UNDO.
DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-orden FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.

DEF VAR s-codcia AS INT INIT 001.

FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddoc = 'cot'
    AND nroped = '506749830':
    FOR EACH facdpedi OF faccpedi EXCLUSIVE-LOCK:
        x-canate = 0.
        /* por pedidos en curso */
        FOR EACH b-cpedi NO-LOCK WHERE b-cpedi.codcia = s-codcia
            AND b-cpedi.coddoc = 'ped'
            AND b-cpedi.coddiv = faccpedi.coddiv
            AND b-cpedi.codref = faccpedi.coddoc
            AND b-cpedi.nroref = faccpedi.nroped
            AND LOOKUP(b-cpedi.flgest, 'C,G,X') > 0,
            EACH b-dpedi OF b-cpedi NO-LOCK WHERE b-dpedi.codmat = facdpedi.codmat:
            x-canate = x-canate + b-dpedi.canped.
        END.
        IF facdpedi.canate <> x-canate  THEN DO:
            MESSAGE facdpedi.nroped facdpedi.codmat facdpedi.canped facdpedi.canate x-canate
                VIEW-AS ALERT-BOX.
            facdpedi.canate = x-canate.
        END.
    END.
END.
