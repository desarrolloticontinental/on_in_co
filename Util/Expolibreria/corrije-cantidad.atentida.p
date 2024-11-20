DEF VAR x-canate AS DEC NO-UNDO.
DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = 001
    AND gn-divi.coddiv = '00018':
    FOR EACH faccpedi WHERE codcia = 1
        AND faccpedi.coddiv = gn-divi.coddiv
        AND coddoc = 'cot'
        AND coddiv = gn-divi.coddiv
        AND fchped >= 10/01/2013
        AND fchped < TODAY
        AND LOOKUP(flgest, "P,C") > 0:
        FOR EACH facdpedi OF faccpedi NO-LOCK:
            x-canate = 0.
            FOR EACH b-cpedi NO-LOCK WHERE b-cpedi.codcia = 1
                AND b-cpedi.coddoc = 'ped'
                AND b-cpedi.coddiv = faccpedi.coddiv
                AND b-cpedi.codref = faccpedi.coddoc
                AND b-cpedi.nroref = faccpedi.nroped
                AND LOOKUP (b-cpedi.flgest, 'A,R') = 0,
                EACH b-dpedi OF b-cpedi NO-LOCK WHERE b-dpedi.codmat = facdpedi.codmat:
                /*x-canate = x-canate + b-dpedi.canate.*/
                x-canate = x-canate + b-dpedi.canped.
            END.
            IF facdpedi.canate <> x-canate THEN DO:
                IF x-canate > facdpedi.canate THEN x-canate = facdpedi.canate.
                IF facdpedi.canate <> x-canate THEN DO:
                    DISPLAY
                    faccpedi.fchped
                    faccpedi.coddiv
                    faccpedi.nroped
                    facdpedi.codmat
                    facdpedi.canate
                    x-canate
                    WITH STREAM-IO NO-BOX WIDTH 200 NO-LABELS.
    /*                 facdpedi.canate = x-canate.                                                             */
    /*                 DISPLAY faccpedi.fchped facdpedi.nroped facdpedi.codmat facdpedi.canped facdpedi.canate */
    /*                     WITH STREAM-IO NO-BOX.                                                              */
    /*                 PAUSE 0.                                                                                */
                END.

            END.
        END.
    END.
END.

