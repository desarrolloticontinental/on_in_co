DEF TEMP-TABLE t-faccpedi LIKE faccpedi.
DEF TEMP-TABLE t-facdpedi LIKE facdpedi.

DEF BUFFER pedido FOR faccpedi.

FOR EACH faccpedi NO-LOCK WHERE codcia = 1
    AND coddoc = 'cot'
    AND fchped >= DATE(01,01,2019)
    AND fchped <= DATE(01,15,2019)
    AND LOOKUP(coddiv, '00018,00015,00002') > 0
    AND flgest = 'C':
    CREATE t-faccpedi.
    BUFFER-COPY faccpedi TO t-faccpedi.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE t-facdpedi.
        BUFFER-COPY facdpedi TO t-facdpedi 
            ASSIGN 
            t-faccpedi.usuario = 'MRC-00'
            t-faccpedi.fchven = DATE(12,31,2019).
    END.
    FOR EACH pedido NO-LOCK WHERE pedido.codcia = 1
        AND pedido.coddoc = 'ped'
        AND pedido.codref = faccpedi.coddoc
        AND pedido.nroref = faccpedi.nroped
        AND pedido.flgest = 'C':
        CREATE t-faccpedi.
        BUFFER-COPY pedido TO t-faccpedi
            ASSIGN 
            t-faccpedi.flgest = 'G' 
            t-faccpedi.usuario = 'MRC-00'
            t-faccpedi.fchven = DATE(12,31,2019).
        FOR EACH facdpedi OF pedido NO-LOCK:
            CREATE t-facdpedi.
            BUFFER-COPY facdpedi TO t-facdpedi
                ASSIGN
                t-facdpedi.canate = 0
                t-facdpedi.flgest = 'P'.
        END.
    END.
END.

OUTPUT TO d:\faccpedi.d.
FOR EACH t-faccpedi:
    EXPORT t-faccpedi.
END.
OUTPUT TO d:\facdpedi.d.
FOR EACH t-facdpedi:
    EXPORT t-facdpedi.
END.


DEF TEMP-TABLE t-almcrepo LIKE almcrepo.
DEF TEMP-TABLE t-almdrepo LIKE almdrepo.

FOR EACH almcrepo NO-LOCK WHERE almcrepo.codcia = 1
    AND (almcrepo.FchDoc >= date(08,01,2019) OR almcrepo.Fecha >= date(08,01,2019))
    AND almcrepo.flgest <> 'A':
    CREATE t-almcrepo.
    BUFFER-COPY almcrepo TO t-almcrepo ASSIGN t-almcrepo.flgest = 'P' t-almcrepo.usuario = 'MRC-00'.
    FOR EACH almdrepo OF almcrepo NO-LOCK:
        CREATE t-almdrepo.
        BUFFER-COPY almdrepo TO t-almdrepo.
    END.
END.

OUTPUT TO d:\almcrepo.d.
FOR EACH t-almcrepo.
    EXPORT t-almcrepo.
END.
OUTPUT TO d:\almdrepo.d.
FOR EACH t-almdrepo.
    EXPORT t-almdrepo.
END.

