DEF VAR x-codcli AS CHAR FORMAT 'x(11)'.

OUTPUT TO c:\tmp\detalle.txt.
PUT UNFORMATTED 
    'COTIZACION|NROCOT|DOC|NUMERO|EMISION|FLAG|CLIENTE|NOMBRE|CONDICION|TOTAL|PRODUCTO|DESCRIPCION|LINEA|SUBLINEA|CANTPEDIDA|CANTDESP|UNIDAD|IMPORTE|DIVDESPACHO|COMPROB|NROCOMPROB|FCHCOMPROB|FLGCOMPROB|DESPACHADO|IMPDESPACHADO'
    SKIP.
    FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 001
        AND LOOKUP(faccpedi.coddiv, '00015,10015,20015') > 0
        AND faccpedi.coddoc = 'ped'
        AND LOOKUP(faccpedi.flgest, 'A,R') = 0
        AND faccpedi.fchped >= 10/01/2012,
        EACH facdpedi OF faccpedi NO-LOCK,
        FIRST Almmmatg OF Facdpedi NO-LOCK:
        FIND FIRST ccbcdocu WHERE ccbcdocu.codcia = 001
            AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
            AND ccbcdocu.codped = faccpedi.coddoc
            AND ccbcdocu.nroped = faccpedi.nroped
            AND ccbcdocu.flgest <> 'A'
            AND CAN-FIND(FIRST ccbddocu OF ccbcdocu WHERE ccbddocu.codmat = facdpedi.codmat NO-LOCK)
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE ccbcdocu THEN DO:
            PUT UNFORMATTED
                faccpedi.codref '|'
                faccpedi.nroref '|'
                faccpedi.coddoc '|'
                faccpedi.nroped '|'
                faccpedi.fchped '|'
                faccpedi.flgest '|'
                faccpedi.codcli '|'
                faccpedi.nomcli '|'
                faccpedi.fmapgo '|'
                faccpedi.imptot '|'
                facdpedi.codmat '|'
                almmmatg.desmat '|'
                almmmatg.codfam '|'
                almmmatg.subfam '|'
                facdpedi.canped '|'
                facdpedi.canate '|'
                facdpedi.undvta '|'
                facdpedi.implin
                SKIP.
            NEXT.
        END.
        FOR EACH ccbcdocu WHERE ccbcdocu.codcia = 001
            AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
            AND ccbcdocu.codped = faccpedi.coddoc
            AND ccbcdocu.nroped = faccpedi.nroped
            AND ccbcdocu.flgest <> 'A',
            FIRST ccbddocu OF ccbcdocu NO-LOCK WHERE ccbddocu.codmat = facdpedi.codmat:
            PUT UNFORMATTED
                faccpedi.codref '|'
                faccpedi.nroref '|'
                faccpedi.coddoc '|'
                faccpedi.nroped '|'
                faccpedi.fchped '|'
                faccpedi.flgest '|'
                faccpedi.codcli '|'
                faccpedi.nomcli '|'
                faccpedi.fmapgo '|'
                faccpedi.imptot '|'
                facdpedi.codmat '|'
                almmmatg.desmat '|'
                almmmatg.codfam '|'
                almmmatg.subfam '|'
                facdpedi.canped '|'
                facdpedi.canate '|'
                facdpedi.undvta '|'
                facdpedi.implin '|'
                ccbcdocu.coddiv '|'
                ccbcdocu.coddoc '|'
                ccbcdocu.nrodoc '|'
                ccbcdocu.fchdoc '|'
                ccbcdocu.flgest '|'
                ccbddocu.candes '|'
                ccbddocu.implin 
                SKIP.
        END.
    END.

