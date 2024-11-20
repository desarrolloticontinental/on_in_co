DEF BUFFER pedido FOR faccpedi.
DEF BUFFER b-cdocu FOR ccbcdocu.
DEF VAR x-impate AS DEC NO-UNDO.

OUTPUT TO d:\tmp\ccamus.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = 1 
    and coddiv = '00015' 
    and fchped >= 11/01/2015 
    and fchped <= 05/31/2016
    AND flgest <> 'A':
    /* buscamos pedidos */
    FOR EACH pedido NO-LOCK WHERE pedido.codcia = 001
        AND pedido.coddoc = 'ped'
        AND pedido.codref = faccpedi.coddoc
        AND pedido.nroref = faccpedi.nroped
        AND pedido.flgest <> 'A':
        /* buscamos facturas */
        x-impate = 0.
        FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 001
            AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
            AND ccbcdocu.codped = pedido.coddoc
            AND ccbcdocu.nroped = pedido.nroped
            AND ccbcdocu.flgest <> 'A':
            x-impate = x-impate + ccbcdocu.imptot.
            FOR EACH b-cdocu NO-LOCK WHERE b-cdocu.codcia = 001
                AND b-cdocu.coddoc = 'N/C'
                AND b-cdocu.cndcre = 'D'
                AND b-cdocu.codref = ccbcdocu.coddoc
                AND b-cdocu.nroref = ccbcdocu.nrodoc
                AND b-cdocu.flgest <> 'A':
                x-impate = x-impate - b-cdocu.imptot.
            END.
        END.
        PUT UNFORMATTED
            faccpedi.coddiv '|'
            faccpedi.coddoc '|'
            faccpedi.nroped '|'
            faccpedi.fchped '|'
            faccpedi.codcli '|'
            faccpedi.nomcli '|'
            faccpedi.imptot '|'
            faccpedi.libre_c01 '|'
            pedido.coddoc '|'
            pedido.nroped '|'
            pedido.fchped '|'
            pedido.codalm '|'
            pedido.divdes '|'
            pedido.imptot '|'
            x-impate
            SKIP.
    END.
END.
