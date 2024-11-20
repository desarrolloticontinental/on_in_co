DEF TEMP-TABLE t-rutac LIKE di-rutac.
DEF TEMP-TABLE t-rutad LIKE di-rutad.
DEF BUFFER bt-rutad FOR t-rutad.
DEF TEMP-TABLE tt-rutac LIKE di-rutac.
DEF TEMP-TABLE tt-rutad LIKE di-rutad.

/* cargamos la información */
FOR EACH di-rutac NO-LOCK WHERE codcia = 1
    AND DI-RutaC.CodDoc = 'H/R'
    AND DI-RutaC.FchDoc >= 12/01/2009
    AND flgest <> 'A':
    CREATE t-rutac.
    BUFFER-COPY di-rutac TO t-rutac.
    FOR EACH di-rutad OF di-rutac NO-LOCK,
        FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.coddoc = di-rutad.codref
        AND ccbcdocu.nrodoc = di-rutad.nroref:
        CREATE t-rutad.
        BUFFER-COPY di-rutad TO t-rutad.
    END.
END.

/* buscamos duplicados */
DEF VAR sw AS LOG NO-UNDO.

FOR EACH t-rutac NO-LOCK:
    sw = NO.
    FOR EACH t-rutad OF t-rutac NO-LOCK:
        IF CAN-FIND(FIRST bt-rutad WHERE bt-rutad.codref = t-rutad.codref
                    AND bt-rutad.nroref = t-rutad.nroref
                    AND ROWID(bt-rutad) <> ROWID(t-rutad) NO-LOCK) THEN DO:
            CREATE tt-rutad.
            BUFFER-COPY t-rutad TO tt-rutad.
            IF sw = NO THEN DO:
                CREATE tt-rutac.
                BUFFER-COPY t-rutac TO tt-rutac.
                sw = YES.
            END.
        END.
    END.
END.


OUTPUT TO c:\tmp\hr-duplicados.txt.
FOR EACH tt-rutac NO-LOCK, EACH tt-rutad OF tt-rutac NO-LOCK,
    FIRST ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = 1
    AND ccbcdocu.coddoc = tt-rutad.codref
    AND ccbcdocu.nrodoc = tt-rutad.nroref:
    DISPLAY 
        ccbcdocu.coddoc 
        "|"
        ccbcdocu.nrodoc 
        "|"
        ccbcdocu.nomcli
        "|"
        ccbcdocu.codcli
        "|"
        ccbcdocu.codmon
        "|"
        ccbcdocu.imptot
        "|"
        tt-RutaC.CodDiv FORMAT 'x(5)'
        "|"
        tt-RutaC.CodVeh FORMAT 'x(10)'
        "|"
        tt-RutaC.FchDoc 
        "|"
        tt-RutaC.Nomtra 
        "|"
        tt-RutaC.NroDoc 
        "|"
        tt-RutaC.usuario
        WITH STREAM-IO NO-BOX DOWN WIDTH 320.
END.
OUTPUT CLOSE.
