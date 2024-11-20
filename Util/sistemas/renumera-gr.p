/* Renumera Guias de Remision */
DEF TEMP-TABLE t-ccbcdocu LIKE ccbcdocu.
DEF TEMP-TABLE t-ccbddocu LIKE ccbddocu.
DEF VAR x-nroini AS INT INIT 263656.

FOR EACH ccbcdocu EXCLUSIVE-LOCK WHERE codcia = 1 AND
    coddoc = 'g/r'
    AND nrodoc >= '015263642' AND nrodoc <= '015263655':
    CREATE t-ccbcdocu.
    BUFFER-COPY ccbcdocu TO t-ccbcdocu
        ASSIGN t-ccbcdocu.nrodoc = '015' + STRING(x-nroini,'999999').
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
        CREATE t-ccbddocu.
        BUFFER-COPY ccbddocu TO t-ccbddocu
            ASSIGN t-ccbddocu.nrodoc = t-ccbcdocu.nrodoc.
    END.
    ccbcdocu.flgest = "A".
    x-nroini = x-nroini + 1.
END.

FOR EACH t-ccbcdocu:
    CREATE ccbcdocu.
    BUFFER-COPY t-ccbcdocu TO ccbcdocu.
    FOR EACH t-ccbddocu OF t-ccbcdocu:
        CREATE ccbddocu.
        BUFFER-COPY t-ccbddocu TO ccbddocu.
    END.
END.
