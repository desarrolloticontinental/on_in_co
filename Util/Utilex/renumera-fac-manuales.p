DEF VAR s-codcia AS INT INIT 1.
DEF VAR s-coddiv AS CHAR INIT '00517'.

DEF TEMP-TABLE t-cdocu LIKE ccbcdocu.
DEF TEMP-TABLE t-ddocu LIKE ccbddocu.
DEF TEMP-TABLE t-dcaja LIKE ccbdcaja.
DEF TEMP-TABLE t-cmov LIKE almcmov.
DEF TEMP-TABLE t-dmov LIKE almdmov.

principal:
DO TRANSACTION ON ERROR UNDO, RETURN ON STOP UNDO, RETURN:
    FOR EACH ccbcdocu WHERE codcia = s-codcia
        AND coddiv = s-coddiv
        AND nrodoc BEGINS '774':
        CREATE t-cdocu.
        BUFFER-COPY ccbcdocu TO t-cdocu
            ASSIGN t-cdocu.nrodoc = '744' + SUBSTRING(ccbcdocu.nrodoc,4).
        FOR EACH ccbddocu OF ccbcdocu:
            CREATE t-ddocu.
            BUFFER-COPY ccbddocu TO t-ddocu
                ASSIGN t-ddocu.nrodoc = t-cdocu.nrodoc.
            DELETE ccbddocu.
        END.
        FOR EACH ccbdcaja WHERE ccbdcaja.codcia = s-codcia
            AND ccbdcaja.codref = ccbcdocu.coddoc
            AND ccbdcaja.nroref = ccbcdocu.nrodoc:
            CREATE t-dcaja.
            BUFFER-COPY ccbdcaja TO t-dcaja
                ASSIGN t-dcaja.nroref = t-cdocu.nrodoc.
            DELETE ccbdcaja.
        END.
        FOR EACH almcmov WHERE almcmov.codcia = s-codcia
            AND almcmov.codref = ccbcdocu.coddoc
            AND almcmov.nroref = ccbcdocu.nrodoc:
            CREATE t-cmov.
            BUFFER-COPY almcmov TO t-cmov
                ASSIGN t-cmov.nroser = 744 t-cmov.nroref = t-cdocu.nrodoc.
            FOR EACH almdmov OF almcmov:
                CREATE t-dmov.
                BUFFER-COPY almdmov TO t-dmov
                    ASSIGN t-dmov.nroser = t-cmov.nroser.
                DELETE almdmov.
            END.
            DELETE almcmov.
        END.
        DELETE ccbcdocu.
    END.
    /* Grabamos la informacion */
    FOR EACH t-cdocu:
        CREATE ccbcdocu.
        BUFFER-COPY t-cdocu TO ccbcdocu NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO principal, RETURN.
    END.
    FOR EACH t-ddocu:
        CREATE ccbddocu.
        BUFFER-COPY t-ddocu TO ccbddocu NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO principal, RETURN.
    END.
    FOR EACH t-dcaja:
        CREATE ccbdcaja.
        BUFFER-COPY t-dcaja TO ccbdcaja NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO principal, RETURN.
    END.
    FOR EACH t-cmov:
        CREATE almcmov.
        BUFFER-COPY t-cmov TO almcmov NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO principal, RETURN.
    END.
    FOR EACH t-dmov:
        CREATE almdmov.
        BUFFER-COPY t-dmov TO almdmov NO-ERROR.
        IF ERROR-STATUS:ERROR THEN UNDO principal, RETURN.
    END.
END.

