DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00514' NO-UNDO.
DEF VAR s-nroser AS INT INIT 400 NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'FAC' NO-UNDO.
DEF VAR x-fchdoc AS DATE NO-UNDO.
DEF TEMP-TABLE t-cdocu LIKE ccbcdocu.
DEF TEMP-TABLE t-ddocu LIKE ccbddocu.
DEF VAR s-nrodoc AS INT INIT 1 NO-UNDO.
DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.

DO TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
    FOR EACH ccbcdocu WHERE ccbcdocu.codcia = 1
        AND ccbcdocu.coddiv = s-coddiv
        AND ccbcdocu.coddoc = s-coddoc
        AND ccbcdocu.flgest <> 'A'
        AND ccbcdocu.fchdoc >= DATE(12,25,2016)
        AND ccbcdocu.fchdoc <= DATE(01,31,2017)
        AND ccbcdocu.tipo = 'MOSTRADOR':
        IF ccbcdocu.fchdoc <= DATE(12,31,2016) THEN x-fchdoc = DATE(01,30,2017).
        ELSE x-fchdoc = DATE(01,31,2017).
        CREATE t-cdocu.
        BUFFER-COPY ccbcdocu
            TO t-cdocu
            ASSIGN
            t-cdocu.nrodoc = STRING(s-nroser, '999') + STRING(s-nrodoc, '99999999')
            t-cdocu.fchdoc = x-fchdoc
            t-cdocu.tpofac = "B"    /* por baja sunat */
            t-cdocu.codref = ccbcdocu.coddoc
            t-cdocu.nroref = ccbcdocu.nrodoc
            t-cdocu.libre_c01 = ccbcdocu.coddoc
            t-cdocu.libre_c02 = ccbcdocu.nrodoc.
        FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
            CREATE t-ddocu.
            BUFFER-COPY ccbddocu
                TO t-ddocu
                ASSIGN
                t-ddocu.nrodoc = t-cdocu.nrodoc
                t-ddocu.fchdoc = t-cdocu.fchdoc.
        END.
        ASSIGN
            s-nrodoc = s-nrodoc + 1.
        ASSIGN
            ccbcdocu.flgest = "B".      /* Baja Sunat */
    END.
    FOR EACH t-cdocu:
        CREATE ccbcdocu.
        BUFFER-COPY t-cdocu TO ccbcdocu.
    END.
    FOR EACH t-ddocu:
        CREATE ccbddocu.
        BUFFER-COPY t-ddocu TO ccbddocu.
    END.
END.

