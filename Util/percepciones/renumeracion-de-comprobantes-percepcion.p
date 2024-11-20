DISABLE TRIGGERS FOR LOAD OF ccbcmvto.
DISABLE TRIGGERS FOR LOAD OF ccbdmvto.

DEF TEMP-TABLE t-cab LIKE ccbcmvto.
DEF TEMP-TABLE t-det LIKE ccbdmvto.


DO TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
    FOR EACH ccbcmvto WHERE codcia = 1
        AND coddoc = 'per'
        AND nrodoc BEGINS '915'
        AND nrodoc >= '915005001':
        CREATE t-cab.
        BUFFER-COPY ccbcmvto
            TO t-cab
            ASSIGN t-cab.nrodoc = '914' + SUBSTRING(ccbcmvto.nrodoc,4).
        FOR EACH ccbdmvto WHERE ccbdmvto.codcia = ccbcmvto.codcia
            AND ccbdmvto.coddiv = ccbcmvto.coddiv
            AND ccbdmvto.coddoc = ccbcmvto.coddoc
            AND ccbdmvto.nrodoc = ccbcmvto.nrodoc:
            CREATE t-det.
            BUFFER-COPY ccbdmvto
                TO t-det
                ASSIGN t-det.nrodoc = t-cab.nrodoc.
            DELETE ccbdmvto.
        END.
        DELETE ccbcmvto.
    END.
    FOR EACH t-cab:
        CREATE ccbcmvto.
        BUFFER-COPY t-cab TO ccbcmvto.
    END.
    FOR EACH t-det:
        CREATE ccbdmvto.
        BUFFER-COPY t-det TO ccbdmvto.
    END.
END.


