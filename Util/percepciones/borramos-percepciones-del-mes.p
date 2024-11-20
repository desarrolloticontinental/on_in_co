
FOR EACH ccbcmvto WHERE codcia = 1 AND coddoc = 'per' 
    AND fchdoc >= 11/01/2013 AND fchdoc <= 11/30/2013
    AND NOT nrodoc BEGINS '914'.
    FOR EACH Ccbdmvto WHERE Ccbdmvto.codcia = Ccbcmvto.codcia
        AND Ccbdmvto.coddiv = Ccbcmvto.coddiv
        AND Ccbdmvto.coddoc = Ccbcmvto.coddoc
        AND Ccbdmvto.nrodoc = Ccbcmvto.nrodoc:
        DELETE ccbdmvto.
    END.
    DELETE ccbcmvto.
END.
