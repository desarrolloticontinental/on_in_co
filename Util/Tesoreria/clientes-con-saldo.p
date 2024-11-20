DEF TEMP-TABLE t-clie LIKE gn-clie.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND (coddoc = 'fac' OR coddoc = 'bol'
         OR coddoc = 'let' OR coddoc = 'n/d'
         OR coddoc = 'let')
    AND flgest = 'P',
    FIRST gn-clie NO-LOCK WHERE gn-clie.codcia = 000
    AND gn-clie.codcli = ccbcdocu.codcli:
    FIND t-clie OF gn-clie NO-ERROR.
    IF NOT AVAILABLE t-clie THEN DO:
        CREATE t-clie.
        BUFFER-COPY gn-clie TO t-clie.
    END.
END.

OUTPUT TO c:\tmp\clientes-con-saldo.txt.
FOR EACH t-clie:
    DISPLAY t-clie.codcli
        t-clie.nomcli.
END.
OUTPUT CLOSE.

