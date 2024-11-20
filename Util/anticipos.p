OUTPUT TO m:\tmp\anticipos.txt.
FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'a/r'
    AND flgest = 'P'
    NO-LOCK:
    DISPLAY
        coddiv
        coddoc
        nrodoc
        fchdoc
        codref
        nroref
        codcli
        nomcli
        codmon
        imptot
        sdoact COLUMN-LABEL 'Saldo'
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

