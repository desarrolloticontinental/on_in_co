OUTPUT TO m:\tmp\tottus.txt.
FOR EACH ccbcdocu NO-LOCK WHERE codcia = 1
    AND coddoc = 'fac'
    AND fchdoc >= 01/01/09
    AND flgest <> 'A'
    AND codcli = '20508565934',
    EACH ccbddocu OF ccbcdocu NO-LOCK,
    FIRST almmmatg OF ccbddocu NO-LOCK:
    DISPLAY
        ccbddocu.codmat
        almmmatg.desmat
        ccbddocu.candes
        ccbddocu.undvta
        ccbcdocu.codmon
        ccbddocu.preuni
        ccbddocu.implin
        ccbcdocu.nrodoc
        WITH STREAM-IO NO-BOX WIDTH 200.
END.
OUTPUT CLOSE.

