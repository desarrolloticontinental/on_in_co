DEF VAR X AS CHAR.
DEF VAR Y AS INT.

INPUT FROM c:\tmp\codigos.prn.
OUTPUT TO c:\tmp\stucci3.txt.
REPEAT :
    IMPORT UNFORMATTED X.
    IF X = '' THEN LEAVE.
    Y = 0.
    FOR EACH gn-divi NO-LOCK WHERE codcia = 001,
        EACH ccbddocu NO-LOCK WHERE ccbddocu.codcia = 1
        AND ccbddocu.codmat = SUBSTRING(X,1,6)
        AND ccbddocu.coddiv = gn-divi.coddiv
        AND ccbddocu.coddoc = "FAC"
        AND ccbddocu.nrodoc BEGINS '015',
        FIRST ccbcdocu OF ccbddocu NO-LOCK 
        BY ccbcdocu.fchdoc DESC BY ccbcdocu.nrodoc DESC:
        PUT UNFORMATTED
            ccbcdocu.coddiv '|'
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.codcli '|'
            ccbcdocu.nomcli '|'
            ccbddocu.codmat '|'
            ccbddocu.candes '|'
            ccbddocu.undvta
            SKIP.
        Y = Y + 1.
        IF Y = 3 THEN LEAVE.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.
