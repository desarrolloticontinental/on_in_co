DEF VAR x-linea AS CHAR.

INPUT FROM c:\tmp\facturas.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    FIND ccbcdocu WHERE codcia = 1
        AND coddoc = SUBSTRING(x-linea,1,3)
        AND nrodoc = SUBSTRING(x-linea,4).
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK,
        FIRST almmmatg OF ccbddocu:
        PUT UNFORMATTED
            ccbcdocu.coddoc '|'
            ccbcdocu.nrodoc '|'
            ccbcdocu.fchdoc '|'
            ccbcdocu.codmon '|'
            ccbcdocu.tpocmb '|'
            ccbddocu.codmat '|'
            almmmatg.desmat '|'
            almmmatg.codfam '|'
            almmmatg.subfam '|'
            ccbddocu.candes '|'
            ccbddocu.undvta '|'
            ccbddocu.implin
            SKIP.
    END.
END.
