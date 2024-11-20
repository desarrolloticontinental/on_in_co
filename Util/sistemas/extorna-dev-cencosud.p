DEF VAR x-linea AS CHAR.
DEF BUFFER b-ddocu FOR ccbddocu.    /* FAC */

INPUT FROM d:\cencosud.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF TRUE <> (x-linea > '') THEN LEAVE.
    /* buscamos n/c */
    FIND ccbcdocu WHERE codcia = 1
        AND coddoc = SUBSTRING(x-linea,1,3)
        AND nrodoc = SUBSTRING(x-linea,6)
        NO-LOCK.
    FOR EACH ccbddocu OF ccbcdocu NO-LOCK,
        /* buscamos FAC */
        FIRST b-ddocu WHERE b-ddocu.codcia = 1
        AND b-ddocu.coddoc = ccbcdocu.codref
        AND b-ddocu.nrodoc = ccbcdocu.nroref
        AND b-ddocu.codmat = ccbddocu.codmat.
        /* b-ddocu.candev = b-ddocu.candev - ccbddocu.candev */
        DISPLAY ccbcdocu.coddoc ccbcdocu.nrodoc 
            b-ddocu.coddoc b-ddocu.nrodoc 
            ccbddocu.codmat ccbddocu.candes b-ddocu.candev
            WITH STREAM-IO NO-BOX WIDTH 320.
    END.
END.
