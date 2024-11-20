DEF NEW SHARED VAR s-codcia AS INT INIT 001.

FIND ccbcdocu WHERE codcia = s-codcia
    AND coddoc = 'g/r'
    AND nrodoc = '228017259'.

/* armamos llave */
DEF VAR x-nroser LIKE almcmov.nroser NO-UNDO.
DEF VAR x-nrodoc LIKE almcmov.nrodoc NO-UNDO.

x-nroser = INTEGER(SUBSTRING(ccbcdocu.nrodoc,1,3)).
x-nrodoc = 900000000 + INTEGER(SUBSTRING(ccbcdocu.nrodoc,4)) .

FIND almcmov WHERE almcmov.codcia = s-codcia
    AND almcmov.codalm = ccbcdocu.codalm
    AND almcmov.tipmov = "S"
    AND almcmov.codmov = 02
    AND almcmov.nroser = x-nroser
    AND almcmov.nrodoc = x-nrodoc
    NO-LOCK NO-ERROR.
IF AVAILABLE almcmov THEN DO:
    MESSAGE 'mov' ccbcdocu.codalm x-nroser x-nrodoc 'ya existe'.
    RETURN.
END.
CREATE almcmov.
ASSIGN
    almcmov.codcia = s-codcia
    almcmov.codalm = ccbcdocu.codalm
    almcmov.tipmov = "S"
    almcmov.codmov = 02
    almcmov.nroser = x-nroser
    almcmov.nrodoc = x-nrodoc
    almcmov.fchdoc = ccbcdocu.fchdoc
    almcmov.nrorf1 = ccbcdocu.coddoc + ccbcdocu.nrodoc
    almcmov.nrorf2 = ccbcdocu.codped + ccbcdocu.nroped
    almcmov.codcli = ccbcdocu.codcli
    almcmov.codven = ccbcdocu.codven
    almcmov.codmon = ccbcdocu.codmon
    almcmov.usuario = ccbcdocu.usuario
    almcmov.codref = ccbcdocu.coddoc
    almcmov.nroref = ccbcdocu.nrodoc
    .
FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
    CREATE almdmov.
    BUFFER-COPY almcmov TO almdmov
        ASSIGN
        almdmov.candes = ccbddocu.candes
        almdmov.factor = ccbddocu.factor
        almdmov.preuni = ccbddocu.preuni
        almdmov.codund = ccbddocu.undvta
        almdmov.codmat = ccbddocu.codmat
        almdmov.aftigv = ccbddocu.aftigv
        almdmov.implin = ccbddocu.implin
        almdmov.prebas = ccbddocu.prebas
        almdmov.impigv = ccbddocu.impigv
        .
END.
