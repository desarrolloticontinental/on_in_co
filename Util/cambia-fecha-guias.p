

FOR EACH ccbcdocu where
    codcia = 001 and
    coddoc = 'FAC' and
    nrodoc >= "015105433" and
    nrodoc <= "015105439":
    
    fchdoc = 08/29/08.
    
    display NRODOC nroref nomcli. UPDATE fchdoc.
/*
    find almcmov where almcmov.codcia = 001
        and almcmov.codalm = ccbcdocu.codalm
        and almcmov.tipmov = 's'
        and almcmov.codmov = 02
        and almcmov.nrodoc = integer(ccbcdocu.nrosal).
    /*
    almcmov.fchdoc = ccbcdocu.fchdoc.
    */
    for each almdmov of almcmov:
        /*
        almdmov.fchdoc = almcmov.fchdoc.
        */
        display almdmov.fchdoc.
    end.
*/
end.
