def var x-nrodoc as char init "030000126".

for each ccbccaja where codcia = 001 and coddoc = 'i/c'
    and nrodoc = x-nrodoc:
    for each ccbdcaja of ccbccaja:
        find first ccbcdocu where ccbcdocu.codcia = 001
            and ccbcdocu.coddoc = ccbdcaja.codref
            and ccbcdocu.nrodoc = ccbdcaja.nroref.
        display ccbdcaja.codref ccbdcaja.nroref ccbcdocu.imptot nrosal.
        for each ccbddocu of ccbcdocu:
            display ccbddocu.codmat ccbddocu.candes.
        end.
        find almcmov where almcmov.codcia = 001
            and almcmov.codalm = ccbcdocu.codalm
            and almcmov.tipmov = 's'
            and almcmov.codmov = 02
            and almcmov.nrodoc = integer(ccbcdocu.nrosal).
        for each almdmov of almcmov:
            display codmat candes.
        end.
    end.
end.    
message "anulamos" view-as alert-box question
buttons yes-no update rpta as log.
if rpta = no then return.

for each ccbccaja where codcia = 001 and coddoc = 'i/c'
    and nrodoc = x-nrodoc:
    for each ccbdcaja of ccbccaja:
        find first ccbcdocu where ccbcdocu.codcia = 001
            and ccbcdocu.coddoc = ccbdcaja.codref
            and ccbcdocu.nrodoc = ccbdcaja.nroref.
        for each ccbddocu of ccbcdocu:
            delete ccbddocu.
        end.
        find almcmov where almcmov.codcia = 001
            and almcmov.codalm = ccbcdocu.codalm
            and almcmov.tipmov = 's'
            and almcmov.codmov = 02
            and almcmov.nrodoc = integer(ccbcdocu.nrosal).
        for each almdmov of almcmov:
            delete almdmov.
        end.
        almcmov.flgest = 'A'.
        assign        
            ccbcdocu.flgest = 'A'
            ccbcdocu.sdoact = 0.
        delete ccbdcaja.
    end.
    ccbccaja.flgest = 'A'.
end.    
