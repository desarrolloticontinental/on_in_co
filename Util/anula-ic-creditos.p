def var x-nrodoc as char init "011073187".

for each ccbccaja where codcia = 001 and coddoc = 'i/c'
    and nrodoc = x-nrodoc:
    for each ccbdcaja of ccbccaja:
        find ccbcdocu where ccbcdocu.codcia = 001
            and ccbcdocu.coddoc = ccbdcaja.codref
            and ccbcdocu.nrodoc = ccbdcaja.nroref.
        display ccbdcaja.codref ccbdcaja.nroref ccbcdocu.imptot nrosal.
    end.
end.    
message "anulamos" view-as alert-box question
buttons yes-no update rpta as log.
if rpta = no then return.

for each ccbccaja where codcia = 001 and coddoc = 'i/c'
    and nrodoc = x-nrodoc:
    for each ccbdcaja of ccbccaja:
        find ccbcdocu where ccbcdocu.codcia = 001
            and ccbcdocu.coddoc = ccbdcaja.codref
            and ccbcdocu.nrodoc = ccbdcaja.nroref.
        assign
            ccbcdocu.sdoact = ccbcdocu.sdoact + ccbdcaja.imptot
            ccbcdocu.flgest = 'P'
            ccbcdocu.fchcan = ?.
        delete ccbdcaja.
    end.
    ccbccaja.flgest = 'A'.
end.    
