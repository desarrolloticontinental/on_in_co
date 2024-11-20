def temp-table t-cdoc like ccbcdocu.
for each ccbccaja where codcia = 001
    and coddoc = 'i/c'
    and coddiv = '00000'
    and fchdoc >= 09/01/2005
    and fchdoc <= 09/30/2005
    and flgest <> 'a' no-lock,
    each ccbdcaja of ccbccaja no-lock,
    first ccbcdocu where ccbcdocu.codcia = 001
        and ccbcdocu.coddoc = ccbdcaja.codref
        and ccbcdocu.nrodoc = ccbdcaja.nroref 
        and ccbcdocu.flgest = 'C' 
        and lookup(trim(ccbcdocu.coddoc), 'fac,bol,tck,n/d,chq') > 0
        no-lock:
    find t-cdoc of ccbcdocu no-lock no-error.
    if not available t-cdoc then do:
        create t-cdoc.
        buffer-copy ccbcdocu to t-cdoc.
    end.        
end.

for each t-cdoc:
    display t-cdoc.coddoc t-cdoc.nrodoc t-cdoc.imptot t-cdoc.flgest.
end.
    
        
