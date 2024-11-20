def buffer b-ccbcdocu for ccbcdocu.

output to c:\tmp\nabonoabr2005.txt.
for each ccbcdocu where codcia = 1 and coddoc = 'n/c' and
    fchdoc >= date(04,01,2005) and fchdoc <= date(04,30,2005)
    /*and lookup(coddiv, '00000,00001,00002,00003') > 0*/
    and flgest <> 'a' no-lock,
    first b-ccbcdocu where b-ccbcdocu.codcia = ccbcdocu.codcia
        and b-ccbcdocu.coddoc = ccbcdocu.codref
        and b-ccbcdocu.nrodoc = ccbcdocu.nroref no-lock:
    if b-ccbcdocu.fchdoc <= date(07,31,2003)
    then display 
            ccbcdocu.coddiv column-label 'Div'
            ccbcdocu.nrodoc column-label 'N/Credito' 
            ccbcdocu.codcli column-label 'Cliente'
            b-ccbcdocu.coddoc column-label 'Ref'
            b-ccbcdocu.nrodoc column-label 'Numero'
            ccbcdocu.codmon column-label 'Mon'
            ccbcdocu.imptot column-label 'Importe'
            with width 120. 
end.    
output close.
