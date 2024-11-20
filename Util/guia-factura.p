def buffer b-cdocu for ccbcdocu.

output to c:\tmp\guia-factura.txt.
for each ccbcdocu no-lock where codcia = 001
    and coddoc = 'g/r'
    and coddiv = '00000'
    and flgest = 'F'
    and fchdoc >= 11/01/07
    and fchdoc <= 11/30/07,
    first b-cdocu no-lock where b-cdocu.codcia = 001
        and b-cdocu.coddoc = ccbcdocu.codref
        and b-cdocu.nrodoc = ccbcdocu.nroref:
    display
        ccbcdocu.fchdoc
        ccbcdocu.nrodoc column-label 'guia'
        b-cdocu.fchdoc
        b-cdocu.nrodoc  column-label 'factura'
        b-cdocu.imptot
        b-cdocu.codmon
        b-cdocu.codcli
        b-cdocu.nomcli
        with stream-io width 320.    
end.    
output close.
