def var x-imptot as dec.
def var x-impcto as dec.

output to c:\tmp\margenes.txt.
for each ccbcdocu where codcia = 001 and coddoc = 'fac'
    and coddiv = '00000'
    and fchdoc >= 06/15/2005
    and fchdoc <= 08/15/2005
    and flgest <> 'a' no-lock:
    assign
        x-imptot = ccbcdocu.imptot
        x-impcto = ccbcdocu.impcto.
    if ccbcdocu.codmon = 2
    then assign
            x-imptot = x-imptot * ccbcdocu.tpocmb
            x-impcto = x-impcto * ccbcdocu.tpocmb.
    display codven fchdoc coddoc nrodoc codcli nomcli x-imptot x-impcto
        with stream-io width 200.            
end.
output close.
