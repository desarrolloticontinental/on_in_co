def var x-impnac as dec no-undo format '(>>>,>>>,>>9.99)'.
def var x-impusa as dec no-undo format '(>>>,>>>,>>9.99)'.

output to c:\tmp\ventas.txt.
for each gn-divi where codcia = 001 no-lock:
    for each ccbcdocu no-lock where codcia = 001
            and lookup(trim(coddoc), 'fac,bol,tck,n/c,n/d') > 0
            and flgest <> 'A'
            and coddiv = gn-divi.coddiv
            and fchdoc >= 01/01/2005
            and fchdoc <= 12/31/2005
            break by coddoc:
        if first-of(coddoc)
        then assign
                x-impnac = 0
                x-impusa = 0.        
        if codmon = 1
        then x-impnac = x-impnac + imptot.
        else x-impusa = x-impusa + imptot.
        if last-of(coddoc)
        then do:
            display gn-divi.coddiv coddoc x-impnac x-impusa.
            pause 0.
        end.
    end.
end.
output close.
