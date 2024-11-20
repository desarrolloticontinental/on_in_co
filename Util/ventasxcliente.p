output to c:\tmp\ventasxcliente.txt.
for each ccbcdocu no-lock where codcia = 001
    and codcli = '20446155688'
    and fchdoc >= 01/01/2005
    and fchdoc <= 12/31/2005
    and flgest <> 'a':
    if lookup(trim(coddoc), 'fac,bol,n/d,n/c,let') > 0
        then do:
        display coddiv fchdoc nomcli coddoc nrodoc codmon imptot
            with stream-io no-box width 200.
    end.
end.
output close.
