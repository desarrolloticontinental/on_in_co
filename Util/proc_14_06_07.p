output to c:\tmp\viru.txt.
for each ccbcdocu no-lock where codcia = 001
    and codcli = '10159444517'
    and lookup(trim(coddoc), 'fac,bol,n/c,n/d') > 0
    and flgest <> 'a'
    and fchdoc >= 01/01/2005
    and fchdoc <= 04/30/2005:
    display
        fchdoc 
        coddoc
        nrodoc
        codcli
        nomcli
        codmon
        impvta
        with stream-io no-box width 320.
end.
output close.
