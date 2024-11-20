def var x-codcli as char init '20439189313' no-undo.
def var x-archivo as char format 'x(30)' no-undo.

x-archivo = 'c:\tmp\DAOT' + x-codcli + '.txt'.
output to value(x-archivo).
for each gn-divi no-lock where codcia = 001:    
    for each ccbcdocu no-lock where codcia = 001
        and coddiv = gn-divi.coddiv
        and codcli = x-codcli
        and coddoc = 'fac'
        and fchdoc >= 01/01/2005
        and fchdoc <= 12/31/2005:
        display fchdoc coddoc nrodoc codcli nomcli codmon
            impbrt impdto impigv porigv impvta imptot
            with stream-io no-box width 200.
    end.        
end.
output close.
