output to c:\tmp\ventas.txt.

for each gn-divi no-lock where gn-divi.codcia = 001:
    for each ccbcdocu no-lock where codcia = 001
            and coddiv = gn-divi.coddiv
            and lookup(ccbcdocu.coddoc,'fac,bol,n/d,n/c') > 0
            and ccbcdocu.fchdoc >= 09/01/2007
            and ccbcdocu.fchdoc <= 12/31/2007,
            each ccbddocu no-lock of ccbcdocu,
            first almmmatg of ccbddocu no-lock:
        display 
            ccbcdocu.coddiv
            ccbcdocu.fchdoc
            ccbcdocu.coddoc
            ccbddocu.nrodoc
            ccbcdocu.codcli
            ccbcdocu.nomcli
            ccbddocu.codmat
            almmmatg.desmat
            ccbddocu.undvta
            ccbddocu.candes
            ccbddocu.implin
            ccbcdocu.codmon
            with stream-io width 320.
            
    end.
end.
output close.
