output to c:\tmp\miluska.txt.
for each gn-divi where codcia = 001 no-lock:
for each ccbcdocu where 
    codcia = 001
    and coddiv = gn-divi.coddiv
    and lookup(coddoc,'bol,fac') > 0
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003
    and tpofac = 'S'
    and flgest <> 'a'
    and nrosal = ''
    no-lock,
    each ccbddocu of ccbcdocu no-lock,
    first almmmatg of ccbddocu no-lock:
    display 
        ccbcdocu.coddiv 
        ccbcdocu.fchdoc 
        ccbcdocu.coddoc 
        ccbcdocu.nrodoc
        ccbcdocu.codcli 
        ccbcdocu.nomcli 
        ccbcdocu.glosa
        ccbcdocu.codmon 
        ccbcdocu.imptot 
        ccbcdocu.tpocmb
        ccbddocu.codmat
        almmmatg.desmat
        ccbddocu.candes
        with stream-io no-box width 320.
end.
end.
output close.
