def buffer docu for ccbcdocu.
&SCOPED-DEFINE x-codmat 033473
output to c:\tmp\abril2009-{&x-codmat}.txt.
for each gn-divi where codcia = 001 no-lock:
    for each ccbcdocu where codcia = 001
        and coddiv = gn-divi.coddiv
        and coddoc = 'n/c'
        and fchdoc >= 04/01/2009
        and fchdoc <= 04/30/2009
        and flgest <> 'a'
        no-lock,
        first docu where docu.codcia = 001
            and docu.coddoc = ccbcdocu.codref
            and docu.nrodoc = ccbcdocu.nroref no-lock,
        each ccbddocu of docu no-lock where 
            ccbddocu.codmat = '{&x-codmat}':
        display ccbcdocu.coddiv ccbcdocu.coddoc ccbcdocu.nrodoc ccbcdocu.fchdoc
            docu.coddoc docu.nrodoc
            ccbddocu.codmat ccbddocu.candes ccbddocu.undvta ccbddocu.factor docu.codmon ccbddocu.implin
            with stream-io width 200.
    end.        
end.
output close.
