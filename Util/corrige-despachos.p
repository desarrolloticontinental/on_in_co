def buffer cpedi for faccpedi.
def buffer dpedi for facdpedi.

for each faccpedi where codcia = 001
        and coddiv = '00000'
        and coddoc = 'o/d'
        and flgest = 'p' 
        and fchped >= 01/01/2007
        and fchped < today
        no-lock:
    display coddiv coddoc nroped usuario fchped.
    pause 0.
    for each facdpedi of faccpedi:
        /*display canped canate.*/
        canate = 0.
    end.
    for each ccbcdocu where ccbcdocu.codcia = 001
        and ccbcdocu.coddiv = '00000'
        and ccbcdocu.coddoc = 'g/r'
        and ccbcdocu.codped = faccpedi.coddoc
        and ccbcdocu.nroped = faccpedi.nroped
        and ccbcdocu.flgest <> 'a' no-lock,
        each ccbddocu of ccbcdocu no-lock:
        find facdpedi of faccpedi where facdpedi.codmat = ccbddocu.codmat
            exclusive-lock no-error.
        if available facdpedi 
        then /*display ccbddocu.candes.*/ facdpedi.canate = facdpedi.canate + ccbddocu.candes.
        release facdpedi.
    end.            
end.
