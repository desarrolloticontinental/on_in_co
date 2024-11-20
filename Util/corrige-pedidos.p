def buffer cpedi for faccpedi.
def buffer dpedi for facdpedi.

for each faccpedi where codcia = 001
        and coddiv = '00000'
        and coddoc = 'ped'
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
    for each cpedi where cpedi.codcia = 001
        and cpedi.coddoc = 'o/d'
        and cpedi.nroref = faccpedi.nroped
        and cpedi.flgest <> 'a' no-lock,
        each dpedi of cpedi no-lock:
        find facdpedi of faccpedi where facdpedi.codmat = dpedi.codmat
            exclusive-lock no-error.
        if available facdpedi then facdpedi.canate = facdpedi.canate + dpedi.canped.
        release facdpedi.
    end.            
end.
