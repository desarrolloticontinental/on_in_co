output to c:\tmp\comision-contado-detalle.txt.
for each ccbcdocu where codcia = 001
    and coddiv = '00000'
    and lookup(trim(coddoc), 'fac,bol') > 0
    and flgest <> 'a'
    and codven = '015'
    and ((fchdoc >= 03/26/2005 and fchdoc <= 12/31/2005) or
            (fchdoc >= 01/02/2006 and fchdoc <= 03/25/2006))
    and lookup(trim(fmapgo), '001,002') > 0,
    each ccbddocu of ccbcdocu no-lock,
    first almmmatg of ccbddocu no-lock:
    display 
        codven 
        fmapgo 
        ccbcdocu.fchdoc 
        ccbcdocu.coddoc 
        ccbcdocu.nrodoc 
        ccbcdocu.codcli 
        codmon 
        ccbcdocu.pordto
        ccbcdocu.imptot
        ccbddocu.codmat
        almmmatg.desmat
        candes
        undvta
        preuni
        implin
        with width 320.
end.
output close.

output to c:\tmp\comision-credito-detalle.txt.
for each ccbcdocu where codcia = 001
    and coddiv = '00000'
    and lookup(trim(coddoc), 'fac,bol') > 0
    and flgest <> 'a'
    and codven = '015'
    and ((fchdoc >= 03/26/2005 and fchdoc <= 12/31/2005) or
            (fchdoc >= 01/02/2006 and fchdoc <= 03/25/2006))
    and lookup(trim(fmapgo), '001,002') = 0,
    each ccbddocu of ccbcdocu no-lock,
    first almmmatg of ccbddocu no-lock:
    display 
        codven 
        fmapgo 
        ccbcdocu.fchdoc 
        ccbcdocu.coddoc 
        ccbcdocu.nrodoc 
        ccbcdocu.codcli 
        codmon 
        ccbcdocu.pordto
        ccbcdocu.imptot
        ccbddocu.codmat
        almmmatg.desmat
        candes
        undvta
        preuni
        implin
        with width 320.
end.
output close.

output to c:\tmp\comision-contado-fotocopia-detalle.txt.
for each ccbcdocu where codcia = 001
    and coddiv = '00000'
    and lookup(trim(coddoc), 'fac,bol') > 0
    and flgest <> 'a'
    and codven = '015'
    and ((fchdoc >= 03/26/2005 and fchdoc <= 12/31/2005) or
            (fchdoc >= 01/02/2006 and fchdoc <= 03/25/2006))
    and lookup(trim(fmapgo), '001,002') > 0,
    first ccbddocu of ccbcdocu no-lock where lookup(trim(codmat), '005206,005207') > 0,
    first almmmatg of ccbddocu no-lock:
    display 
        codven 
        fmapgo 
        ccbcdocu.fchdoc 
        ccbcdocu.coddoc 
        ccbcdocu.nrodoc 
        ccbcdocu.codcli 
        codmon 
        ccbcdocu.pordto
        ccbcdocu.imptot
        ccbddocu.codmat
        almmmatg.desmat
        candes
        undvta
        preuni
        implin
        with width 320.
end.
output close.

output to c:\tmp\comision-credito-fotocopia-detalle.txt.
for each ccbcdocu where codcia = 001
    and coddiv = '00000'
    and lookup(trim(coddoc), 'fac,bol') > 0
    and flgest <> 'a'
    and codven = '015'
    and ((fchdoc >= 03/26/2005 and fchdoc <= 12/31/2005) or
            (fchdoc >= 01/02/2006 and fchdoc <= 03/25/2006))
    and lookup(trim(fmapgo), '001,002') = 0,
    first ccbddocu of ccbcdocu no-lock where lookup(trim(codmat), '005206,005207') > 0,
    first almmmatg of ccbddocu no-lock:
    display 
        codven 
        fmapgo 
        ccbcdocu.fchdoc 
        ccbcdocu.coddoc 
        ccbcdocu.nrodoc 
        ccbcdocu.codcli 
        codmon 
        ccbcdocu.pordto
        ccbcdocu.imptot
        ccbddocu.codmat
        almmmatg.desmat
        candes
        undvta
        preuni
        implin
        with width 320.
end.
output close.
