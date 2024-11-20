output to c:\tmp\provincias.txt.
for each faccpedi no-lock where codcia = 001
    and coddoc = 'cot'
    and coddiv = '00000'
    and nroped begins '10'
    and flgest <> 'a',
    each facdpedi of faccpedi no-lock,
    first almmmatg of facdpedi no-lock:
    display faccpedi.fchped faccpedi.fchven faccpedi.codcli
        faccpedi.nomcli facdpedi.codmat almmmatg.desmat 
        facdpedi.canped
        facdpedi.undvta
        with stream-io no-box width 200.
end.
output close.
