def var x-lista as log no-undo.

output to c:\tmp\cotizaciones.txt.
for each faccpedi no-lock where codcia = 001
    and coddiv = '00000'
    and coddoc = 'cot'
    and fchped >= 01/01/08
    and fchped <= 05/31/08
    and flgest <> 'a':
    x-lista = yes.
    for each facdpedi of faccpedi no-lock:
        if canate > 0 then do:
            x-lista = no.
            leave.
        end.
    end.
    if x-lista = yes
    then display 
            faccpedi.fchped
            faccpedi.coddoc
            faccpedi.nroped
            faccpedi.codcli
            faccpedi.nomcli
            with stream-io no-box width 200.
            
    
end.    
output close.
