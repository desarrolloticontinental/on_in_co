output to c:\tmp\mov-s05.txt.
for each almcmov no-lock where codcia = 1 and codalm = '11'
    and fchdoc >= date(12,01,2004)
    and tipmov = 's' and codmov = 05,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    display 
        almcmov.fchdoc  column-label 'Fecha'
        almcmov.nrodoc  column-label 'Documento'
        almmmatg.codmat column-label 'Material'
        almmmatg.desmat column-label 'Descripcion'
        almmmatg.desmar column-label 'Marca'
        almmmatg.undbas column-label 'Und Base'
        almdmov.candes  column-label 'Despachado'
        Almdmov.CodUnd  column-label 'Und Despacho'
        almmmatg.monvta column-label 'Moneda'
        Almmmatg.CtoTot column-label 'Cto Repos Total'
        almmmatg.preofi column-label 'Precio Oficina'
        almmmatg.chr__01 column-label 'Und Oficina'
        almcmov.observ  column-label 'Observaciones'
        almcmov.nrorf1  column-label 'Ref 1'
        almcmov.nrorf2  column-label 'Ref 2'
        almcmov.cco     column-label 'Cco'
        with stream-io no-box width 280.
end.
output close.
    
    
