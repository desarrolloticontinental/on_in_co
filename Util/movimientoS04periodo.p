def var x-fecha as date.
def var x-ctopro as dec.

output to c:\tmp\mov-s05    .txt.
for each almcmov no-lock where codcia = 1 
    and fchdoc >= 01/01/2005
    and fchdoc <= 06/30/2005
    and tipmov = 's' and codmov = 05,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    x-ctopro = 0.
    find last almstkge where almstkge.codcia = 1
        and AlmStkge.codmat = almmmatg.codmat
        and AlmStkge.Fecha <= x-fecha no-lock no-error.
    if available almstkge then x-ctopro = AlmStkge.CtoUni.
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
        x-ctopro        column-label 'Precio Promedio'
        almcmov.observ  column-label 'Observaciones'
        almcmov.nrorf1  column-label 'Ref 1'
        almcmov.nrorf2  column-label 'Ref 2'
        almcmov.cco     column-label 'Cco'
        with stream-io no-box width 320.
end.
output close.
    
    
