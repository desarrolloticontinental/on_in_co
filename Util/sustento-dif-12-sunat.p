def temp-table t-matg like almmmatg
    field costo as dec
    field i01 as dec
    field s01 as dec
    field i99 as dec
    field s99 as dec
    field s50 as dec
    field nrorf1 like almcmov.nrorf1
    index llave01 as primary desmat.
    

for each almcmov no-lock where codcia = 001
    and codalm = '12'
    and fchdoc >= 01/01/2003
    and fchdoc <= 04/28/2004
    and tipmov = 'i'
    and codmov = 01,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
    assign
        i01 = i01 + almdmov.candes.
end.
for each almcmov no-lock where codcia = 001
    and codalm = '12'
    and fchdoc >= 01/01/2003
    and fchdoc <= 04/28/2004
    and tipmov = 's'
    and codmov = 01,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
    assign
        s01 = s01 + almdmov.candes.
end.
for each almcmov no-lock where codcia = 001
    and codalm = '12'
    and fchdoc >= 01/01/2003
    and fchdoc <= 04/28/2004
    and tipmov = 'i'
    and codmov = 99,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
    assign
        i99 = i99 + almdmov.candes.
end.
for each almcmov no-lock where codcia = 001
    and codalm = '12'
    and fchdoc >= 01/01/2003
    and fchdoc <= 04/28/2004
    and tipmov = 's'
    and codmov = 99,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
    assign
        s99 = s99 + almdmov.candes.
end.
for each almcmov no-lock where codcia = 001
    and codalm = '12'
    and fchdoc >= 01/01/2003
    and fchdoc <= 04/28/2004
    and tipmov = 's'
    and codmov = 50,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg
            assign t-matg.nrorf1 = almcmov.nrorf1.
    end.
    assign
        s50 = s50 + almdmov.candes.
end.

output to c:\tmp\sunat12.txt.
for each t-matg where i01 + s01 + i99 + s99 <> 0:
    find last Almstkge where AlmStkge.CodCia = 001
        and AlmStkge.codmat = t-matg.codmat
        and AlmStkge.Fecha <= 12/31/2003 no-lock no-error.
    if available almstkge then t-matg.costo = AlmStkge.CtoUni.
    display t-matg.codmat t-matg.desmat t-matg.desmar t-matg.undbas
        t-matg.costo t-matg.i01 t-matg.s01 t-matg.i99 t-matg.s99
        t-matg.s50 format '->>>,>>>,>>9.99'
        t-matg.nrorf1
        with stream-io no-box width 320.
end.    
output close.
