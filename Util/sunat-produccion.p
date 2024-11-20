def temp-table t-matg like almmmatg
    field cantidad as dec extent 12.
    
for each almdmov no-lock where codcia = 001
    and codalm = '12'
    and tipmov = 'i'
    and codmov = 50
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003,
    first almmmatg of almdmov no-lock where lookup(trim(Almmmatg.catconta[1]), 'PT,PP') > 0:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
    t-matg.cantidad[month(fchdoc)] = t-matg.cantidad[month(fchdoc)] + (candes * factor).
end.
        
output to c:\tmp\i50-2003.txt.
for each t-matg:
    display 
        t-matg.catconta[1] 
        t-matg.codmat
        t-matg.desmat
        t-matg.desmar
        t-matg.undbas
        t-matg.cantidad
        with stream-io no-box width 320.
end.    
output close.
