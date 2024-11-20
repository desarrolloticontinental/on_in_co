/* transferencias valorizadas */
def var x-nompro like gn-prov.nompro.
def var x-ctolis like almmmatg.ctolis.

output to c:\tmp\susana.txt.
for each almdmov no-lock where codcia = 001
    and codalm = '15'
    and almori = '19'
    and tipmov = 's'
    and codmov = 03
    and fchdoc >= 01/01/2006,
    first almmmatg of almdmov no-lock:
    x-ctolis = ctolis.
    if monvta = 2 then x-ctolis = x-ctolis * almmmatg.tpocmb.
    x-nompro = ''.
    find gn-prov where gn-prov.codcia = 000
        and gn-prov.codpro = codpr1 no-lock no-error.
    if available gn-prov then x-nompro = gn-prov.nompro.    
    display 
        fchdoc 
        nroser
        nrodoc 
        codfam
        almmmatg.codmat
        desmat
        desmar
        undbas
        candes
        x-ctolis
        codpr1
        x-nompro
        with stream-io no-box width 320.
end.
output close.
