def temp-table detalle like almmmatg
    field i as dec format '>>>>>>.99' extent 100 
    field s as dec format '>>>>>>.99' extent 100.

for each almdmov no-lock where codcia = 001
    and codalm = '42'
    and fchdoc >= 12/01/07
    and fchdoc <= TODAY,
    first almmmatg of almdmov no-lock:
    find detalle of almmmatg exclusive-lock no-error.
    if not available detalle then create detalle.
    buffer-copy almmmatg to detalle.
    if almdmov.tipmov = 'i'
    then detalle.i[almdmov.codmov] = detalle.i[almdmov.codmov] + almdmov.candes * almdmov.factor.
    else detalle.s[almdmov.codmov] = detalle.s[almdmov.codmov] + almdmov.candes * almdmov.factor.
end.


output to c:\tmp\alm42.txt.
for each detalle no-lock:
    display
        detalle.codmat column-label 'Codigo'
        detalle.desmat format 'x(35)'
        detalle.undbas column-label 'Und'
        detalle.i[1]
        detalle.i[2]
        detalle.i[3] format '>>>>>>>.99'
        detalle.i[4]
        detalle.i[5]
        detalle.i[6]
        detalle.i[7]
        detalle.i[9]
        detalle.i[10]
        detalle.i[11]
        detalle.i[13]
        detalle.i[17]
        detalle.i[33]
        detalle.i[50]
        detalle.i[54]
        detalle.i[99]
        detalle.s[1]
        detalle.s[2]
        detalle.s[3] format '>>>>>>>.99'
        detalle.s[4]
        detalle.s[5]
        detalle.s[9]
        detalle.s[10]
        detalle.s[11]
        detalle.s[13]
        detalle.s[17]
        detalle.s[99]
        with stream-io no-box width 320.
        
end.
output close.
