def var x-ctolis as dec.

def temp-table detalle like almmmatg
    field stk15 as dec
    field stk30 as dec.

for each almmmatg no-lock where codcia = 001:
    create detalle.
    buffer-copy almmmatg to detalle.
    find almmmate where almmmate.codcia = 001
        and almmmate.codmat = almmmatg.codmat
        and almmmate.codalm = '15'
        no-lock no-error.
    if available almmmate then detalle.stk15 = almmmate.stkact.
    find almmmate where almmmate.codcia = 001
        and almmmate.codmat = almmmatg.codmat
        and almmmate.codalm = '30'
        no-lock no-error.
    if available almmmate then detalle.stk30 = almmmate.stkact.
end.

output to c:\tmp\alm15-30.txt.
for each detalle where detalle.stk15 <> 0 or detalle.stk30 <> 0:
    if detalle.monvta = 1
    then x-ctolis = detalle.ctolis.
    else x-ctolis = detalle.ctolis * 3.156.
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        x-ctolis
        detalle.stk15
        detalle.stk30
        with stream-io no-box width 320.
end.
output close.
