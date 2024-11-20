/* Existencias por almacen (San Miguel) */

def temp-table detalle like almmmatg
    field stkact as dec
    field ctouni as dec
    field valtot as dec
    field ultsal as date
    field ulting as date
    field nompro as char format 'x(40)'.
    
def var x-nroitm as int no-undo.    

for each almmmatg where codcia = 001 no-lock:
    find last almstkal of almmmatg
        where codalm = '16'
        and fecha <= 12/31/2005
        no-lock no-error.
    if not available almstkal or almstkal.stkact = 0 then next.
    create detalle.
    buffer-copy almmmatg to detalle
        assign
            detalle.stkact = almstkal.stkact.
    find last almstkge of almmmatg
        where almstkge.fecha <= 12/31/2005
        no-lock no-error.
    if available almstkge
    then assign
            detalle.ctouni = almstkge.ctouni
            detalle.valtot = detalle.stkact * detalle.ctouni.    
    find gn-prov where gn-prov.codcia = 000
        and gn-prov.codpro = almmmatg.codpr1
        no-lock no-error.
    if available gn-prov then detalle.nompro = gn-prov.nompro.
end.    
    
def frame f-detalle
    x-nroitm
    detalle.codfam
    detalle.subfam
    detalle.codmat
    detalle.desmat
    detalle.desmar
    detalle.undbas
    detalle.stkact
    detalle.ctouni
    detalle.valtot
    detalle.catconta[1]
    detalle.nompro
    with stream-io no-box no-labels width 320 1 down.

output to c:\tmp\alm16-2005.txt.
for each detalle break by detalle.codcia by detalle.codfam by detalle.subfam by detalle.desmat:
    if first-of(detalle.subfam) then x-nroitm = 0.
    x-nroitm = x-nroitm + 1.
    
    display
    x-nroitm
    detalle.codfam
    detalle.subfam
    detalle.codmat
    detalle.desmat
    detalle.desmar
    detalle.undbas
    detalle.stkact
    detalle.ctouni
    detalle.valtot
    detalle.catconta[1]
    detalle.nompro
    with frame f-detalle.

    accumulate detalle.valtot (sub-total by detalle.codfam by detalle.subfam).    
    accumulate detalle.valtot total.    

    if last-of(detalle.subfam) then do:
        underline detalle.valtot with frame f-detalle.
        display
            accum sub-total by detalle.subfam detalle.valtot @ detalle.valtot
            with frame f-detalle.
        down 1 with frame f-detalle.
    end.
    if last-of(detalle.codfam) then do:
        underline detalle.valtot with frame f-detalle.
        display
            accum sub-total by detalle.codfam detalle.valtot @ detalle.valtot
            with frame f-detalle.
        down 1 with frame f-detalle.
    end.
    if last-of(detalle.codcia) then do:
        underline detalle.valtot with frame f-detalle.
        display
            accum total detalle.valtot @ detalle.valtot
            with frame f-detalle.
        down 1 with frame f-detalle.
    end.
end.
output close.
