/*
    Objetivo: Inventario + movimientos al cierre de inventario
    Fecha: 16-03-2007
*/
def temp-table detalle like almmmatg
    field caninv like invconteo.caninv
    field ctouni like almstkge.ctouni
    field saldoini as dec
    field saldofin as dec
    field ingresos as dec
    field egresos as dec.

def var x-fchinv as date no-undo.
def var x-corte  as date no-undo.
def var x-codalm as char no-undo.
def var x-codmat as char no-undo.

assign
    x-fchinv = 12/04/2006
    x-corte  = 12/31/2006
    x-codalm = '85'
    x-codmat = ''.
    
for each invconteo no-lock where codcia = 001
        and codalm = x-codalm
        and fchinv = x-fchinv
        and codmat begins x-codmat,
        first almmmatg of invconteo no-lock:
    create detalle.
    buffer-copy almmmatg to detalle.
    detalle.caninv = invconteo.caninv.
end.

for each almdmov no-lock where codcia = 001
        and codalm = x-codalm
        and fchdoc > x-fchinv
        and fchdoc <= x-corte
        and codmat begins x-codmat,
        first almmmatg of almdmov no-lock:
/*    if tipmov = 'i' and codmov = 03 then next.
 *     if tipmov = 's' and codmov = 03 then next.
 *     if tipmov = 'i' and codmov = 33 then next.*/
    if codmov = 01 then next.
    find detalle of almmmatg exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
        buffer-copy almmmatg to detalle.
    end.   
    if tipmov = 'i'
    then detalle.ingresos = detalle.ingresos + (candes * factor).
    else detalle.egresos  = detalle.egresos  + (candes * factor).
end.

output to c:\tmp\invfinal03.txt.
for each detalle:
    find last almstkal where almstkal.codcia = 001
        and almstkal.codmat = detalle.codmat
        and almstkal.codalm = x-codalm
        and almstkal.fecha < x-fchinv
        no-lock no-error.
    if available almstkal then detalle.saldoini = almstkal.stkact.
    for each almdmov no-lock where codcia = 001
            and codalm = x-codalm
            and fchdoc = x-fchinv
            and codmat = detalle.codmat:
        if codmov = 01 then next.
        if tipmov = 'i'
        then detalle.saldoini = detalle.saldoini + (candes * factor).
        else detalle.saldoini = detalle.saldoini - (candes * factor).
    end.

    find last almstkal where almstkal.codcia = 001
        and almstkal.codmat = detalle.codmat
        and almstkal.codalm = x-codalm
        and almstkal.fecha <= x-corte
        no-lock no-error.
    if available almstkal then detalle.saldofin = almstkal.stkact.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = detalle.codmat
        and almstkge.fecha <= x-fchinv
        no-lock no-error.
    if available almstkge then detalle.ctouni = AlmStkge.CtoUni.
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.ctouni
        detalle.saldoini
        detalle.caninv
        detalle.ingresos
        detalle.egresos
        detalle.saldofin
        detalle.catconta[1]
        with stream-io no-box width 320.
end.
output close.
