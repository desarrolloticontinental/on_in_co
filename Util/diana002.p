
def temp-table detalle
    field codcia like almmmatg.codcia
    field codmat like almmmatg.codmat
    field undbas like almmmatg.undbas
    field desmat like almmmatg.desmat
    field codpr1 like almmmatg.codpr1
    field venta  as dec extent 12
    field costo  as dec extent 12
    field stock  as dec extent 12.

def var x-nromes-1 as int init 01.
def var x-nromes-2 as int init 08.
def var x-periodo  as int init 2006.
def var x-fecha    as date.

/* ventas y costos */
for each evtarti no-lock where codcia = 001
    and nrofch >= x-periodo * 100 + x-nromes-1
    and nrofch <= x-periodo * 100 + x-nromes-2,
    first almmmatg of evtarti no-lock:
    find detalle where detalle.codcia = evtarti.codcia
        and detalle.codpr1 = almmmatg.codpr1
        exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
        buffer-copy almmmatg to detalle.
    end.
    assign
        detalle.venta[evtarti.codmes] = detalle.venta[evtarti.codmes] + EvtArti.VtaxMesMn
        detalle.costo[evtarti.codmes] = detalle.costo[evtarti.codmes] + EvtArti.CtoxMesMn.        
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = evtarti.codmat
        and almstkge.fecha <= x-fecha
        no-lock no-error.
    if evtarti.codmes < 12
    then x-fecha = date(evtarti.codmes + 1, 01, x-periodo) - 1.
    else x-fecha = date(01, 01, x-periodo + 1) - 1.
    if available almstkge then detalle.stock[evtarti.codmes] = almstkge.stkact * AlmStkge.CtoUni.
end.    

output to c:\tmp\diana.txt.
for each detalle, first gn-prov no-lock where gn-prov.codcia = 000 and gn-prov.codpro = detalle.codpr1:
    display
        detalle.codpr1
        gn-prov.nompro
        /*detalle.codmat
        detalle.desmat
        detalle.undbas*/
        detalle.venta[1] format '->>>,>>9'
        detalle.costo[1] format '->>>,>>9'
        detalle.stock[1] format '->>>,>>9'
        detalle.venta[2] format '->>>,>>9'
        detalle.costo[2] format '->>>,>>9'
        detalle.stock[2] format '->>>,>>9'
        detalle.venta[3] format '->>>,>>9'
        detalle.costo[3] format '->>>,>>9'
        detalle.stock[3] format '->>>,>>9'
        detalle.venta[4] format '->>>,>>9'
        detalle.costo[4] format '->>>,>>9'
        detalle.stock[4] format '->>>,>>9'
        detalle.venta[5] format '->>>,>>9'
        detalle.costo[5] format '->>>,>>9'
        detalle.stock[5] format '->>>,>>9'
        detalle.venta[6] format '->>>,>>9'
        detalle.costo[6] format '->>>,>>9'
        detalle.stock[6] format '->>>,>>9'
        detalle.venta[7] format '->>>,>>9'
        detalle.costo[7] format '->>>,>>9'
        detalle.stock[7] format '->>>,>>9'
        detalle.venta[8] format '->>>,>>9'
        detalle.costo[8] format '->>>,>>9'
        detalle.stock[8] format '->>>,>>9'
        with stream-io no-box width 320.
end.
output close.
