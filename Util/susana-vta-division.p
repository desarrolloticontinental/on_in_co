def var x-division as char no-undo.
def var i as int no-undo.

x-division = '00000,00001,00002,00003,00005,00008,00012,00014,00015,00016'.

def temp-table detalle like almmmatg
    field vta00 as dec
    field vta01 as dec
    field vta02 as dec
    field vta03 as dec
    field vta05 as dec
    field vta08 as dec
    field vta12 as dec
    field vta14 as dec
    field vta15 as dec
    field vta16 as dec
    field ctouni as dec
    field stkact as dec.
    
for each gn-divi no-lock where codcia = 001:
    for each evtarti no-lock where EvtArti.CodCia = 001
            and EvtArti.CodDiv = gn-divi.coddiv
            and EvtArti.Nrofch >= 200604
            and EvtArti.Nrofch <= 200612,
            first almmmatg of evtarti no-lock:
        find detalle of almmmatg exclusive-lock no-error.
        if not available detalle then do:
            create detalle.
            buffer-copy almmmatg to detalle.
        end.
        case gn-divi.coddiv:
            when '00000' then vta00 = vta00 + EvtArti.CanxMes.
            when '00001' then vta01 = vta01 + EvtArti.CanxMes.
            when '00002' then vta02 = vta02 + EvtArti.CanxMes.
            when '00003' then vta03 = vta03 + EvtArti.CanxMes.
            when '00005' then vta05 = vta05 + EvtArti.CanxMes.
            when '00008' then vta08 = vta08 + EvtArti.CanxMes.
            when '00012' then vta12 = vta12 + EvtArti.CanxMes.
            when '00014' then vta14 = vta14 + EvtArti.CanxMes.
            when '00015' then vta15 = vta15 + EvtArti.CanxMes.
            when '00016' then vta16 = vta16 + EvtArti.CanxMes.
        end case.
    end.            
end.

output to c:\tmp\susy2006.txt.
for each detalle:
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = detalle.codmat
        and almstkge.fecha <= 05/15/2007
        no-lock no-error.
    if available almstkge then detalle.ctouni = AlmStkge.CtoUni.
    for each almacen no-lock where almacen.codcia = 001:
        find last almstkal where almstkal.codcia = 001
            and almstkal.codalm = almacen.codalm
            and almstkal.codmat = detalle.codmat
            and almstkal.fecha <= 05/15/2007
            no-lock no-error.
        if available almstkal 
        then detalle.stkact = detalle.stkact + AlmStkal.StkAct.            
    end.
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.vta00
        detalle.vta01
        detalle.vta02
        detalle.vta03
        detalle.vta05
        detalle.vta08
        detalle.vta12
        detalle.vta14
        detalle.vta15
        detalle.vta16
        detalle.ctouni
        detalle.stkact
        with stream-io no-box width 320.
end.
output close.
