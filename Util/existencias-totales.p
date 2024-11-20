def var x-fecha as date no-undo.

x-fecha = today.

def temp-table detalle like almmmatg
    field stk03 as dec
    field stk04 as dec
    field stk05 as dec
    field stk15 as dec
    field stk30 as dec
    field stk11 as dec
    field stk85 as dec
    field stk86 as dec
    field stk16 as dec
    field stk03c as dec
    field stk83b as dec
    field stk160 as dec
    field stk20 as dec
    field stk18 as dec
    field otros as dec
    field preuni as dec.
    
for each almmmatg no-lock where codcia = 001:
    display almmmatg.codmat.
    pause 0.
    create detalle.
    buffer-copy almmmatg to detalle.
    for each almacen no-lock where codcia = 001:
        find last almstkal where almstkal.codcia = 001
            and almstkal.codalm = almacen.codalm
            and almstkal.codmat = almmmatg.codmat
            and almstkal.fecha <= x-fecha
            no-lock no-error.
        if not available almstkal then next.
        case almacen.codalm:
            when '03' then detalle.stk03 = almstkal.stkact.
            when '04' then detalle.stk04 = almstkal.stkact.
            when '05' then detalle.stk05 = almstkal.stkact.
            when '15' then detalle.stk15 = almstkal.stkact.
            when '30' then detalle.stk30 = almstkal.stkact.
            when '11' then detalle.stk11 = almstkal.stkact.
            when '85' then detalle.stk85 = almstkal.stkact.
            when '86' then detalle.stk86 = almstkal.stkact.
            when '16' then detalle.stk16 = almstkal.stkact.
            when '03c' then detalle.stk03c = almstkal.stkact.
            when '83b' then detalle.stk83b = almstkal.stkact.
            when '160' then detalle.stk160 = almstkal.stkact.
            when '20' then detalle.stk20 = almstkal.stkact.
            when '18' then detalle.stk18 = almstkal.stkact.
        end case.            
    end.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almmmatg.codmat
        and almstkge.fecha <= x-fecha
        no-lock no-error.
    if available almstkge then detalle.preuni = AlmStkge.CtoUni.
    if detalle.preuni <= 0
    then if almmmatg.monvta = 1
        then detalle.preuni = almmmatg.ctotot.
        else detalle.preuni = almmmatg.ctotot * almmmatg.tpocmb.
end.    

output to c:\tmp\existot.txt.
for each detalle:
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.catconta[1]
        detalle.codfam
        detalle.subfam
        detalle.preuni  format '->>>,>>>,>>9.99'
        detalle.stk03   format '->>>,>>>,>>9.99'
        detalle.stk04   format '->>>,>>>,>>9.99'
        detalle.stk05   format '->>>,>>>,>>9.99'
        detalle.stk15   format '->>>,>>>,>>9.99'
        detalle.stk30    format '->>>,>>>,>>9.99'
        detalle.stk11    format '->>>,>>>,>>9.99'
        detalle.stk85    format '->>>,>>>,>>9.99'
        detalle.stk86    format '->>>,>>>,>>9.99'
        detalle.stk16    format '->>>,>>>,>>9.99'
        detalle.stk03c   format '->>>,>>>,>>9.99'
        detalle.stk83b   format '->>>,>>>,>>9.99'
        detalle.stk160    format '->>>,>>>,>>9.99'
        detalle.stk20    format '->>>,>>>,>>9.99'
        detalle.stk18    format '->>>,>>>,>>9.99'
        with stream-io no-box width 320.
end.
output close.
