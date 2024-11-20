def temp-table t-matg like almmmatg
    field stk11     as dec
    field stk131    as dec
    field stk130    as dec
    field stk152    as dec
    field stk143    as dec
    field stk153    as dec
    field stk151    as dec
    field stk22     as dec.
    
for each almacen where codcia = 001
        and lookup(trim(codalm), '11,130,131,152,143,153,151,22') > 0 no-lock:
    for each invconteo where codcia = 001
            and codalm = almacen.codalm
            and fchinv = 12/19/2005 no-lock,
            first almmmatg of invconteo no-lock:
        find t-matg of almmmatg exclusive-lock no-error.
        if not available t-matg then do:
            create t-matg.
            buffer-copy almmmatg to t-matg.
        end.
        case almacen.codalm:
            when '11' then t-matg.stk11 = invconteo.caninv.
            when '130' then t-matg.stk130 = invconteo.caninv.
            when '131' then t-matg.stk131 = invconteo.caninv.
            when '152' then t-matg.stk152 = invconteo.caninv.
            when '143' then t-matg.stk143 = invconteo.caninv.
            when '153' then t-matg.stk153 = invconteo.caninv.
            when '151' then t-matg.stk151 = invconteo.caninv.
            when '22' then t-matg.stk22 = invconteo.caninv.
        end.
    end.
end.

output to c:\tmp\ate-conti.txt.
for each t-matg:
    display t-matg.codmat t-matg.desmat t-matg.desmar t-matg.undbas
        t-matg.stk11
        t-matg.stk130 t-matg.stk131 t-matg.stk152 t-matg.stk143
        t-matg.stk153 t-matg.stk151 t-matg.stk22
        with stream-io no-box width 230.
end.
output close.
