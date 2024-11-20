def var x-fecha as date.
x-fecha = 01/02/2006.

def temp-table t-matg like almmmatg
    field ctouni as dec
    field stk03 as dec
    field stk04 as dec
    field stk05 as dec
    field tot03 as dec
    field tot04 as dec
    field tot05 as dec.

for each almmmatg where codcia = 001 no-lock,
    last almstkge of almmmatg no-lock where almstkge.fecha <= x-fecha,
    each almmmate of almmmatg no-lock where lookup(trim(codalm), '03,04,05') > 0,
    last almstkal of almmmate no-lock where almstkal.fecha <= x-fecha:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then create t-matg.
    buffer-copy almmmatg to t-matg
        assign t-matg.ctouni = almstkge.ctouni.
    case almmmate.codalm:
        when '03' then t-matg.stk03 = t-matg.stk03 + almstkal.stkact.
        when '04' then t-matg.stk04 = t-matg.stk04 + almstkal.stkact.
        when '05' then t-matg.stk05 = t-matg.stk05 + almstkal.stkact.
    end case.
end.

output to c:\tmp\stock01022006.txt.
for each t-matg:
    display
        t-matg.codmat
        t-matg.desmat format 'x(60)'
        t-matg.desmar
        t-matg.undbas format 'x(10)'
        t-matg.ctouni format '->>>,>>>,>>9.999999'
        t-matg.stk03 format '->>>,>>>,>>9.9999'
        t-matg.stk04 format '->>>,>>>,>>9.9999'
        t-matg.stk05 format '->>>,>>>,>>9.9999'
        with stream-io no-box no-labels width 320.
end.
output close.
