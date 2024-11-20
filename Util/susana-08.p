def var x-validos as char init '11,04,03,05,16,04a,03a,05a'.

def temp-table t-matg like almmmatg
    field alm11 as dec
    field alm04 as dec
    field alm03 as dec
    field alm05 as dec
    field alm16 as dec
    field otros as dec
    field ctopro as dec.
    

for each almmmatg no-lock where codcia = 001
    and codpr1 = '10005035':
    for each almacen no-lock where almacen.codcia = 001
            and lookup(trim(almacen.codalm), x-validos) > 0:
        find almmmate where almmmate.codcia = 001
            and almmmate.codalm = almacen.codalm
            and almmmate.codmat = almmmatg.codmat
            no-lock no-error.
        if available almmmate then do:
            find t-matg of almmmatg exclusive-lock no-error.
            if not available t-matg then create t-matg.
            buffer-copy almmmatg to t-matg.
            case almacen.codalm:
                when '11' then t-matg.alm11 = almmmate.stkact.
                when '04' then t-matg.alm04 = almmmate.stkact.
                when '03' then t-matg.alm03 = almmmate.stkact.
                when '05' then t-matg.alm05 = almmmate.stkact.
                when '16' then t-matg.alm16 = almmmate.stkact.
                otherwise t-matg.otros = t-matg.otros + almmmate.stkact.
            end case.
            find last almstkge of almmmatg no-lock no-error.
            if available almstkge 
            then t-matg.ctopro = almstkge.ctouni.
        end.
    end.
end.

output to c:\tmp\susana.txt.
for each t-matg where (alm11 + alm04 + alm03 + alm05 + alm16 + otros ) <> 0:
    display t-matg.codmat t-matg.desmat t-matg.desmar t-matg.undbas
        alm11 alm04 alm03 alm05 alm16 otros ctopro
        with stream-io no-box width 320.
end.
output close.
