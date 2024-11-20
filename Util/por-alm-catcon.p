def var x-fecha as date no-undo.

def temp-table detalle like almmmatg
    field categoria as char
    field alm12 as dec format '(>>>,>>>,>>9.99)'
    field alm11 as dec format '(>>>,>>>,>>9.99)'
    field alm130 as dec format '(>>>,>>>,>>9.99)'
    field alm131 as dec format '(>>>,>>>,>>9.99)'
    field alm152 as dec format '(>>>,>>>,>>9.99)'
    field alm143 as dec format '(>>>,>>>,>>9.99)'
    field alm160 as dec format '(>>>,>>>,>>9.99)'
    field alm85 as dec format '(>>>,>>>,>>9.99)'
    field alm86 as dec format '(>>>,>>>,>>9.99)'
    field alm87 as dec format '(>>>,>>>,>>9.99)'
    field alm03 as dec format '(>>>,>>>,>>9.99)'
    field alm04 as dec format '(>>>,>>>,>>9.99)'
    field alm05 as dec format '(>>>,>>>,>>9.99)'
    field alm83b as dec format '(>>>,>>>,>>9.99)'
    field alm16 as dec format '(>>>,>>>,>>9.99)'
    field almotros as dec format '(>>>,>>>,>>9.99)'
    index llave01 as primary categoria.
    
x-fecha = date(12,31,2006).

for each almmmatg no-lock where codcia = 001:
    display almmmatg.codmat.
    pause 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almmmatg.codmat
        and almstkge.fecha <= x-fecha
        no-lock no-error.
    if not available almstkge then next.
    for each almacen no-lock where codcia = 001:
        find last almstkal where almstkal.codcia = 001
            and almstkal.codmat = almmmatg.codmat
            and almstkal.codalm = almacen.codalm
            and almstkal.fecha <= x-fecha
            no-lock no-error.
        if not available almstkal then next.
        find detalle where detalle.categoria = almmmatg.catconta[1]
            exclusive-lock no-error.
        if not available detalle then create detalle.
        detalle.categoria = almmmatg.catconta[1].
        case almacen.codalm:
            when '12' then detalle.alm12 = detalle.alm12 + almstkal.stkact * almstkge.ctouni.
            when '11' then detalle.alm11 = detalle.alm11 + almstkal.stkact * almstkge.ctouni.
            when '130' then detalle.alm130 = detalle.alm130 + almstkal.stkact * almstkge.ctouni.
            when '131' then detalle.alm131 = detalle.alm131 + almstkal.stkact * almstkge.ctouni.
            when '152' then detalle.alm152 = detalle.alm152 + almstkal.stkact * almstkge.ctouni.
            when '143' then detalle.alm143 = detalle.alm143 + almstkal.stkact * almstkge.ctouni.
            when '160' then detalle.alm160 = detalle.alm160 + almstkal.stkact * almstkge.ctouni.
            when '85' then detalle.alm85 = detalle.alm85 + almstkal.stkact * almstkge.ctouni.
            when '86' then detalle.alm86 = detalle.alm86 + almstkal.stkact * almstkge.ctouni.
            when '87' then detalle.alm87 = detalle.alm87 + almstkal.stkact * almstkge.ctouni.
            when '03' then detalle.alm03 = detalle.alm03 + almstkal.stkact * almstkge.ctouni.
            when '04' then detalle.alm04 = detalle.alm04 + almstkal.stkact * almstkge.ctouni.
            when '05' then detalle.alm05 = detalle.alm05 + almstkal.stkact * almstkge.ctouni.
            when '83b' then detalle.alm83b = detalle.alm83b + almstkal.stkact * almstkge.ctouni.
            when '16' then detalle.alm16 = detalle.alm16 + almstkal.stkact * almstkge.ctouni.
            otherwise detalle.almotros = detalle.almotros + almstkal.stkact * almstkge.ctouni.
        end case.
    end.
end.

output to c:\tmp\susy.txt.
for each detalle:
    display
        detalle.categoria
        detalle.alm12 
        detalle.alm11 
        detalle.alm130
        detalle.alm131 
        detalle.alm152 
        detalle.alm143 
        detalle.alm160 
        detalle.alm85 
        detalle.alm86 
        detalle.alm87 
        detalle.alm03 
        detalle.alm04 
        detalle.alm05 
        detalle.alm83b 
        detalle.alm16 
        detalle.almotros
        with stream-io no-box width 320.
end.
output close.
