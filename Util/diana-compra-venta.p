def temp-table detalle
    field codcia like almmmatg.codcia
    field codmat like almmmatg.codmat
    field desmat like almmmatg.desmat
    field desmar like almmmatg.desmar
    field undbas like almmmatg.undbas
    field codfam like almmmatg.codfam
    field subfam like almmmatg.subfam
    field compra as dec
    field ventas as dec.

for each almmmatg no-lock where codcia = 001:
display almmmatg.codmat.
pause 0.
    for each almdmov no-lock where codcia = 001
            and codmat = almmmatg.codmat
            and tipmov = 'i'
            and codmov = 02
            and fchdoc >= 10/01/2007:
       find detalle of almmmatg exclusive-lock no-error.
        if not available detalle then do:
            create detalle.
            buffer-copy almmmatg to detalle.
        end.
        assign
            detalle.compra = detalle.compra + almdmov.candes * almdmov.factor.
    end.            
    for each almdmov no-lock where codcia = 001
            and codmat = almmmatg.codmat
            and tipmov = 'i'
            and codmov = 50
            and fchdoc >= 10/01/2007:
        find detalle of almmmatg exclusive-lock no-error.
        if not available detalle then do:
            create detalle.
            buffer-copy almmmatg to detalle.
        end.
        assign
            detalle.compra = detalle.compra + almdmov.candes * almdmov.factor.
    end.            
    for each almdmov no-lock where codcia = 001
            and codmat = almmmatg.codmat
            and tipmov = 's'
            and codmov = 02
            and fchdoc >= 10/01/2007:
        find detalle of almmmatg exclusive-lock no-error.
        if not available detalle then do:
            create detalle.
            buffer-copy almmmatg to detalle.
        end.
        assign
            detalle.venta = detalle.venta + almdmov.candes * almdmov.factor.
    end.            
end.


output to c:\tmp\compra-venta.txt.
for each detalle:
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.codfam
        detalle.subfam
        detalle.undbas
        detalle.compra format '>,>>>,>>>,>>9.99'
        detalle.venta  format '>,>>>,>>>,>>9.99'
        with stream-io no-box width 320.
end.
output close.
        
