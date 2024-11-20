def temp-table detalle like almmmatg
    field codpro like gn-prov.codpro
    field nompro like gn-prov.nompro
    field alm03  as dec format '->>>,>>>,>>9.99'
    field alm04  as dec format '->>>,>>>,>>9.99'
    field alm05  as dec format '->>>,>>>,>>9.99'
    field alm11  as dec format '->>>,>>>,>>9.99'
    field alm30  as dec format '->>>,>>>,>>9.99'
    index llave01 codpro.

/* carga informacion */
for each almmmate no-lock where codcia = 001
        and lookup(trim(codalm), '03,04,05,11,30') > 0
        and stkact <> 0,
        first almmmatg of almmmate no-lock,
        first gn-prov where gn-prov.codcia = 000
            and gn-prov.codpro = almmmatg.codpr1 no-lock:
    display almmmate.codalm almmmatg.codmat.
    pause 0.
    find detalle where detalle.codpro = gn-prov.codpro
        exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
        buffer-copy almmmatg to detalle
        assign
            detalle.codpro = gn-prov.codpro
            detalle.nompro = gn-prov.nompro.
    end.
    case almmmate.codalm:
        when '03' then detalle.alm03 = detalle.alm03 + almmmate.stkact.
        when '04' then detalle.alm04 = detalle.alm04 + almmmate.stkact.
        when '05' then detalle.alm05 = detalle.alm05 + almmmate.stkact.
        when '11' then detalle.alm11 = detalle.alm11 + almmmate.stkact.
        when '30' then detalle.alm30 = detalle.alm30 + almmmate.stkact.
    end case.
end.
    
output to c:\tmp\diana.txt.
for each detalle:
    detalle.alm03 = detalle.alm03 * detalle.ctolis.
    detalle.alm04 = detalle.alm04 * detalle.ctolis.
    detalle.alm05 = detalle.alm05 * detalle.ctolis.
    detalle.alm11 = detalle.alm11 * detalle.ctolis.
    detalle.alm30 = detalle.alm30 * detalle.ctolis.
    display
        detalle.codpro
        detalle.nompro
        detalle.monvta
        detalle.alm03
        detalle.alm04
        detalle.alm05
        detalle.alm11
        detalle.alm30
        with stream-io no-box width 200.
end.
output close.
