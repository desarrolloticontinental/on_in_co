/* productos sin ventas y con stock mayor a 12 meses */

def temp-table detalle like almmmatg
    field stkact11 as dec
    field stkact03 as dec
    field stkact04 as dec
    field stkact05 as dec
    field stkact16 as dec
    field stkact131 as dec
    field stkactotros as dec.

for each almacen no-lock where almacen.codcia = 001:
    for each almmmate no-lock where almmmate.codcia = 001
            and almmmate.codalm = almacen.codalm
            and almmmate.stkact <> 0,
            first almmmatg of almmmate no-lock where 
                almmmatg.fching <= 09/30/2006
                and lookup(trim(almmmatg.codfam), '008,009') = 0:
        display almacen.codalm almmmatg.codmat.
        pause 0.
        find detalle of almmmatg exclusive-lock no-error.
        if not available detalle then do:
            create detalle.
            buffer-copy almmmatg to detalle.
        end.
        case almacen.codalm:
            when '11' then detalle.stkact11 = almmmate.stkact.
            when '03' then detalle.stkact03 = almmmate.stkact.
            when '04' then detalle.stkact04 = almmmate.stkact.
            when '05' then detalle.stkact05 = almmmate.stkact.
            when '16' then detalle.stkact16 = almmmate.stkact.
            when '131' then detalle.stkact131 = almmmate.stkact.
            otherwise detalle.stkactOtros = detalle.stkactOtros + almmmate.stkact.
        end case.
    end.        
end.

def var x-ctopro as dec no-undo.
def var x-ctocom as dec no-undo.
def var x-tpocmb as dec init 3.20 no-undo.
def var x-stktot as dec no-undo.
def var x-nompro as char format 'x(45)' no-undo.
output to c:\tmp\sinventa.txt.
for each detalle:
    find last almdmov use-index almd02 where almdmov.codcia = 001
        and almdmov.codmat = detalle.codmat
        and almdmov.tipmov = 's'
        and almdmov.codmov = 02
        no-lock no-error.
    if available almdmov and almdmov.fchdoc > 03/31/2007 then next.
    x-stktot = detalle.stkact11 + detalle.stkact03 + detalle.stkact04 +     
                detalle.stkact05 + detalle.stkact16 + detalle.stkact131 +
                detalle.stkactotros.
    x-ctocom = detalle.ctolis.
    if detalle.monvta = 2 then x-ctocom = x-ctocom * x-tpocmb.
    x-nompro = ''.
    find gn-prov where gn-prov.codcia = 000
        and gn-prov.codpro = detalle.codpr1
        no-lock no-error.
    if available gn-prov then x-nompro = gn-prov.nompro.
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        x-ctocom
        detalle.stkact11
        detalle.stkact03
        detalle.stkact04
        detalle.stkact05
        detalle.stkact16
        detalle.stkact131
        detalle.stkactOtros
        x-stktot
        detalle.codfam
        detalle.subfam
        x-nompro
        detalle.tipart
        with stream-io no-box width 320.
end.
output close.
