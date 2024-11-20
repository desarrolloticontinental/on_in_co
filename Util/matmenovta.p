def var x-fecha-1 as date.
def var x-fecha-2 as date.
def var s-codcia as int init 1.
def var x-stkact as dec.
def var x-fching as date.


def temp-table reporte
    field codcia like almmmatg.codcia
    field codmat like almmmatg.codmat
    field codfam like almmmatg.codfam
    field desmat like almmmatg.desmat
    field desmar like almmmatg.desmar
    field undbas like almmmatg.undbas
    field tipart like almmmatg.tipart
    field t-cuenta as int
    field t-canven as dec
    field t-reposicion as dec
    field t-stkact as dec extent 10
    field t-compras as dec
    index llave01 as primary codmat.

assign
    x-fecha-1 = date(05,01,2003)    /* movimientos desde */
    x-fecha-2 = date(10,31,2004)    /* movimientos hasta */
    x-fching = date(08,31,2004).    /* creados hasta esta fecha */

catalogo:    
for each almmmatg where codcia = s-codcia 
        and tpoart <> 'd'           /* solo activos */
        and fching <= x-fching
        no-lock:
    display almmmatg.codmat.
    pause 0.
    ventas:
    for each almacen where codcia = s-codcia no-lock:
        for each almdmov use-index almd03 where codcia = s-codcia
                and codalm = almacen.codalm
                and codmat = almmmatg.codmat
                and tipmov = 's'
                and codmov = 02
                and fchdoc >= x-fecha-1
                and fchdoc <= x-fecha-2
                no-lock:
            find reporte of almmmatg exclusive-lock no-error.
            if not available reporte then create reporte.
            buffer-copy almmmatg to reporte
                assign 
                    reporte.t-cuenta = reporte.t-cuenta + 1
                    reporte.t-canven = reporte.t-canven + almdmov.candes * almdmov.factor
                    reporte.t-reposicion = (if almmmatg.monvta = 1 then almmmatg.ctolis else almmmatg.ctolis * almmmatg.tpocmb).
            if reporte.t-cuenta > 10 then next catalogo.
        end.                
    end.
end.
for each reporte where t-cuenta <= 10:
    /* stocks de almacen */
    for each almmmate where almmmate.codcia = reporte.codcia
        and almmmate.codmat = reporte.codmat 
        and lookup(trim(almmmate.codalm), "03,03a,04,04a,05,05a,83,11,22,16") > 0 no-lock:
        case almmmate.codalm:
            when '03' then  t-stkact[1] = reporte.t-stkact[1] + almmmate.stkact.
            when '03a' then t-stkact[2] = reporte.t-stkact[2] + almmmate.stkact.
            when '04' then  t-stkact[3] = reporte.t-stkact[3] + almmmate.stkact.
            when '04a' then t-stkact[4] = reporte.t-stkact[4] + almmmate.stkact.
            when '05' then  t-stkact[5] = reporte.t-stkact[5] + almmmate.stkact.
            when '05a' then t-stkact[6] = reporte.t-stkact[6] + almmmate.stkact.
            when '83' then  t-stkact[7] = reporte.t-stkact[7] + almmmate.stkact.
            when '11' then  t-stkact[8] = reporte.t-stkact[8] + almmmate.stkact.
            when '22' then  t-stkact[9] = reporte.t-stkact[9] + almmmate.stkact.
            when '16' then  t-stkact[10] = reporte.t-stkact[10] + almmmate.stkact.
        end case.
    end.
    /* compras */
    for each almdmov use-index almd02 where codcia = s-codcia
            and codmat = reporte.codmat
            and tipmov = 'i'
            and codmov = 02
            and fchdoc >= x-fecha-1
            and fchdoc <= x-fecha-2
            no-lock:
        assign
            reporte.t-compras = reporte.t-compras + almdmov.candes * almdmov.factor.
    end.
end.
    
output to c:\tmp\sinmov.txt.
for each reporte where t-cuenta <= 10:
    display 
        reporte.codmat 
        reporte.desmat
        reporte.tipart 
        reporte.codfam
        reporte.desmar 
        reporte.undbas 
        reporte.t-canven 
        reporte.t-compras
        reporte.t-reposicion
        reporte.t-stkact[1]
        reporte.t-stkact[2]
        reporte.t-stkact[3]
        reporte.t-stkact[4]
        reporte.t-stkact[5]
        reporte.t-stkact[6]
        reporte.t-stkact[7]
        reporte.t-stkact[8]
        reporte.t-stkact[9]
        reporte.t-stkact[10]
        WITH stream-io no-labels no-box width 250.
end.
output close.
