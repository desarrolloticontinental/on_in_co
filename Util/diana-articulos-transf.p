def var x-linea as char format 'x(10)'.
def temp-table t-matg like almmmatg
    field ventas as dec format '->>>>9' extent 31.

input from c:\tmp\suministros.prn.
repeat:
    import unformatted x-linea.
    find almmmatg where codcia = 001
        and codmat = substring(x-linea,1,6)
        no-lock no-error.
    if available almmmatg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
end.
input close.

/* buscamos las compras por mes */
def var i as int.
def var j as int.
for each t-matg:
    for each almdmov no-lock where codcia = 001
        and codalm = '11'
        and codmat = t-matg.codmat
        and tipmov = 'i'
        and codmov = 03
        and fchdoc >= 03/01/2006
        and fchdoc <= 03/31/2006:
        i = day(fchdoc).
        t-matg.ventas[i] = t-matg.ventas[i] + (candes * factor).
    end.
end.    

output to c:\tmp\diana-transf-mar.txt.
for each t-matg:
    display 
        t-matg.codmat 
        t-matg.desmat       format 'x(40)'
        /*t-matg.desmar       format 'x(10)'*/
        t-matg.undbas
        t-matg.ventas[1]
        t-matg.ventas[2]
        t-matg.ventas[3]
        t-matg.ventas[4]
        t-matg.ventas[5]
        t-matg.ventas[6]
        t-matg.ventas[7]
        t-matg.ventas[8]
        t-matg.ventas[9]
        t-matg.ventas[10]
        t-matg.ventas[11]
        t-matg.ventas[12]
        t-matg.ventas[13]
        t-matg.ventas[14]
        t-matg.ventas[15]
        t-matg.ventas[16]
        t-matg.ventas[17]
        t-matg.ventas[18]
        t-matg.ventas[19]
        t-matg.ventas[20]
        t-matg.ventas[21]
        t-matg.ventas[22]
        t-matg.ventas[23]
        t-matg.ventas[24]
        t-matg.ventas[25]
        t-matg.ventas[26]
        t-matg.ventas[27]
        t-matg.ventas[28]
        t-matg.ventas[29]
        t-matg.ventas[30]
        t-matg.ventas[31]
        with stream-io no-box no-labels width 320.
end.
output close.
