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

/* buscamos las ventas por mes */
def var x-coddiv as char.
def var i as int.
def var j as int.

x-coddiv = '00000,00001,00002,00003,00004,00005,00008,00011,00013,00014,00016'.
x-coddiv = '00000'.
for each t-matg:
    do j = 1 to num-entries(x-coddiv):
        for each evtarti where codcia = 001
            and coddiv  = entry(j, x-coddiv)
            and codmat = t-matg.codmat
            and nrofch = 200604 no-lock:
            do i = 1 to 31:
                t-matg.ventas[i] = t-matg.ventas[i] + canxdia[i].
            end.        
        end.
    end.
end.    

output to c:\tmp\diana-abr.txt.
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
