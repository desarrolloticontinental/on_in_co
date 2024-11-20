def temp-table t-matg like almmmatg
    field nompro as char format 'x(40)'
    field candes as dec extent 6
    field fchdoc as date extent 6
    index llave01 as primary unique codcia codmat.
def temp-table t-mov
    field codcia like almmmatg.codcia
    field codmat like almmmatg.codmat
    field fchdoc as date 
    field candes as dec 
    index llave01 as primary unique codcia codmat fchdoc.
def var x-linea as char format 'x(10)'.    

input from c:\tmp\codigos-fabrica.txt.
repeat:
    import unformatted x-linea.
    find almmmatg where almmmatg.codcia = 001
        and almmmatg.codmat = substring(x-linea,1,6)
        no-lock no-error.
    if available almmmatg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg
            assign t-matg.candes = 0
                    t-matg.fchdoc = ?.
    end.
end.
for each t-matg where t-matg.codmat <> '':
    find gn-prov where gn-prov.codcia = 000
        and gn-prov.codpro = t-matg.codpr1
        no-lock no-error.
    if available gn-prov then t-matg.nompro = gn-prov.nompro.
    for each almdmov use-index almd03 where almdmov.codcia = 001
        and almdmov.codalm = '11'
        and almdmov.codmat = t-matg.codmat
        and almdmov.tipmov = 'S'
        and almdmov.codmov = 03
        and Almdmov.AlmOri = '12' 
        and almdmov.fchdoc >= 10/01/2005 
        and almdmov.fchdoc <= today - 1 no-lock:
        find t-mov where t-mov.codcia = almdmov.codcia
            and t-mov.codmat = almdmov.codmat
            and t-mov.fchdoc = almdmov.fchdoc exclusive-lock no-error.
        if not available t-mov then create t-mov.
        assign
            t-mov.codcia = almdmov.codcia
            t-mov.codmat = almdmov.codmat
            t-mov.fchdoc = almdmov.fchdoc
            t-mov.candes = t-mov.candes + almdmov.candes.
    end.
end.
def var x-mes as int.
for each t-mov:
    find t-matg of t-mov exclusive-lock no-error.
    case month(t-mov.fchdoc):
        when 10 then x-mes = 1.
        when 11 then x-mes = 2.
        when 12 then x-mes = 3.
        when 1 then x-mes = 4.
        when 2 then x-mes = 5.
        when 3 then x-mes = 6.
    end case.    
    if t-mov.candes > t-matg.candes[x-mes] then do:
        assign
            t-matg.candes[x-mes] = t-mov.candes
            t-matg.fchdoc[x-mes] = t-mov.fchdoc.
    end.
end.
output to c:\tmp\fabrica.txt.
for each t-matg where t-matg.codmat <> '':
    display 
        t-matg.codmat 
        t-matg.desmat 
        t-matg.codpr1 
        t-matg.nompro
        t-matg.candes[1] t-matg.fchdoc[1]
        t-matg.candes[2] t-matg.fchdoc[2]
        t-matg.candes[3] t-matg.fchdoc[3]
        t-matg.candes[4] t-matg.fchdoc[4]
        t-matg.candes[5] t-matg.fchdoc[5]
        t-matg.candes[6] t-matg.fchdoc[6]
        with stream-io no-box width 320.
end.
output close.
