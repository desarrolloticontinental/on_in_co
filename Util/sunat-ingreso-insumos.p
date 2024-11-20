def temp-table t-matg like almmmatg
    field i02 as dec format '->>>,>>>,>>9.99'
    field i17 as dec format '->>>,>>>,>>9.99'
    field s03 as dec format '->>>,>>>,>>9.99'
    field ctopro as dec format '->>>,>>>,>>9.99'
    field nrodoc like almdmov.nrodoc
    field fchdoc like almdmov.fchdoc
    field stkact as dec format '->>>,>>>,>>9.99'
    index llave01 as primary codmat fchdoc.

def var x-tipmov as char init 'i,i,s'.
def var x-codmov as char init '02,17,03'.
def var i as int.

do i = 1 to 3:
for each almdmov where codcia = 001
    and codalm = '11'
    and tipmov = entry(i, x-tipmov)
    and codmov = integer(entry(i,x-codmov))
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003,
    first almmmatg of almdmov no-lock where codfam = '008' and subfam = '100'
        and desmat begins 'tinta':
    create t-matg.
    buffer-copy almmmatg to t-matg
        assign
            t-matg.fchdoc = almdmov.fchdoc
            t-matg.nrodoc = almdmov.nrodoc.
    case almdmov.tipmov:
        when 'i' then
            case almdmov.codmov:
                when 02 then t-matg.i02 = almdmov.candes * almdmov.factor.
                when 17 then t-matg.i17 = almdmov.candes * almdmov.factor.
            end case.
        when 's' then
            case almdmov.codmov:
                when 03 then t-matg.s03 = almdmov.candes * almdmov.factor.
            end case.
    end case.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almdmov.codmat
        and almstkge.fecha <= almdmov.fchdoc
        no-lock no-error.
    if available almstkge 
    then assign
            t-matg.ctopro = AlmStkge.CtoUni.
    find last almstkal where almstkal.codcia = 001
        and almstkal.codalm = '11'
        and almstkal.codmat = almdmov.codmat
        and almstkal.fecha <= 12/31/2003
        no-lock no-error.
    if available almstkal then t-matg.stkact = AlmStkal.StkAct.
end.    
end.
output to c:\tmp\sunat-ingreso-insumos.txt.
for each t-matg:
    display
        t-matg.codmat
        t-matg.desmat
        t-matg.desmar
        t-matg.codpr1
        t-matg.ctopro
        t-matg.nrodoc
        t-matg.fchdoc
        t-matg.i02
        t-matg.i17
        t-matg.s03
        t-matg.stkact
        with stream-io no-box width 320.
        
end.
output close.
