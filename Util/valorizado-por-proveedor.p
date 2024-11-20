def var x-stkval as dec.
def var x-items as int init 0.
def var x-nompro as char format 'x(40)'.
def var x-stk79 as dec.

output to c:\tmp\valoxproveedor.txt.
for each almmmatg no-lock where codcia = 001
    and tpoart <> 'D'
    break by codpr1:
    x-stkval = 0.
    x-stk79 = 0.
    find last almstkal where almstkal.codcia = almmmatg.codcia
        and almstkal.codmat = almmmatg.codmat
        and almstkal.codalm = '79'
        and almstkal.fecha <= 07/31/2006
        no-lock no-error.
    if available almstkal then x-stk79 = almstkal.stkact.
    find last almstkge where almstkge.codcia = almmmatg.codcia
        and almstkge.codmat = almmmatg.codmat
        and almstkge.fecha <= date(07,31,2006)
        no-lock no-error.
    if available almstkge then x-stkval = (AlmStkge.StkAct - x-stk79) * AlmStkge.CtoUni.
    accumulate x-stkval (TOTAL BY codpr1).
    x-items = x-items + 1.
    if last-of (codpr1)
    then do:
        x-nompro = 'sin proveedor'.
        find gn-prov where gn-prov.codcia = 000
            and gn-prov.codpro = almmmatg.codpr1
            no-lock no-error.
        if available gn-prov then x-nompro = gn-prov.nompro.
        display 
            codpr1 
            x-nompro 
            x-items 
            (accum total by codpr1 x-stkval) format '->>>,>>>,>>9.99'
            with stream-io width 320 no-labels.
        x-items = 0.
    end.
end.
output close.
