def var x-costo as dec no-undo format '->>>,>>>,>>9.99'.
def var x-total as dec no-undo format '->>>,>>>,>>9.99'.
def var s-total as dec no-undo format '->>>,>>>,>>9.99'.

output to c:\tmp\diana.txt.
for each almmmatg no-lock where codcia = 001 ,
    each almmmate of almmmatg no-lock,
    first gn-prov no-lock where gn-prov.codcia = 000
        and gn-prov.codpro = almmmatg.codpr1
    break by codpr1 by codalm:
    assign
        x-costo = 0.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almmmatg.codmat
        and fecha <= today no-lock no-error.
    if available almstkge then x-costo = almstkge.ctouni.
    x-total = x-costo * almmmate.stkact.
    accumulate x-total (sub-total by codpr1 by codalm).
    if last-of(codalm) then do:
        s-total = (accum sub-total by codalm x-total).
        if s-total <> 0 then
        display 
            codpro 
            nompro 
            codalm
            accum sub-total by codalm x-total @ x-total
            with stream-io no-box width 200.
    end.
    if last-of(codpr1) then do:
        underline x-total.
        display 
            codpro 
            nompro 
            accum sub-total by codpr1 x-total @ x-total
            with stream-io no-box width 200.
        down 1.
    end.
end.
output close.
