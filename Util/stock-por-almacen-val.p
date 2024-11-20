def var x-total as dec format '->>>,>>>,>>>,>>9.99'.

output to c:\tmp\stkvaldic06.txt.
for each almacen no-lock where codcia = 001:
    x-total = 0.
    for each almmmate no-lock where codcia = 001 
            and codalm = almacen.codalm:
        find last almstkal where almstkal.codcia = 001
            and almstkal.codmat = almmmate.codmat
            and almstkal.codalm = almacen.codalm
            and almstkal.fecha <= 12/31/2006
            no-lock no-error.
        find last almstkge where almstkge.codcia = 001
            and almstkge.codmat = almmmate.codmat
            and almstkge.fecha <= 12/31/2006
            no-lock no-error.
        if available almstkge and available almstkal
        then x-total = x-total + (almstkal.stkact * AlmStkge.CtoUni).
    end.
    display almacen.codalm x-total.
end.
output close.
