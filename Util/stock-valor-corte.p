def var x-imptot as dec format '->>>,>>>,>>9.99' no-undo.
def var x-ctopro as dec no-undo.
def stream texto.
output stream texto to c:\tmp\susy.txt.
for each almacen no-lock where codcia = 001:
    x-imptot = 0.
    for each almmmate no-lock where codcia = 001
            and codalm = almacen.codalm
            and stkact <> 0:
        display almmmate.codalm almmmate.codmat.
        pause 0.
        find last almstkal where almstkal.codcia = 001
            and almstkal.codmat = almmmate.codmat
            and almstkal.codalm = almacen.codalm
            and almstkal.fecha <= 12/31/2006
            no-lock no-error.
        find last almstkge where almstkge.codcia = 001
            and almstkge.codmat = almmmate.codmat
            and almstkge.fecha <= 12/31/2006
            no-lock no-error.
        if available almstkal and available almstkge
        then x-imptot = x-imptot + (AlmStkal.StkAct * AlmStkge.CtoUni).
    end.            
    display stream texto almacen.codalm x-imptot.
    pause 0.
end.
output stream texto close.
