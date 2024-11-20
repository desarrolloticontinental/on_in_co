def temp-table detalle
    field catconta as char
    field saldo as dec format '->>>,>>>,>>>.99' extent 12.
    
for each almmmatg no-lock where codcia = 001:
    display almmmatg.codmat.
    pause 0.
    for each almstkge no-lock where almstkge.codcia = almmmatg.codcia
            and almstkge.codmat = almmmatg.codmat
            and almstkge.fecha >= 01/01/2005
            and almstkge.fecha <= 12/31/2005
            by almstkge.fecha:
        if month(fecha + 1 ) <> month(fecha) then do:
            find detalle where detalle.catconta = almmmatg.catconta[1]
                exclusive-lock no-error.
            if not available detalle then do:
                create detalle.
                detalle.catconta = almmmatg.catconta[1].
            end.
            detalle.saldo[month(fecha)] = detalle.saldo[month(fecha)] +
                                            (AlmStkge.CtoUni * AlmStkge.StkAct).
        end.
    end.            
end.

output to c:\tmp\conti-2005.txt.
for each detalle:
    display
        detalle.catconta
        detalle.saldo[1]
        detalle.saldo[2]
        detalle.saldo[3]
        detalle.saldo[4]
        detalle.saldo[5]
        detalle.saldo[6]
        detalle.saldo[7]
        detalle.saldo[8]
        detalle.saldo[9]
        detalle.saldo[10]
        detalle.saldo[11]
        detalle.saldo[12]
        with stream-io no-box no-labels width 200.
end.
output close.
