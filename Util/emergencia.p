def var x-saldo as dec no-undo.
def var x-nompro like ate.gn-prov.nompro no-undo.

def temp-table detalle like ate.almmmatg
    field stkact04 as dec
    field valstk04 as dec
    field stkact15 as dec
    field valstk15 as dec
    field stkact30 as dec
    field valstk30 as dec
    field candesp as dec
    field candess as dec
    field valdesp as dec
    field valdess as dec.
    
for each expo.faccpedi no-lock where codcia = 001
        and coddoc = 'cot'
        and fchped >= 01/10/2007
        and coddiv = '00015'
        and flgest = 'P',
        each expo.facdpedi of expo.faccpedi no-lock where (canped - canate) > 0,
        first ate.almmmatg of expo.facdpedi no-lock:
    display expo.faccpedi.fchped expo.faccpedi.nroped.
    pause 0.
    x-saldo = expo.facdpedi.canped - expo.facdpedi.canate.
    find detalle of expo.facdpedi exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
        buffer-copy ate.almmmatg to detalle.
    end.
    if expo.facdpedi.canate > 0
    then 
        assign
            detalle.candesp = detalle.candesp + x-saldo
            detalle.valdesp = detalle.valdesp + 
                            x-saldo * 
                            (if expo.faccpedi.codmon = 1
                                then expo.facdpedi.preuni / expo.faccpedi.tpocmb
                                else expo.facdpedi.preuni).
    else 
        assign
            detalle.candess = detalle.candess + x-saldo
            detalle.valdess = detalle.valdess + 
                            x-saldo * 
                            (if expo.faccpedi.codmon = 1
                                then expo.facdpedi.preuni / expo.faccpedi.tpocmb
                                else expo.facdpedi.preuni).
end.        

for each expo.almmmate no-lock where codcia = 001 and codalm = '15' and stkact > 0,
        first ate.almmmatg of expo.almmmate no-lock:
    find detalle of ate.almmmatg exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
        buffer-copy ate.almmmatg to detalle.
    end.
    detalle.stkact15 = expo.almmmate.stkact.
    assign
        detalle.valstk15 = detalle.stkact15 *
                        (if detalle.monvta = 1 
                            then detalle.ctotot / detalle.tpocmb
                            else detalle.ctotot).
end.

for each detalle:
    display detalle.codmat.
    pause 0.
    find lima.almmmate of detalle where lima.almmmate.codalm = '04'
        no-lock no-error.
    if available lima.almmmate then detalle.stkact04 = lima.almmmate.stkact.
    find lima.almmmate of detalle where lima.almmmate.codalm = '30'
        no-lock no-error.
    if available lima.almmmate then detalle.stkact30 = lima.almmmate.stkact.
/*    find expo.almmmate of detalle where expo.almmmate.codalm = '15'
 *         no-lock no-error.
 *     if available expo.almmmate then detalle.stkact15 = expo.almmmate.stkact.*/
    assign
        detalle.valstk04 = detalle.stkact04 * 
                        (if detalle.monvta = 1 
                            then detalle.ctotot / detalle.tpocmb
                            else detalle.ctotot)
/*        detalle.valstk15 = detalle.stkact15 *
 *                         (if detalle.monvta = 1 
 *                             then detalle.ctotot / detalle.tpocmb
 *                             else detalle.ctotot)*/
        detalle.valstk30 = detalle.stkact30 * 
                        (if detalle.monvta = 1 
                            then detalle.ctotot / detalle.tpocmb
                            else detalle.ctotot).
end.

/*
for each ate.almmmatg no-lock where codcia = 001
        and tpoart <> 'D':
    display ate.almmmatg.codmat.
    pause 0.
    create detalle.
    buffer-copy ate.almmmatg to detalle.
    find lima.almmmate of ate.almmmatg where lima.almmmate.codalm = '04'
        no-lock no-error.
    if available lima.almmmate then detalle.stkact04 = lima.almmmate.stkact.
    find lima.almmmate of ate.almmmatg where lima.almmmate.codalm = '30'
        no-lock no-error.
    if available lima.almmmate then detalle.stkact30 = lima.almmmate.stkact.
    find expo.almmmate of ate.almmmatg where expo.almmmate.codalm = '15'
        no-lock no-error.
    if available expo.almmmate then detalle.stkact15 = expo.almmmate.stkact.
    assign
        detalle.valstk04 = detalle.stkact04 * 
                        (if ate.almmmatg.monvta = 1 
                            then ate.almmmatg.ctotot / ate.almmmatg.tpocmb
                            else ate.almmmatg.ctotot)
        detalle.valstk15 = detalle.stkact15 *
                        (if ate.almmmatg.monvta = 1 
                            then ate.almmmatg.ctotot / ate.almmmatg.tpocmb
                            else ate.almmmatg.ctotot)
        detalle.valstk30 = detalle.stkact30 * 
                        (if ate.almmmatg.monvta = 1 
                            then ate.almmmatg.ctotot / ate.almmmatg.tpocmb
                            else ate.almmmatg.ctotot).
end.    
*/


output to c:\tmp\balance.txt.
for each detalle no-lock:
    x-nompro = ''.
    find ate.gn-prov where ate.gn-prov.codcia = 000
        and ate.gn-prov.codpro = detalle.codpr1
        no-lock no-error.
    if available ate.gn-prov then x-nompro = ate.gn-prov.nompro.
    display
        detalle.codmat
        detalle.desmat
        detalle.undbas
        /*detalle.desmar*/
        detalle.codpr1
        x-nompro
        detalle.stkact04 format '->>>,>>>,>>9.99'
        detalle.valstk04 format '->>>,>>>,>>9.99'
        detalle.stkact15 format '->>>,>>>,>>9.99'
        detalle.valstk15 format '->>>,>>>,>>9.99'
        detalle.stkact30 format '->>>,>>>,>>9.99'
        detalle.valstk30 format '->>>,>>>,>>9.99'
        detalle.candesp format '->>>,>>>,>>9.99'
        detalle.valdesp format '->>>,>>>,>>9.99'
        detalle.candess format '->>>,>>>,>>9.99'
        detalle.valdess format '->>>,>>>,>>9.99'
        with stream-io no-box width 320.
end.
output close.
