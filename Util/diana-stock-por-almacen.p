/* ahora valorizado */
def var x-preofi as dec no-undo.

def temp-table detalle like almmmatg
    field stkalm as dec extent 9.
def var i as int no-undo.
    
for each almacen no-lock where codcia = 001
        and lookup(trim(codalm), '03,04,05,11,12,22,16,48,83') > 0:
    i = lookup(trim(almacen.codalm), '03,04,05,11,12,22,16,48,83').
    for each almmmatg where codcia = 001 and codfam = '010' no-lock:
        x-preofi = if almmmatg.monvta = 1 then almmmatg.preofi else almmmatg.preofi * almmmatg.tpocmb.
        find last almstkal where almstkal.codcia = 001
            and almstkal.codmat = almmmatg.codmat
            and almstkal.codalm = almacen.codalm
            and almstkal.fecha <= 04/30/2006
            no-lock no-error.
        find last almstkge where almstkge.codcia = 001
            and almstkge.codmat = almmmatg.codmat
            and almstkge.fecha <= 04/30/2006
            no-lock no-error.
        if available almstkal and available almstkge then do:
            find detalle of almmmatg exclusive-lock no-error.
            if not available detalle then create detalle.
            buffer-copy almmmatg to detalle
                assign
                    detalle.stkalm[i] = AlmStkal.StkAct * x-preofi.
/*                    detalle.stkalm[i] = AlmStkal.StkAct * AlmStkge.CtoUni.*/
        end.
    end.    
end.

output to c:\tmp\diana.txt.
for each detalle:
    display 
        detalle.codmat 
        detalle.desmat
        detalle.desmar
        detalle.undbas 
        detalle.stkalm[1] column-label 'alm 03'
        detalle.stkalm[2] column-label 'alm 04'
        detalle.stkalm[3] column-label 'alm 05'
        detalle.stkalm[4] column-label 'alm 11'
        detalle.stkalm[5] column-label 'alm 12'
        detalle.stkalm[6] column-label 'alm 22'
        detalle.stkalm[7] column-label 'alm 16'
        detalle.stkalm[8] column-label 'alm 48'
        detalle.stkalm[9] column-label 'alm 83'
        with stream-io no-box width 320.
end.
output close.
