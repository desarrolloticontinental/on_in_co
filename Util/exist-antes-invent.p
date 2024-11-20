def var x-ctopro as dec.
def var x-ctorep as dec.

def temp-table t-matg like almmmatg
    field stk11     as dec
    field stk131    as dec
    field stk130    as dec
    field stk152    as dec
    field stk143    as dec
    field stk153    as dec
    field stk151    as dec
    field stk22     as dec
    field ctopro    as dec
    field ctorep    as dec.

for each almacen where codcia = 001 
    and lookup(trim(almacen.codalm), '11,131,130,152,143,153,151,22') > 0 
    no-lock:
  for each almmmate where almmmate.codcia = 001
        and almmmate.codalm = almacen.codalm
        and almmmate.stkact <> 0 no-lock,
        first almmmatg of almmmate no-lock:

    x-ctopro = 0.
    FIND LAST AlmStkGe WHERE almstkge.codcia = 001
        AND almstkge.codmat = almmmatg.codmat
        AND almstkge.fecha <= TODAY
        NO-LOCK NO-ERROR.
    IF AVAILABLE AlmStkGe THEN x-ctopro = almstkge.ctouni.

    x-ctorep = Almmmatg.CtoLis.
    IF Almmmatg.MonVta = 2
    THEN x-ctorep = x-ctorep * Almmmatg.TpoCmb. 

    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg
            assign 
                t-matg.ctopro = x-ctopro
                t-matg.ctorep = x-ctorep.    
    end.
    case almacen.codalm:
        when '11' then t-matg.stk11 = almmmate.stkact.
        when '131' then t-matg.stk131 = almmmate.stkact.
        when '130' then t-matg.stk130 = almmmate.stkact.
        when '152' then t-matg.stk152 = almmmate.stkact.
        when '143' then t-matg.stk143 = almmmate.stkact.
        when '153' then t-matg.stk153 = almmmate.stkact.
        when '151' then t-matg.stk151 = almmmate.stkact.
        when '22' then t-matg.stk22 = almmmate.stkact.
    end case.            
  end.
end.                
output to c:\tmp\antes-invt.txt.
for each t-matg:
    display t-matg.codmat t-matg.desmat t-matg.desmar t-matg.undbas
        t-matg.codfam t-matg.subfam
        t-matg.stk11 t-matg.stk131 t-matg.stk130 t-matg.stk152
        t-matg.stk143 t-matg.stk153 t-matg.stk151 t-matg.stk22
        t-matg.ctopro t-matg.ctorep
        with stream-io no-box width 230.
end.
output close.
