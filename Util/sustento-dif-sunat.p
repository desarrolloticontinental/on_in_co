def var x-codalm as char init '12'.

def temp-table t-matg like almmmatg
    field costo as dec
    field i01 as dec
    field s01 as dec
    field i99 as dec
    field s99 as dec
    index llave01 as primary desmat.
    

for each almcmov no-lock where codcia = 001
    and codalm = x-codalm
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003
    and tipmov = 'i'
    and codmov = 01,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
    assign
        i01 = i01 + almdmov.candes.
end.
for each almcmov no-lock where codcia = 001
    and codalm = x-codalm
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003
    and tipmov = 's'
    and codmov = 01,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
    assign
        s01 = s01 + almdmov.candes.
end.
for each almcmov no-lock where codcia = 001
    and codalm = x-codalm
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003
    and tipmov = 'i'
    and codmov = 99,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
    assign
        i99 = i99 + almdmov.candes.
end.
for each almcmov no-lock where codcia = 001
    and codalm = x-codalm
    and fchdoc >= 01/01/2003
    and fchdoc <= 12/31/2003
    and tipmov = 's'
    and codmov = 99,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    find t-matg of almmmatg exclusive-lock no-error.
    if not available t-matg then do:
        create t-matg.
        buffer-copy almmmatg to t-matg.
    end.
    assign
        s99 = s99 + almdmov.candes.
end.

def var x-canpos as dec format '>>>,>>>,>>9.99'.
def var x-canneg as dec format '>>>,>>>,>>9.99'.
def var x-imppos as dec format '>>>,>>>,>>9.99'.
def var x-impneg as dec format '>>>,>>>,>>9.99'.

output to c:\tmp\sunat12sf.txt.
for each t-matg break by t-matg.codcia by t-matg.codfam by t-matg.subfam by t-matg.desmat:
    find last Almstkge where AlmStkge.CodCia = 001
        and AlmStkge.codmat = t-matg.codmat
        and AlmStkge.Fecha <= 12/31/2003 no-lock no-error.
    if available almstkge then t-matg.costo = AlmStkge.CtoUni.
    assign
        x-canpos = t-matg.i01 + t-matg.i99
        x-canneg = t-matg.s01 + t-matg.s99
        x-imppos = x-canpos * t-matg.costo
        x-impneg = x-canneg * t-matg.costo.
    accumulate x-canpos (sub-total by t-matg.codfam by t-matg.subfam).
    accumulate x-canneg (sub-total by t-matg.codfam by t-matg.subfam).
    accumulate x-imppos (sub-total by t-matg.codfam by t-matg.subfam).
    accumulate x-impneg (sub-total by t-matg.codfam by t-matg.subfam).
    accumulate x-canpos (total by t-matg.codcia).
    accumulate x-canneg (total by t-matg.codcia).
    accumulate x-imppos (total by t-matg.codcia).
    accumulate x-impneg (total by t-matg.codcia).
    display t-matg.codfam t-matg.subfam t-matg.codmat t-matg.desmat t-matg.desmar t-matg.undbas
        t-matg.costo t-matg.i01 t-matg.s01 t-matg.i99 t-matg.s99
        x-canpos x-canneg x-imppos x-impneg
        with stream-io no-box width 320.
    if last-of(t-matg.subfam) then do:
        underline
            x-canpos x-canneg x-imppos x-impneg.
        display
            (accum sub-total by t-matg.subfam x-canpos) @ x-canpos
            (accum sub-total by t-matg.subfam x-canneg) @ x-canneg
            (accum sub-total by t-matg.subfam x-imppos) @ x-imppos
            (accum sub-total by t-matg.subfam x-impneg) @ x-impneg
            with stream-io no-box width 320.
    end.
    if last-of(t-matg.codfam) then do:
        underline
            x-canpos x-canneg x-imppos x-impneg.
        display
            (accum sub-total by t-matg.codfam x-canpos) @ x-canpos
            (accum sub-total by t-matg.codfam x-canneg) @ x-canneg
            (accum sub-total by t-matg.codfam x-imppos) @ x-imppos
            (accum sub-total by t-matg.codfam x-impneg) @ x-impneg
            with stream-io no-box width 320.
    end.
    if last-of(t-matg.codcia) then do:
        underline
            x-canpos x-canneg x-imppos x-impneg.
        display
            /*(accum sub-total by t-matg.codcia x-canpos) @ x-canpos
            (accum sub-total by t-matg.codcia x-canneg) @ x-canneg*/
            (accum sub-total by t-matg.codcia x-imppos) @ x-imppos
            (accum sub-total by t-matg.codcia x-impneg) @ x-impneg
            with stream-io no-box width 320.
    end.
end.    
output close.
