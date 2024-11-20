def var x-codalm as char init '03,03a,04,04a,05,05a,17,83,300'.

def temp-table t-matg like almmmatg
    field codalm like almdmov.codalm
    field nroser like almcmov.nroser
    field nrodoc like almcmov.nrodoc
    field fchdoc like almcmov.fchdoc
    field costo as dec
    field i01 as dec
    field s01 as dec
    field i99 as dec
    field s99 as dec
    index llave01 as primary desmat.
    
def var i as int no-undo.
do i = 1 to num-entries(x-codalm):
    for each almcmov no-lock where codcia = 001
        and codalm = entry(i, x-codalm)
        and fchdoc >= 01/01/2003
        and fchdoc <= 12/31/2003
        and tipmov = 'i'
        and codmov = 01,
        each almdmov of almcmov no-lock,
        first almmmatg of almdmov no-lock:
        create t-matg.
        buffer-copy almmmatg to t-matg
            assign
                t-matg.codalm = almdmov.codalm
                t-matg.nroser = almcmov.nroser
                t-matg.nrodoc = almcmov.nrodoc
                t-matg.fchdoc = almdmov.fchdoc
                t-matg.i01 = almdmov.candes.
    end.
    for each almcmov no-lock where codcia = 001
        and codalm = entry(i, x-codalm)
        and fchdoc >= 01/01/2003
        and fchdoc <= 12/31/2003
        and tipmov = 's'
        and codmov = 01,
        each almdmov of almcmov no-lock,
        first almmmatg of almdmov no-lock:
        create t-matg.
        buffer-copy almmmatg to t-matg
            assign
                t-matg.codalm = almdmov.codalm
                t-matg.nroser = almcmov.nroser
                t-matg.nrodoc = almcmov.nrodoc
                t-matg.fchdoc = almdmov.fchdoc
                s01 = almdmov.candes.
    end.
    for each almcmov no-lock where codcia = 001
        and codalm = entry(i, x-codalm)
        and fchdoc >= 01/01/2003
        and fchdoc <= 12/31/2003
        and tipmov = 'i'
        and codmov = 99,
        each almdmov of almcmov no-lock,
        first almmmatg of almdmov no-lock:
        create t-matg.
        buffer-copy almmmatg to t-matg
            assign
                t-matg.codalm = almdmov.codalm
                t-matg.nroser = almcmov.nroser
                t-matg.nrodoc = almcmov.nrodoc
                t-matg.fchdoc = almdmov.fchdoc
                i99 = almdmov.candes.
    end.
    for each almcmov no-lock where codcia = 001
        and codalm = entry(i, x-codalm)
        and fchdoc >= 01/01/2003
        and fchdoc <= 12/31/2003
        and tipmov = 's'
        and codmov = 99,
        each almdmov of almcmov no-lock,
        first almmmatg of almdmov no-lock:
        create t-matg.
        buffer-copy almmmatg to t-matg
            assign
                t-matg.codalm = almdmov.codalm
                t-matg.nroser = almcmov.nroser
                t-matg.nrodoc = almcmov.nrodoc
                t-matg.fchdoc = almdmov.fchdoc
                s99 = almdmov.candes.
    end.
end.

def var x-canpos as dec format '>>>,>>>,>>9.99'.
def var x-canneg as dec format '>>>,>>>,>>9.99'.
def var x-imppos as dec format '>>>,>>>,>>9.99'.
def var x-impneg as dec format '>>>,>>>,>>9.99'.

output to c:\tmp\sunat-total-sf.txt.
for each t-matg break by t-matg.codcia by t-matg.codalm by t-matg.codfam by t-matg.subfam by t-matg.nrodoc:
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
    display 
        t-matg.codalm
        t-matg.codfam 
        t-matg.subfam
        t-matg.nrodoc
        t-matg.fchdoc 
        t-matg.codmat 
        t-matg.desmat 
        t-matg.desmar 
        t-matg.undbas
        t-matg.costo 
        t-matg.i01 
        t-matg.s01 
        t-matg.i99 
        t-matg.s99
        x-canpos 
        x-canneg 
        x-imppos 
        x-impneg
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
        down 1.
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
        down 1.
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
        down 1.
    end.
end.    
output close.
