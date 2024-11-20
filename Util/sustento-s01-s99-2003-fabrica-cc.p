def var x-s01 as dec.
def var x-s99 as dec.
def var x-i99 as dec.
def var x-i01 as dec.
def var x-canpos as dec.
def var x-canneg as dec.
def var x-totpos as dec.
def var x-totneg as dec.
def var x-costo  as dec.

def var i as int no-undo.
def var j as int no-undo.
def var x-tipmov as char init 'i,s'.
def var x-codmov as char init '01,99'.

def temp-table detalle like almmmatg
    field codalm like almdmov.codalm
    field nrodoc like almdmov.nrodoc
    field fchdoc like almdmov.fchdoc
    field costo as dec
    field s01 as dec
    field s99 as dec
    field i99 as dec
    field i01 as dec
    field canpos as dec
    field canneg as dec
    field totpos as dec
    field totneg as dec
    index llave01 is primary codalm codfam subfam desmat.
for each almacen where almacen.codcia = 001 
        and lookup(trim(codalm), '11,12') > 0
        no-lock:
    do i = 1 to 2:
        do j = 1 to 2:
            for each almdmov no-lock where codcia = 001
                and codalm = almacen.codalm
                and fchdoc >= 01/01/2003
                and fchdoc <= 12/31/2003
                and tipmov = entry(i,x-tipmov)
                and codmov = integer(entry(j,x-codmov)),
                first almmmatg of almdmov no-lock:
                assign
                    x-s01 = 0
                    x-s99 = 0
                    x-i01 = 0
                    x-i99 = 0
                    x-canpos = 0
                    x-canneg = 0
                    x-totneg = 0
                    x-totpos = 0
                    x-costo = 0.
                find last almstkge where almstkge.codcia = 001
                    and almstkge.codmat = almdmov.codmat
                    and almstkge.fecha <= almdmov.fchdoc 
                    no-lock no-error.
                if available almstkge then x-costo = AlmStkge.CtoUni.
                case tipmov:
                    when 's' then do:
                        x-canneg = (candes * factor).
                        x-totneg = (candes * factor) * x-costo.
                        if codmov = 01 then x-s01 = candes.
                        if codmov = 99 then x-s99 = candes.
                    end.
                    when 'i' then do:
                        x-canpos = (candes * factor).
                        x-totpos = (candes * factor) * x-costo.
                        if codmov = 01 then x-i01 = candes.
                        if codmov = 99 then x-i99 = candes.
                    end.
                end case.
                create detalle.
                buffer-copy almmmatg to detalle
                    assign
                        detalle.codalm = almdmov.codalm
                        detalle.nrodoc = almdmov.nrodoc
                        detalle.fchdoc = almdmov.fchdoc
                        detalle.costo = x-costo
                        detalle.i01 = x-i01 
                        detalle.s01 = x-s01
                        detalle.i99 = x-i99
                        detalle.s99 = x-s99
                        detalle.canpos = x-canpos
                        detalle.canneg = x-canneg
                        detalle.totpos = x-totpos
                        detalle.totneg = x-totneg.
            end.
        end.            
    end.
end.

output to c:\tmp\sunat-s01-s99-2003-fabrica.txt.
for each detalle break by detalle.codcia 
        by detalle.codalm 
        by detalle.codfam 
        by detalle.subfam 
        by detalle.desmat:
    accumulate detalle.i01 (sub-total by detalle.subfam).
    accumulate detalle.i99 (sub-total by detalle.subfam).
    accumulate detalle.s01 (sub-total by detalle.subfam).
    accumulate detalle.s99 (sub-total by detalle.subfam).
    accumulate detalle.canpos (sub-total by detalle.subfam).
    accumulate detalle.canneg (sub-total by detalle.subfam).
    accumulate detalle.totpos (sub-total by detalle.subfam).
    accumulate detalle.totneg (sub-total by detalle.subfam).
    display
        detalle.codalm
        detalle.nrodoc
        detalle.fchdoc
        detalle.codfam
        detalle.subfam
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.costo
        detalle.i01
        detalle.s01
        detalle.i99
        detalle.s99
        detalle.canpos
        detalle.canneg
        detalle.totpos
        detalle.totneg
        detalle.catconta[1]
        with stream-io no-box no-labels width 320.
    if last-of(detalle.subfam) then do:
        underline 
            detalle.i01
            detalle.s01
            detalle.i99
            detalle.s99
            detalle.canpos
            detalle.canneg
            detalle.totpos
            detalle.totneg.
        display
            accum sub-total by detalle.subfam detalle.i01 @ detalle.i01
            accum sub-total by detalle.subfam detalle.s01 @ detalle.s01
            accum sub-total by detalle.subfam detalle.i99 @ detalle.i99
            accum sub-total by detalle.subfam detalle.s99 @ detalle.s99
            accum sub-total by detalle.subfam detalle.canpos @ detalle.canpos
            accum sub-total by detalle.subfam detalle.canneg @ detalle.canneg
            accum sub-total by detalle.subfam detalle.totpos @ detalle.totpos
            accum sub-total by detalle.subfam detalle.totneg @ detalle.totneg.
    end.
end.
output close.
