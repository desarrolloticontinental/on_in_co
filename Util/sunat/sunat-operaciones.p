def var x-items as int.
def var x-total as dec.

/* Ingresos por Ordenes de compra 
for each almacen no-lock where almacen.codcia = 001:
    for each almdmov no-lock where codcia = almacen.codcia
        and almdmov.codalm = almacen.codalm
        and almdmov.tipmov = 'i'
        and almdmov.codmov = 02
        and almdmov.fchdoc >= 01/01/2003
        and almdmov.fchdoc <= 12/31/2003:
        assign
            x-items = x-items + 1
            x-total = x-total + almdmov.impmn1.
    end.        
end.
message x-items skip x-total.
*/
/* Salidas por Transferencias 
for each almacen no-lock where almacen.codcia = 001:
    for each almdmov no-lock where codcia = almacen.codcia
        and almdmov.codalm = almacen.codalm
        and almdmov.tipmov = 's'
        and almdmov.codmov = 03
        and almdmov.fchdoc >= 01/01/2003
        and almdmov.fchdoc <= 12/31/2003:
        assign
            x-items = x-items + 1.
        find last almstkge where almstkge.codcia = almdmov.codcia
            and almstkge.codmat = almdmov.codmat
            and almstkge.fecha <= almdmov.fchdoc
            no-lock no-error.
        if available almstkge
        then x-total = x-total + (AlmStkge.CtoUni * almdmov.candes * almdmov.factor).
    end.        
end.
message x-items skip x-total.
*/
/* Devoluciones a proveedores 
for each almacen no-lock where almacen.codcia = 001:
    for each almdmov no-lock where codcia = almacen.codcia
        and almdmov.codalm = almacen.codalm
        and almdmov.tipmov = 's'
        and almdmov.codmov = 09
        and almdmov.fchdoc >= 01/01/2003
        and almdmov.fchdoc <= 12/31/2003:
        assign
            x-items = x-items + 1.
        find last almstkge where almstkge.codcia = almdmov.codcia
            and almstkge.codmat = almdmov.codmat
            and almstkge.fecha <= almdmov.fchdoc
            no-lock no-error.
        if available almstkge
        then x-total = x-total + (AlmStkge.CtoUni * almdmov.candes * almdmov.factor).
    end.        
end.
message x-items skip x-total.
*/
/* Devoluciones Consignación a proveedores */
for each almacen no-lock where almacen.codcia = 001:
    for each almdmov no-lock where codcia = almacen.codcia
        and almdmov.codalm = almacen.codalm
        and almdmov.tipmov = 's'
        and almdmov.codmov = 26
        and almdmov.fchdoc >= 01/01/2003
        and almdmov.fchdoc <= 12/31/2003:
        assign
            x-items = x-items + 1.
        find last almstkge where almstkge.codcia = almdmov.codcia
            and almstkge.codmat = almdmov.codmat
            and almstkge.fecha <= almdmov.fchdoc
            no-lock no-error.
        if available almstkge
        then x-total = x-total + (AlmStkge.CtoUni * almdmov.candes * almdmov.factor).
    end.        
end.
message x-items skip x-total.

/* Salidas por Ventas 
for each almacen no-lock where almacen.codcia = 001:
    for each almdmov no-lock where codcia = almacen.codcia
        and almdmov.codalm = almacen.codalm
        and almdmov.tipmov = 's'
        and almdmov.codmov = 02
        and almdmov.fchdoc >= 01/01/2003
        and almdmov.fchdoc <= 12/31/2003:
        assign
            x-items = x-items + 1.
        find last almstkge where almstkge.codcia = almdmov.codcia
            and almstkge.codmat = almdmov.codmat
            and almstkge.fecha <= almdmov.fchdoc
            no-lock no-error.
        if available almstkge
        then x-total = x-total + (AlmStkge.CtoUni * almdmov.candes * almdmov.factor).
    end.        
end.
message x-items skip x-total.
*/
/* Devoluciónde Clientes 
for each almacen no-lock where almacen.codcia = 001:
    for each almdmov no-lock where codcia = almacen.codcia
        and almdmov.codalm = almacen.codalm
        and almdmov.tipmov = 'i'
        and almdmov.codmov = 09
        and almdmov.fchdoc >= 01/01/2003
        and almdmov.fchdoc <= 12/31/2003:
        assign
            x-items = x-items + 1.
        find last almstkge where almstkge.codcia = almdmov.codcia
            and almstkge.codmat = almdmov.codmat
            and almstkge.fecha <= almdmov.fchdoc
            no-lock no-error.
        if available almstkge
        then x-total = x-total + (AlmStkge.CtoUni * almdmov.candes * almdmov.factor).
    end.        
end.
message x-items skip x-total.
*/
