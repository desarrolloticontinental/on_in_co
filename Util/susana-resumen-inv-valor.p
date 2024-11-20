def var x-desde as date.
def var x-hasta as date.
def var s-codcia as int init 001.
def temp-table detalle
    field codalm as char format 'x(3)'
    field items  as int
    field valor  as dec format '->>>,>>>,>>9.99'.  

assign
    x-desde = 04/15/2007
    x-hasta = 05/25/2007.
    
for each invconfig no-lock where InvConfig.CodCia = s-codcia
        and InvConfig.FchInv >= x-desde and InvConfig.FchInv <= x-hasta:
    find detalle where detalle.codalm = invconfig.codalm
        exclusive-lock no-error.
     if not available detalle then create detalle.
     assign
        detalle.codalm = invconfig.codalm.
    FOR EACH InvConteo NO-LOCK WHERE InvConteo.CodCia = S-CODCIA
            AND InvConteo.CodAlm = invconfig.codalm
            AND InvConteo.FchInv = invconfig.fchinv:
        assign
            detalle.items = detalle.items + 1.
        find last almstkge where almstkge.codcia = s-codcia
            and almstkge.codmat = invconteo.codmat
            and almstkge.fecha <= InvConfig.FchInv
            no-lock no-error.
        if available almstkge
        then detalle.valor = detalle.valor + (InvConteo.CanInv * AlmStkge.CtoUni).            
    END.            
end.

output to c:\tmp\res-inv-valo.txt.
for each detalle.
    display
        detalle.codalm
        detalle.items
        detalle.valor.
end.
output close.
