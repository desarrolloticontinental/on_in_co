/* Ventas acumuladas por division */
def var x-nroitm as int no-undo.


def temp-table detalle like almmmatg
    field candes as dec
    field preuni as dec
    field ventamn as dec
    field costomn as dec
    field margen as dec
    field nompro as char format 'x(40)'.


for each evtarti no-lock where codcia = 001
    and coddiv = '00012'
    and nrofch >= 200604
    and nrofch <= 200607,
    first almmmatg of evtarti no-lock:
    find detalle of almmmatg exclusive-lock no-error.
    if not available detalle then do:
        create detalle.
    end.
    buffer-copy almmmatg to detalle
        assign
            detalle.candes = detalle.candes + EvtArti.CanxMes
            detalle.ventamn = detalle.ventamn + EvtArti.VtaxMesMn
            detalle.costomn = detalle.costomn + EvtArti.CtoxMesMn.
    find gn-prov where gn-prov.codcia = 000
        and gn-prov.codpro = almmmatg.codpr1
        no-lock no-error.
    if available gn-prov then detalle.nompro = gn-prov.nompro.
end.

for each detalle:
    detalle.margen = (detalle.ventamn - detalle.costomn) / detalle.costomn * 100.
end.       

output to c:\tmp\venta16a.txt.
for each detalle break by detalle.codcia by detalle.codfam by detalle.subfam by detalle.margen desce:
    if first-of(detalle.subfam) then x-nroitm = 0.
    x-nroitm = x-nroitm + 1.
    display
    x-nroitm
    detalle.codfam
    detalle.subfam
    detalle.codmat
    detalle.desmat
    detalle.desmar
    detalle.undbas
    detalle.candes
    detalle.ventamn
    detalle.costomn
    detalle.margen
    detalle.nompro
    with stream-io no-box no-labels width 320 1 down.
    accumulate detalle.ventamn (sub-total by detalle.codfam by detalle.subfam).
    accumulate detalle.ventamn total.    
    accumulate detalle.costomn (sub-total by detalle.codfam by detalle.subfam).
    accumulate detalle.costomn total.    
    if last-of(detalle.subfam) then do:
        underline detalle.ventamn detalle.costomn.
        display
            accum sub-total by detalle.subfam detalle.ventamn @ detalle.ventamn
            accum sub-total by detalle.subfam detalle.costomn @ detalle.costomn.
        down 1.
    end.
    if last-of(detalle.codfam) then do:
        underline detalle.ventamn detalle.costomn.
        display
            accum sub-total by detalle.codfam detalle.ventamn @ detalle.ventamn
            accum sub-total by detalle.codfam detalle.costomn @ detalle.costomn.
        down 1.
    end.
    if last-of(detalle.codcia) then do:
        underline detalle.ventamn detalle.costomn.
        display
            accum total detalle.ventamn @ detalle.ventamn
            accum total detalle.costomn @ detalle.costomn.
        down 1.
    end.
end.
output close.
