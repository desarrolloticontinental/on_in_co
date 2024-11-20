

def temp-table detalle
    field codcia like gn-prov.codcia
    field codpro like gn-prov.codpro
    field nompro like gn-prov.nompro
    field venta as dec
    field costo as dec
    field margen as dec.
    

for each evtprov no-lock where codcia = 001
    and coddiv = '00012'
    and nrofch >= 200604
    and nrofch <= 200607,
    first gn-prov where gn-prov.codcia = 000
        and gn-prov.codpro = evtprov.codpro no-lock:
    find detalle of gn-prov exclusive-lock no-error.
    if not available detalle then create detalle.
    buffer-copy gn-prov to detalle
        assign
            detalle.venta = detalle.venta + EvtProv.VtaxMesMn 
            detalle.costo = detalle.costo + EvtProv.CtoxMesMn.
end.

for each detalle:
    detalle.margen = (detalle.venta - detalle.costo) / detalle.costo * 100.
end.

output to c:\tmp\provee-abr-jul.txt.
for each detalle by detalle.margen desc:
    display
        detalle.codpro
        detalle.nompro
        detalle.venta
        detalle.costo
        detalle.margen
        with stream-io no-box width 320.
end.
output close.
