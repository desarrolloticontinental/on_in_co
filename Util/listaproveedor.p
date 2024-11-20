output to c:\tmp\listaproveedor.txt.
for each lg-dmatpr no-lock,
    first almmmatg of lg-dmatpr no-lock, 
    first lg-cmatpr of lg-dmatpr where lg-cmatpr.codcia = 001
        and lg-cmatpr.flgest = 'a' no-lock,
    first gn-prov where gn-prov.codcia = 000
        and gn-prov.codpro = lg-cmatpr.codpro no-lock:
    display lg-cmatpr.codpro gn-prov.nompro lg-dmatpr.codmat
        almmmatg.desmat almmmatg.desmar
        almmmatg.undcmp lg-dmatpr.codmon
        lg-dmatpr.preact lg-dmatpr.igvmat
        lg-dmatpr.dsctos[1] lg-dmatpr.dsctos[2] lg-dmatpr.dsctos[3]
        lg-dmatpr.precos
        with stream-io width 250.
end.
output close.
