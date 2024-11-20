def temp-table detalle
    field codpro like gn-prov.codpro
    field nompro like gn-prov.nompro
    field importe as dec extent 12
    index llave01 is primary nompro.

for each almcmov no-lock where codcia = 001 and tipmov = 'i'    
    and (codmov = 02 or codmov = 26 or codmov = 17)
    and fchdoc >= 01/01/2005 and fchdoc <= 06/30/2005,
    first gn-prov no-lock where gn-prov.codcia = 000
        and gn-prov.codpro = almcmov.codpro,
    each almdmov of almcmov no-lock:
    find detalle where detalle.codpro = gn-prov.codpro
        exclusive-lock no-error.
    if not available detalle
    then create detalle.
    assign  
        detalle.codpro = gn-prov.codpro
        detalle.nompro = gn-prov.nompro
        detalle.importe[month(almdmov.fchdoc)] = detalle.importe[month(almdmov.fchdoc)] +
                                                ( if almdmov.codmon = 2 then almdmov.impcto
                                                else almdmov.impcto / almdmov.tpocmb ).
end.

output to c:\tmp\comprasxprovee.txt.                                                
for each detalle:
    display
        detalle.nompro column-label 'Proveedor'
        detalle.importe[1]  format '->>,>>>,>>>.99' column-label 'Enero'
        detalle.importe[2]  format '->>,>>>,>>>.99' column-label 'Febrero'
        detalle.importe[3]  format '->>,>>>,>>>.99' column-label 'Marzo'
        detalle.importe[4]  format '->>,>>>,>>>.99' column-label 'Abril'
        detalle.importe[5]  format '->>,>>>,>>>.99' column-label 'Mayo'
        detalle.importe[6]  format '->>,>>>,>>>.99' column-label 'Junio'
        detalle.importe[7]  format '->>,>>>,>>>.99' column-label 'Julio'
        detalle.importe[8]  format '->>,>>>,>>>.99' column-label 'Agosto'
        detalle.importe[9]  format '->>,>>>,>>>.99' column-label 'Setiembre'
        detalle.importe[10]  format '->>,>>>,>>>.99' column-label 'Octubre'
        detalle.importe[11]  format '->>,>>>,>>>.99' column-label 'Noviembre'
        detalle.importe[12]  format '->>,>>>,>>>.99' column-label 'Diciembre'
        with stream-io width 320.
end.
output close.
