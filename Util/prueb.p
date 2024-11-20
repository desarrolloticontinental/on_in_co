def temp-table detalle like cb-dmov.

for each cb-dmov no-lock where codcia = 001
    and codope = '062'
    and periodo = 2006
    and nromes >= 06 and nromes <= 08
    and lookup(trim(codcta), '121201,121202') > 0
    and lookup(trim(coddoc), '01,03,07') > 0
    and clfaux = '@cl':
    create detalle.
    buffer-copy cb-dmov to detalle.
end.    

def var x-imptot as dec.
for each detalle,
    first gn-clie where gn-clie.codcia = 000
        and gn-clie.codcli = detalle.codaux 
        break by detalle.codaux:
    if first-of(detalle.codaux) then x-imptot = 0.
    if detalle.tpomov = No
    then x-imptot = x-imptot + detalle.impmn1.
    else x-imptot = x-imptot - detalle.impmn2.
    if last-of(detalle.codaux) then do:
        display 
            detalle.codaux
            gn-clie.nomcli
            detalle.codope
            x-imptot
            with stream-io no-box.
    end.
end.
