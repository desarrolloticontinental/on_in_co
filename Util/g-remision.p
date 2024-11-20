output to c:\tmp\g-remision.txt.
for each faccorre no-lock where codcia = 001 and coddoc = 'g/r',
    first gn-divi of faccorre no-lock,
    first almacen no-lock where almacen.codcia = 001
        and almacen.codalm = faccorre.codalm :
    display
        nroser
        faccorre.coddiv
        desdiv
        faccorre.codalm
        Almacen.Descripcion
        with stream-io no-box width 200.
end.
output close.
