output to c:\almacenes.txt.
for each almacen no-lock where codcia = 001:
    display
        Almacen.CodAlm 
        Almacen.Descripcion
        autmov
        tdoart
        corring
        corrsal
        clave
        flgrep
        with stream-io no-box width 200.
end.
output close.
