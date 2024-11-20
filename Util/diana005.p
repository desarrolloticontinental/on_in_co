output to c:\tmp\diana.txt.
for each almacen no-lock where codcia = 001:
    for each almcmov where codcia = 001
        and codalm = almacen.codalm
        and tipmov = 'i'
        and codmov = 02
        and fchdoc >= 01/01/08,
        first lg-cocmp where lg-cocmp.codcia = 001
            and lg-cocmp.nrodoc = integer(almcmov.nrorf1)
            and cndcmp = '900':
        display
            almcmov.codalm
            almcmov.tipmov
            almcmov.codmov
            almcmov.nrodoc
            lg-cocmp.nrodoc
            lg-cocmp.cndcmp format 'x(3)'
            with stream-io no-box width 200.
    end.        
end.
output close.
