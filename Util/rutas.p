output to c:\tmp\rutas.txt.
for each di-rutac no-lock where codcia = 001
    and coddoc = 'h/r'
    and coddiv = '00000'
    and fchdoc >= 01/01/2006,
    each di-rutag of di-rutac no-lock where codalm = '11',
    first almcmov where almcmov.codcia = 001
        and almcmov.codalm = di-rutag.codalm
        and almcmov.tipmov = 's'
        and almcmov.codmov = 03
        and almcmov.nroser = di-rutag.serref
        and almcmov.nrodoc = di-rutag.nroref
        and lookup(trim(almcmov.almdes), '17,18,19,20,04,15,102,103,104,117,121,146,147,149,150,154') > 0:
    display di-rutac.nrodoc almcmov.almdes almcmov.nroser almcmov.nrodoc 
        di-rutac.codveh format 'x(10)' di-rutac.fchdoc
        with stream-io no-box width 200.
end.
output close.
