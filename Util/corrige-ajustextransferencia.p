def temp-table t-cmov like almcmov
    index llave01 as primary codcia codalm tipmov codmov nroser nrodoc.
def temp-table t-dmov like almdmov.
def temp-table t-matg
    field codmat as char format 'x(6)'
    index llave01 as primary codmat.

for each almacen where codcia = 001 no-lock:
    for each almcmov where codcia = 001
            and codalm = almacen.codalm 
            and fchdoc >= 01/01/2005
            and tipmov = 'i'
            and codmov = 66
            and usuario = 'OCT102005'
            no-lock:
        create t-cmov.
        buffer-copy almcmov to t-cmov
            assign 
                t-cmov.tipmov = 'S'
                t-cmov.codmov = 67
                t-cmov.usuario = 'OCT132005'.
        for each almdmov of almcmov where 
                lookup(trim(codmat), '005206,004443') = 0 no-lock:
            create t-dmov.
            buffer-copy almdmov to t-dmov
                assign 
                    t-dmov.tipmov = 'S'
                    t-dmov.codmov = 67.
        end.                    
    end.            
end.

for each t-cmov:
    find almcmov of t-cmov no-lock no-error.
    if not available almcmov then do:
        create almcmov.
        buffer-copy t-cmov to almcmov.
    end.
end.
for each t-dmov:
    create almdmov.
    buffer-copy t-dmov to almdmov.                        
    create t-matg.
    buffer-copy t-dmov to t-matg.
end.

output to c:\tmp\materiales.txt.
for each t-matg:
    display t-matg.codmat with stream-io no-labels no-box.
end.
output close.
