def buffer cmov for almcmov.
def buffer dmov for almdmov.
def var x-nrodoc as char format 'x(9)'.
def var x-candes as dec.
def var x-nroref as char format 'x(15)'.

output to c:\tmp\salida10desde2006.txt.
for each almcmov no-lock where codcia = 001
    and codalm = '11'
    and tipmov = 's'
    and codmov = 10
    and nroser = 017
    and fchdoc >= 01/01/2006
    and fchdoc <= 03/30/2007,
    each almdmov of almcmov no-lock,
    first almmmatg of almdmov no-lock:
    assign
        x-nrodoc = ''
        x-candes = 0
        x-nroref = ''.
    for each cmov no-lock where cmov.codcia = 001
        and cmov.codalm = almcmov.codalm
        and cmov.tipmov = 'i'
        and cmov.codmov = 10
        and cmov.fchdoc >= 01/01/2003
        and cmov.fchdoc <= 12/31/2004
        and ( cmov.nrorf1 begins string(almcmov.nroser, '999') + string(almcmov.nrodoc, '999999')
            or cmov.nrorf1 begins string(almcmov.nroser, '999') + '-' + string(almcmov.nrodoc, '999999') ):
        find dmov of cmov where dmov.codmat = almdmov.codmat no-lock no-error.
        if available dmov then do:
            assign
                x-nrodoc = string(cmov.nroser, '999') + string(cmov.nrodoc, '999999')
                x-nroref = cmov.nrorf1
                x-candes = dmov.candes.
            leave.
        end.            
    end.        
        
    display almmmatg.codmat almmmatg.desmat almmmatg.desmar
        almmmatg.undbas almdmov.candes almdmov.vctomn1
        almcmov.observ
        almcmov.nroser almcmov.nrodoc
        x-nrodoc x-nroref x-candes
        with stream-io no-box width 320.
end.
output close.
