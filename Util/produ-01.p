output to c:\tmp\cons-mat-prima-oct.txt.
for each almdmov where codcia = 001
    and codalm = '12'
    and tipmov = 's'
    and codmov = 50
    and fchdoc >= 01/01/2006
    and fchdoc <= 11/01/2006 no-lock,
    first almcmov of almdmov no-lock,
    first almmmatg of almdmov no-lock:
    display almdmov.nrodoc almdmov.fchdoc nrorf1 almdmov.codmat desmat desmar codund candes Almdmov.VctoMn1
    with stream-io no-box width 200.
end.
output close.
    
    
