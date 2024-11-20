output to d:\sie\derecho.txt.
for each pl-dhabien
    where codcia = 1 and
    periodo = 2008 and
    nromes = 2 no-lock:
    display
        codper
        patper
        matper
        nomper
        with stream-io width 223.
end.
output close.
        
