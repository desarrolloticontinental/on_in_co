output to D:\sie\barras19.txt.
def var x-linea as char format 'x(109)'.
for each almmmatg where codcia = 1 and codbrr <> '' no-lock:
    x-Linea = string(codmat,'x(7)') +
            string(codbrr,'x(14)') +
            string(undbas,'x(8)') +
            string(unda,'x(8)') +
            string(undb,'x(6)') +
            string(undc,'x(4)') +
            string(desmat,'x(50)') +
            string(desmar,'x(12)').
    display x-linea
            with stream-io no-box no-labels width 110.
end.
output close.
