def temp-table t-matg like almmmatg.
def var x-linea as char format 'x(10)'.

input from c:\tmp\materiales.txt.
repeat:
    import unformatted x-linea.
    create t-matg.
    assign
        t-matg.codcia = 001
        t-matg.codmat = trim(x-linea).
end.
input close.

for each t-matg where t-matg.codcia = 001 no-lock:
    display t-matg.codmat.
    pause 0.
    for each almdmov no-lock where codcia = 001 and almdmov.codmat = t-matg.codmat:
        find first almcmov of almdmov no-lock no-error.
        if not available almcmov
        then do:
            create almcmov.
            buffer-copy almdmov to almcmov
                assign 
                    almcmov.almdes = almdmov.almori
                    almcmov.usuario = 'ADMIN13052005'.
        end.
    end.
end.
