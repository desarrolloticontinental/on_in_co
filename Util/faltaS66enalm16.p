disable triggers for load of almcmov.
disable triggers for load of almdmov.

def var x-linea as char format 'x(50)'.
def buffer c-mov for almcmov.
def buffer d-mov for almdmov.

input from c:\tmp\faltas66.prn.
repeat:
    import unformatted x-linea.
    find last almdmov where almdmov.codcia = 001
        and almdmov.codalm = '16'
        and almdmov.tipmov = 'I'
        and almdmov.codmov = 66
        and almdmov.codmat = substring(x-linea,1,6)
        and almdmov.fchdoc <= 03/31/2005
        no-lock no-error.
    if available almdmov
        and almdmov.candes = decimal(substring(x-linea,7,13))
    then do:
        find almcmov of almdmov no-lock no-error.
        if available almcmov
        then do:
            create c-mov.
            buffer-copy almcmov to c-mov
                assign c-mov.tipmov = 'S'
                        c-mov.usuario = '20.07.2005'
                        no-error.
        end.
        create d-mov.
        buffer-copy almdmov to d-mov
            assign d-mov.tipmov = 'S'.
    end.
end.
input close.
