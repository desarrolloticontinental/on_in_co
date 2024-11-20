def var x-linea as char format 'x(50)'.
def var x-codmat as char.
def var x-stkalm as dec.
def var x-stkact as dec.
def var x-s66 as dec.
def var x-s67 as dec.
def var x-i66 as dec.
def var x-i67 as dec.
def var x-i01 as dec.
def var x-s01 as dec.
def var x-fecha as date.
def stream reporte.

output stream reporte to c:\tmp\diferencias.txt.
x-fecha = date(04,28,2005).
input from c:\tmp\existencia.prn.
repeat:
    import unformatted x-linea.
    assign
        x-codmat = substring(x-linea,1,6)
        x-stkact = decimal(substring(x-linea,7,13)).
    assign
        x-stkalm = 0
        x-s66 = 0
        x-s67 = 0
        x-i66 = 0
        x-i67 = 0
        x-i01 = 0
        x-s01 = 0.
    find last almstkal where codcia = 001
        and codalm = '16'
        and codmat = x-codmat
        and fecha <= x-fecha no-lock no-error.
    if available almstkal then x-stkalm = almstkal.stkact.
    for each almdmov no-lock where almdmov.codcia = 001
        and almdmov.codalm = '16'
        and almdmov.codmat = x-codmat
        and almdmov.fchdoc >= 01/01/2004
        and almdmov.fchdoc <= x-fecha
        and (almdmov.codmov = 66 or almdmov.codmov = 67 or almdmov.codmov = 01):
        if (almdmov.codmov = 66 or almdmov.codmov = 67)
        then do:
            case almdmov.tipmov:
                when 'i' then do:
                    if almdmov.codmov = 66
                    then x-i66 = x-i66 + almdmov.candes.
                    else x-i67 = x-i67 + almdmov.candes.
                end.                
                when 's' then do:
                    if almdmov.codmov = 66
                    then x-s66 = x-s66 + almdmov.candes.
                    else x-s67 = x-s67 + almdmov.candes.
                end.                
            end case.
        end.
        if (almdmov.codmov = 01)
        then do:
            case almdmov.tipmov:
                when 'i' then x-i01 = x-i01 + almdmov.candes.
                when 's' then x-s01 = x-s01 + almdmov.candes.
            end case.
        end.
    end.
    if x-stkact <> x-stkalm then do:
        display stream reporte
            x-codmat
            x-stkact column-label 'Tienda'
            x-stkalm column-label 'Ate'
            x-s66 
            x-i66
            x-s67
            x-i67
            x-s01
            x-i01
            with stream-io width 200.
    end.
end.
input close.
output stream reporte close.
