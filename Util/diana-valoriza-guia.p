def var x-linea as char format 'x(100)'.
def var x-codalm as char.

def temp-table detalle
    field nroser as char
    field nrodoc as char
    field ordser as char
    field valor  as dec.

input from c:\tmp\guias.prn.
repeat:
    import unformatted x-linea.
    create detalle.
    assign
        nroser = substring(x-linea,1,3)
        nrodoc = substring(x-linea,11,6)
        ordser = substring(x-linea,21,6).
end.
input close.
    
for each detalle:
    case nroser:
        when '014' then x-codalm = '11'.
        when '073' then x-codalm = '19'.
        when '057' then x-codalm = '04'.
        otherwise next.
    end case.
    find almcmov where almcmov.codcia = 001
        and almcmov.codalm = x-codalm
        and almcmov.tipmov = 's'
        and almcmov.codmov = 03
        and almcmov.nroser = integer(detalle.nroser)
        and almcmov.nrodoc = integer(detalle.nrodoc)
        no-lock no-error.
    if available almcmov then do:
        for each almdmov of almcmov no-lock:
            detalle.valor = detalle.valor + candes * vctomn1.
        end.
    end.
end.

output to c:\tmp\diana.txt.
for each detalle:
    display detalle.nroser detalle.nrodoc detalle.ordser detalle.valor.
end.
output close.
