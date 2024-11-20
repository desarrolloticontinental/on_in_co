def var x-codmat as char.
def temp-table detalle like almmmatg
    field tipmov as char extent 2
    field codmov as inte extent 2
    field nrodoc as inte extent 2
    field fchdoc as date extent 2
    field almdes as char extent 2.

input from c:\tmp\cod85.txt.
repeat:
    import unformatted x-codmat.
    create detalle.
    assign
        detalle.codcia = 001
        detalle.codmat = x-codmat.
end.
input close.

output to c:\tmp\movimientos.txt.
for each detalle, first almmmatg of detalle no-lock:
    display detalle.codmat.
    find last almdmov use-index almd07 where almdmov.codcia = 001
        and almdmov.codmat = detalle.codmat
        and almdmov.tipmov = 'i'
        and almdmov.codmov = 02
        no-lock no-error.
    if available almdmov 
    then assign
            detalle.tipmov[1] = almdmov.tipmov
            detalle.codmov[1] = almdmov.codmov
            detalle.nrodoc[1] = almdmov.nrodoc
            detalle.fchdoc[1] = almdmov.fchdoc.
    find last almdmov use-index almd07 where almdmov.codcia = 001
        and almdmov.codmat = detalle.codmat
        and almdmov.tipmov = 'i'
        and almdmov.codmov = 06
        no-lock no-error.
    if available almdmov
    then do:
        if detalle.fchdoc[1] = ? OR detalle.fchdoc[1] < almdmov.fchdoc
        then assign
                detalle.tipmov[1] = almdmov.tipmov
                detalle.codmov[1] = almdmov.codmov
                detalle.nrodoc[1] = almdmov.nrodoc
                detalle.fchdoc[1] = almdmov.fchdoc.
    end.
    find last almdmov use-index almd07 where almdmov.codcia = 001
        and almdmov.codmat = detalle.codmat
        and almdmov.tipmov = 's'
        and almdmov.codmov = 03
        no-lock no-error.
    if available almdmov
    then assign
            detalle.tipmov[2] = almdmov.tipmov
            detalle.codmov[2] = almdmov.codmov
            detalle.nrodoc[2] = almdmov.nrodoc
            detalle.fchdoc[2] = almdmov.fchdoc
            detalle.almdes[2] = almdmov.almori.
    display
        detalle.codmat
        almmmatg.desmat
        almmmatg.desmar
        almmmatg.undbas
        detalle.fchdoc[1]
        detalle.tipmov[1]
        detalle.codmov[1] format '99'       when detalle.codmov[1] <> 00
        detalle.nrodoc[1] format '999999'   when detalle.codmov[1] <> 00
        detalle.fchdoc[2]
        detalle.tipmov[2]
        detalle.codmov[2] format '99'       when detalle.codmov[2] <> 00
        detalle.nrodoc[2] format '999999'   when detalle.codmov[2] <> 00
        detalle.almdes[2]
        with stream-io no-box width 320.
end.
output close.
