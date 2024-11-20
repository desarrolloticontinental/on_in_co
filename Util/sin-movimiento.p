def var x-fchdoc as date no-undo.

output to c:\tmp\sin-movimiento.txt.
for each almmmatg no-lock where codcia = 001:
    if almmmatg.codfam = '008' or almmmatg.codfam = '009' then next.
    find last almstkge where almstkge.codcia = 001
        and almstkge.codmat = almmmatg.codmat
        and almstkge.fecha <= 04/30/2007
        no-lock no-error.
    if not available almstkge or almstkge.stkact = 0 then next.
    find last almdmov use-index almd02 where almdmov.codcia = 001
        and almdmov.codmat = almmmatg.codmat
        and almdmov.tipmov = 'i'
        no-lock no-error.
    if available almdmov and almdmov.fchdoc >= 04/30/2006 then next.
    find last almdmov use-index almd02 where almdmov.codcia = 001
        and almdmov.codmat = almmmatg.codmat
        and almdmov.tipmov = 's'
        and almdmov.codmov = 02
        and almdmov.fchdoc >= 04/30/2006
        and almdmov.fchdoc <= 04/30/2007
        no-lock no-error.
    if not available almdmov 
    then do:
        find last almdmov use-index almd02 where almdmov.codcia = 001
            and almdmov.codmat = almmmatg.codmat
            and almdmov.tipmov = 's'
            and almdmov.codmov = 02
            no-lock no-error.
        x-fchdoc = if available almdmov then almdmov.fchdoc else ?.
        display
            almmmatg.codfam
            x-fchdoc
            almmmatg.codmat
            almmmatg.desmat
            almmmatg.desmar
            almmmatg.undbas
            almstkge.stkact
            almstkge.ctouni
            with stream-io no-box width 200.
    end.
end.
output close.
