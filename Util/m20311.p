def temp-table dmov like almdmov
    field cantidad as dec extent 7.


for each almdmov where almdmov.codcia = 001
    and almdmov.codmat = '014625'
    and almdmov.fchdoc >= 10/01/2002
    and almdmov.fchdoc <= 12/31/2003
    no-lock:
    create dmov.
    buffer-copy almdmov to dmov.
    case almdmov.codalm:
        when '11' then dmov.cantidad[1] = (almdmov.candes * almdmov.factor).
        when '12' then dmov.cantidad[2] = (almdmov.candes * almdmov.factor).
        when '139' then dmov.cantidad[3] = (almdmov.candes * almdmov.factor).
        when '143' then dmov.cantidad[4] = (almdmov.candes * almdmov.factor).
        when '81' then dmov.cantidad[5] = (almdmov.candes * almdmov.factor).
        when '95' then dmov.cantidad[6] = (almdmov.candes * almdmov.factor).
        when '135' then dmov.cantidad[7] = (almdmov.candes * almdmov.factor).
    end case.
end.

output to c:\tmp\m14625.txt.
for each dmov ,
    first almmmatg of dmov no-lock
    by dmov.tipmov by dmov.codmov by dmov.fchdoc:
    display
        dmov.fchdoc
        dmov.tipmov
        dmov.codmov
        dmov.nrodoc
        almmmatg.undbas
        dmov.cantidad[1] column-label '11'
        dmov.cantidad[2] column-label '12'
        dmov.cantidad[3] column-label '139'
        dmov.cantidad[4] column-label '143'
        dmov.cantidad[5] column-label '81'
        dmov.cantidad[6] column-label '95'
        dmov.cantidad[7] column-label '135'
        with stream-io no-box width 200.
end.
output close.
