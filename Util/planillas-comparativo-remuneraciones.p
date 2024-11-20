def temp-table t-pers like pl-pers
    field cargo like pl-flg-mes.cargo
    field seccion like pl-flg-mes.seccion
    field ene2005 as dec
    field feb2005 as dec
    field mar2005 as dec
    field ene2006 as dec
    field feb2006 as dec
    field mar2006 as dec.
    
for each pl-pers no-lock where pl-pers.codcia = 001,
        each pl-mov-mes no-lock where pl-mov-mes.codcia = 001
            and pl-mov-mes.codpln = 01
            and pl-mov-mes.codper = pl-pers.codper
            and periodo = 2005
            and nromes >= 01
            and nromes <= 03
            and codcal = 001,
        first pl-bole no-lock where pl-bole.codcal = pl-mov-mes.codcal
            and pl-bole.codpln = pl-mov-mes.codpln
            and pl-bole.codmov = pl-mov-mes.codmov
            and pl-bole.tpobol = 'Remuneraciones',
        first pl-flg-mes of pl-mov-mes no-lock:
    find t-pers of pl-pers exclusive-lock no-error.
    if not available t-pers then do:
        create t-pers.
        buffer-copy pl-pers to t-pers.
    end.
    assign
        t-pers.cargo = pl-flg-mes.cargo
        t-pers.seccion = pl-flg-mes.seccion.
    case pl-mov-mes.nromes:
        when 1 then t-pers.ene2005 = t-pers.ene2005 + pl-mov-mes.valcal-mes.
        when 2 then t-pers.feb2005 = t-pers.feb2005 + pl-mov-mes.valcal-mes.
        when 3 then t-pers.mar2005 = t-pers.mar2005 + pl-mov-mes.valcal-mes.
    end case.
end.
for each pl-pers no-lock where pl-pers.codcia = 001,
        each pl-mov-mes no-lock where pl-mov-mes.codcia = 001
            and pl-mov-mes.codpln = 01
            and pl-mov-mes.codper = pl-pers.codper
            and periodo = 2006
            and nromes >= 01
            and nromes <= 03
            and codcal = 001,
        first pl-bole no-lock where pl-bole.codcal = pl-mov-mes.codcal
            and pl-bole.codpln = pl-mov-mes.codpln
            and pl-bole.codmov = pl-mov-mes.codmov
            and pl-bole.tpobol = 'Remuneraciones',
        first pl-flg-mes of pl-mov-mes no-lock:
    find t-pers of pl-pers exclusive-lock no-error.
    if not available t-pers then do:
        create t-pers.
        buffer-copy pl-pers to t-pers.
    end.
    assign
        t-pers.cargo = pl-flg-mes.cargo
        t-pers.seccion = pl-flg-mes.seccion.
    case pl-mov-mes.nromes:
        when 1 then t-pers.ene2006 = t-pers.ene2006 + pl-mov-mes.valcal-mes.
        when 2 then t-pers.feb2006 = t-pers.feb2006 + pl-mov-mes.valcal-mes.
        when 3 then t-pers.mar2006 = t-pers.mar2006 + pl-mov-mes.valcal-mes.
    end case.
end.

output to c:\tmp\comparativo.txt.
for each t-pers:
    display 
        t-pers.codper 
        t-pers.patper
        t-pers.matper
        t-pers.nomper
        t-pers.cargo    format 'x(25)' view-as text
        t-pers.seccion  format 'x(30)' view-as text
        ene2005 
        feb2005 
        mar2005
        ene2006
        feb2006
        mar2006
        with stream-io no-box width 320.
end.
output close.
