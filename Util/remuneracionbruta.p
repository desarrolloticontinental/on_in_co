def temp-table t-mes like pl-mov-mes
    index llave01 as primary codcia codper periodo nromes.

for each pl-mov-mes no-lock where codcia = 001
        and PL-MOV-MES.codcal = 001
        and pl-mov-mes.codmov = 401
        and pl-mov-mes.periodo >= 2000,
        first pl-flg-mes of pl-mov-mes no-lock where 
            (pl-flg-mes.seccion = 'FABRICA' OR pl-flg-mes.cargo begins 'vendedor'):
    if pl-mov-mes.periodo = 2005 and pl-mov-mes.nromes > 04 then next.
    create t-mes.
    buffer-copy pl-mov-mes to t-mes.
end.
for each pl-mov-mes no-lock where codcia = 001
        and PL-MOV-MES.codcal = 004
        and pl-mov-mes.codmov = 401
        and pl-mov-mes.periodo >= 2000,
        first pl-flg-mes of pl-mov-mes no-lock where
            (pl-flg-mes.seccion = 'FABRICA' OR pl-flg-mes.cargo begins 'vendedor'):
    if pl-mov-mes.periodo = 2005 and pl-mov-mes.nromes > 04 then next.
    find t-mes where t-mes.codcia = pl-mov-mes.codcia
        and t-mes.codper = pl-mov-mes.codper
        and t-mes.periodo = pl-mov-mes.periodo
        and t-mes.nromes = pl-mov-mes.nromes
        exclusive-lock no-error.
    if not available t-mes then create t-mes.
    buffer-copy pl-mov-mes to t-mes
        assign
             t-mes.valcal-mes = t-mes.valcal-mes + pl-mov-mes.valcal-mes.
end.

output to c:\tmp\rembruta.txt.
for each t-mes, first pl-pers of t-mes:
    display t-mes.codper pl-pers.matper pl-pers.patper pl-pers.nomper
        t-mes.periodo t-mes.nromes t-mes.valcal-mes
        with stream-io no-labels width 200.
end.
output close.
