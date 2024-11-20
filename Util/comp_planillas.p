def temp-table detalle
    field codper like pl-pers.codper
    field patper like pl-pers.patper
    field matper like pl-pers.matper
    field nomper like pl-pers.nomper
    field seccion like PL-FLG-MES.seccion
    field periodo as int format '9999'
    field valcal as dec format '>>,>>9.99' extent 12
    index llave01 as primary codper periodo.

def var i as int.
def var j as int.

do j = 2004 to 2005:
do i = 1 to 12:
    for each pl-mov-mes where pl-mov-mes.codcia = 001
            and pl-mov-mes.periodo = j
            and pl-mov-mes.nromes = i
            and PL-MOV-MES.codcal = 1
            and PL-MOV-MES.CodMov = 401 no-lock,
            first pl-flg-mes of pl-mov-mes no-lock,
            first pl-pers of pl-mov-mes no-lock:
        find detalle where detalle.codper = pl-mov-mes.codper
            and detalle.periodo = pl-mov-mes.periodo
            exclusive-lock no-error.
        if not available detalle then create detalle.
        assign
            detalle.codper = pl-mov-mes.codper
            detalle.patper = pl-pers.patper
            detalle.matper = pl-pers.matper
            detalle.nomper = pl-pers.nomper
            detalle.periodo = pl-mov-mes.periodo
            detalle.seccion = pl-flg-mes.seccion
            detalle.valcal[i] = pl-mov-mes.valcal-mes.
    end.
end.
end.
output to c:\tmp\comparativo.txt.
for each detalle:
    display
        detalle.seccion
        detalle.codper
        detalle.patper
        detalle.matper
        detalle.nomper
        detalle.periodo
        detalle.valcal[1]
        detalle.valcal[2]
        detalle.valcal[3]
        detalle.valcal[4]
        detalle.valcal[5]
        detalle.valcal[6]
        detalle.valcal[7]
        detalle.valcal[8]
        detalle.valcal[9]
        detalle.valcal[10]
        detalle.valcal[11]
        detalle.valcal[12]
        with stream-io no-labels width 300.        
end.
output close.
