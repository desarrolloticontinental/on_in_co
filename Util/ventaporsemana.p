def var x-implin as dec.
def temp-table detalle
    field fmapgo like ccbcdocu.fmapgo
    field nombre as char format 'x(30)'
    field semana as dec extent 52.

    for each ccbcdocu where codcia = 001
            and lookup(trim(coddoc), 'fac,bol,tck,n/c') > 0
            and ccbcdocu.fchdoc >= 01/01/2006,
            each ccbddocu of ccbcdocu no-lock where codmat = '005206',
            first gn-convt no-lock where gn-convt.codig = ccbcdocu.fmapgo:
        find first pl-sem where pl-sem.codcia = 001
            and pl-sem.periodo = 2006
            and pl-sem.fecini <= ccbcdocu.fchdoc
            and ccbcdocu.fchdoc <= pl-sem.fecfin 
            no-lock no-error.
        if not available pl-sem then next.
        find last gn-tcmb where gn-tcmb.fecha <= ccbcdocu.fchdoc
            no-lock no-error.
        if not available gn-tcmb then next.
        /*display ccbcdocu.coddoc ccbcdocu.nrodoc.*/
        find detalle where detalle.fmapgo = ccbcdocu.fmapgo
            exclusive-lock no-error.
        if not available detalle then create detalle.
        assign
            detalle.fmapgo = ccbcdocu.fmapgo
            detalle.nombre = gn-convt.nombr.
	 if ccbcdocu.codmon = 1
	 then x-implin = ccbddocu.implin / gn-tcmb.compra.
        else x-implin = ccbddocu.implin.
        x-implin = ccbddocu.candes * ccbddocu.factor.
        if ccbcdocu.coddoc = 'n/c' then x-implin = -1 * x-implin.
        detalle.semana[pl-sem.nrosem] = detalle.semana[pl-sem.nrosem] + x-implin.
    end.

output to c:\tmp\m5206cantidad.txt.
for each detalle:
    display 
        detalle.fmapgo column-label 'codigo'
        detalle.nombre format 'x(20)'
        detalle.semana[1]
        detalle.semana[2]
        detalle.semana[3]
        detalle.semana[4]
        detalle.semana[5]
        detalle.semana[6]
        detalle.semana[7]
        detalle.semana[8]
        detalle.semana[9]
        detalle.semana[10]
        detalle.semana[11]
        detalle.semana[12]
        detalle.semana[13]
        detalle.semana[14]
        detalle.semana[15]
        detalle.semana[16]
        detalle.semana[17]
        detalle.semana[18]
        detalle.semana[19]
        detalle.semana[20]
        detalle.semana[21]
        detalle.semana[22]
        detalle.semana[23]
        detalle.semana[24]
        detalle.semana[25]
        detalle.semana[26]
        with stream-io width 320 no-box.
end.
output close.
