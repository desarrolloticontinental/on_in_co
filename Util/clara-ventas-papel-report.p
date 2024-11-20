def var x-codmat as char.
def var x-fmapgo like ccbcdocu.fmapgo.
def buffer b-docu for ccbcdocu.

def temp-table detalle
    field fmapgo like ccbcdocu.fmapgo
    field codmat like almmmatg.codmat
    field desmat like almmmatg.desmat
    field desmar like almmmatg.desmar
    field nombr  like gn-ConVt.Nombr 
    field impnac as dec format '->>>,>>>,>>9.99'
    field impusa as dec format '->>>,>>>,>>9.99'.


for each almmmatg no-lock where codcia = 001 
    and codfam = '011' and desmar begins 'report'
    and codpr1 = 'nd000015':
    if x-codmat = ''
    then x-codmat = trim(codmat).
    else x-codmat = x-codmat + ',' + trim(codmat).
end.

for each gn-divi no-lock:
    for each ccbcdocu no-lock where codcia = 001
            and coddiv = gn-divi.coddiv
            and lookup(trim(coddoc), 'fac,bol,tck,n/c') > 0
            and flgest <> 'a'
            and fchdoc >= 01/01/2007,
            each ccbddocu of ccbcdocu no-lock,
            first almmmatg of ccbddocu no-lock:
        if lookup(trim(ccbddocu.codmat), x-codmat) > 0 then do:
            x-fmapgo = ccbcdocu.fmapgo.
            if ccbcdocu.coddoc = 'n/c' then do:
                find b-docu where b-docu.codcia = 001
                    and b-docu.coddoc = ccbcdocu.codref
                    and b-docu.nrodoc = ccbcdocu.nroref
                    no-lock no-error.
                if available b-docu then x-fmapgo = b-docu.fmapgo.
            end.
            find first detalle where detalle.codmat = ccbddocu.codmat
                and detalle.fmapgo = x-fmapgo
                exclusive-lock no-error.
            if not available detalle then do:
                create detalle.
                assign
                    detalle.codmat = almmmatg.codmat
                    detalle.desmat = almmmatg.desmat
                    detalle.desmar = almmmatg.desmar
                    detalle.fmapgo = x-fmapgo.
                find gn-convt where gn-convt.codig = detalle.fmapgo
                    no-lock no-error.
                if available gn-convt then detalle.nombr = gn-convt.nombr.
            end.
            if ccbcdocu.codmon = 1
            then if ccbcdocu.coddoc = 'n/c'
                then detalle.impnac = detalle.impnac - implin.
                else detalle.impnac = detalle.impnac + implin.
            else if ccbcdocu.coddoc = 'n/c'
                then detalle.impusa = detalle.impusa - implin.
                else detalle.impusa = detalle.impusa + implin.
        end.
    end.
end.

output to c:\tmp\papelreport.txt.
for each detalle by detalle.fmapgo by detalle.codmat:
    display
        detalle.fmapgo
        detalle.nombr
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.impnac
        detalle.impusa
        with stream-io no-box width 200.
end.
output close.
