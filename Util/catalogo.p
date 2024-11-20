def var x-ctotot as dec.
def var x-prevta as dec extent 10 format '>>>,>>9.9999'.
def var x-preofi as dec format '>>>,>>9.9999'.
def var f-factor as dec.

output to c:\tmp\catalogo.txt.
for each almmmatg where codcia = 001 and tpoart <> 'D' no-lock:
    assign
        x-ctotot = almmmatg.ctotot
        x-prevta[2] = almmmatg.prevta[2]
        x-prevta[3] = almmmatg.prevta[3]
        x-prevta[4] = almmmatg.prevta[4]
        x-preofi = almmmatg.preofi.
    if almmmatg.monvta = 1 then
    assign
        x-ctotot = almmmatg.ctotot / almmmatg.tpocmb
        x-prevta[2] = almmmatg.prevta[2] / almmmatg.tpocmb
        x-prevta[3] = almmmatg.prevta[3] / almmmatg.tpocmb
        x-prevta[4] = almmmatg.prevta[4] / almmmatg.tpocmb
        x-preofi = almmmatg.preofi / almmmatg.tpocmb.
    f-factor = 1.
    if undA <> '' then do:
        find almtconv where almtconv.codunid = almmmatg.undbas
            and almtconv.codalter = undA 
            no-lock no-error.
        if available almtconv then f-factor = almtconv.equival.
    end.
    x-prevta[2] = x-prevta[2] / f-factor.
    f-factor = 1.
    if undA <> '' then do:
        find almtconv where almtconv.codunid = almmmatg.undbas
            and almtconv.codalter = undB
            no-lock no-error.
        if available almtconv then f-factor = almtconv.equival.
    end.
    x-prevta[3] = x-prevta[3] / f-factor.
    f-factor = 1.
    if undA <> '' then do:
        find almtconv where almtconv.codunid = almmmatg.undbas
            and almtconv.codalter = undC
            no-lock no-error.
        if available almtconv then f-factor = almtconv.equival.
    end.
    x-prevta[4] = x-prevta[4] / f-factor.

    display codmat desmat desmar codfam subfam codpr1 tipart undbas 
        x-ctotot undA x-prevta[2] undB x-prevta[3] undC x-prevta[4]
        chr__01 x-preofi
        with stream-io no-labels width 320.
end.
output close.
