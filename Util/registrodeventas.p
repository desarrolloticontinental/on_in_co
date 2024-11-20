def var x-impbrt as dec format '->>>,>>>,>>9.99'.
def var x-impexo as dec format '->>>,>>>,>>9.99'.
def var x-impisc as dec format '->>>,>>>,>>9.99'.
def var x-impvta as dec format '->>>,>>>,>>9.99'.
def var x-impigv as dec format '->>>,>>>,>>9.99'.
def var x-imptot as dec format '->>>,>>>,>>9.99'.
def var x-impdto as dec format '->>>,>>>,>>9.99'.
def var x-factor as int.
def var x-tpovta as dec.
def var x-tpocmp as dec.

for each ccbcdocu no-lock where codcia = 001
    and (coddoc = 'fac' or coddoc = 'bol' or coddoc = 'n/c')
    and coddiv = '00015'
    and fchdoc >= 01/01/2007
    and fchdoc <= 01/31/2007
    and flgest <> 'a':
    if impexo + impisc + impvta + impigv <> imptot
    then display coddoc nrodoc.

    FIND gn-tcmb WHERE gn-tcmb.fecha = fchdoc NO-LOCK NO-ERROR.
    IF AVAILABLE gn-tcmb 
    THEN assign
            x-tpovta = gn-tcmb.venta
            x-tpocmp = gn-tcmb.compra.
    x-factor = if coddoc = 'n/c' then -1 else 1.
    if codmon = 1
    then assign
            x-impbrt = x-impbrt + impbrt * x-factor
            x-impdto = x-impdto + impdto * x-factor
            x-impexo = x-impexo + impexo * x-factor
            x-impisc = x-impisc + impisc * x-factor
            x-impvta = x-impvta + impvta * x-factor
            x-impigv = x-impigv + impigv * x-factor
            x-imptot = x-imptot + imptot * x-factor.
    else assign
            x-impbrt = x-impbrt + round(impbrt * x-factor * x-tpovta, 2)
            x-impdto = x-impdto + round(impdto * x-factor * x-tpovta, 2)
            x-impexo = x-impexo + round(impexo * x-factor * x-tpovta, 2)
            x-impisc = x-impisc + round(impisc * x-factor * x-tpovta, 2)
            x-impvta = x-impvta + round(impvta * x-factor * x-tpovta, 2)
            x-impigv = x-impigv + round(impigv * x-factor * x-tpovta, 2)
            x-imptot = x-imptot + round(imptot * x-factor * x-tpovta, 2).
end.    
display
    x-impbrt
    x-impdto
    x-impexo
    x-impisc
    x-impvta
    x-impigv
    x-imptot
    with 1 col.
    
