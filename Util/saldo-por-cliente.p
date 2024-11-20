def temp-table t-clie like gn-clie
    field saldomn as dec label 'Saldo S/.'
    field saldome as dec label 'Saldo US$'.
def var x-codcli as char format 'x(11)'.
def var x-fecha as date no-undo.

x-fecha = 04/30/08.

input from c:\tmp\clientes.prn.
repeat:
    import unformatted x-codcli.
    find t-clie where t-clie.codcli = x-codcli
        exclusive-lock no-error.
    if not available t-clie then do:
        create t-clie.
        t-clie.codcli = x-codcli.
    end.
end.
input close.

for each t-clie where codcli <> '', first gn-clie of t-clie no-lock:
    /* cargamos saldo inicial */
    for each ccbcdocu no-lock where codcia = 001
            and codcli = t-clie.codcli
            and lookup(trim(coddoc), 'fac,bol,tck,let,n/d,chq') > 0
            and flgest <> 'a'
            and fchdoc <= x-fecha:
        if ccbcdocu.codmon = 1
        then t-clie.saldomn = t-clie.saldomn + ccbcdocu.imptot.
        else t-clie.saldome = t-clie.saldome + ccbcdocu.imptot.
        /* cargamos cancelaciones */
        for each ccbdcaja no-lock where codcia = 001
                and codref = ccbcdocu.coddoc
                and nroref = ccbcdocu.nrodoc:
        end.
    end.            
/*
    /* cargamos cancelaciones */
    for each ccbcdocu no-lock where codcia = 001
            and codcli = t-clie.codcli
            and lookup(trim(coddoc), 'n/c,a/r') > 0
            and flgest <> 'a'
            and fchdoc <= x-fecha:
        if ccbcdocu.codmon = 1
        then t-clie.saldomn = t-clie.saldomn + ccbcdocu.imptot.
        else t-clie.saldome = t-clie.saldome + ccbcdocu.imptot.
    end.            
*/
end.
