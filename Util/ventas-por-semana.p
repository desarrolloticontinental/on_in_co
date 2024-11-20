def var x-cantidad as dec no-undo.
def var x-importe  as dec no-undo.
def var x-semana   as int no-undo.
def var x-coe      as dec no-undo.
DEF VAR x-fecha-d AS DATE NO-UNDO.
DEF VAR x-fecha-h AS DATE NO-UNDO.
def buffer cdocu for ccbcdocu.
def buffer ddocu for ccbddocu.

def temp-table detalle
    field codmat like almmmatg.codmat
    field desmat like almmmatg.desmat
    field undbas like almmmatg.undbas
    FIELD desmar LIKE almmmatg.desmar 
    field impsem as dec format '->>>>>>9.99' extent 16
    field vtasem as dec format '->>>>>>9.99' extent 16.

ASSIGN
    x-fecha-d = 12/17/07
    x-fecha-h = 04/06/08.
for each gn-divi no-lock where gn-divi.codcia = 001 AND gn-divi.coddiv <> '99999':
    display gn-divi.coddiv.
    pause 0.
    for each ccbcdocu no-lock where codcia = 001
        and coddiv = gn-divi.coddiv
        and fchdoc >= x-fecha-d
        and fchdoc <= x-fecha-h
        and coddoc = 'fac',
        each ccbddocu of ccbcdocu no-lock,
        first almmmatg of ccbddocu no-lock where almmmatg.codfam = '010':
        {r:\on_in_co\util\ventas-por-semana.i}
    end.    
    for each ccbcdocu no-lock where codcia = 001
        and coddiv = gn-divi.coddiv
        and fchdoc >= x-fecha-d
        and fchdoc <= x-fecha-h
        and coddoc = 'bol',
        each ccbddocu of ccbcdocu no-lock,
        first almmmatg of ccbddocu no-lock where almmmatg.codfam = '010':
        {r:\on_in_co\util\ventas-por-semana.i}
    end.    
    for each ccbcdocu no-lock where codcia = 001
        and coddiv = gn-divi.coddiv
        and fchdoc >= x-fecha-d
        and fchdoc <= x-fecha-h
        and coddoc = 'tck',
        each ccbddocu of ccbcdocu no-lock,
        first almmmatg of ccbddocu no-lock where almmmatg.codfam = '010':
        {r:\on_in_co\util\ventas-por-semana.i}
    end.    
    for each ccbcdocu no-lock where codcia = 001
        and coddiv = gn-divi.coddiv
        and fchdoc >= x-fecha-d
        and fchdoc <= x-fecha-h
        and coddoc = 'n/c',
        each ccbddocu of ccbcdocu no-lock,
        first almmmatg of ccbddocu no-lock where almmmatg.codfam = '010':
        {r:\on_in_co\util\ventas-por-semana.i}
    end.    
    for each ccbcdocu no-lock where codcia = 001
        and coddiv = gn-divi.coddiv
        and fchdoc >= x-fecha-d
        and fchdoc <= x-fecha-h
        and coddoc = 'n/c'
        and cndcre = 'N':
        find cdocu where cdocu.codcia = ccbcdocu.codcia
            and cdocu.coddoc = ccbcdocu.codref
            and cdocu.nrodoc = ccbcdocu.nroref
            no-lock no-error.
        if not available cdocu then next.
        if cdocu.flgest = 'A' then next.
        x-coe = ccbcdocu.imptot / cdocu.imptot.
        for each ddocu of cdocu no-lock,
                first almmmatg of ddocu no-lock where almmmatg.codfam = '010':
            find detalle where detalle.codmat = ddocu.codmat
                exclusive-lock no-error.
            if not available detalle then do:
                create detalle.
                buffer-copy almmmatg to detalle.
            end.
            if ccbcdocu.fchdoc >= 12/17/07 and ccbcdocu.fchdoc <= 12/23/07 then x-semana = 1.
            if ccbcdocu.fchdoc >= 12/24/07 and ccbcdocu.fchdoc <= 12/30/07 then x-semana = 2.
            if ccbcdocu.fchdoc >= 12/31/07 and ccbcdocu.fchdoc <= 01/06/07 then x-semana = 3.
            if ccbcdocu.fchdoc >= 01/07/08 and ccbcdocu.fchdoc <= 01/13/08 then x-semana = 4.
            if ccbcdocu.fchdoc >= 01/14/08 and ccbcdocu.fchdoc <= 01/20/08 then x-semana = 5.
            if ccbcdocu.fchdoc >= 01/21/08 and ccbcdocu.fchdoc <= 01/27/08 then x-semana = 6.
            if ccbcdocu.fchdoc >= 01/28/08 and ccbcdocu.fchdoc <= 02/03/08 then x-semana = 7.
            if ccbcdocu.fchdoc >= 02/04/08 and ccbcdocu.fchdoc <= 02/10/08 then x-semana = 8.
            if ccbcdocu.fchdoc >= 02/11/08 and ccbcdocu.fchdoc <= 02/17/08 then x-semana = 9.
            if ccbcdocu.fchdoc >= 02/18/08 and ccbcdocu.fchdoc <= 02/24/08 then x-semana = 10.
            if ccbcdocu.fchdoc >= 02/25/08 and ccbcdocu.fchdoc <= 03/02/08 then x-semana = 11.
            if ccbcdocu.fchdoc >= 03/03/08 and ccbcdocu.fchdoc <= 03/09/08 then x-semana = 12.
            if ccbcdocu.fchdoc >= 03/10/08 and ccbcdocu.fchdoc <= 03/16/08 then x-semana = 13.
            if ccbcdocu.fchdoc >= 03/17/08 and ccbcdocu.fchdoc <= 03/23/08 then x-semana = 14.
            if ccbcdocu.fchdoc >= 03/24/08 and ccbcdocu.fchdoc <= 03/30/08 then x-semana = 15.
            if ccbcdocu.fchdoc >= 03/31/08 and ccbcdocu.fchdoc <= 04/06/08 then x-semana = 16.
            x-cantidad = (ddocu.candes * ddocu.factor * x-coe).
            detalle.vtasem[x-semana] = detalle.vtasem[x-semana] - x-cantidad.
            find last gn-tcmb where gn-tcmb.fecha <= ccbcdocu.fchdoc no-lock.
            if ccbcdocu.codmon = 1
            then x-importe = ddocu.implin * x-coe.
            else x-importe = ddocu.implin * gn-tcmb.venta * x-coe.
            detalle.impsem[x-semana] = detalle.impsem[x-semana] - x-importe.
        end.
    end.    
end.

/* importes */
output to m:\tmp\importes2008.txt.
for each detalle no-lock:
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.impsem[1]
        detalle.impsem[2]
        detalle.impsem[3]
        detalle.impsem[4]
        detalle.impsem[5]
        detalle.impsem[6]
        detalle.impsem[7]
        detalle.impsem[8]
        detalle.impsem[9]
        detalle.impsem[10]
        detalle.impsem[11]
        detalle.impsem[12]
        detalle.impsem[13]
        detalle.impsem[14]
        detalle.impsem[15]
        detalle.impsem[16]
        with stream-io no-box width 320.
end.
output close.

/* cantidades */
output to m:\tmp\cantidades2008.txt.
for each detalle no-lock:
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.vtasem[1]
        detalle.vtasem[2]
        detalle.vtasem[3]
        detalle.vtasem[4]
        detalle.vtasem[5]
        detalle.vtasem[6]
        detalle.vtasem[7]
        detalle.vtasem[8]
        detalle.vtasem[9]
        detalle.vtasem[10]
        detalle.vtasem[11]
        detalle.vtasem[12]
        detalle.vtasem[13]
        detalle.vtasem[14]
        detalle.vtasem[15]
        detalle.vtasem[16]
        with stream-io no-box width 320.
end.
output close.

