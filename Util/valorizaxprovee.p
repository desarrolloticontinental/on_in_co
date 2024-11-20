def var x-stkact as dec.
def var x-ctorep as dec.
def var x-nompro as char format 'x(40)'.
def var x-alm03 as dec format '->,>>9.99'.
def var x-alm03a as dec format '->,>>9.99'.
def var x-alm04 as dec format '->,>>9.99'.
def var x-alm04a as dec format '->,>>9.99'.
def var x-alm05 as dec format '->,>>9.99'.
def var x-alm05a as dec format '->,>>9.99'.
def var x-alm48 as dec format '->,>>9.99'.
def var x-alm83b as dec format '->,>>9.99'.
def var x-alm11 as dec format '->,>>9.99'.
def var x-alm130 as dec format '->,>>9.99'.
def var x-alm131 as dec format '->,>>9.99'.
def var x-alm22 as dec format '->,>>9.99'.
def var x-alm300 as dec format '->,>>9.99'.
def var x-alm17 as dec format '->,>>9.99'.
def var x-alm18 as dec format '->,>>9.99'.
def var x-alm16 as dec format '->,>>9.99'.


output to c:\tmp\valorxprovee.txt.
for each almmmatg where codcia = 1:
    assign
        x-alm03 = 0
        x-alm03a = 0
        x-alm04 = 0
        x-alm04a = 0
        x-alm05 = 0
        x-alm05a = 0
        x-alm48 = 0
        x-alm83b = 0
        x-alm11 = 0
        x-alm130 = 0
        x-alm131 = 0
        x-alm22 = 0
        x-alm300 = 0
        x-alm17 = 0
        x-alm18 = 0
        x-alm16 = 0
        x-stkact = 0.
        x-nompro = ''.
    for each almmmate of almmmatg where 
        lookup(trim(almmmate.codalm), '03,03a,04,04a,05,05a,48,83b,11,130,131,22,300,17,18,16') > 0:
        x-stkact = x-stkact + almmmate.stkact.
        case almmmate.codalm:
            when '03' then x-alm03 = almmmate.stkact.
            when '03a' then x-alm03a = almmmate.stkact.
            when '04' then x-alm04 = almmmate.stkact.
            when '04a' then x-alm04a = almmmate.stkact.
            when '05' then x-alm05 = almmmate.stkact.
            when '05a' then x-alm05a = almmmate.stkact.
            when '48' then x-alm48 = almmmate.stkact.
            when '83b' then x-alm83b = almmmate.stkact.
            when '11' then x-alm11 = almmmate.stkact.
            when '130' then x-alm130 = almmmate.stkact.
            when '131' then x-alm131 = almmmate.stkact.
            when '22' then x-alm22 = almmmate.stkact.
            when '300' then x-alm300 = almmmate.stkact.
            when '17' then x-alm17 = almmmate.stkact.
            when '18' then x-alm18 = almmmate.stkact.
            when '16' then x-alm16 = almmmate.stkact.
        end case.
    end.
    x-ctorep = ctolis.
    if almmmatg.monvta = 1
    then x-ctorep = x-ctorep / almmmatg.tpocmb.
    find gn-prov where gn-prov.codcia = 0 and gn-prov.codpro = almmmatg.codpr1 no-lock no-error.
    if available gn-prov then x-nompro = gn-prov.nompro.
    if x-stkact <> 0 then
    display 
        almmmatg.codpr1 
        x-nompro 
        almmmatg.codmat 
        almmmatg.desmat 
        desmar format 'x(20)'
        x-stkact 
        x-ctorep 
        x-alm03
        x-alm03a
        x-alm04
        x-alm04a
        x-alm05
        x-alm05a
        x-alm48
        x-alm83b
        x-alm11
        x-alm130
        x-alm131
        x-alm22
        x-alm300
        x-alm17
        x-alm18
        x-alm16
        with stream-io no-labels width 380.
end.
output close.




/*for each almmmatg where codcia = 1 and codpr1 = '11224199':
 *     x-stkact = 0.
 *     for each almmmate of almmmatg where 
 *         lookup(trim(almmmate.codalm), '03,03a,04,04a,05,05a,83b,11,300,17,18,16') > 0:
 *         x-stkact = x-stkact + almmmate.stkact.
 *     end.
 *     x-ctorep = ctolis.
 *     if almmmatg.monvta = 1
 *     then x-ctorep = x-ctorep / almmmatg.tpocmb.
 *     if x-stkact <> 0 then
 *     display almmmatg.codmat almmmatg.desmat desmar x-stkact x-ctorep with stream-io no-labels width 200.
 * end.*/
