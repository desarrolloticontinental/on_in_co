def var x-101 as dec format '>>>,>>>,>>9.99'.
def var x-103 as dec format '>>>,>>>,>>9.99'.
def var x-131 as dec format '>>>,>>>,>>9.99'.
def var x-134 as dec format '>>>,>>>,>>9.99'.
def var x-138 as dec format '>>>,>>>,>>9.99'.
def var x-209 as dec format '>>>,>>>,>>9.99'.

def var x-linea as char format 'x(200)'.

output to c:\tmp\dianasu.txt.
for each pl-flg-mes where codcia = 1 and periodo = 2004
        and nromes = 03 and seccion = 'FABRICA' no-lock,
        first pl-pers of pl-flg-mes:
    ASSIGN
        x-101 = 0
        x-103 = 0
        x-131 = 0
        x-134 = 0
        x-138 = 0
        x-209 = 0.
    for each pl-mov-mes of pl-flg-mes where codcal = 0:
        CASE pl-mov-mes.codmov:
            WHEN 101 THEN x-101 = x-101 + pl-mov-mes.valcal-mes.
            WHEN 103 THEN x-103 = x-103 + pl-mov-mes.valcal-mes.
            WHEN 131 THEN x-131 = x-131 + pl-mov-mes.valcal-mes.
            WHEN 134 THEN x-134 = x-134 + pl-mov-mes.valcal-mes.
            WHEN 138 THEN x-138 = x-138 + pl-mov-mes.valcal-mes.
            WHEN 209 THEN x-209 = x-209 + pl-mov-mes.valcal-mes.
        END CASE.
    end.
    x-linea = pl-pers.codper + '|' +
                ( trim(pl-pers.patper) + ' ' + trim(pl-pers.matper) +
                    ', ' + trim(pl-pers.nomper) ) + '|' +
                string(x-101) + '|' + string(x-103) + '|' +
                string(x-131) + '|' + string(x-134) + '|' +
                string(x-138) + '|' + string(x-209).
    display x-linea with no-labels no-box stream-io width 250.
end.
output close.
