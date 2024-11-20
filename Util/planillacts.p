def var x-101 as dec.
def var x-103 as dec.
def var x-120 as dec.
def var x-134 as dec.
def var x-141 as dec.
def var x-210 as dec.
def var x-605 as dec.
def var x-606 as dec.
def var x-613 as dec.
def var x-403 as dec.

output to c:\tmp\cts.txt.
for each pl-mov-mes where codcia = 1 and periodo = 2005 and nromes = 04
    and PL-MOV-MES.codcal = 006 break by codper:
    if first-of(codper)
    then assign
            x-101 = 0
            x-103 = 0
            x-120 = 0
            x-134 = 0
            x-141 = 0
            x-210 = 0
            x-605 = 0
            x-606 = 0
            x-613 = 0
            x-403 = 0.
    case codmov:
        when 101 then x-101 = valcal-mes.
        when 103 then x-103 = valcal-mes.
        when 120 then x-120 = valcal-mes.
        when 134 then x-134 = valcal-mes.
        when 141 then x-141 = valcal-mes.
        when 210 then x-210 = valcal-mes.
        when 605 then x-605 = valcal-mes.
        when 613 then x-613 = valcal-mes.
        when 403 then x-403 = valcal-mes.
    end case.
    if last-of(codper)
    then display codper x-101 x-103 x-120 x-134 x-141 x-210 x-605 x-606 x-613 x-403
        with stream-io width 200.
end.    
output close.
