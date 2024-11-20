def var x-125 as dec.
def var x-126 as dec.
def var x-127 as dec.
def var x-131 as dec.
def var x-209 as dec.

output to c:\tmp\nov04abril05.txt.
for each pl-mov-mes no-lock where codcia = 1
    and codcal = 001
    and periodo >= 2004 and periodo <= 2005
    break by codper by periodo by nromes:
    if periodo = 2004 and nromes < 11 then next.
    if periodo = 2005 and nromes > 04 then next.
    if first-of(codper) or first-of(nromes)
    then assign
            x-125 = 0
            x-126 = 0
            x-127 = 0
            x-131 = 0
            x-209 = 0.
    case codmov:
        when 125 then x-125 = valcal-mes.
        when 126 then x-126 = valcal-mes.
        when 127 then x-127 = valcal-mes.
        when 131 then x-131 = valcal-mes.
        when 209 then x-209 = valcal-mes.
    end case.
    if last-of(codper) or last-of(nromes)
    then display codper periodo nromes x-125 x-126 x-127 x-131 x-209
        with stream-io width 200.
end.
output close.

    
