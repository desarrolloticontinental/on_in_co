def var x-stkact as dec format '->>>,>>>,>>9.99' no-undo.
def var x-i06 as dec format '->>>,>>>,>>9.99' no-undo.
def var x-i02 as dec format '->>>,>>>,>>9.99' no-undo.
def var x-s50 as dec format '->>>,>>>,>>9.99' no-undo.

output to c:\tmp\saldo2003.txt.
for each almmmatg no-lock where codcia = 001
    and catconta[1] = 'MP':
    assign
        x-stkact = 0
        x-i06 = 0
        x-i02 = 0
        x-s50 = 0.
    find last almstkal where almstkal.codcia = 001
        and codalm = '11'
        and almstkal.codmat = almmmatg.codmat
        and fecha <= 01/01/2003 no-lock no-error.
    if available almstkal then x-stkact = almstkal.stkact.        
    find last almstkal where almstkal.codcia = 001
        and codalm = '12'
        and almstkal.codmat = almmmatg.codmat
        and fecha <= 01/01/2003 no-lock no-error.
    if available almstkal then x-stkact = x-stkact + almstkal.stkact.        
    for each almdmov no-lock where almdmov.codcia = 001
        and lookup(trim(almdmov.codalm), '11,12') > 0 
        and almdmov.tipmov = 'i'
        and almdmov.codmov = 06
        and almdmov.codmat = almmmatg.codmat
        and almdmov.fchdoc > 01/01/2003
        and almdmov.fchdoc <= 12/31/2003:
        x-i06 = x-i06 + almdmov.candes * almdmov.factor.
    end.        
    for each almdmov no-lock where almdmov.codcia = 001
        and lookup(trim(almdmov.codalm), '11,12') > 0 
        and almdmov.tipmov = 'i'
        and almdmov.codmov = 02
        and almdmov.codmat = almmmatg.codmat
        and almdmov.fchdoc > 01/01/2003
        and almdmov.fchdoc <= 12/31/2003:
        x-i02 = x-i02 + almdmov.candes * almdmov.factor.
    end.        
    for each almdmov no-lock where almdmov.codcia = 001
        and lookup(trim(almdmov.codalm), '11,12') > 0 
        and almdmov.tipmov = 's'
        and almdmov.codmov = 50
        and almdmov.codmat = almmmatg.codmat
        and almdmov.fchdoc > 01/01/2003
        and almdmov.fchdoc <= 12/31/2003:
        x-s50 = x-s50 + almdmov.candes * almdmov.factor.
    end.        
    if (x-stkact + x-i06 + x-i02 + x-s50) <> 0 then
    display almmmatg.catconta[1] almmmatg.codmat almmmatg.desmat format 'x(60)'
        almmmatg.desmar almmmatg.undbas
        x-stkact
        x-i06
        x-i02
        x-s50
        with stream-io width 320.
end.
output close.
