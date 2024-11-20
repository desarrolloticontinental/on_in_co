def var x-items as int.

output to c:\tmp\item-dif-inv.txt.
for each invconfig where codcia = 001
    and fchinv >= 04/16/2007 and fchinv <= 5/8/2007:
    x-items = 0.
    for each almcmov no-lock where codcia = 001
        and codalm = invconfig.codalm
        and codmov = 01
        and fchdoc = invconfig.fchinv,
        each almdmov of almcmov no-lock:
        x-items = x-items + 1.
    end.
    display invconfig.codalm invconfig.fchinv x-items.
end.
output close.
    
