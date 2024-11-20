def var x-stkate as dec.
def var x-stksm as dec.
def var x-fecha as date.

x-fecha = 11/13/07.

output to c:\tmp\errores.txt.
for each integral.almmmatg no-lock where integral.almmmatg.codcia = 001:
    find last integral.almstkal where
        integral.almstkal.codcia = 001
        and integral.almstkal.codalm = '16'
        and integral.almstkal.codmat = integral.almmmatg.codmat
        and integral.almstkal.fecha <= x-fecha
        no-lock no-error.
    if not available integral.almstkal then next.
    x-stkate = integral.almstkal.stkact.
    x-stksm = 0.
    find last sm.almsub where
        sm.almsub.codcia = 001
        and sm.almsub.codalm = '16'
        and sm.almsub.codmat = integral.almmmatg.codmat
        and sm.almsub.fchdoc <= x-fecha
        no-lock no-error.
    if available sm.almsub then x-stksm = sm.almsub.stksub.
    if x-stkate <> x-stksm
    then display integral.almmmatg.codmat x-stkate x-stksm.
end.
output close.
