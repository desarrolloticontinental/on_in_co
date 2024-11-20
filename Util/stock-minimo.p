def var x-linea as char format 'x(50)'.
def var x-codmat as char format 'x(6)'.
def var x-stkmin as dec .

input from c:\tmp\stmin05.prn.
repeat:
    import unformatted x-linea.
    x-codmat = substring(x-linea,1,6).
    x-stkmin = decimal(substring(x-linea,7,14)).
    display x-codmat.
    pause 0.
    find almmmate where codcia = 001
        and codmat = x-codmat
        and codalm = '05'
        exclusive-lock no-error.
    if available almmmate
    then stkmin = x-stkmin.
    release almmmate.
end.
input close.
