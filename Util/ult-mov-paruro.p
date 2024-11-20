def var x-codmat as char.

input from c:\tmp\sinrotacion-paruro.txt.
output to c:\tmp\ult-mov-paruro.txt.
repeat:
    import unformatted x-codmat.
    find last almdmov use-index almd03
        where codcia = 001
        and codalm = '05'
        and codmat = x-codmat
        and tipmov = 's'
        and codmov = 02
        no-lock no-error.
    if available almdmov then
    display fchdoc codalm codmat tipmov codmov nrodoc (candes * factor)
    with stream-io no-box.
end.
output close.
input close.
