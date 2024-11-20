
/*
for each pl-dhabiente where
    codcia = 1 and
    periodo = 2008 :
    if fchalta = ? then do:
    end.
end.
*/

DEF VAR ffecha as date.

for each pl-flg-mes where
    pl-flg-mes.codcia = 1 and
    pl-flg-mes.periodo = 2008 and
    pl-flg-mes.nromes = 3 no-lock:
    for each pl-dhabiente where
        pl-dhabiente.codcia = pl-flg-mes.codcia and
        pl-dhabiente.periodo = pl-flg-mes.periodo and
        pl-dhabiente.codper = pl-flg-mes.codper and
        pl-dhabiente.nromes >= pl-flg-mes.nromes:
        if fchalta = ? then do:
            if month(fecing) = 3 then
                ffecha = fecing.
            else ffecha = 03/01/08.
            /*
            fchalta = ffecha.
            */
            display pl-flg-mes.codper pl-dhabiente.nromes fecing fchalta ffecha.
            
        end.
    end.
end.
