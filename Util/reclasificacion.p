
find almcmov where codcia = 001
and codalm = '03'
and tipmov = 'i'
and codmov = 13
and nrodoc = 6700.
display fchdoc.
for each almdmov of almcmov:
    display codmat candes.
    almdmov.codmov = 15.
end.
almcmov.codmov = 15.
