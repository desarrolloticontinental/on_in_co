
for each almacen no-lock where codcia = 001:
for each almcmov where codcia = 001 and codalm = almacen.codalm
and tipmov = 'i' and codmov = 09
and codcli = '20147739835'
and fchdoc >= 01/01/2007
and nrorf1 = '015087808':
display fchdoc nrodoc nrorf1 nrorf2.
end.
end.
