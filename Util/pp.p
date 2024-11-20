for each almcmov where codcia = 1 and
    tipmov = 's' and codmov = 01
    and codalm = '11'
    and fchdoc = date(12,27,2004):
    display fchdoc codalm tipmov codmov nrodoc flgest.
    for each almdmov of almcmov:
        display codmat.
    end.
end.    
