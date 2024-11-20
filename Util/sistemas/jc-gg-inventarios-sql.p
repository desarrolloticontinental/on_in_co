DEF VAR x-fecha AS DATE.
x-fecha = TODAY.

SELECT almmmatg.codmat,
    almmmatg.desmat,
    almmmatg.desmar,
    almmmatg.codfam,
    almtfami.desfam,
    almmmatg.subfam,
    almsfami.dessub,
    almmmate.codalm, 
    almmmatg.catconta[1],
    almacen.descripcion, 
    almstkal.stkact,
    AlmStkge.CtoUni
    FROM almacen, almmmate, almmmatg, almtfami, almsfami, almstkal, almstkge
    WHERE almacen.codcia = 001 AND almacen.campo-c[6] = 'Si' AND almacen.codalm = '11w' AND
    almmmate.codcia = almacen.codcia AND 
    almmmate.codalm = almacen.codalm AND 
    almmmatg.codcia = almmmate.codcia AND 
    almmmatg.codmat = almmmate.codmat AND 
    LOOKUP(almmmatg.catconta[1], 'MC,MI,PM,PT') > 0 AND
    almtfami.codcia = almmmatg.codcia AND 
    almtfami.codfam = almmmatg.codfam AND 
    almsfami.codcia = almmmatg.codcia AND 
    almsfami.codfam = almmmatg.codfam AND 
    almsfami.subfam = almmmatg.subfam AND
    almstkal.codcia = almmmate.codcia AND
    almstkal.codalm = almmmate.codalm AND
    almstkal.codmat = almmmate.codmat AND 
    almstkal.fecha = (SELECT MAX(almstkal.fecha) 
                      FROM almstkal WHERE almstkal.codcia = almmmate.codcia AND
                      almstkal.codalm = almmmate.codalm AND
                      almstkal.codmat = almmmate.codmat AND
                      almstkal.fecha <= x-fecha) AND
    almstkge.fecha = (SELECT MAX(almstkge.fecha) 
                      FROM almstkge WHERE almstkge.codcia = almmmate.codcia AND
                      almstkge.codmat = almmmate.codmat AND
                      almstkge.fecha <= x-fecha)
    .



