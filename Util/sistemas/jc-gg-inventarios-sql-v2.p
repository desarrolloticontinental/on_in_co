DEF VAR x-fecha AS DATE.
x-fecha = TODAY.

SELECT 
    /*
    almmmatg.codmat,
    almmmatg.desmat,
    almmmatg.desmar,
    almmmatg.codfam,
    almtfami.desfam,
    almmmatg.subfam,
    almsfami.dessub,
    almmmate.codalm, */
    almmmate.codmat,
    MONTH(almstkal.fecha),
    YEAR(almstkal.fecha),
    almstkal.stkact
    FROM almmmate
    JOIN almacen ON (
        almacen.codcia = almmmate.codcia AND
        almacen.codalm = almmmate.codalm AND
        almacen.campo-c[6] = "Si")
    JOIN almmmatg ON (
        almmmatg.codcia = almmmate.codcia AND
        almmmatg.codmat = almmmate.codmat AND
        LOOKUP(almmmatg.catconta[1], 'MC,MI,PM,PT') > 0 )
    JOIN almtfami ON (
        almtfami.codcia = almmmatg.codcia AND
        almtfami.codfam = almmmatg.codfam)
    JOIN almsfami ON (
        almsfami.codcia = almmmatg.codcia AND
        almsfami.codfam = almmmatg.codfam AND
        almsfami.subfam = almmmatg.subfam)
    LEFT JOIN almstkal ON (
        almstkal.codcia = almmmate.codcia AND
        almstkal.codalm = almmmate.codalm AND
        almstkal.codmat = almmmate.codmat AND
        almstkal.fecha >= DATE(01,01,2019) AND
        almstkal.fecha <= x-fecha)
    LEFT JOIN almstkge cab ON (
        cab.codcia = almmmate.codcia AND
        cab.codmat = almmmate.codmat AND
        cab.fecha >= DATE(01,01,2019)
        )
    WHERE almmmate.codcia = 1 AND almmmate.codalm = "11w" AND almmmate.codmat = '002511' 
    GROUP BY almmmate.codalm, almmmate.codmat, MONTH(almstkal.fecha), YEAR(almstkal.fecha)
    HAVING cab.fecha = (SELECT MAX(det.fecha)
                     FROM almstkge det
                     WHERE det.codcia = cab.codcia AND
                     det.codmat = cab.codmat AND
                     MONTH(det.fecha) = MONTH(cab.fecha) AND
                     YEAR(det.fecha) = YEAR(cab.fecha))
    .


