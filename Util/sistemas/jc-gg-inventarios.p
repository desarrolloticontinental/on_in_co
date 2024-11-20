DEFINE TEMP-TABLE detalle
    FIELD codmat LIKE almmmatg.codmat
    FIELD desmat LIKE almmmatg.desmat
    FIELD desmar LIKE almmmatg.desmar
    FIELD codfam LIKE almmmatg.codfam
    FIELD desfam LIKE Almtfami.desfam
    FIELD subfam LIKE almmmatg.subfam
    FIELD dessub LIKE AlmSFami.dessub
    FIELD catconta AS CHAR FORMAT 'x(5)'
    FIELD codalm LIKE almmmate.codalm
    FIELD desalm LIKE Almacen.Descripcion
    FIELD stkact AS DECI FORMAT '->>>,>>>,>>9.9999'
    FIELD ctouni AS DECI FORMAT '->>>,>>>,>>9.9999'
    FIELD mes AS INTE FORMAT '99'
    FIELD periodo AS INTE FORMAT '9999'
    FIELD compras AS DECI FORMAT '->>>,>>>,>>9.9999'
    .
DEF VAR k AS INTE NO-UNDO.
DEF VAR j AS INTE NO-UNDO.
DEF VAR x-Fecha AS DATE NO-UNDO.

/* Solo almacenes comerciales */
FOR EACH almacen NO-LOCK WHERE almacen.codcia = 1 AND almacen.campo-c[6] = 'Si' AND codalm = '11w',
    EACH almmmate NO-LOCK WHERE almmmate.codcia = almacen.codcia AND
    almmmate.codalm = almacen.codalm,
    FIRST almmmatg NO-LOCK WHERE almmmatg.codcia = almmmate.codcia AND
    almmmatg.codmat = almmmate.codmat AND
    LOOKUP(almmmatg.catconta[1], 'MC,MI,PM,PT') > 0,
    FIRST almtfami OF almmmatg NO-LOCK,
    FIRST almsfami OF almmmatg NO-LOCK:
    DO j = 2023 TO YEAR(TODAY):
        DO k = 1 TO 12:
            x-Fecha = ADD-INTERVAL(DATE(k,01,j), 1, 'month') - 1.
            IF x-Fecha > TODAY THEN LEAVE.
            CREATE detalle.
            BUFFER-COPY almmmatg TO detalle
                ASSIGN
                detalle.desfam = almtfami.desfam
                detalle.dessub = almsfami.dessub
                detalle.catconta = almmmatg.catconta[1]
                detalle.codalm = almacen.codalm
                detalle.desalm = almacen.descripcion
                detalle.mes = MONTH(x-Fecha).
                detalle.periodo = YEAR(x-Fecha).
                .
            FIND LAST AlmStkal WHERE AlmStkal.CodCia = almacen.codcia AND
                AlmStkal.CodAlm = almacen.codalm AND
                AlmStkal.codmat = almmmate.codmat AND
                AlmStkal.Fecha <= x-Fecha
                NO-LOCK NO-ERROR.
            IF AVAILABLE almstkal THEN detalle.stkact = AlmStkal.StkAct.
            FIND LAST AlmStkge WHERE AlmStkge.CodCia = almacen.codcia AND
                AlmStkge.codmat = almmmate.codmat AND
                AlmStkge.Fecha <= x-Fecha
                NO-LOCK NO-ERROR.
            IF AVAILABLE AlmStkge THEN detalle.ctouni = AlmStkge.CtoUni.
            /* Compras */
            FOR EACH almdmov NO-LOCK WHERE almdmov.codcia = almacen.codcia AND
                almdmov.codalm = almacen.codalm AND
                almdmov.codmat = almmmatg.codmat AND
                almdmov.tipmov = "I" AND
                almdmov.codmov = 90 AND
                almdmov.fchdoc >= DATE(k,01,j) AND
                almdmov.fchdoc <= x-Fecha,
                FIRST almcmov OF almdmov NO-LOCK WHERE almcmov.flgest <> 'A':
                detalle.compras = detalle.compras + (almdmov.candes * almdmov.factor).
            END.
        END.
    END.
END.

OUTPUT TO d:\detalle.txt.
FOR EACH detalle NO-LOCK:
    EXPORT DELIMITER ';' detalle.
END.
OUTPUT CLOSE.

