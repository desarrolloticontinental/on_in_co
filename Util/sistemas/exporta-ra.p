DEF VAR s-codcia AS INTE INIT 001 NO-UNDO.
DEF VAR x-fchini AS DATE NO-UNDO.
DEF VAR x-fchfin AS DATE NO-UNDO.

DEF VAR x-Glosa AS CHAR NO-UNDO.
DEF VAR x-Libre_c01 AS CHAR NO-UNDO.
DEF VAR x-Libre_c02 AS CHAR NO-UNDO.

ASSIGN
    x-fchini = DATE(01,01,2019)
    x-fchfin = DATE(12,31,2019).

OUTPUT TO d:\reposiciones2019.txt.
PUT UNFORMATTED
    'ALMACEN|SERIE|NUMERO|FECHA|SOLICITANTE|USAURIO|FECHA|HORA|FCH APR|HOR APR|USR APR|'
    'FLGEST|FLGSIT|GLOSA|REPOSICION|CROSS DOCKING|ALMACEN XD|'
    'ARTICULO|CAN REQ|CAN APROB|CAN ATEN|CAN TRAN|CAN GEN'
    SKIP.
    
FOR EACH almcrepo NO-LOCK WHERE almcrepo.codcia = s-codcia 
    AND almcrepo.fchdoc >= x-fchini
    AND almcrepo.fchdoc <= x-fchfin:
    ASSIGN
        x-Glosa = almcrepo.glosa
        x-Libre_c01 = almcrepo.libre_c01
        x-Libre_c02 = almcrepo.libre_c02.
    RUN lib/limpiar-texto-abc (INPUT almcrepo.glosa, INPUT " ", OUTPUT x-Glosa).
    RUN lib/limpiar-texto-abc (INPUT almcrepo.libre_c01, INPUT " ", OUTPUT x-Libre_c01).
    RUN lib/limpiar-texto-abc (INPUT almcrepo.libre_c02, INPUT " ", OUTPUT x-Libre_c02).
    FOR EACH almdrepo OF almcrepo NO-LOCK:
        PUT UNFORMATTED
            almcrepo.codalm '|'
            almcrepo.nroser '|'
            almcrepo.nrodoc '|'
            almcrepo.fchdoc '|'
            almcrepo.almped '|'
            almcrepo.usuario '|'
            almcrepo.fecha '|'
            almcrepo.hora '|'
            almcrepo.fchapr '|'
            almcrepo.horapr '|'
            almcrepo.usrapr '|'
            almcrepo.flgest '|'
            almcrepo.flgsit '|'
            x-glosa '|'
            almcrepo.motreposicion '|'
            almcrepo.crossdocking '|'
            almcrepo.almacenxd '|'
            almdrepo.codmat '|'
            almdrepo.canreq '|'
            almdrepo.canapro '|'
            almdrepo.canate '|'
            almdrepo.cantran '|'
            almdrepo.cangen
            SKIP.
    END.
END.
OUTPUT CLOSE.
