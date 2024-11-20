DEF TEMP-TABLE x-temp
    FIELD evento AS CHAR FORMAT 'x(15)'
    FIELD nroped AS CHAR FORMAT 'x(9)'
    FIELD almacen AS CHAR FORMAT 'x(10)'.

DEF VAR x-linea AS CHAR FORMAT 'x(100)'.
INPUT FROM c:\tmp\despachos.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea <> '' THEN DO:
        CREATE x-temp.
        ASSIGN
            x-temp.evento = SUBSTRING(x-linea,1,15)
            x-temp.nroped = SUBSTRING(x-linea,18,9)
            x-temp.almacen = SUBSTRING(x-linea,34,10).
    END.
END.
INPUT CLOSE.

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00015'.
DEF VAR FILL-IN-Fecha-1 AS DATE.
DEF VAR FILL-IN-Fecha-2 AS DATE.

ASSIGN
    FILL-IN-Fecha-1 = 10/25/10
    FILL-IN-Fecha-2 = 01/31/11.
DEF TEMP-TABLE detalle
    FIELD codcia AS INT
    FIELD codmat LIKE almmmatg.codmat
    FIELD canped LIKE facdpedi.canped
    FIELD penate LIKE facdpedi.canped
    FIELD implin LIKE facdpedi.implin
    FIELD semana AS CHAR FORMAT 'x(30)'
    FIELD evento AS CHAR FORMAT 'x(15)'
    FIELD almacen AS CHAR FORMAT 'x(10)'
    INDEX Llave01 AS PRIMARY codcia codmat semana evento almacen.

DEF STREAM REPORTE.

DEF VAR x-Evento AS CHAR FORMAT 'x(15)'.
DEF VAR x-Almacen AS CHAR FORMAT 'x(10)'.
DEF VAR x-Semana LIKE detalle.semana NO-UNDO.
DEF VAR x-Propios AS INT NO-UNDO.
DEF VAR x-Total AS INT NO-UNDO.

ESTADISTICAS:
FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND coddoc = "COT"
    AND fchped >= FILL-IN-Fecha-1 
    AND fchped <= FILL-IN-Fecha-2
    AND flgest = 'P':
    IF Faccpedi.fchped < 01/06/2011 OR Faccpedi.UsrSac = "*"
        THEN x-Evento = "EXPO OCT 2010".
    ELSE x-Evento = "EXPO ENE 2011".
    ASSIGN
        x-Almacen = 'NN'.
    FIND FIRST x-temp WHERE x-temp.nroped = faccpedi.nroped
        NO-LOCK NO-ERROR.
    IF AVAILABLE x-temp THEN
        ASSIGN
        x-Evento = x-temp.evento
        x-Almacen = x-temp.almacen.

    FOR EACH facdpedi OF faccpedi NO-LOCK:
        FIND FIRST Evtsemanas WHERE EvtSemanas.CodCia = s-codcia
            AND faccpedi.fchent >= EvtSemanas.FecIni
            AND faccpedi.fchent <= EvtSemanas.FecFin
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Evtsemanas THEN DO:
            x-Semana = 'FUERA DE RANGO: ' + STRING(faccpedi.fchped,'99/99/9999').
        END.
        ELSE DO:
            x-Semana = 'Del ' + STRING(EvtSemanas.FecIni, '99/99/9999') +
                ' al ' + STRING(EvtSemanas.FecFin, '99/99/9999').
        END.
        FIND detalle WHERE detalle.codcia = faccpedi.codcia
            AND detalle.codmat = facdpedi.codmat
            AND detalle.semana = x-semana
            AND detalle.evento = x-evento
            AND detalle.almacen = x-almacen
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            ASSIGN
                detalle.codcia = s-codcia
                detalle.codmat = facdpedi.codmat
                detalle.semana = x-semana
                detalle.evento = x-evento
                detalle.almacen = x-almacen.
        END.
        ASSIGN
            detalle.canped = detalle.canped + ( facdpedi.canped * facdpedi.factor )
            detalle.penate = detalle.penate + ( ( facdpedi.canped - facdpedi.canate ) * facdpedi.factor)
            detalle.implin = detalle.implin + facdpedi.implin.
    END.
END.


DEF VAR x-Llave AS CHAR FORMAT 'x(800)' NO-UNDO.
DEF VAR x-Titulo AS CHAR FORMAT 'x(800)' NO-UNDO.
DEF VAR x-NomPro LIKE gn-prov.nompro NO-UNDO.
DEF VAR x-Archivo AS CHAR NO-UNDO.

x-Archivo = SESSION:TEMP-DIRECTORY + STRING(NEXT-VALUE(sec-arc,integral)) + ".txt".

OUTPUT STREAM REPORTE TO c:\tmp\despacjos.txt.
x-Titulo = 'Evento|Almacen|Semana|Producto|Marca|Linea|Sublinea|Proveedor|Pedida|Por atender|Unidad|Importe|'.
x-Titulo = REPLACE(x-Titulo, '|', CHR(9) ).
PUT STREAM REPORTE x-Titulo SKIP.
FOR EACH detalle NO-LOCK,
    FIRST almmmatg OF detalle NO-LOCK,
    FIRST almtfami OF almmmatg NO-LOCK,
    FIRST almsfami OF almmmatg NO-LOCK:
    x-nompro = 'NN'.
    FIND gn-prov WHERE gn-prov.codcia = 000
        AND gn-prov.codpro = almmmatg.codpr1
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-prov THEN x-nompro = gn-prov.nompro.
    x-Llave = 
        detalle.evento + '|' +
        detalle.almacen + '|' +
        detalle.semana + '|' +
        detalle.codmat + ' ' + almmmatg.desmat + '|' +
        almmmatg.desmar + '|' +
        almmmatg.codfam + ' ' + Almtfami.desfam + '|' +
        almmmatg.subfam + ' ' + AlmSFami.dessub + '|' +
        almmmatg.codpr1 + ' ' + x-nompro + '|' + 
        STRING (detalle.canped, '>>>,>>>,>>9.99') + '|' +
        STRING (detalle.penate, '->>>,>>>,>>9.99') + '|' +
        Almmmatg.UndBas + '|' +
        STRING (detalle.implin, '>>>,>>>,>>9.99') + '|'.
    x-Llave = REPLACE ( x-Llave, '|', CHR(9) ).
    PUT STREAM REPORTE x-LLave SKIP.
END.
OUTPUT STREAM REPORTE CLOSE.
/*
/* CARGAMOS EL EXCEL */
RUN lib/filetext-to-excel(x-Archivo, 'Despachos', YES).
*/
