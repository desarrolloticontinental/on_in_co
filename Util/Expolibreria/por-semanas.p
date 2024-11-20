DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00015'.
DEF VAR FILL-IN-Fecha-1 AS DATE.
DEF VAR FILL-IN-Fecha-2 AS DATE.

ASSIGN
    FILL-IN-Fecha-1 = 01/06/11
    FILL-IN-Fecha-2 = 01/08/11.
DEF TEMP-TABLE detalle
    FIELD codcia AS INT
    FIELD codmat LIKE almmmatg.codmat
    FIELD canped LIKE facdpedi.canped
    FIELD penate LIKE facdpedi.canped
    FIELD implin LIKE facdpedi.implin
    FIELD semana AS CHAR FORMAT 'x(30)'
    INDEX Llave01 AS PRIMARY codcia codmat semana.

DEF STREAM REPORTE.

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
    /* SOLO LOS QUE HAN COMPRADO PRODUCTOS PROPIOS */
/*     FOR EACH facdpedi OF faccpedi NO-LOCK,                                    */
/*         FIRST almmmatg OF facdpedi NO-LOCK:                                   */
/*         IF LOOKUP(almmmatg.codfam, '010,011,012') = 0 THEN NEXT ESTADISTICAS. */
/*     END.                                                                      */
    /* TODO MENOS LO ANTERIOR */
    ASSIGN
        x-Propios = 0
        x-Total = 0.
    FOR EACH facdpedi OF faccpedi NO-LOCK,
        FIRST almmmatg OF facdpedi NO-LOCK:
        IF LOOKUP(almmmatg.codfam, '010,011,012') = 0 THEN x-Propios = x-Propios + 1.
        x-Total = x-Total + 1.
    END.
    IF x-Propios = x-Total THEN NEXT ESTADISTICAS.

    FOR EACH facdpedi OF faccpedi NO-LOCK:
        FIND FIRST Evtsemanas WHERE EvtSemanas.CodCia = s-codcia
            AND faccpedi.fchent >= EvtSemanas.FecIni
            AND faccpedi.fchent <= EvtSemanas.FecFin
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Evtsemanas THEN DO:
            x-Semana = 'FUERA DE RANGO:'.
        END.
        ELSE DO:
            x-Semana = 'Del ' + STRING(EvtSemanas.FecIni, '99/99/9999') +
                ' al ' + STRING(EvtSemanas.FecFin, '99/99/9999').
        END.
        FIND detalle WHERE detalle.codcia = faccpedi.codcia
            AND detalle.codmat = facdpedi.codmat
            AND detalle.semana = x-semana
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE detalle THEN DO:
            CREATE detalle.
            ASSIGN
                detalle.codcia = s-codcia
                detalle.codmat = facdpedi.codmat
                detalle.semana = x-semana.
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

OUTPUT STREAM REPORTE TO VALUE (x-Archivo).
x-Titulo = 'Semana|Producto|Marca|Linea|Sublinea|Proveedor|Pedida|Por atender|Unidad|Importe|'.
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
    x-Llave = detalle.semana + '|' +
        detalle.codmat + ' ' + almmmatg.desmat + '|' +
        almmmatg.desmar + '|' +
        almmmatg.codfam + ' ' + Almtfami.desfam + '|' +
        almmmatg.subfam + ' ' + AlmSFami.dessub + '|' +
        almmmatg.codpr1 + ' ' + x-nompro + '|' + 
        STRING (detalle.canped, '>>>,>>>,>>9.99') + '|' +
        STRING (detalle.penate, '>>>,>>>,>>9.99') + '|' +
        Almmmatg.UndBas + '|' +
        STRING (detalle.implin, '>>>,>>>,>>9.99') + '|'.
    x-Llave = REPLACE ( x-Llave, '|', CHR(9) ).
    PUT STREAM REPORTE x-LLave SKIP.
END.
OUTPUT STREAM REPORTE CLOSE.

/* CARGAMOS EL EXCEL */
RUN lib/filetext-to-excel(x-Archivo, 'Despachos', YES).
