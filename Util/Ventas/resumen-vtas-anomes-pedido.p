DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-fchdoc-1 AS DATE NO-UNDO.
DEF VAR x-fchdoc-2 AS DATE NO-UNDO.
DEF VAR x-codped AS CHAR NO-UNDO.
DEF VAR x-nroped AS CHAR NO-UNDO.
DEF VAR x-nromes AS INT NO-UNDO.
DEF VAR x-periodo AS INT NO-UNDO.
DEF VAR x-candes AS DEC NO-UNDO.
DEF VAR x-implin AS DEC NO-UNDO.
DEF VAR x-TpoCmbVta AS DEC NO-UNDO.

ASSIGN
    x-fchdoc-1 = DATE(01,01,2015)
    /*x-fchdoc-1 = DATE(03,01,2016)*/
    x-fchdoc-2 = DATE(03,31,2016).

DEF TEMP-TABLE detallado LIKE ccbddocu
    FIELD nromes AS INT
    FIELD periodo AS INT
    FIELD codped LIKE ccbcdocu.codped
    FIELD nroped LIKE ccbcdocu.nroped
    INDEX llave01 AS PRIMARY codcia periodo nromes almdes codmat codped nroped
    INDEX llave02 codcia periodo nromes almdes codmat nroped.
DEF TEMP-TABLE resumido LIKE detallado
    FIELD pedidos AS INT.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,
    EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddiv = gn-divi.coddiv
    AND ccbcdocu.fchdoc >= x-fchdoc-1
    AND ccbcdocu.fchdoc <= x-fchdoc-2
    AND ccbcdocu.flgest <> 'A'
    AND LOOKUP(ccbcdocu.tpofac, 'A,S') = 0
    AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL,TCK') > 0,
    EACH ccbddocu OF ccbcdocu NO-LOCK, FIRST almmmatg OF ccbddocu NO-LOCK:
/*     DISPLAY gn-divi.coddiv ccbcdocu.fchdoc ccbcdocu.coddoc ccbcdocu.nrodoc WITH STREAM-IO NO-BOX WIDTH 320. */
/*     PAUSE 0.                                                                                                */
    ASSIGN
        x-codped = ''
        x-nroped = ''
        x-nromes = MONTH(ccbcdocu.fchdoc)
        x-periodo = YEAR(ccbcdocu.fchdoc)
        x-candes = ccbddocu.candes * ccbddocu.factor
        x-implin = ccbddocu.implin - ccbddocu.impdto2.
     IF ccbcdocu.codmon = 2 THEN DO:
         FIND LAST Gn-Tcmb WHERE Gn-Tcmb.Fecha <= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
         IF NOT AVAIL Gn-Tcmb THEN 
             FIND FIRST Gn-Tcmb WHERE Gn-Tcmb.Fecha >= CcbCdocu.FchDoc NO-LOCK NO-ERROR.
         IF AVAIL Gn-Tcmb THEN 
             ASSIGN
                 x-TpoCmbVta = Gn-Tcmb.Venta.
        x-implin = x-implin * x-TpoCmbVta.
     END.
    IF LOOKUP(ccbcdocu.codped, 'PED,P/M') > 0 THEN
        ASSIGN
        x-codped = ccbcdocu.codped
        x-nroped = ccbcdocu.nroped.
    FIND detallado WHERE detallado.codcia = s-codcia
        AND detallado.nromes = x-nromes
        AND detallado.periodo = x-periodo
        AND detallado.almdes = ccbddocu.almdes
        AND detallado.codmat = ccbddocu.codmat
        AND detallado.codped = x-codped
        AND detallado.nroped = x-nroped
        NO-ERROR.
    IF NOT AVAILABLE detallado THEN DO:
        CREATE detallado.
        BUFFER-COPY ccbddocu
            EXCEPT ccbddocu.candes ccbddocu.implin ccbddocu.impdto2
            TO detallado.
    END.
    ASSIGN
        detallado.codcia = s-codcia
        detallado.nromes = x-nromes
        detallado.periodo= x-periodo
        detallado.codped = x-codped
        detallado.nroped = x-nroped
        detallado.candes = detallado.candes + x-candes
        detallado.implin = detallado.implin + x-implin.
END.

/* Resumimos */
FOR EACH detallado NO-LOCK 
    BY detallado.codcia
    BY detallado.periodo
    BY detallado.nromes
    BY detallado.almdes
    BY detallado.codmat:
    FIND resumido WHERE resumido.codcia = detallado.codcia
        AND resumido.periodo = detallado.periodo
        AND resumido.nromes = detallado.nromes
        AND resumido.almdes = detallado.almdes
        AND resumido.codmat = detallado.codmat
        NO-ERROR.
    IF NOT AVAILABLE resumido THEN DO:
        CREATE resumido.
        ASSIGN
            resumido.codcia = detallado.codcia
            resumido.periodo = detallado.periodo
            resumido.nromes = detallado.nromes
            resumido.almdes = detallado.almdes
            resumido.codmat = detallado.codmat.
    END.
    ASSIGN
        resumido.candes = resumido.candes + detallado.candes
        resumido.implin = resumido.implin + detallado.implin.
    IF LOOKUP(detallado.codped, 'PED,P/M') > 0 THEN resumido.pedidos = resumido.pedidos + 1.
END.

OUTPUT TO d:\tmp\ene2015-mar2016.txt.
PUT UNFORMATTED
    'PERIODO|MES|ALMACEN|ARTICULO|DESCRIPCION|UNIDAD|CANTIDAD|IMPORTE|# DE PEDIDOS|LINEA|SUBLINEA'
    SKIP.
FOR EACH resumido NO-LOCK, FIRST almmmatg OF resumido NO-LOCK,
    FIRST Almtfami OF Almmmatg NO-LOCK,
    FIRST Almsfami OF Almmmatg NO-LOCK:
    PUT UNFORMATTED
        resumido.periodo '|'
        resumido.nromes '|'
        resumido.almdes '|'
        resumido.codmat '|'
        almmmatg.desmat '|'
        almmmatg.undstk '|'
        resumido.candes '|'
        resumido.implin '|'
        resumido.pedidos '|'
        almmmatg.codfam ' ' Almtfami.desfam '|'
        almmmatg.subfam ' ' AlmSFami.dessub
        SKIP.
END.
