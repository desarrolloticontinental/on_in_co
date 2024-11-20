DEF TEMP-TABLE detalle 
    FIELD fecha   AS DATE   FORMAT '99/99/9999' LABEL 'Fecha'
    FIELD codalm  AS CHAR   FORMAT 'x(3)'   LABEL 'Almacen'
    FIELD desalm  AS CHAR   FORMAT 'x(40)'  LABEL 'Descripcion'
    FIELD imptot AS DEC     FORMAT '(>>>,>>>,>>>,>>9.99)'   LABEL 'Valor'.

DEF VAR mes AS INT NO-UNDO.
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-periodo AS INT INIT 2013 NO-UNDO.
DEF VAR x-fchini AS DATE NO-UNDO.
DEF VAR x-fchfin AS DATE NO-UNDO.

FOR EACH almmmatg NO-LOCK WHERE almmmatg.codcia = s-codcia
    AND LOOKUP(almmmatg.codfam, '000,001,002,003,004,005,007,008,010,011,012,013,014') > 0:
    DISPLAY almmmatg.codmat. PAUSE 0.
    /* buscamos saldos por mes */
    FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia AND almacen.codalm <> '11T':
        DO mes = 1 TO 12:
            RUN src/bin/_dateif (mes, s-periodo, OUTPUT x-fchini, OUTPUT x-fchfin).
            FIND LAST almstkal WHERE almstkal.codcia = almmmatg.codcia
                AND almstkal.codalm = almacen.codalm
                AND almstkal.codmat = almmmatg.codmat
                AND almstkal.fecha <= x-fchfin
                NO-LOCK NO-ERROR.
            IF AVAILABLE almstkal THEN DO:
                FIND detalle WHERE detalle.fecha = x-fchfin AND
                    detalle.codalm = almacen.codalm
                    NO-ERROR.
                IF NOT AVAILABLE detalle THEN CREATE detalle.
                ASSIGN
                    detalle.fecha  = x-fchfin
                    detalle.codalm = almacen.codalm.
                FIND LAST almstkge WHERE almstkge.codcia = s-codcia
                    AND almstkge.codmat = almmmatg.codmat
                    AND almstkge.fecha <= x-fchfin
                    NO-LOCK NO-ERROR.
                IF AVAILABLE almstkge THEN
                    ASSIGN
                    detalle.imptot = detalle.imptot + (AlmStkal.StkAct * AlmStkge.CtoUni).
            END.
        END.
    END.
END.

OUTPUT TO c:\tmp\stocksxalmacen.txt.
FOR EACH detalle NO-LOCK.
    DISPLAY detalle.fecha detalle.codalm detalle.desalm detalle.imptot
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

