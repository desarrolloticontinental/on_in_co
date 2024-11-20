
DEFINE VAR lFecha AS DATE.
DEFINE VAR lAlm AS CHAR.
DEFINE VAR s-codcia AS INT INIT 1.

DEFINE TEMP-TABLE tt-saldos
    FIELDS  t-codmat    AS CHAR FORMAT 'x(6)'   COLUMN-LABEL "Codigo"
    FIELDS  t-desmat    AS CHAR FORMAT 'x(60)'  COLUMN-LABEL "Descripcion"
    FIELDS  t-codalm    AS CHAR FORMAT 'x(5)'   COLUMN-LABEL "Almacen"
    FIELDS  t-cantidad  AS DEC  FORMAT '->>,>>>,>>>,>>9.99' COLUMN-LABEL "Stock"
    FIELDS  t-undstk    AS CHAR FORMAT 'x(5)'   COLUMN-LABEL 'U.M.'
    /*FIELDS  t-costo     AS DEC  FORMAT '->>,>>>,>>>,>>9.999999' COLUMN-LABEL "Costo  Kardex almacen"*/
    FIELDS  t-costoge   AS DEC  FORMAT '->>,>>>,>>>,>>9.999999' COLUMN-LABEL "Costo Unitario Kardex General"
    INDEX idx01 IS PRIMARY t-codalm t-codmat.


lFecha = 12/31/2016.    /* mm/dd/aaaa */
FOR EACH almmmatg WHERE almmmatg.codcia = 1 NO-LOCK,
    EACH almmmate OF almmmatg NO-LOCK:
/*     IF (almmmate.codalm <> '11T' AND almmmate.codalm <> '99') THEN DO: */
    
        FIND LAST almstkal WHERE almstkal.codcia = almmmate.codcia AND 
                                almstkal.codalm = almmmate.codalm AND
                                almstkal.codmat = almmmatg.codmat AND 
                                almstkal.fecha <= lFecha NO-LOCK NO-ERROR.
        IF AVAILABLE almstkal AND almstkal.stkact <> 0 THEN DO:
            /* Costo promedio Kardex General */
            FIND LAST almstkge WHERE almstkge.codcia = 1 AND 
                                        almstkge.codmat = almmmatg.codmat AND 
                                        almstkge.fecha <= lFecha NO-LOCK NO-ERROR.
            CREATE tt-saldos.
                ASSIGN tt-saldos.t-codmat   = almmmatg.codmat
                        tt-saldos.t-desmat  = almmmatg.desmat
                        tt-saldos.t-codalm    = almmmate.codalm
                        tt-saldos.t-cantidad  = almstkal.stkact
                        tt-saldos.t-undstk  = almmmatg.undstk
                        /*tt-saldos.t-costo = almstkal.ctouni*/
                        tt-saldos.t-costoge = IF(AVAILABLE almstkge) THEN almstkge.ctouni ELSE 0.
        END.
/*     END. */
END.

DEFINE VAR hProc AS HANDLE NO-UNDO.

RUN lib\Tools-to-excel PERSISTENT SET hProc.

def var c-csv-file as char no-undo.
def var c-xls-file as char no-undo. /* will contain the XLS file path created */

c-xls-file = 'd:\tmp\Saldos-a-Dic2016.xlsx'.

run pi-crea-archivo-csv IN hProc (input  buffer tt-saldos:handle,
                        /*input  session:temp-directory + "file"*/ c-xls-file,
                        output c-csv-file) .

run pi-crea-archivo-xls  IN hProc (input  buffer tt-saldos:handle,
                        input  c-csv-file,
                        output c-xls-file) .

DELETE PROCEDURE hProc.

