/* EXPORTAR LA INFORMACION RELEVANTE DEL SERVIDOR UTILEX A ARCHIVOS TEXTO (*.d) */

DEF VAR s-codcia AS INTE INIT 001 NO-UNDO.
DEF VAR x-fecha AS DATE NO-UNDO.

DEF STREAM s-faccpedi.
DEF STREAM s-facdpedi.
DEF STREAM s-faccorre.
DEF STREAM s-gn-clie.
DEF STREAM s-gn-cliecyg.
DEF STREAM s-gn-clied.
DEF STREAM s-ccbccaja.
DEF STREAM s-ccbdcaja.
DEF STREAM s-ccbcmov.
DEF STREAM s-ccbdmov.
DEF STREAM s-ccbcdocu.
DEF STREAM s-ccbddocu.
DEF STREAM s-vtadtick.
DEF STREAM s-felogfac.
DEF STREAM s-felogerr.
DEF STREAM s-almcmov.
DEF STREAM s-almdmov.

/* DEFINE VARIABLE a AS INT64. */
/* a = ETIME(yes).             */

x-fecha = DATE(04,06,2023).

RUN uno.
RUN DOS.
RUN tres.
RUN cuatro.
RUN cinco.
RUN seis.
RUN siete.

/* MESSAGE (ETIME / 1000) 'segundos' VIEW-AS ALERT-BOX INFORMATION. */

PROCEDURE uno:
/* ********** */

OUTPUT STREAM s-faccpedi TO faccpedi.d.
OUTPUT STREAM s-facdpedi TO facdpedi.d.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
    FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia AND
        faccpedi.coddiv = gn-divi.coddiv AND
        faccpedi.fchped >= x-fecha:
        EXPORT STREAM s-faccpedi faccpedi.
        FOR EACH facdpedi OF faccpedi NO-LOCK:
            EXPORT STREAM s-facdpedi facdpedi.
        END.
    END.
END.
OUTPUT STREAM s-faccpedi CLOSE.
OUTPUT STREAM s-facdpedi CLOSE.

END PROCEDURE.


PROCEDURE DOS:
/* ********** */

OUTPUT STREAM s-faccorre TO faccorre.d.

FOR EACH faccorre NO-LOCK WHERE faccorre.codcia = 1:
    EXPORT STREAM s-faccorre faccorre.
END.
OUTPUT STREAM s-faccorre CLOSE.

END PROCEDURE.

PROCEDURE tres:
/* ********** */

OUTPUT STREAM s-gn-clie TO gn-clie.d.
OUTPUT STREAM s-gn-cliecyg TO gn-cliecyg.d.
OUTPUT STREAM s-gn-clied TO gn-clied.d.

FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = 0 AND
    gn-clie.fching >= x-fecha:
    EXPORT STREAM s-gn-clie gn-clie.
    FOR EACH gn-cliecyg OF gn-clie NO-LOCK:
        EXPORT STREAM s-gn-cliecyg gn-cliecyg.
    END.
    FOR EACH gn-clied OF gn-clie NO-LOCK:
        EXPORT STREAM s-gn-clied gn-clied.
    END.
END.
OUTPUT STREAM s-gn-clie CLOSE.
OUTPUT STREAM s-gn-cliecyg CLOSE.
OUTPUT STREAM s-gn-clied CLOSE.

END PROCEDURE.

PROCEDURE cuatro:
/* ********** */

OUTPUT STREAM s-ccbccaja TO ccbccaja.d.
OUTPUT STREAM s-ccbdcaja TO ccbdcaja.d.
OUTPUT STREAM s-ccbcmov TO ccbcmov.d.
OUTPUT STREAM s-ccbdmov TO ccbdmov.d.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
    FOR EACH ccbccaja NO-LOCK WHERE ccbccaja.codcia = s-codcia AND
        ccbccaja.coddiv = gn-divi.coddiv AND
        ccbccaja.fchdoc >= x-fecha:
        EXPORT STREAM s-ccbccaja ccbccaja.
        FOR EACH ccbdcaja OF ccbccaja NO-LOCK:
            EXPORT STREAM s-ccbdcaja ccbdcaja.
        END.
        FOR EACH ccbcmov NO-LOCK WHERE ccbcmov.codcia = s-codcia AND
            ccbcmov.codref = ccbccaja.coddoc AND
            ccbcmov.nroref = ccbccaja.nrodoc:
            EXPORT STREAM s-ccbcmov ccbcmov.
        END.
        FOR EACH ccbdmov NO-LOCK WHERE ccbdmov.codcia = s-codcia AND
            ccbdmov.codref = ccbccaja.coddoc AND
            ccbdmov.nroref = ccbccaja.nrodoc:
            EXPORT STREAM s-ccbdmov ccbdmov.
        END.
    END.
END.
OUTPUT STREAM s-ccbccaja CLOSE.
OUTPUT STREAM s-ccbdcaja CLOSE.
OUTPUT STREAM s-ccbcmov CLOSE.
OUTPUT STREAM s-ccbdmov CLOSE.

END PROCEDURE.

PROCEDURE cinco:
/* ********** */

OUTPUT STREAM s-ccbcdocu TO ccbcdocu.d.
OUTPUT STREAM s-ccbddocu TO ccbddocu.d.
OUTPUT STREAM s-felogfac TO felogcomprobantes.d.
OUTPUT STREAM s-felogerr TO felogerrores.d.

FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia AND
        ccbcdocu.coddiv = gn-divi.coddiv AND
        ccbcdocu.fchdoc >= x-fecha:
        EXPORT STREAM s-ccbcdocu ccbcdocu.
        FOR EACH ccbddocu OF ccbcdocu NO-LOCK:
            EXPORT STREAM s-ccbddocu ccbddocu.
        END.
        FOR EACH felogcomprobantes OF ccbcdocu NO-LOCK:
            EXPORT STREAM s-felogfac felogcomprobantes.
        END.
        FOR EACH felogerrores OF ccbcdocu NO-LOCK:
            EXPORT STREAM s-felogerr felogerrores.
        END.
    END.
END.
OUTPUT STREAM s-ccbcdocu CLOSE.
OUTPUT STREAM s-ccbddocu CLOSE.
OUTPUT STREAM s-felogfac CLOSE.
OUTPUT STREAM s-felogerr CLOSE.

END PROCEDURE.

PROCEDURE seis:
/* ********** */

OUTPUT STREAM s-vtadtick TO vtadtickets.d.

FOR EACH vtadtickets NO-LOCK WHERE vtadtickets.codcia = s-codcia AND
    DATE(vtadtickets.fecha) >= x-fecha:
    EXPORT STREAM s-vtadtick vtadtickets.
END.
OUTPUT STREAM s-vtadtick CLOSE.

END PROCEDURE.


PROCEDURE siete:
/* ************ */

OUTPUT STREAM s-almcmov TO almcmov.d.
OUTPUT STREAM s-almdmov TO almdmov.d.

FOR EACH almacen NO-LOCK WHERE almacen.codcia = s-codcia:
    FOR EACH almcmov NO-LOCK WHERE almcmov.codcia = s-codcia AND
        almcmov.codalm = almacen.codalm AND
        almcmov.fchdoc >= x-fecha:
        EXPORT STREAM s-almcmov almcmov.
        FOR EACH almdmov OF almcmov NO-LOCK:
            EXPORT STREAM s-almdmov almdmov.
        END.
    END.
END.
OUTPUT STREAM s-almcmov CLOSE.
OUTPUT STREAM s-almdmov CLOSE.

END PROCEDURE.

