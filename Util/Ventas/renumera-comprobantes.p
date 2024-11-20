/*  Objetivo: Renumera los comprobantes que han sido impresos en otro correlativo 
    Sintaxis: RUN Carga-Tempo( <Correlativo errado>, <Correlativo correcto>).
*/
DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.

def temp-table cdocu like ccbcdocu.
def temp-table ddocu like ccbddocu.
def temp-table dcaja like ccbdcaja.

def var x-nroold as char format 'x(9)'.
def var x-nronew as char format 'x(9)'.

def var i-nroold as int format '99999999999'.
def var i-nronew as int format '99999999999'.
/* ***************************************** */
/*                      DICE      DEBE DECIR */
/* ***************************************** */
def var ccoddoc as char init 'BOL'.     /* <<<<< OJO <<<<< */
DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
    run carga-tempo ('76900000001','76900000239') NO-ERROR.
    IF ERROR-STATUS:ERROR = YES THEN UNDO, RETURN ERROR.

    for each cdocu:
        create ccbcdocu.
        buffer-copy cdocu to ccbcdocu.
    end.
    for each ddocu:
        create ccbddocu.
        buffer-copy ddocu to ccbddocu no-error.
    end.
    for each dcaja:
        create ccbdcaja.
        buffer-copy dcaja to ccbdcaja no-error.
    end.

END.
/*

*/
RETURN.


procedure carga-tempo:

    def input parameter cnroold as char.
    def input parameter cnronew as char.

    find ccbcdocu where ccbcdocu.codcia = 001
        and ccbcdocu.coddoc = ccoddoc
        and ccbcdocu.nrodoc = cnroold
        NO-ERROR.
    IF NOT AVAILABLE ccbcdocu THEN DO:
        MESSAGE 'error' ccoddoc cnroold.
        RETURN ERROR.
    END.
    FIND LAST almcmov WHERE almcmov.codcia = 001
        AND almcmov.codref = ccbcdocu.coddoc
        AND almcmov.nroref = ccbcdocu.nrodoc
        NO-ERROR.
    IF AVAILABLE almcmov THEN
        ASSIGN
            almcmov.nroref = cnronew.
    create cdocu.
    buffer-copy ccbcdocu to cdocu
        assign cdocu.nrodoc = cnronew.
    for each ccbddocu of ccbcdocu:
        create ddocu.
        buffer-copy ccbddocu to ddocu
            assign ddocu.nrodoc = cdocu.nrodoc.
        delete ccbddocu.
    end.
    for each ccbdcaja where ccbdcaja.codcia = 001
            and ccbdcaja.codref = ccbcdocu.coddoc
            and ccbdcaja.nroref = ccbcdocu.nrodoc:
        create dcaja.
        buffer-copy ccbdcaja to dcaja
            assign dcaja.nroref = cnronew.
        delete ccbdcaja.
    end.            
    delete ccbcdocu.


end procedure.
