/*  Objetivo: Renumera las N/C que han .sido impresos en otro correlativo 
    Sintaxis: RUN Carga-Tempo( <Correlativo errado>, <Correlativo correcto>).
*/

def temp-table cdocu like ccbcdocu.
def temp-table ddocu like ccbddocu.
def temp-table dcaja like ccbdmov.

def var x-nroold as char format 'x(9)'.
def var x-nronew as char format 'x(9)'.

def var i-nroold as int format '999999999'.
def var i-nronew as int format '999999999'.
def var ccoddoc as char init 'N/C'.     /* <<<<< OJO <<<<< */

run carga-tempo ('088003792','088003793').
run carga-tempo ('088003793','088003792').

/*
for each cdocu by cdocu.nrodoc:
    display cdocu.nrodoc.
    for each ddocu of cdocu by ddocu.nroitm:
        display ddocu.codmat ddocu.candes.
    end.
end.
*/
for each cdocu:
    create ccbcdocu.
    buffer-copy cdocu to ccbcdocu.
end.
for each ddocu:
    create ccbddocu.
    buffer-copy ddocu to ccbddocu no-error.
end.
for each dcaja:
    create ccbdmov.
    buffer-copy dcaja to ccbdmov no-error.
end.

procedure carga-tempo:

    def input parameter cnroold as char.
    def input parameter cnronew as char.

    find ccbcdocu where ccbcdocu.codcia = 001
        and ccbcdocu.coddoc = ccoddoc
        and ccbcdocu.nrodoc = cnroold.
    create cdocu.
    buffer-copy ccbcdocu to cdocu
        assign cdocu.nrodoc = cnronew.
    for each ccbddocu of ccbcdocu:
        create ddocu.
        buffer-copy ccbddocu to ddocu
            assign ddocu.nrodoc = cdocu.nrodoc.
        delete ccbddocu.
    end.
    for each ccbdmov where ccbdmov.codcia = 001
            and ccbdmov.coddoc = ccbcdocu.coddoc
            and ccbdmov.nrodoc = ccbcdocu.nrodoc:
        create dcaja.
        buffer-copy ccbdmov to dcaja
            assign dcaja.nrodoc = cnronew.
        delete ccbdmov.
    end.            
    delete ccbcdocu.

end procedure.
