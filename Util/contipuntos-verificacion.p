def var s-codcia as int init 001 no-undo.
def var x-nrocard like gn-card.nrocard no-undo.
def var x-fecha-1 as date no-undo.
def var x-fecha-2 as date no-undo.
def var x-puntos as int no-undo.
def var x-linea as char format 'x(100)' no-undo.
def var s-puntos as int no-undo.
def buffer b-cdocu for ccbcdocu.
assign
    x-fecha-1 = 01/10/2007
    x-fecha-2 = 04/30/2007.
    
input from c:\tmp\contipuntos.prn.
output to c:\tmp\errores.txt.
repeat:
    import unformatted x-linea.
    x-nrocard = substring(x-linea,1,6).
    s-puntos = integer(substring(x-linea,61,13)).
    run contipuntos.
    if x-puntos <> s-puntos then 
    display x-nrocard x-puntos s-puntos.
end.
input close.
output close.

procedure contipuntos:

  x-puntos = 0.
  FOR EACH Ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND LOOKUP(TRIM(ccbcdocu.coddiv), '00001,00002,00003,00008,00014') > 0
        AND LOOKUP(TRIM(ccbcdocu.coddoc), 'FAC,BOL') > 0
        AND ccbcdocu.nrocard <> ''
        AND ccbcdocu.nrocard BEGINS x-nrocard
        AND ccbcdocu.fchdoc >= x-fecha-1
        AND ccbcdocu.fchdoc <= x-fecha-2
        AND ccbcdocu.flgest <> 'A',
        FIRST GN-CARD WHERE gn-card.NroCard = ccbcdocu.nrocard NO-LOCK:
    IF ccbcdocu.puntos <= 0 THEN NEXT.
    x-puntos = x-puntos + ccbcdocu.puntos.
    /* buscamos la nota de credito por devolucion */
    FOR EACH B-CDOCU WHERE B-CDOCU.codcia = s-codcia
            AND B-CDOCU.coddoc = 'N/C'
            AND B-CDOCU.codref = Ccbcdocu.coddoc
            AND B-CDOCU.nroref = Ccbcdocu.nrodoc
            AND B-CDOCU.flgest <> 'A'
            NO-LOCK:
        x-puntos = x-puntos - B-CDOCU.Puntos.
    END.        
  END.

end procedure.

