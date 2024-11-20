/* ACTIVAR NOTA DE PEDIDO */

DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-coddiv AS CHAR INIT '00015'.
DEF VAR s-nroped AS CHAR INIT '015111567'.

DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.

FIND faccpedi WHERE codcia = s-codcia
    AND coddoc = 'PED'
    AND nroped = s-nroped
    NO-ERROR.
IF NOT AVAILABLE faccpedi OR flgest <> 'R' THEN DO:
    MESSAGE 'NO se pudo activar el pedido'
        VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.
FIND b-cpedi WHERE b-cpedi.codcia = s-codcia
    AND b-cpedi.coddoc = 'COT'
    AND b-cpedi.nroped = Faccpedi.nroref.
FOR EACH facdpedi OF faccpedi NO-LOCK:
    FIND b-dpedi WHERE b-dpedi.codcia = s-codcia
        AND b-dpedi.coddoc = b-cpedi.coddoc
        AND b-dpedi.nroped = b-cpedi.nroped
        AND b-dpedi.codmat = facdpedi.codmat.
    b-dpedi.canate = b-dpedi.canate + facdpedi.canped.
END.
b-cpedi.flgest = 'P'.
FOR EACH vtadtrkped WHERE vtadtrkped.codcia = s-codcia
    AND vtadtrkped.coddoc = faccpedi.coddoc
    AND vtadtrkped.nroped = faccpedi.nroped:
    vtadtrkped.flgsit = 'P'.
    FIND FIRST vtactrkped OF vtadtrkped.
    vtactrkped.flgsit = 'P'.
END.
faccpedi.flgest = 'P'.
faccpedi.glosa = ''.
