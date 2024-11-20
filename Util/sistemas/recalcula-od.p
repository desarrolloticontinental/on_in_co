DEF VAR x-articulo-ICBPer AS CHAR INIT '099268'.
DEF VAR s-nrodec AS INT INIT 4.
DEF VAR s-porigv AS DEC INIT 18.
DEF VAR s-codcia AS INT INIT 001.
DEF VAR s-codcli AS CHAR.
DEF VAR cl-codcia AS INT INIT 0.
DEF VAR s-cmpbnte AS CHAR.

FIND faccpedi WHERE codcia = 1 and coddoc = 'o/d' and nroped = '124064652'.
faccpedi.flgigv = YES.
faccpedi.porigv = 18.
s-codcli = faccpedi.codcli.
s-cmpbnte = FacCPedi.Cmpbnte.

DEF BUFFER pedido FOR facdpedi.
FOR EACH facdpedi OF faccpedi, FIRST pedido NO-LOCK WHERE pedido.codcia = 1
    AND pedido.coddoc = faccpedi.codref
    AND pedido.nroped = faccpedi.nroref
    AND pedido.codmat = facdpedi.codmat:
    facdpedi.preuni = pedido.preuni.
END.

FOR EACH facdpedi OF faccpedi, FIRST almmmatg OF facdpedi NO-LOCK, FIRST almsfami OF almmmatg NO-LOCK:
    facdpedi.aftigv = almmmatg.aftigv.
    facdpedi.libre_d02 = 0.
    {vtagn/calculodetallemayorcredito.i &Tabla="Facdpedi"}
END.
{vtagn/totales-cotizacion-unificada.i &Cabecera="Faccpedi" &Detalle="Facdpedi"}


