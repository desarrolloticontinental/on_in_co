DEF BUFFER pedido FOR faccpedi.
DEF BUFFER cotizacion FOR faccpedi.

    DEF VAR x-Articulo-ICBPer AS CHAR INIT '099268'.

FOR EACH ccbcdocu EXCLUSIVE-LOCK WHERE codcia = 1
    AND coddoc = 'fai'
    AND fchdoc >= 01/01/2020
    AND codcli = '20100047218'
    AND flgest = "P",
    FIRST pedido NO-LOCK WHERE pedido.codcia = 1
    AND pedido.coddoc = ccbcdocu.codped
    AND pedido.nroped = ccbcdocu.nroped,
    FIRST cotizacion NO-LOCK WHERE cotizacion.codcia = 1
    AND cotizacion.coddoc = pedido.codref
    AND cotizacion.nroped = pedido.nroref
    AND cotizacion.deliverygroup = '6':
    FOR EACH ccbddocu OF ccbcdocu EXCLUSIVE-LOCK,
        FIRST facdpedi OF cotizacion NO-LOCK WHERE facdpedi.codmat = ccbddocu.codmat:
        ASSIGN
            ccbddocu.impdto = FacDPedi.ImpDto 
            ccbddocu.impdto2 = FacDPedi.ImpDto2 
            ccbddocu.impigv = FacDPedi.ImpIgv 
            ccbddocu.impisc = FacDPedi.ImpIsc 
            ccbddocu.implin = FacDPedi.ImpLin 
            ccbddocu.prebas = FacDPedi.PreBas 
            ccbddocu.preuni = FacDPedi.PreUni.
    END.
    RUN Totales.
END.

PROCEDURE Totales:

    /* Rutina General */
    {vtagn/i-total-factura.i &Cabecera="ccbcdocu" &Detalle="ccbddocu"}

END PROCEDURE.
