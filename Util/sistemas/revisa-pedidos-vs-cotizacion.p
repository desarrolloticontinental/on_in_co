DEF BUFFER cotizacion FOR faccpedi.
DEF BUFFER det_cotizacion FOR facdpedi.
DEF BUFFER pedido FOR faccpedi.
DEF BUFFER det_pedido FOR facdpedi.
DEF VAR s-codcia AS INTE INIT 001.

OUTPUT TO d:\revisarAnterior.txt.
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia AND gn-divi.canalventa = 'FER' /*gn-divi.coddiv = '20018'*/:
    FOR EACH cotizacion NO-LOCK WHERE cotizacion.codcia = s-codcia AND
        cotizacion.coddiv = gn-divi.coddiv AND
        cotizacion.coddoc = 'COT' AND
        cotizacion.flgest <> 'A' AND
        (cotizacion.fchped >= 10/01/2022 AND cotizacion.fchped <= 03/31/2023 ) :
        FOR EACH pedido NO-LOCK WHERE pedido.codcia = s-codcia AND
            pedido.coddoc = 'PED' AND
            pedido.codref = cotizacion.coddoc AND
            pedido.nroref = cotizacion.nroped AND
            pedido.flgest <> 'A':
            FOR EACH det_pedido OF pedido NO-LOCK:
                FIND FIRST det_cotizacion OF cotizacion WHERE
                    det_cotizacion.codmat = det_pedido.codmat NO-LOCK NO-ERROR.
                IF AVAILABLE det_cotizacion AND det_pedido.canped > det_cotizacion.canped
                    THEN
                    DISPLAY
                    det_pedido.coddoc 
                    det_pedido.nroped 
                    pedido.codref
                    pedido.nroref
                    pedido.codorigen
                    pedido.nroorigen
                    det_pedido.codmat 
                    det_pedido.canped COLUMN-LABEL 'PEDIDO'
                    det_cotizacion.canped COLUMN-LABEL 'COTIZADO'
                    det_pedido.fchped COLUMN-LABEL 'Emision PEDIDO'
                    det_cotizacion.fchped COLUMN-LABEL 'Emision COTIZADO'
                    WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320.
            END.
        END.
    END.
END.
