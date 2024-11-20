OUTPUT TO d:\revisar.txt.
SELECT 
    pedido.coddoc,
    pedido.nroped,
    cotizacion.coddoc,
    cotizacion.nroped,
    det_pedido.codmat,
    det_pedido.canped COLUMN-LABEL 'PEDIDO', 
    det_cotizacion.canped COLUMN-LABEL 'COTIZADO'
    FROM faccpedi  cotizacion, faccpedi pedido, facdpedi det_pedido,
    facdpedi det_cotizacion 
    WHERE cotizacion.codcia = 1 AND
    cotizacion.coddiv = '20018' AND
    cotizacion.coddoc = 'cot' AND
    cotizacion.fchped >= DATE(01,01,2023) AND
    cotizacion.flgest <> 'A' AND
    pedido.codcia = 1 AND
    pedido.coddoc = 'ped' AND
    pedido.codref = cotizacion.coddoc AND
    pedido.nroref = cotizacion.nroped AND
    pedido.codcli = cotizacion.codcli AND
    pedido.flgest <> 'A' AND
    det_pedido.codcia = pedido.codcia AND
    det_pedido.coddoc = pedido.coddoc AND
    det_pedido.nroped = pedido.nroped AND
    det_cotizacion.codcia = cotizacion.codcia AND
    det_cotizacion.coddoc = cotizacion.coddoc AND
    det_cotizacion.nroped = cotizacion.nroped AND
    det_cotizacion.codmat = det_pedido.codmat AND
    det_pedido.canped > det_cotizacion.canped
    WITH STREAM-IO NO-BOX NO-UNDERLINE WIDTH 320
    .
    




