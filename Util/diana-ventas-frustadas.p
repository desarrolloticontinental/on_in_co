def buffer pedido for faccpedi.
def buffer despacho for faccpedi.

def temp-table detalle
    field codmat like almmmatg.codmat column-label 'Codigo'
    field desmat like almmmatg.desmat format 'x(40)'
    field desmar like almmmatg.desmar
    field codfam like almmmatg.codfam
    field subfam like almmmatg.subfam 
    field undbas like almmmatg.undbas column-label 'Und'
    field ped as dec extent 12 format '>>>,>>>' 
    field des as dec extent 12 format '>>>,>>>'.

def var s-codcia as int init 001 no-undo.
def var s-coddiv as char init '00000' no-undo.
def var desdef as date no-undo.
def var hastaf as date no-undo.

assign
    desdef = 01/01/08
    hastaf = 04/30/08.

/* cotizaciones */
for each faccpedi no-lock where codcia = s-codcia
        and coddoc = 'cot'
        and coddiv = s-coddiv
        and fchped >= desdef
        and fchped <= hastaf
        and flgest <> 'A':
    /* solo las que tenga pedidos */
    find first pedido where pedido.codcia = s-codcia
            and pedido.coddiv = s-coddiv
            and pedido.coddoc = 'ped'
            and pedido.nroref = faccpedi.nroped
            and pedido.flgest <> 'a'
            no-lock no-error.
    if not available pedido then next.
    display faccpedi.coddoc faccpedi.nroped faccpedi.fchped.
    pause 0.
    for each facdpedi of faccpedi no-lock,
            first almmmatg of facdpedi no-lock:
        find detalle where detalle.codmat = almmmatg.codmat
            exclusive-lock no-error.
        if not available detalle then do:
            create detalle.
            buffer-copy almmmatg to detalle.
        end.
        assign
            detalle.ped[month(faccpedi.fchped)] = detalle.ped[month(faccpedi.fchped)] +
                                                    (facdpedi.canped * facdpedi.factor).
    end.
    /* pedidos por cotizacion */
    for each pedido no-lock where pedido.codcia = s-codcia
            and pedido.coddiv = s-coddiv
            and pedido.coddoc = 'ped'
            and pedido.nroref = faccpedi.nroped
            and pedido.flgest <> 'a':
        /* despachos */
        for each despacho no-lock where despacho.codcia = s-codcia
                and despacho.coddiv = s-coddiv
                and despacho.coddoc = 'o/d'
                and despacho.flgest <> 'a'
                and despacho.nroref = pedido.nroped:
            /* guias de remision */
            for each ccbcdocu no-lock where ccbcdocu.codcia = s-codcia
                    and ccbcdocu.coddoc = 'g/r'
                    and ccbcdocu.coddiv = s-coddiv
                    and ccbcdocu.flgest = 'F'
                    and ccbcdocu.codped = despacho.coddoc
                    and ccbcdocu.nroped = despacho.nroped,
                    each ccbddocu of ccbcdocu no-lock,
                    first almmmatg of ccbddocu no-lock:
                find detalle where detalle.codmat = almmmatg.codmat
                    exclusive-lock no-error.
                if not available detalle then do:
                    create detalle.
                    buffer-copy almmmatg to detalle.
                end.
                assign
                    detalle.des[month(faccpedi.fchped)] = detalle.des[month(faccpedi.fchped)] +
                                                            (ccbddocu.candes * ccbddocu.factor).
            end.
        end.            
    end.
end.
output to c:\tmp\frustado2008.txt.
for each detalle no-lock:
    display detalle with stream-io no-box width 320.
end.
output close.
