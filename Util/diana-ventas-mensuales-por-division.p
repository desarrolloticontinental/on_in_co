def temp-table t-clie like gn-clie.
def var s-codcia as int init 001 no-undo.
def var s-coddiv as char init '00000' no-undo.
def var desdef as date no-undo.
def var hastaf as date no-undo.
def var x-linea as char format 'x(20)'.

def temp-table detalle
    field coddiv like gn-divi.coddiv
    field codmat like almmmatg.codmat
    field desmat like almmmatg.desmat
    field undbas like almmmatg.undbas
    field desmar like almmmatg.desmar
    field codfam like almmmatg.codfam
    field subfam like almmmatg.subfam
    field cantidad as dec extent 12
    field ventas   as dec extent 12.

assign
    desdef = 01/01/08
    hastaf = 04/30/08.
    
for each gn-divi no-lock where codcia = s-codcia:
    display gn-divi.coddiv.
    pause 0.
    for each EvtArti NO-LOCK WHERE Evtarti.CodCia = S-CODCIA
            AND   Evtarti.CodDiv = gn-divi.Coddiv
            AND  (Evtarti.Nrofch >= INTEGER(STRING(YEAR(DesdeF),"9999") + STRING(MONTH(DesdeF),"99"))
            AND   Evtarti.Nrofch <= INTEGER(STRING(YEAR(HastaF),"9999") + STRING(MONTH(HastaF),"99")) )
            USE-INDEX LLAVE01,
            FIRST Almmmatg WHERE Almmmatg.Codcia = S-CODCIA AND Almmmatg.CodMat = Evtarti.Codmat:
        find detalle where detalle.coddiv = gn-divi.coddiv
            and detalle.codmat = evtarti.codmat
            exclusive-lock no-error.
        if not available detalle then do:
            create detalle.
            buffer-copy almmmatg to detalle
                assign
                    detalle.coddiv = gn-divi.coddiv.
        end.
        assign
            detalle.cantidad[EvtArti.Codmes] = detalle.cantidad[EvtArti.Codmes] + EvtArti.CanxMes
            detalle.ventas[EvtArti.Codmes]   = detalle.ventas[EvtArti.Codmes]   + EvtArti.VtaxMesMe.
    END.    
end.

output to c:\tmp\diana2008.txt.
for each detalle no-lock:
    display
        detalle.coddiv
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.codfam
        detalle.subfam
        detalle.undbas
        detalle.cantidad
        with stream-io no-box width 320.
end.    
output close.
