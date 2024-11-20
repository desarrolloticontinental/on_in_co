def temp-table t-clie like gn-clie.
def var s-codcia as int init 001 no-undo.
def var s-coddiv as char init '00000' no-undo.
def var desdef as date no-undo.
def var hastaf as date no-undo.
def var x-linea as char format 'x(20)'.

input from c:\tmp\clientes.prn.
repeat:
    import unformatted x-linea.
    create t-clie.
    assign
        t-clie.codcia = 000
        t-clie.codcli = substring(x-linea,1,11).
end.
input close.


def temp-table detalle
    field codcli like gn-clie.codcli
    field nomcli like gn-clie.nomcli
    field codmat like almmmatg.codmat
    field desmat like almmmatg.desmat
    field undbas like almmmatg.undbas
    field cantidad as dec extent 12
    field ventas   as dec extent 12.

assign
    desdef = 01/01/05
    hastaf = 01/31/05.
    
for each t-clie no-lock where codcli <> '',first gn-clie of t-clie no-lock:
    FOR EACH Evtclarti NO-LOCK WHERE Evtclarti.CodCia = S-CODCIA
            AND   Evtclarti.CodDiv = s-Coddiv
            AND   Evtclarti.CodCli = t-clie.codcli
            AND  (Evtclarti.Nrofch >= INTEGER(STRING(YEAR(DesdeF),"9999") + STRING(MONTH(DesdeF),"99"))
            AND   Evtclarti.Nrofch <= INTEGER(STRING(YEAR(HastaF),"9999") + STRING(MONTH(HastaF),"99")) )
            USE-INDEX LLAVE01,
            FIRST Almmmatg WHERE Almmmatg.Codcia = S-CODCIA AND Almmmatg.CodMat = Evtclarti.Codmat:
        find detalle where detalle.codcli = t-clie.codcli
            and detalle.codmat = evtclarti.codmat
            exclusive-lock no-error.
        if not available detalle then do:
            create detalle.
            buffer-copy gn-clie to detalle
                assign
                    detalle.codmat = almmmatg.codmat
                    detalle.desmat = almmmatg.desmat
                    detalle.undbas = almmmatg.undbas.
        end.
        assign
            detalle.cantidad[EvtClArti.Codmes] = detalle.cantidad[EvtClArti.Codmes] + EvtClArti.CanxMes
            detalle.ventas[EvtClArti.Codmes]   = detalle.ventas[EvtClArti.Codmes]   + EvtClArti.VtaxMesMe.
            

    END.    
end.

for each detalle no-lock:
    display
        detalle.codcli
        detalle.nomcli
        detalle.codmat
        detalle.desmat
        detalle.undbas
        detalle.cantidad
        detalle.ventas
        with stream-io no-box width 320.
end.    
    
