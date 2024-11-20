def var s-codcia as int init 001 no-undo.
def var desdef as date no-undo.
def var hastaf as date no-undo.
 
def temp-table detalle
    field codmat like almmmatg.codmat
    field desmat like almmmatg.desmat
    field undbas like almmmatg.undbas
    FIELD desmar LIKE almmmatg.desmar
    field cantidad as dec extent 12
    field ventas   as dec extent 12
    FIELD codpro LIKE gn-prov.codpro
    FIELD nompro LIKE gn-prov.nompro.

assign
    desdef = 01/01/09
    hastaf = TODAY.
    
for each gn-divi no-lock where codcia = s-codcia AND coddiv = '00015':
    FOR EACH Evtarti NO-LOCK WHERE Evtarti.CodCia = S-CODCIA
            AND   Evtarti.CodDiv = gn-divi.Coddiv
            AND  (Evtarti.Nrofch >= INTEGER(STRING(YEAR(DesdeF),"9999") + STRING(MONTH(DesdeF),"99"))
            AND   Evtarti.Nrofch <= INTEGER(STRING(YEAR(HastaF),"9999") + STRING(MONTH(HastaF),"99")) )
            USE-INDEX LLAVE01,
            FIRST Almmmatg WHERE Almmmatg.Codcia = S-CODCIA AND Almmmatg.CodMat = Evtarti.Codmat:
        find detalle where detalle.codmat = Evtarti.codmat
            exclusive-lock no-error.
        if not available detalle then do:
            create detalle.
                assign
                    detalle.codmat = almmmatg.codmat
                    detalle.desmat = almmmatg.desmat
                    detalle.undbas = almmmatg.undbas
                    detalle.desmar = almmmatg.desmar
                    detalle.codpro = almmmatg.codpr1.
            FIND gn-prov WHERE gn-prov.codcia = 0
                AND gn-prov.codpro = almmmatg.codpr1
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-prov THEN detalle.nompro = gn-prov.nompro.
        end.
        assign
            detalle.cantidad[Evtarti.Codmes] = detalle.cantidad[Evtarti.Codmes] + Evtarti.CanxMes
            detalle.ventas[Evtarti.Codmes]   = detalle.ventas[Evtarti.Codmes]   + Evtarti.VtaxMesMe.
    END.    
end.

output to m:\tmp\expolibreria.txt.
for each detalle no-lock:
    display
        detalle.codmat
        detalle.desmat
        detalle.desmar
        detalle.undbas
        detalle.codpro
        detalle.nompro
        detalle.cantidad[1] COLUMN-LABEL 'Enero'
        detalle.cantidad[2] COLUMN-LABEL 'Febrero'
/*         detalle.ventas */
        with stream-io no-box width 320.
end.    
output close.
