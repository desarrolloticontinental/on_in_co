def temp-table detalle
    field codcia as int
    field codfam like almmmatg.codfam
    field vta00 as dec format '->>>,>>>,>>9.99'
    field vta01 as dec format '->>>,>>>,>>9.99'
    field vta02 as dec format '->>>,>>>,>>9.99'
    field vta03 as dec format '->>>,>>>,>>9.99'
    field vta04 as dec format '->>>,>>>,>>9.99'
    field vta05 as dec format '->>>,>>>,>>9.99'
    field vta08 as dec format '->>>,>>>,>>9.99'
    field vta11 as dec format '->>>,>>>,>>9.99'
    field vta12 as dec format '->>>,>>>,>>9.99'
    field vta14 as dec format '->>>,>>>,>>9.99'
    field vta15 as dec format '->>>,>>>,>>9.99'
    field vta16 as dec format '->>>,>>>,>>9.99'.
    
    
for each gn-divi where codcia = 001 no-lock:
    for each evtline where codcia = 001
            and coddiv = gn-divi.coddiv
            and nrofch >= 200601 
            and nrofch <= 200604
            no-lock:
        find detalle where detalle.codcia = evtline.codcia
            and detalle.codfam = evtline.codfam
            exclusive-lock no-error.
        if not available detalle then create detalle.
        assign
            detalle.codcia = evtline.codcia
            detalle.codfam = evtline.codfam.
        case gn-divi.coddiv:
            when '00000' then detalle.vta00 = detalle.vta00 + EvtLine.VtaxMesMn.
            when '00001' then detalle.vta01 = detalle.vta01 + EvtLine.VtaxMesMn.
            when '00002' then detalle.vta02 = detalle.vta02 + EvtLine.VtaxMesMn.
            when '00003' then detalle.vta03 = detalle.vta03 + EvtLine.VtaxMesMn.
            when '00004' then detalle.vta04 = detalle.vta04 + EvtLine.VtaxMesMn.
            when '00005' then detalle.vta05 = detalle.vta05 + EvtLine.VtaxMesMn.
            when '00008' then detalle.vta08 = detalle.vta08 + EvtLine.VtaxMesMn.
            when '00011' then detalle.vta11 = detalle.vta11 + EvtLine.VtaxMesMn.
            when '00012' then detalle.vta12 = detalle.vta12 + EvtLine.VtaxMesMn.
            when '00014' then detalle.vta14 = detalle.vta14 + EvtLine.VtaxMesMn.
            when '00015' then detalle.vta15 = detalle.vta15 + EvtLine.VtaxMesMn.
            when '00016' then detalle.vta16 = detalle.vta16 + EvtLine.VtaxMesMn.
        end case.                        
    end.
end.

output to c:\tmp\lineaacumulado.txt.
for each detalle:
    display
        detalle.codfam
        detalle.vta00
        detalle.vta01
        detalle.vta02
        detalle.vta03
        detalle.vta04
        detalle.vta05
        detalle.vta08
        detalle.vta11
        detalle.vta12
        detalle.vta14
        detalle.vta15
        detalle.vta16
        with stream-io no-box width 320.
end.
output close.
                
