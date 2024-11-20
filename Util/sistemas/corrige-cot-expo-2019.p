DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-tpoped AS CHAR INIT 'E'.
DEF NEW SHARED VAR s-fmapgo AS CHAR.
DEF NEW SHARED VAR s-nrodec AS INT INIT 4.
DEF NEW SHARED VAR s-porigv AS DEC.
DEF NEW SHARED VAR s-codcli AS CHAR.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.
DEF NEW SHARED VAR s-cmpbnte AS CHAR.
DEF NEW SHARED VAR s-codmon AS INT.

DEFINE NEW SHARED TEMP-TABLE ITEM LIKE FacDPedi
    FIELD cRowid AS ROWID.

&SCOPED-DEFINE precio-venta-general vtagn/precio-venta-general-v01.p

DEF BUFFER PEDIDO FOR Faccpedi.
DEF BUFFER ORDEN  FOR Faccpedi.
DEF BUFFER BPEDI  FOR Facdpedi.

DEF NEW SHARED VAR s-coddiv AS CHAR INIT '10067'.
DEF NEW SHARED VAR pCodDiv  AS CHAR INIT '10067'.

OUTPUT TO d:\tmp\corregidos1067.txt.
FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddiv = s-coddiv
    AND coddoc = 'cot'
    AND fchped >= DATE(01,01,2019)
    AND libre_c01 = pcoddiv
    WITH FRAME fReport:
    s-fmapgo = Faccpedi.fmapgo.
    s-porigv = Faccpedi.porigv.
    s-codcli = Faccpedi.codcli.
    s-cmpbnte = Faccpedi.cmpbnte.
    s-codmon = Faccpedi.codmon.
    EMPTY TEMP-TABLE ITEM.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE ITEM.
        BUFFER-COPY facdpedi TO ITEM ASSIGN ITEM.cRowid = ROWID(Facdpedi).
    END.
    RUN recalcular-precios.
    DETALLE:
    FOR EACH ITEM NO-LOCK, FIRST Facdpedi EXCLUSIVE-LOCK WHERE ROWID(Facdpedi) = ITEM.cRowid
        AND Facdpedi.undvta <> ITEM.undvta:
        /* Que no esté facturado */
        FOR EACH PEDIDO NO-LOCK WHERE PEDIDO.codcia = Faccpedi.codcia AND
            PEDIDO.coddoc = 'PED' AND
            PEDIDO.codref = Faccpedi.coddoc AND
            PEDIDO.nroref = Faccpedi.nroped AND
            PEDIDO.flgest <> "A",
            EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia AND
            Ccbcdocu.codped = PEDIDO.coddoc AND
            Ccbcdocu.nroped = PEDIDO.nroped AND
            LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL') > 0 AND
            Ccbcdocu.flgest <> 'A',
            EACH Ccbddocu OF Ccbcdocu NO-LOCK WHERE Ccbddocu.codmat = Facdpedi.codmat:
            NEXT Detalle.
        END.
        DISPLAY faccpedi.coddoc faccpedi.nroped faccpedi.fchped 
            faccpedi.codref faccpedi.nroref
            facdpedi.codmat facdpedi.undvta ITEM.undvta
            facdpedi.canped facdpedi.preuni ITEM.preuni facdpedi.implin ITEM.implin
            WITH FRAME fReport STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
        /* CORREGIMOS COTIZACION */
        ASSIGN Facdpedi.UndVta = ITEM.UndVta Facdpedi.Factor = 1.
        /* CORREGIMOS PEDIDOS */
        FOR EACH PEDIDO NO-LOCK WHERE PEDIDO.codcia = Faccpedi.codcia AND
            PEDIDO.coddoc = 'PED' AND
            PEDIDO.codref = Faccpedi.coddoc AND
            PEDIDO.nroref = Faccpedi.nroped AND
            PEDIDO.flgest <> "A",
            EACH BPEDI OF PEDIDO EXCLUSIVE-LOCK WHERE BPEDI.codmat = Facdpedi.codmat:
            DISPLAY PEDIDO.coddoc PEDIDO.nroped PEDIDO.fchped 
                PEDIDO.codref PEDIDO.nroref
                BPEDI.codmat BPEDI.undvta ITEM.undvta
                BPEDI.canped BPEDI.preuni ITEM.preuni BPEDI.implin ITEM.implin
                WITH FRAME fReport STREAM-IO NO-BOX WIDTH 320.
            PAUSE 0.
            ASSIGN BPEDI.UndVta = ITEM.UndVta BPEDI.Factor = 1.
        END.
        /* CORREGIMOS ORDENES DE DESPACHO */
        FOR EACH PEDIDO NO-LOCK WHERE PEDIDO.codcia = Faccpedi.codcia AND
            PEDIDO.coddoc = 'PED' AND
            PEDIDO.codref = Faccpedi.coddoc AND
            PEDIDO.nroref = Faccpedi.nroped AND
            PEDIDO.flgest <> "A",
            EACH ORDEN NO-LOCK WHERE ORDEN.codcia = PEDIDO.codcia AND
            ORDEN.coddoc = 'O/D' AND
            ORDEN.codref = PEDIDO.coddoc AND
            ORDEN.nroref = PEDIDO.nroped AND
            ORDEN.flgest <> 'A',
            EACH BPEDI OF ORDEN EXCLUSIVE-LOCK WHERE BPEDI.codmat = Facdpedi.codmat:
            DISPLAY ORDEN.coddoc ORDEN.nroped ORDEN.fchped 
                ORDEN.codref ORDEN.nroref
                BPEDI.codmat BPEDI.undvta ITEM.undvta
                BPEDI.canped BPEDI.preuni ITEM.preuni BPEDI.implin ITEM.implin
                WITH FRAME fReport STREAM-IO NO-BOX WIDTH 320.
            PAUSE 0.
            ASSIGN BPEDI.UndVta = ITEM.UndVta BPEDI.Factor = 1.
        END.
    END.
END.
OUTPUT CLOSE.
RETURN.

PROCEDURE recalcular-precios:


    RUN Recalcular-Precio-TpoPed (s-TpoPed).


    END PROCEDURE.

    PROCEDURE Recalcular-Precio-TpoPed:


        DEF INPUT PARAMETER pTpoPed AS CHAR.

        {vtagn/recalcular-cot-gral-v01.i &pTpoPed=pTpoPed}


    END PROCEDURE.

