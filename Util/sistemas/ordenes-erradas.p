DEF BUFFER cotizacion FOR faccpedi.
DEF BUFFER pedido FOR faccpedi.
DEF BUFFER orden FOR faccpedi.
DEF VAR f-Estado AS CHAR.
DEF VAR x-HPK AS CHAR.

OUTPUT TO d:\tmp\revisar.txt.
RUN salida ('00015','10015').
RUN salida ('10060','10060').
RUN salida ('10067','10067').
OUTPUT CLOSE.


PROCEDURE salida:
DEF INPUT PARAMETER pcoddiv AS CHAR.
DEF INPUT PARAMETER plista  AS CHAR.

    FOR EACH cotizacion NO-LOCK WHERE cotizacion.codcia = 1
        AND cotizacion.coddiv = pcoddiv
        AND cotizacion.coddoc = 'cot'
        AND cotizacion.libre_c01 = plista
        AND cotizacion.flgest <> 'A'
        AND cotizacion.fchped >= 01/01/2019,
        EACH pedido NO-LOCK WHERE pedido.codcia = 1
        AND pedido.coddoc = 'ped' 
        AND pedido.codref = cotizacion.coddoc
        AND pedido.nroref = cotizacion.nroped
        AND pedido.flgest <> 'a',
        EACH orden NO-LOCK WHERE orden.codcia = 1
        AND orden.coddoc = 'o/d'
        AND orden.codref = pedido.coddoc
        AND orden.nroref = pedido.nroped
        AND orden.flgest <> "A",     /* Pendientes de facturar*/
        EACH facdpedi OF orden NO-LOCK,
        FIRST almmmatg OF facdpedi NO-LOCK,
        FIRST vtalistamay NO-LOCK WHERE vtalistamay.codcia = 1
        AND vtalistamay.coddiv = plista
        AND vtalistamay.codmat = facdpedi.codmat
        AND vtalistamay.CHR__01 <> facdpedi.undvta:
        RUN vta2/p-faccpedi-flgest (orden.flgest, orden.coddoc, OUTPUT f-Estado).
        x-HPK = ''.
        FIND LAST Vtacdocu WHERE Vtacdocu.codcia = 1
            AND Vtacdocu.codped = 'HPK'
            AND Vtacdocu.codref = orden.coddoc
            AND Vtacdocu.nroref = orden.nroped
            NO-LOCK NO-ERROR.
        IF AVAILABLE Vtacdocu THEN x-HPK = Vtacdocu.nroped.

        PUT UNFORMATTED
            pcoddiv             '|'
            plista              '|'
            cotizacion.coddoc   '|'
            cotizacion.nroped   '|'
            cotizacion.codcli   '|'
            cotizacion.nomcli   '|'
            pedido.coddoc       '|'
            pedido.nroped       '|'
            orden.coddoc        '|'
            orden.nroped        '|'
            f-Estado            '|'
            facdpedi.codmat     '|'
            almmmatg.desmat     '|'
            facdpedi.undvta     '|'
            vtalistamay.CHR__01 '|'
            facdpedi.canped     '|'
            x-HPK
            SKIP.
    END.

END PROCEDURE.
