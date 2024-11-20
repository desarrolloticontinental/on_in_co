/* 1ro revisamos los pedidos */
DEF TEMP-TABLE cdespacho LIKE faccpedi.
DEF TEMP-TABLE ddespacho LIKE facdpedi.
DEF TEMP-TABLE cpedido LIKE faccpedi.
DEF TEMP-TABLE dpedido LIKE facdpedi.
DEF TEMP-TABLE ccotiza LIKE faccpedi.
DEF TEMP-TABLE dcotiza LIKE facdpedi.

DEF VAR s-coddiv AS CHAR INIT '10015' NO-UNDO.
DEF VAR s-codcia AS INT  INIT 001     NO-UNDO.
DEF VAR s-codcli AS CHAR NO-UNDO.
DEF VAR s-fchped AS DATE NO-UNDO.


ASSIGN
    s-codcli = ''
    s-fchped = 01/01/2013.

/*RUN Corregimos-atendido-de-ordenes.*/
/*RUN Corregimos-atendido-de-pedidos.*/
/*RUN Corregimos-atendido-de-cotizaciones.*/


PROCEDURE Corregimos-atendido-de-ordenes:
/* ************************************* */

    FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
        AND coddoc = 'o/d'
        AND coddiv = s-coddiv
        AND LOOKUP(flgest, 'P,C') > 0
        AND (s-codcli = '' OR faccpedi.codcli = s-codcli)
        AND fchped >= s-fchped:
        CREATE cdespacho.
        BUFFER-COPY faccpedi TO cdespacho.
        FOR EACH facdpedi OF faccpedi NO-LOCK:
            CREATE ddespacho.
            BUFFER-COPY facdpedi
                TO ddespacho
                ASSIGN
                ddespacho.canate = 0.
        END.
    END.

    FOR EACH cdespacho:
        FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
            AND ccbcdocu.divori = cdespacho.coddiv
            AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
            AND ccbcdocu.libre_c01 = cdespacho.coddoc
            AND ccbcdocu.libre_c02 = cdespacho.nroped
            AND ccbcdocu.flgest <> "A",
            EACH ccbddocu OF ccbcdocu NO-LOCK,
            FIRST ddespacho OF cdespacho WHERE ddespacho.codmat = ccbddocu.codmat:
            ASSIGN
                ddespacho.canate = ddespacho.canate + ccbddocu.candes.
        END.
    END.
    FOR EACH cdespacho NO-LOCK,
        FIRST faccpedi OF cdespacho NO-LOCK,
        EACH ddespacho OF cdespacho NO-LOCK,
        FIRST facdpedi OF faccpedi WHERE facdpedi.codmat = ddespacho.codmat:
        IF facdpedi.canate <> ddespacho.canate THEN DO:
            DISPLAY
                facdpedi.canped FORMAT '->>>,>>>,>>9.99'
                facdpedi.canate FORMAT '->>>,>>>,>>9.99'
                ddespacho.canate  FORMAT '->>>,>>>,>>9.99'
                cdespacho.nroped 
                cdespacho.codref
                cdespacho.nroref
                ddespacho.codmat
                WITH STREAM-IO NO-BOX WIDTH 320.
            PAUSE 0.
            ASSIGN
                facdpedi.canate = ddespacho.canate.
        END.
    END.

END PROCEDURE.


PROCEDURE Corregimos-atendido-de-pedidos:
/* ************************************* */

/* 1ro cargamos los PEDIDOS */
FOR EACH faccpedi WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddoc = "PED"
    AND faccpedi.coddiv = s-coddiv
    AND (s-codcli = '' OR faccpedi.codcli = s-codcli)
    AND faccpedi.flgest <> "A"
    AND faccpedi.fchped >= s-fchped:
    CREATE cpedido.
    BUFFER-COPY faccpedi TO cpedido.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE dpedido.
        BUFFER-COPY facdpedi
            TO dpedido
            ASSIGN 
            dpedido.canate = 0
            dpedido.flgest = "P".
    END.
END.
/* 2do cargamos los despachos */
FOR EACH cpedido:
    /* despachos en tránsito y ya facturados */
    FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
        AND faccpedi.coddiv = cpedido.coddiv
        AND faccpedi.coddoc = "O/D"
        AND faccpedi.codref = cpedido.coddoc
        AND faccpedi.nroref = cpedido.nroped
        AND faccpedi.flgest = "C",  /* Verificamos contra lo facturado */
        EACH facdpedi OF faccpedi NO-LOCK,
        FIRST dpedido OF cpedido WHERE dpedido.codmat = facdpedi.codmat:
        ASSIGN
            dpedido.canate = dpedido.canate + facdpedi.canate.
    END.
    FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
        AND faccpedi.coddiv = cpedido.coddiv
        AND faccpedi.coddoc = "O/D"
        AND faccpedi.codref = cpedido.coddoc
        AND faccpedi.nroref = cpedido.nroped
        AND faccpedi.flgest = "P",      /* Descarga todo */
        EACH facdpedi OF faccpedi NO-LOCK,
        FIRST dpedido OF cpedido WHERE dpedido.codmat = facdpedi.codmat:
        ASSIGN
            dpedido.canate = dpedido.canate + facdpedi.canped.
    END.
    /* despachos suspendidos (por revisar) */
    FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
        AND faccpedi.coddiv = cpedido.coddiv
        AND faccpedi.coddoc = "O/D"
        AND faccpedi.codref = cpedido.coddoc
        AND faccpedi.nroref = cpedido.nroped
        AND faccpedi.flgest = "S",      /* Restablece lo no atendido */
        EACH facdpedi OF faccpedi NO-LOCK,
        FIRST dpedido OF cpedido WHERE dpedido.codmat = facdpedi.codmat:
        ASSIGN
            dpedido.canate = dpedido.canate - (facdpedi.canped - facdpedi.canate).
    END.
END.

/* revisemos informacion */
FOR EACH cpedido,
    FIRST faccpedi OF cpedido NO-LOCK,
    EACH dpedido OF cpedido,
    FIRST facdpedi OF faccpedi WHERE facdpedi.codmat = dpedido.codmat:
    IF facdpedi.canate <> dpedido.canate THEN DO:
        DISPLAY
            facdpedi.canped FORMAT '->>>,>>>,>>9.99'
            facdpedi.canate FORMAT '->>>,>>>,>>9.99'
            dpedido.canate  FORMAT '->>>,>>>,>>9.99'
            cpedido.nroped 
            cpedido.codref
            cpedido.nroref
            dpedido.codmat
            WITH STREAM-IO NO-BOX WIDTH 320.
        PAUSE 0.
        facdpedi.canate = dpedido.canate.
    END.
END.

END PROCEDURE.


PROCEDURE Corregimos-atendido-de-cotizaciones:
/* ****************************************** */
    /* 1ro cargamos las COTIZACIONES */
    FOR EACH gn-divi NO-LOCK WHERE codcia = s-codcia AND coddiv = s-coddiv:
        FOR EACH faccpedi WHERE faccpedi.codcia = s-codcia
            AND faccpedi.coddoc = "COT"
            AND faccpedi.coddiv = gn-divi.coddiv
            AND (s-codcli = '' OR faccpedi.codcli = s-codcli)
            AND faccpedi.flgest <> "A"
            AND faccpedi.fchped >= s-fchped:
            CREATE ccotiza.
            BUFFER-COPY faccpedi TO ccotiza.
            FOR EACH facdpedi OF faccpedi NO-LOCK:
                CREATE dcotiza.
                BUFFER-COPY facdpedi
                    TO dcotiza
                    ASSIGN 
                    dcotiza.canate = 0
                    dcotiza.flgest = "P".
            END.
        END.                                      
    END.
    /* 2do cargamos los pedidos */
    FOR EACH ccotiza:
        FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
            AND faccpedi.coddiv = ccotiza.coddiv
            AND faccpedi.coddoc = "PED"
            AND faccpedi.codref = ccotiza.coddoc
            AND faccpedi.nroref = ccotiza.nroped
            AND LOOKUP(faccpedi.flgest, "A,R,S,E") = 0,
            EACH facdpedi OF faccpedi NO-LOCK,
            FIRST dcotiza OF ccotiza WHERE dcotiza.codmat = facdpedi.codmat:
            IF faccpedi.flgest = "C"    /* Contra los facturado */
                THEN
                ASSIGN
                dcotiza.canate = dcotiza.canate + facdpedi.canate.
            ELSE
                ASSIGN
                    dcotiza.canate = dcotiza.canate + facdpedi.canped.
        END.
        FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
            AND faccpedi.coddiv = ccotiza.coddiv
            AND faccpedi.coddoc = "PED"
            AND faccpedi.codref = ccotiza.coddoc
            AND faccpedi.nroref = ccotiza.nroped
            AND LOOKUP(faccpedi.flgest, "E,S") > 0,
            EACH facdpedi OF faccpedi NO-LOCK,
            FIRST dcotiza OF ccotiza WHERE dcotiza.codmat = facdpedi.codmat:
            ASSIGN
                dcotiza.canate = dcotiza.canate - (facdpedi.canped - facdpedi.canate).
        END.
    END.

    /* revisemos informacion */
    FOR EACH ccotiza,
        FIRST faccpedi OF ccotiza NO-LOCK,
        EACH dcotiza OF ccotiza,
        FIRST facdpedi OF faccpedi WHERE facdpedi.codmat = dcotiza.codmat:
        IF facdpedi.canate <> dcotiza.canate THEN DO:
            DISPLAY
                ccotiza.coddoc
                ccotiza.nroped 
                /*ccotiza.nomcli*/
                dcotiza.codmat
                facdpedi.canped FORMAT '->>>,>>>,>>9.99'
                facdpedi.canate FORMAT '->>>,>>>,>>9.99'
                dcotiza.canate  FORMAT '->>>,>>>,>>9.99'
                WITH STREAM-IO NO-BOX WIDTH 320.
            PAUSE 0.
            facdpedi.canate = dcotiza.canate.
        END.
    END.

END PROCEDURE.
