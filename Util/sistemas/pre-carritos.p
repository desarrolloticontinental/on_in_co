DEFINE BUFFER b-faccpedi FOR faccpedi.    
DEFINE BUFFER b-facdpedi FOR facdpedi.    

DEFINE TEMP-TABLE t-faccpedi LIKE faccpedi.

DEFINE VAR x-sec AS INT.
DEFINE VAR x-Items-Pre AS INTE NO-UNDO.
DEFINE VAR x-Items-Cot AS INTE NO-UNDO.
DEFINE VAR x-hay-datos AS LOG.

DEFINE TEMP-TABLE Detalle
    FIELD a_nroped LIKE faccpedi.nroped
    FIELD a_fchped LIKE faccpedi.fchped
    FIELD a_e-mail LIKE faccpedi.e-mail
    FIELD a_items AS INTE
    FIELD a_imptot LIKE faccpedi.imptot
    FIELD b_nroped LIKE faccpedi.nroped
    FIELD b_fchped LIKE faccpedi.fchped
    FIELD b_e-mail LIKE faccpedi.e-mail
    FIELD b_items AS INTE
    FIELD b_imptot LIKE faccpedi.imptot
    FIELD b_porcentaje AS DECI
    .
    
DEFINE TEMP-TABLE Detalle-Print LIKE Detalle.

RUN MASTER-TRANSACTION ('PREMIUM', 'd:\tmp\premiumV2.txt').
RUN MASTER-TRANSACTION ('STANDARD', 'd:\tmp\standardV2.txt').


/* ************************* */
PROCEDURE MASTER-TRANSACTION:
/* ************************* */

    DEF INPUT PARAMETER pTipo AS CHAR.
    DEF INPUT PARAMETER pArchivo AS CHAR.

    DEF VAR x-Coincidencias AS INTE NO-UNDO.
    DEFINE VAR x-correos-exonerados AS CHAR.

    x-correos-exonerados = "pwong@continentalperu.com,gianellacr20@gmail.com,cesarcamus28@gmail.com,pruebas.utilex@gmail.com".

    EMPTY TEMP-TABLE Detalle-Print.

    /* por c/u analizar */
    FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = 1 AND 
        faccpedi.coddiv = '00526' AND
        faccpedi.coddoc = "PLE" AND
        faccpedi.OrderType = pTipo AND
        faccpedi.flgest <> 'A':
        x-Items-Pre = 0.
        FOR EACH facdpedi OF faccpedi NO-LOCK:
            x-Items-Pre = x-Items-Pre + 1.
        END.
        IF x-Items-Pre = 0 THEN NEXT.

        CREATE Detalle.
        ASSIGN
            Detalle.a_nroped = faccpedi.nroped
            Detalle.a_fchped = faccpedi.fchped
            Detalle.a_e-mail = faccpedi.e-mail
            Detalle.a_items  = x-Items-Pre
            Detalle.a_imptot = faccpedi.imptot.
        /* barremos todas las COT a ver cual es mejor */
        EMPTY TEMP-TABLE t-faccpedi.
        FOR EACH b-faccpedi NO-LOCK WHERE b-faccpedi.codcia = 1 AND 
            b-faccpedi.coddiv = '00506' AND
            b-faccpedi.coddoc = 'COT' AND 
            b-faccpedi.flgest <> 'A' AND
            b-faccpedi.fchped >= faccpedi.fchped AND 
            LOOKUP(b-faccpedi.e-mail,x-correos-exonerados) = 0:
            /* que no esté registrado */
            IF CAN-FIND(FIRST Detalle-print WHERE Detalle-print.b_nroped = b-faccpedi.nroped NO-LOCK) THEN NEXT.
            /* buscamos coincidencia de items */
            x-Coincidencias = 0.
            FOR EACH facdpedi OF faccpedi NO-LOCK:  /* PLE */
                IF CAN-FIND(FIRST b-facdpedi OF b-faccpedi WHERE b-facdpedi.codmat = facdpedi.codmat NO-LOCK)
                    THEN DO:
                    x-Coincidencias = x-Coincidencias + 1.
                END.
            END.
            IF x-Coincidencias = 0 THEN NEXT.   /* Siguiente COT */
            CREATE t-faccpedi.
            BUFFER-COPY b-faccpedi TO t-faccpedi.
            ASSIGN  t-faccpedi.libre_d02 = x-Coincidencias
                    t-faccpedi.libre_d01 = x-Coincidencias / x-Items-Pre * 100.
        END.
        /* seleccionamos la mejor */
        x-hay-datos = NO.
        FOR EACH t-faccpedi NO-LOCK BY t-faccpedi.libre_d01 DESC:
            x-Items-Cot = t-faccpedi.libre_d02.
            /*
            FOR EACH facdpedi OF t-faccpedi NO-LOCK:
                x-Items-Cot = x-Items-Cot + 1.
            END.
            */
            ASSIGN
                Detalle.b_nroped = t-faccpedi.nroped
                Detalle.b_fchped = t-faccpedi.fchped
                Detalle.b_e-mail = t-faccpedi.e-mail
                Detalle.b_items  = x-Items-Cot
                Detalle.b_imptot = t-faccpedi.imptot
                Detalle.b_porcentaje = t-faccpedi.libre_d01.
            x-hay-datos = YES.
            LEAVE.
        END.
        IF x-hay-datos = YES THEN DO:
            CREATE Detalle-Print.
            BUFFER-COPY Detalle TO Detalle-Print.
        END.
    END.    
    /* Texto */       

    OUTPUT TO VALUE(pArchivo).

    PUT UNFORMATTED "# PRECARRO|FECHA|CORREO|ITEMS|IMPORTE|# PED COMERCIAL|FECHA|CORREO|ITEMS|IMPORTE|FACTOR APROX" SKIP.

    FOR EACH Detalle-Print NO-LOCK:
        PUT UNFORMATTED
            Detalle-Print.a_nroped '|'
            Detalle-Print.a_fchped '|'
            Detalle-Print.a_e-mail '|'
            Detalle-Print.a_items '|'
            Detalle-Print.a_imptot '|'
            Detalle-Print.b_nroped '|'
            Detalle-Print.b_fchped '|'
            Detalle-Print.b_e-mail '|'
            Detalle-Print.b_items '|'
            Detalle-Print.b_imptot '|'
            Detalle-Print.b_porcentaje
            SKIP.
    END.

    OUTPUT CLOSE.

END PROCEDURE.
