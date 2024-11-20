
DEFINE BUFFER b-vtacdocu FOR vtacdocu.                                                  
DEFINE BUFFER b-vtaddocu FOR vtaddocu.

DEFINE VAR x-conteo AS INT.
DEFINE VAR x-correlativo AS INT.

DEFINE TEMP-TABLE tvtacdocu LIKE vtacdocu.
DEFINE TEMP-TABLE tvtaddocu LIKE vtaddocu.

DISABLE TRIGGERS FOR LOAD OF vtacdocu.
DISABLE TRIGGERS FOR LOAD OF vtaddocu.
                                                  
FOR EACH vtacdocu WHERE vtacdocu.codcia = 1 and vtacdocu.coddiv = '20018' and 
            vtacdocu.codped = 'PET' and vtacdocu.usuario = 'vta-218' and 
            vtacdocu.fchped >= 01/09/2022 AND substring(vtacdocu.nroped,1,3) = '015' NO-LOCK:
    
    EMPTY TEMP-TABLE tvtacdocu.
    EMPTY TEMP-TABLE tvtaddocu.

    x-correlativo = x-correlativo + 1.

    FOR EACH vtaddocu WHERE vtaddocu.codcia = vtacdocu.codcia AND
                            vtaddocu.coddiv = vtacdocu.coddiv AND
                            vtaddocu.codped = vtacdocu.codped AND
                            vtaddocu.nroped = vtacdocu.nroped NO-LOCK:

        /* Lo llevamos el detalle al temporal */
        CREATE tvtaddocu.
        BUFFER-COPY vtaddocu TO tvtaddocu.
        ASSIGN tvtaddocu.nroped = '218' + STRING(x-correlativo,"999999").

        /*
        FIND FIRST b-vtaddocu WHERE rowid(b-vtaddocu) = ROWID(vtaddocu) EXCLUSIVE-LOC NO-ERROR.
        IF AVAILABLE b-vtaddocu THEN DO:
            ASSIGN b-vtaddocu.coddiv = '20018'.
        END.
        RELEASE b-vtaddocu NO-ERROR.
        */
    END.

    /* Lo llevamos la cabecera al temporal */
    CREATE tvtacdocu.
    BUFFER-COPY vtacdocu TO tvtacdocu.
    ASSIGN tvtacdocu.nroped = '218' + STRING(x-correlativo,"999999").

    /* Lo llevamos el detalle de temporal a la Base de datos */
    FOR EACH tvtaddocu NO-LOCK:
        CREATE b-vtaddocu.
        BUFFER-COPY tvtaddocu TO b-vtaddocu.
    END.

    /* Lo llevamos la cabecera de temporal a la Base de datos */
    CREATE b-vtacdocu.
    BUFFER-COPY tvtacdocu TO b-vtacdocu.

    RELEASE b-vtacdocu.
    RELEASE b-vtaddocu.

    /*
    FIND FIRST b-vtacdocu WHERE rowid(b-vtacdocu) = ROWID(vtacdocu) EXCLUSIVE-LOC NO-ERROR.
    IF AVAILABLE b-vtacdocu THEN DO:
        ASSIGN b-vtacdocu.coddiv = '20018'
                b-vtacdocu.usuario = 'VTA-218'.
    END.
    RELEASE b-vtacdocu NO-ERROR.
    */

    x-conteo = x-conteo + 1.
END.

MESSAGE "Registros " x-conteo SKIP
        "Numeros " x-conteo.
