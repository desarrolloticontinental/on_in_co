&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 4.88
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DISABLE TRIGGERS FOR LOAD OF faccpedi.
DISABLE TRIGGERS FOR LOAD OF facdpedi.

/* Proceso que verifica los pedidos versus lo facturado */
DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR x-fchped AS DATE NO-UNDO.

DEF BUFFER pedido FOR faccpedi.
DEF BUFFER detalle FOR facdpedi.
    DEF BUFFER ccredi FOR ccbcdocu.
    DEF BUFFER dcredi FOR ccbddocu.

DEF TEMP-TABLE t-pedido LIKE faccpedi
    FIELD prowid AS ROWID.
DEF TEMP-TABLE t-detalle LIKE facdpedi
    FIELD prowid AS ROWID.
DEF TEMP-TABLE t-cotizado LIKE facdpedi
    FIELD prowid AS ROWID.

ASSIGN
    x-fchped = TODAY - (3 * 30).
ASSIGN
    x-fchped = x-fchped - DAY(x-fchped) + 1.
/* CARGAMOS LAS COTIZACIONES CON ATENCIONES EN CERO */
PUT "Cargamos cotizaciones con atencion 0 " DATETIME(TODAY, MTIME) SKIP.
FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
    AND faccpedi.coddoc = 'COT' 
    AND faccpedi.nroped = '069006942' /*'069004620'*/:
    /* ******* */
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE t-cotizado.
        BUFFER-COPY facdpedi TO t-cotizado 
            ASSIGN t-cotizado.prowid = ROWID(facdpedi) t-cotizado.canate = 0.
    END.
    /* CARGAMOS LOS PEDIDOS RELACIONADOS CON LAS COTIZACIONES CON ATENCIONES EN CERO */
    FOR EACH pedido NO-LOCK WHERE pedido.codcia = s-codcia
        /*AND pedido.coddiv = faccpedi.coddiv*/
        AND pedido.codref = faccpedi.coddoc
        AND pedido.nroref = faccpedi.nroped
        AND pedido.coddoc = 'PED'
        AND pedido.fchped >= faccpedi.fchped
        AND LOOKUP(pedido.flgest, 'A,R') = 0:
        CREATE t-pedido.
        BUFFER-COPY pedido TO t-pedido ASSIGN t-pedido.prowid = ROWID(pedido).
        FOR EACH detalle OF pedido NO-LOCK:
            CREATE t-detalle.
            BUFFER-COPY detalle TO t-detalle 
                ASSIGN t-detalle.prowid = ROWID(detalle) t-detalle.canate = 0.
        END.
    END.
END.

/* PEDIDOS QUE AUN NO HAN SIDO APROBADO => CARGAMOS ATENCIONES DE LAS COTIZACIONES */
PUT "Cargamos atenciones a las cotizaciones " DATETIME(TODAY, MTIME) SKIP.
FOR EACH t-cotizado:
    FOR EACH t-pedido WHERE t-pedido.codcia = s-codcia
        AND t-pedido.codref = t-cotizado.coddoc
        AND t-pedido.nroref = t-cotizado.nroped
        AND t-pedido.coddoc = 'PED'
        AND LOOKUP(t-pedido.flgest, 'C,S,E') = 0,
        FIRST t-detalle OF t-pedido WHERE t-detalle.codmat = t-cotizado.codmat:
            ASSIGN t-cotizado.canate = t-cotizado.canate + t-detalle.canped.
    END.
END.
/* LIMPIAMOS LO YA PROCESADO */
FOR EACH t-pedido WHERE LOOKUP(t-pedido.flgest, 'C,S,E') = 0:
    FOR EACH t-detalle OF t-pedido:
        DELETE t-detalle.
    END.
    DELETE t-pedido.
END.

/* CARGAMOS EN LOS PEDIDOS LAS CANTIDADES FACTURADAS */
PUT "Cargamos lo facturado al pedido " DATETIME(TODAY, MTIME) SKIP.
FOR EACH t-pedido, EACH t-detalle OF t-pedido:
    FOR EACH ccbcdocu NO-LOCK WHERE ccbcdocu.codcia = s-codcia
        AND ccbcdocu.fchdoc >= t-pedido.fchped
        AND ccbcdocu.codped = t-pedido.coddoc
        AND ccbcdocu.nroped = t-pedido.nroped
        AND LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
        AND ccbcdocu.flgest <> 'A',
        EACH ccbddocu OF ccbcdocu NO-LOCK WHERE ccbddocu.codmat = t-detalle.codmat:
        ASSIGN t-detalle.canate = t-detalle.canate + ccbddocu.candes.
        FOR EACH ccredi NO-LOCK WHERE ccredi.codcia = s-codcia
            AND ccredi.coddoc = 'N/C'
            AND ccredi.codref = ccbcdocu.coddoc
            AND ccredi.nroref = ccbcdocu.nrodoc
            AND ccredi.cndcre = 'D'
            AND ccredi.flgest <> 'A',
            EACH dcredi OF ccredi NO-LOCK WHERE dcredi.codmat = ccbddocu.codmat:
            ASSIGN t-detalle.canate = t-detalle.canate - dcredi.candes.
        END.
    END.
END.

/* CARGAMOS EN LOS PEDIDOS LAS ORDENES DE DESPACHO EN TRAMITE (AUN NO FACTURADAS
    O PARCIALMENTE FACTURADAS) */
PUT "Cargamos ordenes de despacho en tramite " DATETIME(TODAY, MTIME) SKIP.
FOR EACH t-pedido:
    FOR EACH faccpedi NO-LOCK WHERE faccpedi.codcia = s-codcia
        /*AND faccpedi.coddiv = t-pedido.coddiv*/
        AND faccpedi.codref = t-pedido.coddoc
        AND faccpedi.nroref = t-pedido.nroped
        AND faccpedi.fchped >= t-pedido.fchped
        AND faccpedi.coddoc = 'O/D'
        AND faccpedi.flgest = 'P',
        EACH facdpedi OF faccpedi NO-LOCK WHERE facdpedi.canate = 0:
        FIND t-detalle OF t-pedido WHERE t-detalle.codmat = facdpedi.codmat NO-ERROR.
        IF AVAILABLE t-detalle THEN ASSIGN t-detalle.canate = t-detalle.canate + facdpedi.canped.
    END.                                                            
END.
/* ACTUALIZAMOS LA CANTIDAD ATENDIDA EN LAS COTIZACIONES */
PUT "Actualizamos lo atendido a las cotizaciones " DATETIME(TODAY, MTIME) SKIP.
FOR EACH t-cotizado, FIRST faccpedi OF t-cotizado NO-LOCK:
    FOR EACH t-pedido NO-LOCK WHERE t-pedido.codref = faccpedi.coddoc
        AND t-pedido.nroref = faccpedi.nroped,
        EACH t-detalle OF t-pedido NO-LOCK WHERE t-detalle.codmat = t-cotizado.codmat:
        ASSIGN t-cotizado.canate = t-cotizado.canate + t-detalle.canate.
    END.
END.
/* BUSCAMOS LAS DIFERENCIAS */
FOR EACH t-cotizado, FIRST facdpedi NO-LOCK WHERE ROWID(facdpedi) = t-cotizado.prowid:
    IF t-cotizado.canate > t-cotizado.canped THEN t-cotizado.canate = t-cotizado.canped.
    IF t-cotizado.canate <> facdpedi.canate THEN
    DISPLAY x-fchped t-cotizado.coddiv t-cotizado.coddoc t-cotizado.nroped t-cotizado.codmat t-cotizado.canped t-cotizado.canate facdpedi.canate
        WITH STREAM-IO NO-BOX WIDTH 320.
    PAUSE 0.
END.

PUT "Actualizamos las cotizaciones " DATETIME(TODAY, MTIME) SKIP.
FOR EACH t-cotizado NO-LOCK, FIRST facdpedi WHERE ROWID(facdpedi) = t-cotizado.prowid:
    IF t-cotizado.canate <> facdpedi.canate THEN DO:
        facdpedi.canate = t-cotizado.canate.
        IF facdpedi.canate >= facdpedi.canped THEN facdpedi.flgest = "C". ELSE facdpedi.flgest = "P".
        FIND faccpedi WHERE faccpedi.codcia = s-codcia
            AND faccpedi.coddoc = t-cotizado.coddoc
            AND faccpedi.nroped = t-cotizado.nroped.
        ASSIGN faccpedi.flgest = "C".
        IF CAN-FIND(FIRST facdpedi OF faccpedi WHERE facdpedi.canped > facdpedi.canate NO-LOCK) THEN
            faccpedi.flgest = "P".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


