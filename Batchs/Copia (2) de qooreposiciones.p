&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     : Cargar automáticamente los pedidos de reposición de mercadería
                  para el almacén 500 usado por OpenOrange

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Valores preestablecidos */
DEF VAR s-codcia AS INT  INIT 001   NO-UNDO.
DEF VAR s-codalm AS CHAR INIT '500' NO-UNDO.
DEF VAR s-coddoc AS CHAR INIT 'R/A' NO-UNDO.
DEF VAR s-tipmov AS CHAR INIT 'A'   NO-UNDO.

FIND FIRST Faccorre WHERE Faccorre.codcia = s-codcia
    AND Faccorre.coddoc = s-coddoc
    AND Faccorre.flgest = YES
    AND Faccorre.codalm = s-codalm
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    DISPLAY
        'NO configurado el correlativo para:' s-coddoc 'almacén:' s-codalm SKIP
        WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.

DEFINE BUFFER B-MATE FOR Almmmate.
DEFINE BUFFER B-OOReposiciones FOR OOReposiciones.
DEFINE TEMP-TABLE T-DREPO LIKE almdrepo
    FIELD Campo-R AS ROWID
    INDEX llave01 IS PRIMARY codmat.

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
         HEIGHT             = 4.81
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

DEF VAR x-StockMinimo   AS DEC NO-UNDO.
DEF VAR x-StkAct        AS DEC NO-UNDO.
DEF VAR k               AS INT NO-UNDO.
DEF VAR pReposicion     AS DEC NO-UNDO.
DEF VAR x-Item          AS INT NO-UNDO.
DEF VAR x-TipMat        AS CHAR NO-UNDO.
DEF VAR pComprometido   AS DEC NO-UNDO.
DEF VAR x-StockDisponible AS DEC NO-UNDO.
DEF VAR x-CanReq        AS DEC NO-UNDO.

/* Buscamos los valores generales */
FIND FIRST AlmCfgGn WHERE almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almcfggn THEN DO:
    DISPLAY 'Debe configurar los parámetros generales' SKIP WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN "ADM-ERROR".
END.
FIND Almacen WHERE Almacen.codcia = s-codcia
    AND Almacen.codalm = s-codalm
    NO-LOCK.

FIND FIRST Faccorre WHERE Faccorre.codcia = s-codcia
    AND Faccorre.coddoc = s-coddoc
    AND Faccorre.flgest = YES
    AND Faccorre.codalm = s-codalm
    EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAILABLE Faccorre THEN DO:
    DISPLAY 
        'Correlativo bloqueado' s-coddoc 'almacén:' s-codalm SKIP WITH STREAM-IO NO-BOX NO-LABELS.
    PAUSE 0.
    RETURN.
END.

/* SE VA A GENERAR UNA ORDEN POR CADA REQUERIMIENTO DE OPENORANGE */
/* Control */
FOR EACH OOReposiciones WHERE OOReposiciones.CodCia = s-codcia
    AND OOReposiciones.CodAlm = s-codalm
    AND OOReposiciones.FlagMigracion = 'N',
    FIRST Almmmatg OF OOReposiciones NO-LOCK,
    FIRST Almmmate OF OOReposiciones NO-LOCK
    BREAK BY OOReposiciones.SerNr BY OOReposiciones.CodMat:
    IF FIRST-OF(OOReposiciones.SerNr) THEN EMPTY TEMP-TABLE T-DREPO.
    DISPLAY
        'Orden' OOReposiciones.SerNr 'Producto' OOReposiciones.CodMat 
        'Cantidad' OOReposiciones.CanPed
        WITH STREAM-IO NO-BOX NO-LABELS.
    FIND FIRST T-DREPO WHERE T-DREPO.codmat = OOReposiciones.codmat NO-ERROR.
    IF AVAILABLE T-DREPO THEN NEXT.
    CREATE T-DREPO.
    ASSIGN 
        T-DREPO.Origen = 'AUT'
        T-DREPO.CodCia = s-codcia 
        T-DREPO.CodAlm = s-codalm 
        T-DREPO.AlmPed = "XXX"  /* Temporalmente */
        T-DREPO.CodMat = Almmmate.codmat
        T-DREPO.Campo-R = ROWID(OOReposiciones).
    /* Stock Minimo */
    ASSIGN
        /* <<<< OJO: Tomamos este valor <<< */
        x-StockMinimo = OOReposiciones.CanPed
        x-StkAct = Almmmate.StkAct.
    /* Cantidad de Reposicion */
    pReposicion = x-StockMinimo.            /* <<<< OJO: Tomamos este valor <<< */
    /* distribuimos el pedido entre los almacenes de despacho */
    IF Almmmatg.Chr__02 = "P" THEN x-TipMat = "P". ELSE x-TipMat = "T".
    FOR EACH Almrepos NO-LOCK WHERE  almrepos.CodCia = s-codcia
        AND almrepos.CodAlm = Almmmate.codalm 
        AND almrepos.AlmPed <> Almmmate.codalm
        AND almrepos.TipMat = x-TipMat      /* Propios o Terceros */
        AND pReposicion > 0 /* <<< OJO <<< */
        BY almrepos.Orden:
        FIND B-MATE WHERE B-MATE.codcia = s-codcia
            AND B-MATE.codmat = Almmmate.codmat
            AND B-MATE.codalm = Almrepos.almped
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE B-MATE THEN NEXT.
        RUN Stock-Comprometido (Almmmate.CodMat, Almrepos.AlmPed, OUTPUT pComprometido).
        x-StockDisponible = B-MATE.StkAct - pComprometido.
        IF x-StockDisponible <= 0 THEN NEXT.
        /* Se solicitará la reposición de acuerdo al empaque del producto */
        x-CanReq = MINIMUM(x-StockDisponible, pReposicion).
        IF Almacen.Campo[7] = "REE" AND Almmmatg.CanEmp > 0 THEN DO:
            x-CanReq = ROUND(x-CanReq / Almmmatg.CanEmp, 0) * Almmmatg.CanEmp.
        END.
        IF x-CanReq <= 0 THEN NEXT.    /* Menos que la cantidad por empaque */
        /* Redondeamos la cantidad a enteros */
        IF TRUNCATE(x-CanReq,0) <> x-CanReq THEN DO:
            x-CanReq = TRUNCATE(x-CanReq,0) + 1.
        END.
        /* ********************************* */
        ASSIGN
            T-DREPO.AlmPed = Almrepos.almped
            T-DREPO.Item = x-Item
            T-DREPO.CanReq = x-CanReq
            T-DREPO.CanGen = x-CanReq
            T-DREPO.StkAct = x-StockDisponible.
        ASSIGN
            x-Item = x-Item + 1
            pReposicion = pReposicion - T-DREPO.CanReq.
    END.
    /* GENERAMOS LA ORDEN DE REPOSICION */
    IF LAST-OF(OOReposiciones.SerNr) THEN DO:
        RUN Generar-Pedidos.
    END.
END.

RELEASE FacCorre.
RELEASE almcrepo.
RELEASE almdrepo.
RELEASE OOReposiciones.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Generar-Pedidos) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar-Pedidos Procedure 
PROCEDURE Generar-Pedidos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-CanApro AS DEC NO-UNDO.
DEF VAR x-CanAte  AS DEC NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN "ADM-ERROR" ON STOP UNDO, RETURN "ADM-ERROR":
    FOR EACH T-DREPO WHERE T-DREPO.CanReq > 0 BREAK BY T-DREPO.AlmPed:
        IF FIRST-OF(AlmPed) THEN DO:
            CREATE Almcrepo.
            ASSIGN
                almcrepo.AlmPed = T-DREPO.Almped
                almcrepo.CodAlm = s-codalm
                almcrepo.CodCia = s-codcia
                almcrepo.FchDoc = TODAY
                almcrepo.FchVto = TODAY + 7
                almcrepo.Fecha = TODAY
                almcrepo.Hora = STRING(TIME, 'HH:MM')
                almcrepo.NroDoc = Faccorre.correlativo
                almcrepo.NroSer = Faccorre.nroser
                almcrepo.TipMov = s-tipmov
                almcrepo.Usuario = "AUTOMATICO".
                /*almcrepo.Glosa = fill-in-Glosa.*/
            ASSIGN
                Faccorre.correlativo = Faccorre.correlativo + 1.
            /* APROBADO AUTOMATICAMENTE */
            ASSIGN
                almcrepo.FchApr = TODAY
                Almcrepo.FlgEst = 'P'
                almcrepo.FlgSit = 'A'
                almcrepo.HorApr = STRING(TIME, 'HH:MM')
                almcrepo.UsrApr = "AUTOMATICO".
        END.
        CREATE Almdrepo.
        BUFFER-COPY T-DREPO TO Almdrepo
            ASSIGN
            almdrepo.CodCia = almcrepo.codcia
            almdrepo.CodAlm = almcrepo.codalm
            almdrepo.TipMov = almcrepo.tipmov
            almdrepo.NroSer = almcrepo.nroser
            almdrepo.NroDoc = almcrepo.nrodoc
            almdrepo.CanApro = almdrepo.canreq.
    END.
    FOR EACH T-DREPO:
        FIND B-OOReposiciones WHERE ROWID(B-OOReposiciones) = T-DREPO.Campo-R EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE B-OOReposiciones THEN 
            ASSIGN
            B-OOReposiciones.FlagMigracion = 'X'
            B-OOReposiciones.CanAte = T-DREPO.CanReq.
    END.
END.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Stock-Comprometido) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Stock-Comprometido Procedure 
PROCEDURE Stock-Comprometido :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pCodAlm AS CHAR.
DEF OUTPUT PARAMETER pComprometido AS DEC.

/* CALCULO DEL STOCK COMPROMETIDO */

FIND FacCfgGn WHERE faccfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Faccfggn THEN RETURN.

/* Stock comprometido por PEDIDOS ACTIVOS MOSTRADOR */
DEF VAR TimeOut AS INTEGER NO-UNDO.
DEF VAR TimeNow AS INTEGER NO-UNDO.

/* Tiempo por defecto fuera de campaña */
TimeOut = (FacCfgGn.Dias-Res * 24 * 3600) +
          (FacCfgGn.Hora-Res * 3600) + 
          (FacCfgGn.Minu-Res * 60).

pComprometido = 0.
DEF VAR i AS INT NO-UNDO.
DEF VAR x-CodAlm AS CHAR NO-UNDO.

DO i = 1 TO NUM-ENTRIES(pCodAlm):
    x-CodAlm = ENTRY(i, pCodAlm).
    /**********   Barremos para los PEDIDOS MOSTRADOR ***********************/ 
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.CodCia = s-codcia
        AND Facdpedi.AlmDes = x-CodAlm
        AND Facdpedi.codmat = pcodmat
        AND Facdpedi.coddoc = 'P/M'
        AND Facdpedi.FlgEst = "P" :
        FIND FIRST Faccpedi OF Facdpedi WHERE Faccpedi.FlgEst = "P" NO-LOCK NO-ERROR.
        IF NOT AVAIL Faccpedi THEN NEXT.

        TimeNow = (TODAY - FacCPedi.FchPed) * 24 * 3600.
        TimeNow = TimeNow + TIME - ( (INTEGER(SUBSTRING(FacCPedi.Hora, 1, 2)) * 3600) +
                  (INTEGER(SUBSTRING(FacCPedi.Hora, 4, 2)) * 60) ).
        IF TimeOut > 0 THEN DO:
            IF TimeNow <= TimeOut   /* Dentro de la valides */
            THEN DO:
                /* cantidad en reservacion */
                pComprometido = pComprometido + FacDPedi.Factor * FacDPedi.CanPed.
            END.
        END.
    END.
    /**********   Barremos para los PEDIDOS AL CREDITO   ***********************/ 
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
            AND Facdpedi.almdes = x-CodAlm
            AND Facdpedi.codmat = pCodMat
            AND Facdpedi.coddoc = 'PED'
            AND Facdpedi.flgest = 'P':
        /* RHC 12.12.2011 agregamos los nuevos estados */
        FIND FIRST Faccpedi OF Facdpedi WHERE LOOKUP(Faccpedi.FlgEst, "G,X,P,W,WX,WL") > 0 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Faccpedi THEN NEXT.
        pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
    END.
    /* ORDENES DE DESPACHO CREDITO */
    FOR EACH Facdpedi USE-INDEX Llave04 NO-LOCK WHERE Facdpedi.codcia = s-codcia
            AND Facdpedi.almdes = x-CodAlm
            AND Facdpedi.codmat = pCodMat
            AND Facdpedi.coddoc = 'O/D'
            AND Facdpedi.flgest = 'P':
        FIND FIRST Faccpedi OF Facdpedi WHERE Faccpedi.flgest = 'P' NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Faccpedi THEN NEXT.
        pComprometido = pComprometido + FacDPedi.Factor * (FacDPedi.CanPed - FacDPedi.canate).
    END.
    /* Stock Comprometido por Pedidos por Reposicion Automatica */
    FOR EACH Almcrepo NO-LOCK WHERE Almcrepo.codcia = s-codcia
        AND Almcrepo.TipMov = 'A'
        AND Almcrepo.AlmPed = x-CodAlm
        AND Almcrepo.FlgEst = 'P'
        AND Almcrepo.FlgSit = 'A',
        EACH Almdrepo OF Almcrepo NO-LOCK WHERE Almdrepo.codmat = pCodMat
        AND almdrepo.CanApro > almdrepo.CanAten:
        pComprometido = pComprometido + (Almdrepo.CanApro - Almdrepo.CanAten).
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

