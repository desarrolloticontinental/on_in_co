DEF VAR x-mensaje AS CHAR.
DEF VAR pmensaje AS CHAR.

DEF new SHARED VAR s-codcia AS INTE INIT 001.

FIND ccbcdocu WHERE codcia = 1
    AND coddoc = 'fac'
    AND nrodoc = '27600017562' NO-LOCK.
DEF BUFFER orden FOR faccpedi.
DEF BUFFER pedido FOR faccpedi.
DEF BUFFER cotizacion FOR faccpedi.



/* Separamos el cálculo a partir del comprobante */
FIND FIRST ORDEN WHERE ORDEN.CodCia = CcbCDocu.CodCia AND
    ORDEN.CodDoc = CcbCDocu.Libre_c01 AND
    ORDEN.NroPed = CcbCDocu.Libre_c02 AND
    CAN-FIND(FIRST PEDIDO WHERE PEDIDO.CodCia = ORDEN.CodCia AND
             PEDIDO.CodDoc = ORDEN.CodRef AND
             PEDIDO.NroPed = ORDEN.NroRef AND
             CAN-FIND(FIRST COTIZACION WHERE COTIZACION.CodCia = PEDIDO.CodCia AND 
                      COTIZACION.CodDoc = PEDIDO.CodRef AND
                      COTIZACION.NroPed = PEDIDO.NroRef NO-LOCK)
             NO-LOCK)
    NO-LOCK NO-ERROR.
IF AVAILABLE ORDEN THEN DO:
    FIND FIRST PEDIDO WHERE PEDIDO.CodCia = ORDEN.CodCia AND
             PEDIDO.CodDoc = ORDEN.CodRef AND
             PEDIDO.NroPed = ORDEN.NroRef AND
        CAN-FIND(FIRST COTIZACION WHERE COTIZACION.CodCia = PEDIDO.CodCia AND 
                 COTIZACION.CodDoc = PEDIDO.CodRef AND
                 COTIZACION.NroPed = PEDIDO.NroRef NO-LOCK)
        NO-LOCK.
    FIND FIRST COTIZACION WHERE COTIZACION.CodCia = PEDIDO.CodCia AND 
        COTIZACION.CodDoc = PEDIDO.CodRef AND
        COTIZACION.NroPed = PEDIDO.NroRef NO-LOCK.
    CASE TRUE:
        WHEN PEDIDO.Cliente_Recoge = YES THEN DO:
            MESSAGE 'cliente trecoge'.
/*             RUN Cliente-Recoge.                                                                                      */
/*             IF RETURN-VALUE = 'ADM-ERROR' THEN DO:                                                                   */
/*                 IF TRUE <> (x-Mensaje > '') THEN pMensaje = "NO se pudo actualizar el descuento por CLIENTE RECOGE". */
/*                 RETURN 'ADM-ERROR'.                                                                                  */
/*             END.                                                                                                     */
        END.
        OTHERWISE DO:
            /* Buscamos pronto despacho */
            FIND FIRST VtaCTabla WHERE VtaCTabla.CodCia = s-CodCia AND
                VtaCTabla.Tabla = "DSCTO_DESPACHO" AND
                VtaCTabla.Llave = COTIZACION.Libre_c01 AND
                /*VtaCTabla.Llave = ORDEN.Lista_de_Precios AND*/
                ORDEN.FchEnt <= VtaCTabla.Libre_f01
                NO-LOCK NO-ERROR.
            CASE TRUE:
                WHEN AVAILABLE VtaCTabla THEN DO:
                    MESSAGE 'pronto despacho'.
                    RUN Pronto-Despacho.
                    IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
                        IF TRUE <> (x-Mensaje > '') THEN pMensaje = "NO se pudo actualizar el descuento por PRONTO DESPACHO".
                        RETURN 'ADM-ERROR'.
                    END.
                END.
            END CASE.
        END.
    END CASE.
END.

RETURN 'OK'.

PROCEDURE Pronto-Despacho:
/* ********************** */

    DEF VAR s-Tabla AS CHAR INIT "DSCTO_DESPACHO" NO-UNDO.

    /* Buscamos la configuración */
    FIND FIRST VtaCTabla WHERE VtaCTabla.CodCia = s-CodCia AND 
        VtaCTabla.Tabla = s-Tabla AND 
        VtaCTabla.Llave = COTIZACION.Libre_c01
        /*VtaCTabla.Llave = ORDEN.Lista_de_Precios*/
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE VtaCTabla THEN RETURN 'OK'.
    IF ORDEN.FchEnt > VtaCTabla.Libre_f01 THEN RETURN 'OK'.

    /* Acumulamos Importes por cada Item */
    DEF VAR x-Importe AS de NO-UNDO.
    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK,
        FIRST Almmmatg OF Ccbddocu NO-LOCK:
        /* Inclusiones */
        FIND FIRST VtaDTabla OF VtaCTabla WHERE VtaDTabla.Tipo = Almmmatg.CodFam NO-LOCK NO-ERROR.
        IF NOT AVAILABLE VtaDTabla THEN NEXT.
        x-Importe = x-Importe + (CcbDDocu.ImpLin - CcbDDocu.ImpDto2).   /* Importe neto */
    END.
    IF x-Importe <= 0 THEN RETURN 'OK'.

    DEF VAR x-PorDto AS DEC NO-UNDO.

    x-PorDto = VtaCTabla.Libre_d01.
    IF x-PorDto <= 0 THEN RETURN 'OK'.

    MESSAGE 'calcular descuento'.

    RUN Calcular-Descuento (INPUT x-PorDto,     /* % de descuento */
                            INPUT x-Importe,    /* Importe afecto */
                            INPUT s-Tabla).     /* Motivo del descuento */

    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN 'ADM-ERROR'.

    RETURN 'OK'.


END PROCEDURE.


PROCEDURE Calcular-Descuento:
/* ************************** */

DEF INPUT PARAMETER pPorDto AS DEC.
DEF INPUT PARAMETER pImpTot AS DEC.     /* Importe que está afecto al descuento */
DEF INPUT PARAMETER pMotivo AS CHAR.

DEF VAR x-ImpDto AS DEC NO-UNDO.

/* Calculamos el descuento al total: Se va a repartir entre los artículos que están afectos al decuento */
x-ImpDto = ROUND(pImpTot * (pPorDto / 100), 2).
MESSAGE 'x-impdto' x-impdto.

/* Distribuimos el descuento entre todos los productos */
DEF VAR x-Rowid AS ROWID NO-UNDO.

x-Rowid = ROWID(Ccbcdocu).

DEF BUFFER B-CDOCU FOR Ccbcdocu.
DEF BUFFER B-DDOCU FOR Ccbddocu.

RLOOP:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    {lib/lock-genericov3.i ~
        &Tabla="B-CDOCU" ~
        &Condicion="ROWID(B-CDOCU) = x-Rowid" ~
        &Bloqueo="EXCLUSIVE-LOCK NO-ERROR" ~
        &Accion="RETRY" ~
        &Mensaje="NO" ~
        &txtMensaje="x-Mensaje" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'" }
/*     ASSIGN                                   */
/*         B-CDOCU.Dcto_Otros_VV = 0            */
/*         B-CDOCU.Dcto_Otros_PV = x-ImpDto     */
/*         B-CDOCU.Dcto_Otros_Mot = pMotivo     */
/*         B-CDOCU.Dcto_Otros_Factor = pPorDto. */
    /* ********************************** */
    /* Repartimos el monto por cada item */
    /* ********************************** */
    FOR EACH Ccbddocu OF B-CDOCU NO-LOCK, FIRST Almmmatg OF Ccbddocu NO-LOCK BREAK BY Ccbddocu.codcia BY Ccbddocu.nroitm:
        /* ********************************** */
        /* Veamos si esta INCLUIDO o EXCLUIDO */
        /* ********************************** */
        FIND FIRST VtaDTabla OF VtaCTabla WHERE VtaDTabla.Tipo = Almmmatg.CodFam NO-LOCK NO-ERROR.
        IF pMotivo = "DSCTO_DESPACHO" AND NOT AVAILABLE VtaDTabla THEN NEXT.     /* Inclusiones */
        IF pMotivo = "DSCTO_RECOJO"   AND AVAILABLE VtaDTabla     THEN NEXT.     /* Excepciones */
        /* ********************************** */
        FIND B-DDOCU WHERE ROWID(B-DDOCU) = ROWID(Ccbddocu) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            {lib/mensaje-de-error.i &MensajeError="x-Mensaje"}
            UNDO RLOOP, RETURN 'ADM-ERROR'.
        END.
/*         ASSIGN                                                                                     */
/*             B-DDOCU.Dcto_Otros_Factor = pPorDto                                                    */
/*             B-DDOCU.Dcto_Otros_VV = 0                                                              */
/*             B-DDOCU.Dcto_Otros_PV = ROUND((B-DDOCU.ImpLin - B-DDOCU.ImpDto2) * (pPorDto / 100), 2) */
/*             B-DDOCU.Dcto_Otros_Mot = pMotivo.                                                      */
        ASSIGN
            x-ImpDto = x-ImpDto - ROUND((B-DDOCU.ImpLin - B-DDOCU.ImpDto2) * (pPorDto / 100), 2).
        MESSAGE codmat x-impdto.
/*         IF LAST(Ccbddocu.CodCia) THEN B-DDOCU.Dcto_Otros_PV = B-DDOCU.Dcto_Otros_PV + x-ImpDto.                                */
/*         /* Importe del registro */                                                                                             */
/*         ASSIGN                                                                                                                 */
/*             B-DDOCU.ImpLin = B-DDOCU.ImpLin - B-DDOCU.Dcto_Otros_PV.                                                           */
/*         ASSIGN                                                                                                                 */
/*             B-DDOCU.ImpDto = (B-DDOCU.CanDes * B-DDOCU.PreUni) - B-DDOCU.ImpLin.                                               */
/*         /* Redondeamos */                                                                                                      */
/*         ASSIGN                                                                                                                 */
/*             B-DDOCU.ImpLin = ROUND(B-DDOCU.ImpLin, 2)                                                                          */
/*             B-DDOCU.ImpDto = ROUND(B-DDOCU.ImpDto, 2).                                                                         */
/*         IF B-DDOCU.AftIsc                                                                                                      */
/*             THEN ASSIGN                                                                                                        */
/*                     B-DDOCU.ImpIsc = ROUND(B-DDOCU.PreBas * B-DDOCU.CanDes * (Almmmatg.PorIsc / 100),4).                       */
/*         IF B-DDOCU.AftIgv                                                                                                      */
/*             THEN ASSIGN                                                                                                        */
/*                     B-DDOCU.Dcto_Otros_VV = B-DDOCU.Dcto_Otros_PV / ( 1 + (B-CDOCU.PorIgv / 100) )                             */
/*                     B-CDOCU.Dcto_Otros_VV = B-CDOCU.Dcto_Otros_VV + ( B-DDOCU.Dcto_Otros_PV / ( 1 + (B-CDOCU.PorIgv / 100) ) ) */
/*                     B-DDOCU.ImpIgv = B-DDOCU.ImpLin - ROUND( B-DDOCU.ImpLin  / ( 1 + (B-CDOCU.PorIgv / 100) ), 4 ).            */
/*         ELSE ASSIGN B-DDOCU.Dcto_Otros_VV = B-DDOCU.Dcto_Otros_PV.                                                             */
    END.
/*     {vtagn/i-total-factura.i &Cabecera="B-CDOCU" &Detalle="B-DDOCU"} */
END.

RETURN 'OK'.


END PROCEDURE.
