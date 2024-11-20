FIND FIRST faccpedi WHERE faccpedi.codcia = 1 AND 
                            faccpedi.coddoc = 'O/D' AND 
                            faccpedi.nroped = "506014341" NO-LOCK NO-ERROR. /*OD*/

IF NOT AVAILABLE faccpedi THEN DO:
    MESSAGE "O/D NO EXISTE".
    RETURN.
END.

FIND FIRST faccfggn WHERE faccfggn.codcia = 1 NO-LOCK.

DEF VAR iCountGuide AS INTE.
DEF VAR pmensaje AS CHAR.
DEF NEW SHARED VAR s-codcia AS INTE INIT 1.
DEF NEW SHARED VAR cl-codcia AS INTE INIT 0.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00506'.

DEF TEMP-TABLE pedi LIKE facdpedi.
DEF TEMP-TABLE ITEM LIKE facdpedi.
DEF TEMP-TABLE t-cdocu LIKE ccbcdocu.
DEF TEMP-TABLE t-ddocu LIKE ccbddocu.

DEF BUFFER pedido FOR faccpedi.
DEF BUFFER cotizacion FOR faccpedi.

FIND FIRST PEDIDO WHERE PEDIDO.codcia = Faccpedi.codcia
    AND PEDIDO.coddoc = Faccpedi.codref
    AND PEDIDO.nroped = Faccpedi.nroref
    NO-LOCK.
FIND FIRST COTIZACION WHERE COTIZACION.codcia = PEDIDO.codcia
    AND COTIZACION.coddoc = PEDIDO.codref
    AND COTIZACION.nroped = PEDIDO.nroref
    NO-LOCK.
RUN carga-temporal.

    RUN Crea-Comprobantes.


RETURN.

PROCEDURE carga-temporal:

      EMPTY TEMP-TABLE PEDI.
  DEF VAR iCountItem AS INT INIT 0 NO-UNDO.
  FOR EACH ITEM BY ITEM.NroItm:
      CREATE PEDI.
      BUFFER-COPY ITEM TO PEDI.
      iCountItem = iCountItem + 1.
  END.


END PROCEDURE.

PROCEDURE crea-comprobantes:

    DEFINE VARIABLE iCountItem AS INTEGER INITIAL 1 NO-UNDO.

    ASSIGN
        iCountGuide = 1
        pMensaje = "".
    EMPTY TEMP-TABLE T-CDOCU.
    EMPTY TEMP-TABLE T-DDOCU.
    RLOOP:
    DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
        /* Cabecera de Guía */
        RUN proc_CreaCabecera.
        IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO RLOOP, RETURN 'ADM-ERROR'.
        /* Detalle */
        IF COTIZACION.TpoPed = "LF" THEN DO:   /* LISTA EXPRESS */
            FOR EACH PEDI, FIRST ITEM WHERE ITEM.CodMat = PEDI.CodMat,
                FIRST Almmmatg OF PEDI NO-LOCK /*,
                FIRST Almmmate NO-LOCK WHERE Almmmate.CodCia = s-CodCia 
                    AND Almmmate.CodAlm = PEDI.AlmDes
                    AND Almmmate.CodMat = PEDI.CodMat */
                BREAK BY PEDI.CodCia BY PEDI.NroItm:

                {sunat\igeneracionfactcredito-v4.i}

            END. /* FOR EACH FacDPedi... */
        END.
        ELSE DO:
            FOR EACH PEDI, FIRST ITEM WHERE ITEM.CodMat = PEDI.CodMat,
                FIRST Almmmatg OF PEDI NO-LOCK /*,
                FIRST Almmmate NO-LOCK WHERE Almmmate.CodCia = s-CodCia 
                    AND Almmmate.CodAlm = PEDI.AlmDes
                    AND Almmmate.CodMat = PEDI.CodMat*/
                BREAK BY PEDI.CodCia BY PEDI.Libre_c05 BY PEDI.Libre_c04 BY PEDI.CodMat:

                {sunat\igeneracionfactcredito-v4.i}

            END. /* FOR EACH FacDPedi... */
        END.
        RUN proc_GrabaTotales.
        /* EN CASO DE CERRAR LAS FACTURAS APLICAMOS EL REDONDEO */
        IF Faccpedi.Importe[2] <> 0 THEN DO:
            /* NOS ASEGURAMOS QUE SEA EL ULTIMO REGISTRO */
            IF NOT CAN-FIND(FIRST Facdpedi OF Faccpedi WHERE (Facdpedi.CanPed - Facdpedi.CanAte) > 0
                            NO-LOCK) THEN DO:
                ASSIGN 
                    T-CDOCU.ImpTot = T-CDOCU.ImpTot + Faccpedi.Importe[2]
                    T-CDOCU.Libre_d02 = Faccpedi.Importe[2]
                    T-CDOCU.ImpVta = ROUND ( (T-CDOCU.ImpTot - T-CDOCU.AcuBon[5])/ ( 1 + T-CDOCU.PorIgv / 100 ) , 2)
                    T-CDOCU.ImpIgv = (T-CDOCU.ImpTot - T-CDOCU.AcuBon[5]) - T-CDOCU.ImpVta
                    T-CDOCU.ImpBrt = T-CDOCU.ImpVta + T-CDOCU.ImpDto + T-CDOCU.ImpExo
                    T-CDOCU.SdoAct = T-CDOCU.ImpTot.
            END.
        END.
        /* FIN DE REDONDEO */
    END.
    RETURN 'OK'.


END PROCEDURE.

PROCEDURE proc_CreaCabecera:

DEFINE VAR x-fechaemision AS DATE.
DEFINE VAR cDivOri AS CHAR NO-UNDO.
DEFINE VAR x-fecha-vcto AS DATE.

cDivOri = FacCPedi.CodDiv.  /* Por defecto */
IF AVAILABLE COTIZACION THEN cDivOri = COTIZACION.CodDiv.

x-fechaemision = TODAY.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* ******************************************* */
    /* RHC 24/11/2015 Control de LISTA DE TERCEROS */
    /* ******************************************* */
    IF AVAILABLE COTIZACION AND COTIZACION.TipBon[10] > 0 THEN DO:
        FIND Gn-clie WHERE Gn-clie.codcia = cl-codcia
            AND Gn-clie.codcli = Faccpedi.codcli
            EXCLUSIVE-LOCK NO-ERROR.
        IF ERROR-STATUS:ERROR AND LOCKED(Gn-clie) THEN DO:
            MESSAGE 'NO se pudo actualizar el control de LISTA DE TERCEROS en el cliente'
                VIEW-AS ALERT-BOX ERROR.
            UNDO, RETURN 'ADM-ERROR'.
        END.
        /*
        ASSIGN
            Gn-clie.Libre_d01 = MAXIMUM(Gn-clie.Libre_d01, COTIZACION.TipBon[10]).
        */
    END.
    FIND Gn-clie WHERE Gn-clie.codcia = cl-codcia
        AND Gn-clie.codcli = Faccpedi.codcli
        NO-LOCK NO-ERROR.
    /* ******************************************* */
    CREATE T-CDOCU.
    BUFFER-COPY FacCPedi 
        EXCEPT Faccpedi.ImpDto2 Faccpedi.PorDto Faccpedi.Importe
        TO T-CDOCU
        ASSIGN
        T-CDOCU.CodDiv = s-CodDiv
        T-CDOCU.DivOri = cDivOri    /* OJO: division de estadisticas */
        T-CDOCU.CodAlm = FacCPedi.CodAlm   /* OJO: Almacén despacho */
        T-CDOCU.CodDoc = "FAC"
        T-CDOCU.NroDoc =  '01'
        T-CDOCU.FchDoc = x-fechaemision /* TODAY , Ic - 01Feb2017 */
        T-CDOCU.CodMov = 02
        T-CDOCU.CodRef = FacCPedi.CodDoc           /* CONTROL POR DEFECTO */
        T-CDOCU.NroRef = FacCPedi.NroPed
        T-CDOCU.Libre_c01 = FacCPedi.CodDoc        /* CONTROL ADICIONAL */
        T-CDOCU.Libre_c02 = FacCPedi.NroPed
        T-CDOCU.Libre_c04 = "FAC"
        T-CDOCU.CodPed = FacCPedi.CodRef
        T-CDOCU.NroPed = FacCPedi.NroRef
        T-CDOCU.FchVto = x-fechaemision /* TODAY , Ic - 01Feb2017 */
        T-CDOCU.CodAnt = FacCPedi.Atencion     /* DNI */
        T-CDOCU.TpoCmb = FacCfgGn.TpoCmb[1]
        T-CDOCU.NroOrd = FacCPedi.ordcmp
        T-CDOCU.FlgEst = "P"
        T-CDOCU.TpoFac = "CR"                  /* CREDITO */
        T-CDOCU.Tipo   = "CREDITO"  /*pOrigen*/
        T-CDOCU.CodCaja= ''
        T-CDOCU.usuario = ''
        T-CDOCU.HorCie = STRING(TIME,'hh:mm')
        T-CDOCU.LugEnt = ''
        T-CDOCU.LugEnt2 = FacCPedi.LugEnt2
        T-CDOCU.Glosa = ''
        T-CDOCU.FlgCbd = FacCPedi.FlgIgv
        T-CDOCU.Sede   = FacCPedi.Sede.     /* <<< SEDE DEL CLIENTE <<< */
    /* RHC 18/02/2016 LISTA EXPRESS WEB */          
    IF COTIZACION.TpoPed = "LF" 
        THEN ASSIGN
                T-CDOCU.ImpDto2   = COTIZACION.ImpDto2       /* Descuento TOTAL CON IGV */
                T-CDOCU.Libre_d01 = COTIZACION.Importe[3].   /* Descuento TOTAL SIN IGV */
    /* **************************** */
    /*
    ASSIGN
        FacCorre.Correlativo = FacCorre.Correlativo + 1.
    */
    FIND FIRST gn-convt WHERE gn-convt.Codig = T-CDOCU.FmaPgo NO-LOCK NO-ERROR.
    IF AVAILABLE gn-convt THEN DO:
        T-CDOCU.TipVta = IF gn-ConVt.TotDias = 0 THEN "1" ELSE "2".
        /*T-CDOCU.FchVto = T-CDOCU.FchDoc + INTEGER(ENTRY(NUM-ENTRIES(gn-ConVt.Vencmtos),gn-ConVt.Vencmtos)).*/
    END.
    ASSIGN T-CDOCU.FchVto = x-fecha-vcto.

    FIND gn-clie WHERE gn-clie.CodCia = cl-codcia 
        AND gn-clie.CodCli = T-CDOCU.CodCli 
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie  THEN DO:
        ASSIGN
            T-CDOCU.CodDpto = gn-clie.CodDept 
            T-CDOCU.CodProv = gn-clie.CodProv 
            T-CDOCU.CodDist = gn-clie.CodDist.
    END.
    /* Guarda Centro de Costo */
    FIND gn-ven WHERE
        gn-ven.codcia = s-codcia AND
        gn-ven.codven = T-CDOCU.codven
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ven THEN T-CDOCU.cco = gn-ven.cco.
    
END.
MESSAGE t-cdocu.imptot t-cdocu.sdoact t-cdocu.sdoact.
RETURN 'OK'.


END PROCEDURE.


PROCEDURE proc_GrabaTotales:

    /* Rutina General */
    /*{vtagn/i-total-factura.i &Cabecera="T-CDOCU" &Detalle="T-DDOCU"}*/
    {vtagn/i-total-factura-sunat.i &Cabecera="T-CDOCU" &Detalle="T-DDOCU"}

    /* Caso Lista Express */
    IF COTIZACION.TpoPed = "LF"  THEN ASSIGN T-CDOCU.SdoAct = 0.



END PROCEDURE.
