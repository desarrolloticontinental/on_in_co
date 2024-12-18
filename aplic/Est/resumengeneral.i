&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 3.92
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
/* RHC 30/04/2015 Vamos a tomar el �ltimo costo de reposicion */
/* DEF VAR x-CostoExtCIGV LIKE VentasxProducto.CostoExtCIGV NO-UNDO. */
/* DEF VAR x-CostoNacCIGV LIKE VentasxProducto.CostoNacCIGV NO-UNDO. */

FOR EACH Ventas NO-LOCK WHERE (Ventas.DateKey >= DesdeF AND Ventas.DateKey <= HastaF)
    AND (x-CodMat = '' OR LOOKUP (Ventas.codmat, x-CodMat) > 0)
    AND (x-CodCli = '' OR LOOKUP (Ventas.codcli, x-CodCli) > 0)
    AND (x-CodDiv = '' OR Ventas.coddiv = x-CodDiv)
    AND (x-CodVen = '' OR Ventas.codven = x-CodVen),
/*     FIRST DimVendedor OF Ventas NO-LOCK, */
/*     FIRST DimCliente OF Ventas NO-LOCK,  */
    FIRST DimFecha OF Ventas NO-LOCK,
    FIRST DimProducto OF Ventas NO-LOCK WHERE 
        (x-CodFam = '' OR DimProducto.codfam = x-CodFam)
        AND (x-CodPro = '' OR DimProducto.CodPro[1] = x-CodPro)
        AND (x-SubFam = '' OR DimProducto.subfam = x-SubFam),
    FIRST DimLinea OF DimProducto NO-LOCK,
    FIRST DimSubLinea OF DimProducto NO-LOCK:
    IF x-CuentaReg = 0 OR ( x-CuentaReg MODULO x-MuestraReg ) = 0 THEN
        FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '** TABLA GENERAL ' + 
        ' Fecha ' + STRING (DAY(DimFecha.DateKey), '99') +
        ' ' + DimFecha.CalendarMonthLabel + ' ' + STRING (DimFecha.CalendarYear, '9999') +
        ' DIVISION ' + Ventas.coddiv + ' CLIENTE ' + Ventas.codcli + ' **'.
    x-CuentaReg = x-CuentaReg + 1.
    /* RHC 30/04/2015 */
    ASSIGN
        x-TpoCmbCmp = 1
        x-TpoCmbVta = 1.
    IF DimProducto.MonVta = 1 THEN DO:
        x-CostoNacCIGV = Ventas.Cantidad * DimProducto.CtoTot.
        x-CostoExtCIGV = Ventas.Cantidad * DimProducto.CtoTot / DimProducto.TpoCmb.
    END.
    ELSE DO:
        x-CostoExtCIGV = Ventas.Cantidad * DimProducto.CtoTot.
        x-CostoNacCIGV = Ventas.Cantidad * DimProducto.CtoTot * DimProducto.TpoCmb.
    END.
    /* *************** */

    /* ARMAMOS LA LLAVE */
    ASSIGN
        x-Llave = ''
        cCampania = ''
        cPeriodo = 0
        cNroMes = 0
        cDia = ?
        cDivision = ''
        cDestino = ''
        cCanalVenta = ''
        cProducto = ''
        cRanking = 0
        cCategoria = ''
        cRUtilex = 0
        cCUtilex = ''
        cRMayo = 0
        cCMayo = ''        
        cLinea = ''
        cSublinea = ''
        cMarca = ''
        cUnidad = ''
        cLicencia = ''
        cLicenciatario = ''
        cProveedor = ''
        cCliente = ''
        cCliAcc = ''
        cCliUnico = ''
        cCanal = ''
        cGiro = ''
        cTarjeta = ''
        cDepartamento = ''
        cProvincia = ''
        cDistrito = ''
        cZona = ''
        cClasificacion = ''
        cTipo = ''
        cVendedor = ''
        cSubTipo = ''
        cCodAsoc = ''
        cDelivery = ''
        cListaBase = ''.
    /* LLAVE INICIAL */
    IF RADIO-SET-Tipo = 2 THEN DO:
        IF x-Llave = '' THEN x-Llave = DimFecha.Campania.
        ELSE x-Llave = x-Llave + DimFecha.Campania.
        x-Llave = x-Llave + '|'.
        x-Llave = x-Llave + STRING(YEAR(DimFecha.DateKey), '9999').
        x-Llave = x-Llave + '|'.
        x-Llave = x-Llave + STRING(MONTH(DimFecha.DateKey), '99').      
        x-Llave = x-LLave + '|'.
        ASSIGN
            cCampania = DimFecha.Campania
            cPeriodo  = YEAR(DimFecha.DateKey)
            cNroMes   = MONTH(DimFecha.DateKey).
    END.
    IF RADIO-SET-Tipo = 3 THEN DO:
        IF x-Llave = '' THEN x-Llave = DimFecha.Campania.
        ELSE x-Llave = x-Llave + DimFecha.Campania.
        x-Llave = x-Llave + '|'.
        x-Llave = x-Llave + DimFecha.DateDescription.
        x-Llave = x-LLave + '|'.
        ASSIGN
            cCampania = DimFecha.Campania
            cDia      = DimFecha.DateKey.
    END.
    IF TOGGLE-CodDiv = YES THEN DO:
        FIND FIRST DimDivision OF Ventas NO-LOCK.
         IF x-Llave = '' THEN x-Llave = Ventas.coddiv + '|'.
         ELSE x-Llave = x-LLave + Ventas.coddiv + '|'.
         x-Llave = x-Llave + DimDivision.CanalVenta + '|'.
         ASSIGN
             cDivision = Ventas.coddiv + ' ' + DimDivision.DesDiv
             cCanalVenta = DimDivision.CanalVenta + ' ' + DimDivision.NomCanalVenta.
         FIND FIRST DimDivision WHERE DimDivision.coddiv = Ventas.divdes NO-LOCK.
         ASSIGN
             cDestino = Ventas.divdes + ' ' + DimDivision.DesDiv
             x-Llave = x-LLave + Ventas.divdes + '|'.
         ASSIGN
             cListaBase = Ventas.ListaBase
             x-Llave = x-LLave + Ventas.ListaBase + '|'.
         FIND FIRST DimDivision WHERE DimDivision.coddiv = Ventas.ListaBase NO-LOCK NO-ERROR.
         IF AVAILABLE DimDivision THEN cListaBase  = Ventas.ListaBase + ' ' + DimDivision.DesDiv.

    END.
    IF TOGGLE-CodCli = YES THEN DO:
        ASSIGN
            x-Canal = ""
            x-Giro = ""
            x-NroCard = ""
            x-CodDept = ""
            x-CodProv = ""
            x-CodDist = ""
            x-ClfCli = ""
            x-Zona = "".
        FIND FIRST DimCliente OF Ventas NO-LOCK NO-ERROR.
        IF AVAILABLE DimCliente THEN DO:
            FIND gn-clie WHERE gn-clie.codcia = cl-codcia
                AND gn-clie.codcli = DimCliente.CodCli
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-clie THEN cCliAcc = gn-clie.JfeLog[5].
            ASSIGN
                x-Canal = DimCliente.canal
                x-Giro = DimCliente.gircli
                x-NroCard = ""
                x-CodDept = DimCliente.coddept
                x-CodProv = DimCliente.codprov
                x-CodDist = DimCliente.coddist
                x-ClfCli = DimCliente.clfcli
                x-Zona = "".
        END.
        IF TOGGLE-Resumen-Depto = NO THEN DO:
             IF x-Llave = '' THEN x-Llave = Ventas.codcli.
             ELSE x-Llave = x-Llave + Ventas.codcli.
             ASSIGN
                 cCliente = Ventas.codcli + ' ' + (IF AVAILABLE DimCliente THEN DimCliente.NomCli ELSE "")
                 cTarjeta = "".
            IF AVAILABLE DimCliente THEN DO:
                FIND B-Cliente WHERE B-Cliente.codcli = DimCliente.CodUnico NO-LOCK NO-ERROR.
                IF AVAILABLE B-Cliente THEN cCliUnico = DimCliente.CodCli + ' ' + DimCliente.NomCli.
            END.
        END.
        /* DEPARTAMENTO, PROVINCIA Y DISTRITO */
        IF AVAILABLE DimCliente THEN DO:
            FIND DimUbicacion WHERE DimUbicacion.CodDepto = DimCliente.CodDept 
                AND DimUbicacion.CodProvi = DimCliente.CodProv 
                AND DimUbicacion.CodDistr = DimCliente.CodDist
                NO-LOCK NO-ERROR.
            /* CANAL */
            ASSIGN
                cCanal = DimCliente.Canal + ' ' + TRIM(DimCliente.NomCanal)
                cGiro  = DimCliente.GirCli + ' ' + DimCliente.NomGirCli.
            IF AVAILABLE DimUbicacion THEN
                ASSIGN
                    cDepartamento = DimCliente.CodDept + ' ' + TRIM(DimUbicacion.NomDepto)
                    cProvincia = DimCliente.CodProv + ' ' + TRIM(DimUbicacion.Nomprovi)
                    cDistrito = DimCliente.CodDist + ' ' + TRIM(DimUbicacion.Nomdistr)
                    cZona = DimUbicacion.Zona + ' ' + TRIM(DimUbicacion.NomZona).
        END.
        IF x-Llave = '' THEN x-Llave = x-canal + '|'.
        ELSE x-Llave = x-Llave + x-canal + '|'.
        x-Llave = x-Llave + x-coddept + '|'.
        x-Llave = x-Llave + x-codprov + '|'.
        x-Llave = x-Llave + x-coddist + '|'.
        x-Llave = x-Llave + x-zona + '|'.
        x-Llave = x-Llave + x-clfcli + '|'.
        x-Llave = x-Llave + Ventas.tipo + '|'.
    END.
     /* ******************************************** */
    IF TOGGLE-CodMat = YES THEN DO:
        IF (TOGGLE-Resumen-Linea = NO AND TOGGLE-Resumen-Marca = NO
            AND TOGGLE-Resumen-Solo-Linea = NO) THEN DO:
             IF x-Llave = '' THEN x-Llave = Ventas.codmat.
             ELSE x-Llave = x-Llave + Ventas.codmat.
             x-Llave = x-Llave + DimProducto.codfam + '|'.
             x-Llave = x-Llave + DimProducto.subfam + '|'.
             x-Llave = x-Llave + DimProducto.desmar + '|'.
             x-Llave = x-Llave + DimProducto.UndStk + '|'.
             x-Llave = x-Llave + DimProducto.SubTipo + '|'.
             x-Llave = x-Llave + DimProducto.CodAsoc + '|'.
             ASSIGN
                 cProducto = Ventas.codmat + ' ' + DimProducto.DesMat
                 cRanking = DimProducto.Ranking
                 cCategoria = DimProducto.Categoria
                 cRUtilex = DimProducto.RUtilex
                 CCUtilex = DimProducto.CUtilex
                 cRMayo = DimProducto.RMayorista
                 CCMayo = DimProducto.CMayorista
                 cLinea = DimProducto.codfam + ' ' + DimLinea.NomFam
                 cSublinea = DimProducto.subfam + ' ' + DimSubLinea.NomSubFam 
                 cMarca = DimProducto.desmar
                 cUnidad = DimProducto.UndStk
                 cSubTipo = DimProducto.SubTipo
                 cCodAsoc = DimProducto.CodAsoc.
            FIND B-Producto WHERE B-Producto.codmat = DimProducto.CodAsoc NO-LOCK NO-ERROR.
            IF AVAILABLE B-Producto THEN cCodAsoc = cCodAsoc + ' ' + B-Producto.DesMat.
         END.
         ELSE DO:
             IF TOGGLE-Resumen-Linea = YES THEN DO:
                 IF x-Llave = '' THEN x-Llave = DimProducto.codfam + '|'.
                 ELSE x-Llave = x-Llave + DimProducto.codfam + '|'.
                 x-Llave = x-Llave + DimProducto.subfam + '|'.
                 ASSIGN
                     cLinea = DimProducto.codfam + ' ' + DimLinea.NomFam
                     cSublinea = DimProducto.subfam + ' ' + DimSubLinea.NomSubFam.
             END.
             IF TOGGLE-Resumen-Solo-Linea = YES THEN DO:
                 IF x-Llave = '' THEN x-Llave = DimProducto.codfam + '|'.
                 ELSE x-Llave = x-Llave + DimProducto.codfam + '|'.
                 ASSIGN
                     cLinea = DimProducto.codfam + ' ' + DimLinea.NomFam.
             END.
             IF TOGGLE-Resumen-Marca = YES THEN DO:
                 IF x-Llave = '' THEN x-Llave = DimProducto.desmar + '|'.
                 ELSE x-Llave = x-Llave + DimProducto.desmar + '|'.
                 ASSIGN
                     cMarca = DimProducto.desmar.
             END.
         END.
         x-Llave = x-Llave + DimProducto.licencia + '|'.
         x-Llave = x-Llave + DimProducto.codpro[1] + '|'.
         FIND DimLicencia OF DimProducto NO-LOCK NO-ERROR.
         FIND DimProveedor WHERE DimProveedor.CodPro = DimProducto.CodPro[1] NO-LOCK NO-ERROR.
         ASSIGN
             cLicencia = DimProducto.licencia + ' ' + (IF AVAILABLE DimLicencia THEN DimLicencia.Descripcion ELSE '')
             cProveedor = DimProducto.codpro[1] + ' ' + (IF AVAILABLE DimProveedor THEN DimProveedor.NomPro ELSE '').
        IF AVAILABLE DimLicencia THEN DO:
            cLicenciatario = DimLicencia.Licenciatario.
            FIND DimLicenciatario OF DimLicencia NO-LOCK NO-ERROR.
            IF AVAILABLE DimLicenciatario THEN cLicenciatario = DimLicenciatario.Licenciatario + ' ' + DimLicenciatario.NomLicenciatario.
        END.
     END.
     /* ******************************************** */
     IF TOGGLE-CodVen = YES THEN DO:
         FIND FIRST DimVendedor OF Ventas NO-LOCK NO-ERROR.
         IF x-Llave = '' THEN x-Llave = Ventas.codven + '|'.
         ELSE x-Llave = x-Llave + Ventas.codven + '|'.
         ASSIGN
             cVendedor = Ventas.codven + ' ' + (IF AVAILABLE DimVendedor THEN DimVendedor.NomVen ELSE "").
     END.
     ASSIGN
         x-Llave = x-Llave + Ventas.Tipo + '|'
         cTipo   = Ventas.Tipo
         cDelivery = Ventas.Delivery.
         
     /* ******************************************** */
     FIND FIRST Detalle WHERE Detalle.llave = x-Llave NO-ERROR.
     IF NOT AVAILABLE Detalle THEN DO:
         CREATE Detalle.
         Detalle.llave = x-Llave.
     END.
     ASSIGN
         Detalle.Campania = cCampania
         Detalle.Periodo = cPeriodo
         Detalle.NroMes = cNroMes
         Detalle.Dia = cDia
         Detalle.Division = cDivision
         Detalle.Destino = cDestino
         Detalle.CanalVenta = cCanalVenta
         Detalle.Producto = cProducto
         Detalle.Ranking = cRanking
         Detalle.Categoria = cCategoria
         Detalle.rnkgutlx = cRUtilex
         Detalle.clsfutlx = cCUtilex
         Detalle.rnkgmayo = cRMayo
         Detalle.clsfmayo = cCMayo
         Detalle.Linea = cLinea
         Detalle.Sublinea = cSublinea
         Detalle.Marca = cMarca
         Detalle.Unidad = cUnidad
         Detalle.Licencia = cLicencia
         Detalle.Licenciatario = cLicenciatario
         Detalle.Proveedor = cProveedor
         Detalle.SubTipo = cSubTipo
         Detalle.CodAsoc = cCodAsoc
         Detalle.Cliente = cCliente
         Detalle.CliAcc = cCliAcc
         Detalle.CliUnico = cCliUnico
         Detalle.Canal = cCanal
         Detalle.Giro = cGiro
         Detalle.Tarjeta = cTarjeta
         Detalle.Departamento = cDepartamento
         Detalle.Provincia = cProvincia
         Detalle.Distrito = cDistrito
         Detalle.Zona = cZona 
         Detalle.Clasificacion = cClasificacion
         Detalle.Tipo = cTipo
         Detalle.Vendedor = cVendedor
         Detalle.Delivery = cDelivery
         Detalle.ListaBase = cListaBase
         Detalle.CanxMes   = Detalle.CanxMes   + Ventas.Cantidad
         Detalle.VtaxMesMe = Detalle.VtaxMesMe + Ventas.ImpExtCIGV
         Detalle.VtaxMesMn = Detalle.VtaxMesMn + Ventas.ImpNacCIGV
         Detalle.CtoxMesMe = Detalle.CtoxMesMe + x-CostoExtCIGV
         Detalle.CtoxMesMn = Detalle.CtoxMesMn + x-CostoNacCIGV
         Detalle.ProxMesMe = Detalle.ProxMesMe + Ventas.PromExtCIGV
         Detalle.ProxMesMn = Detalle.ProxMesMn + Ventas.PromNacCIGV.
     ASSIGN
         Detalle.ProxMesMe2 = Detalle.ProxMesMe2 + Ventas.PromExtSIGV
         Detalle.ProxMesMn2 = Detalle.ProxMesMn2 + Ventas.PromNacSIGV.
     /* RHC 19/08/2016 C�digo Padre */
     FIND Almmmatg WHERE Almmmatg.CodCia = s-codcia
         AND Almmmatg.codmat = DimProducto.CodMat
         NO-LOCK NO-ERROR.
     IF AVAILABLE Almmmatg AND NOT (TRUE <> (Almmmatg.CodigoPadre > '')) 
         THEN DO:
         FIND B-Producto WHERE B-Producto.codmat = Almmmatg.CodigoPadre NO-LOCK NO-ERROR.
         IF AVAILABLE B-Producto THEN DO:
             ASSIGN
                 Detalle.CodigoPadre = TRIM(B-Producto.CodMat + ' ' + B-Producto.DesMat)
                 Detalle.FactorPadre = Almmmatg.FactorPadre
                 Detalle.NCantidad = Detalle.CanxMes * Almmmatg.FactorPadre.
         END.
     END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


