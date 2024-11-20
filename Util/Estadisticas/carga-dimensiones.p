    /* DIMENSION TIEMPO */
    DEF VAR P AS INT.
    DEF VAR T AS CHAR INIT "Enero,Febrero,Marzo,Abril,Mayo,Junio,Julio,Agosto,Setiembre,Octubre,Noviembre,Diciembre".
    DEF VAR W AS CHAR INIT "Domingo,Lunes,Martes,Miercoles,Jueves,Viernes,Sabado".
    DEF VAR X AS DATE.
    DEF VAR Y AS DATE.
    DEF VAR Z AS DATE.
    DEF VAR pSemana AS INT.
    DEF VAR pAno AS INT.
    DEF VAR s-codcia AS INT INIT 001.
    DEF VAR cl-codcia AS INT INIT 000.
    DEF VAR pv-codcia AS INT INIT 000.
    DEF VAR cb-codcia AS INT INIT 000.
    DEF VAR x-codfchi AS DATE.
    DEF VAR x-codfchf AS DATE.
    
    ASSIGN
        x-codfchi = 01/01/2008
        x-codfchf = TODAY.
    ASSIGN
        X = x-CodFchI
        Y = x-CodFchF.
    FOR EACH DimFecha WHERE DimFecha.DateKey >= X
        AND DimFecha.DateKey <= Y:
        DELETE DimFecha.
    END.
    DO Z = X TO Y:
        FIND DimFecha WHERE DimFecha.DateKey = Z EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE DimFecha THEN CREATE DimFecha.
        ASSIGN
            DimFecha.DateKey = Z
            DimFecha.FullDateLabel = STRING(Z, '99-99-9999')
            DimFecha.DateDescription = STRING(Z, '99/99/9999')
            DimFecha.CalendarYear = YEAR(Z)
            DimFecha.CalendarYearLabel = "Año " + STRING(YEAR(Z), '9999')
            DimFecha.CalendarHalfYear = YEAR(Z) * 10 + (IF MONTH(Z) <= 6 THEN 1 ELSE 2)
            DimFecha.CalendarHalfYearLabel = (IF MONTH(Z) <= 1 THEN "H1" ELSE "H2").
        IF MONTH(Z) <= 03 
            THEN ASSIGN
                    DimFecha.CalendarQuarter = YEAR(Z) * 10 + 1
                    DimFecha.CalendarQuarterLabel = "Q1".
        ELSE IF MONTH(Z) <= 06
            THEN ASSIGN
                    DimFecha.CalendarQuarter = YEAR(Z) * 10 + 2
                    DimFecha.CalendarQuarterLabel = "Q2".
        ELSE IF MONTH(Z) <= 09
            THEN ASSIGN
                    DimFecha.CalendarQuarter = YEAR(Z) * 10 + 3
                    DimFecha.CalendarQuarterLabel = "Q3". 
        ELSE ASSIGN
                    DimFecha.CalendarQuarter = YEAR(Z) * 10 + 4
                    DimFecha.CalendarQuarterLabel = "Q4".
        ASSIGN
            DimFecha.CalendarMonth = YEAR(Z) * 100 + MONTH(Z)
            DimFecha.CalendarMonthLabel = ENTRY(MONTH(Z), T).
        RUN SemanaIso (Z, OUTPUT pSemana, OUTPUT pAno).
        ASSIGN
            DimFecha.CalendarWeek = pAno * 100 + pSemana
            DimFecha.CalendarWeekLabel = "Semana " + TRIM(STRING(pSemana))
            DimFecha.CalendarDayOfWeek = pAno * 1000 + pSemana * 10 + WEEKDAY(Z)
            DimFecha.CalendarDayOfWeekLabel = ENTRY(WEEKDAY(Z), W).
    END.

    /* DIMENSION CLIENTE */
    FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia:
        FIND DimCliente OF gn-clie NO-ERROR.
        IF NOT AVAILABLE DimCliente THEN CREATE DimCliente.
        BUFFER-COPY gn-clie TO DimCliente.
        DimCliente.NomCli = REPLACE(DimCliente.NomCli, '|', ' ').
        /* Canal */
        FIND almtabla WHERE almtabla.Tabla = 'CN' AND almtabla.Codigo = gn-clie.Canal NO-LOCK NO-ERROR.
        IF AVAILABLE almtabla THEN DimCliente.NomCanal = almtabla.Nombre.
        /* GIRO DEL NEGOCIO */
        FIND almtabla WHERE almtabla.Tabla = 'GN' AND almtabla.Codigo = gn-clie.gircli NO-LOCK NO-ERROR.
        IF AVAILABLE almtabla THEN DimCliente.NomGirCli = almtabla.Nombre.
    END.

    /* DIMENSION TARJETA CLIENTE EXCLUSIVO */
    FOR EACH gn-card NO-LOCK:
        FIND DimTarjeta OF gn-card NO-ERROR.
        IF NOT AVAILABLE DimTarjeta THEN CREATE DimTarjeta.
        ASSIGN
            DimTarjeta.NroCard = gn-card.NroCard 
            DimTarjeta.NomNroCard = gn-card.NomCard.
        DimTarjeta.NomNroCard = REPLACE(DimTarjeta.NomNroCard, '|' ,' ').
    END.

    /* DIMENSION PROVEEDOR */
    FOR EACH gn-prov NO-LOCK WHERE gn-prov.codcia = pv-codcia:
        FIND DimProveedor OF gn-prov NO-ERROR.
        IF NOT AVAILABLE DimProveedor THEN CREATE DimProveedor.
        BUFFER-COPY gn-prov TO DimProveedor.
        DimProveedor.NomPro = REPLACE(DimProveedor.NomPro, '|', ' ').
    END.

    /* Dimension  División */
    FOR EACH DimDivision:
        DELETE DimDivision.
    END.
    FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia:
        CREATE DimDivision.
        BUFFER-COPY gn-divi TO DimDivision.
        FIND vtamcanal WHERE Vtamcanal.Codcia = s-codcia
            AND Vtamcanal.CanalVenta = DimDivision.CanalVenta
            NO-LOCK NO-ERROR.
        IF AVAILABLE vtamcanal THEN DimDivision.NomCanalVenta = Vtamcanal.Descrip.
    END.

    /* DIMENSION LINEAS DE PRODUCTOS */
    FOR EACH DimLinea:
        DELETE DimLinea.
    END.
    FOR EACH Almtfami NO-LOCK WHERE Almtfami.codcia = s-codcia,
        EACH Almsfami OF Almtfami NO-LOCK:
        CREATE DimLinea.
        ASSIGN
            DimLinea.CodFam = Almtfami.codfam
            DimLinea.NomFam = Almtfami.desfam
            DimLinea.NomSubFam = AlmSFami.dessub
            DimLinea.SubFam = Almsfami.subfam.
    END.

    /* DIMENSION PRODUCTO */
    FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codcia = s-codcia:
        FIND DimProducto OF Almmmatg NO-ERROR.
        IF NOT AVAILABLE DimProducto THEN CREATE DimProducto.
        BUFFER-COPY Almmmatg 
            EXCEPT Licencia
            TO DimProducto.
        ASSIGN
            DimProducto.Licencia = Almmmatg.Licencia[1]
            DimProducto.CodPro[1] = Almmmatg.CodPr1
            DimProducto.CodPro[2] = Almmmatg.CodPr2.
        DimProducto.DesMat = REPLACE(DimProducto.DesMat, '|' , ' ').
    END.

    /* DIMENSION LICENCIA */
    FOR EACH Almtabla NO-LOCK WHERE Almtabla.tabla = "LC":
        FIND DimLicencia WHERE DimLicencia.Licencia = Almtabla.codigo
            NO-ERROR.
        IF NOT AVAILABLE DimLicencia THEN CREATE DimLicencia.
        ASSIGN
            DimLicencia.Licencia = Almtabla.codigo
            DimLicencia.Descripcion = Almtabla.nombre.
    END.

    /* DIMENSION VENDEDOR */
    FOR EACH gn-ven NO-LOCK WHERE gn-ven.codcia = s-codcia:
        FIND DimVendedor OF gn-ven NO-ERROR.
        IF NOT AVAILABLE DimVendedor THEN CREATE DimVendedor.
        BUFFER-COPY gn-ven TO DimVendedor.
    END.

    /* Dimension Ubicaciones */
    FOR EACH DimUbicacion:
        DELETE DimUbicacion.
    END.
    FOR EACH TabDepto NO-LOCK,
        EACH TabProvi OF TabDepto NO-LOCK,
        EACH TabDistr OF TabProvi NO-LOCK:
        CREATE DimUbicacion.
        ASSIGN
            DimUbicacion.CodDepto = TabDepto.CodDepto 
            DimUbicacion.NomDepto = TabDepto.NomDepto 
            DimUbicacion.Zona = TabDepto.Zona
            DimUbicacion.CodProvi = TabProvi.CodProvi 
            DimUbicacion.NomProvi = TabProvi.NomProvi
            DimUbicacion.CodDistr = TabDistr.CodDistr 
            DimUbicacion.NomDistr = TabDistr.NomDistr.
        /* Forzamos la zona */
        IF DimUbicacion.CodDepto = '15' AND DimUbicacion.CodProvi = '01' THEN DimUbicacion.Zona = 'LMC'.

        FIND FacTabla WHERE FacTabla.Codigo = DimUbicacion.Zona
            AND FacTabla.CodCia = s-codcia
            AND FacTabla.Tabla = "ZN" 
            NO-LOCK NO-ERROR.
        IF AVAILABLE FacTabla THEN DimUbicacion.NomZona = FacTabla.Nombre.
    END.


    PROCEDURE semanaiso:


DEF INPUT PARAMETER pFecha AS DATE.
DEF OUTPUT PARAMETER pSemana AS INT.
DEF OUTPUT PARAMETER pAno AS INT.

DEF VAR xDia AS INT.
DEF VAR xMes AS INT.
DEF VAR xAno AS INT.
DEF VAR xA AS INT.
DEF VAR xB AS INT.
DEF VAR xC AS INT.
DEF VAR xD AS INT.
DEF VAR xS AS INT.
DEF VAR xE AS INT.
DEF VAR xF AS INT.
DEF VAR xG AS INT.
DEF VAR xN AS INT.
DEF VAR xSemana AS INT.

ASSIGN
    xDia = DAY(pFecha)
    xMes = MONTH(pFecha)
    xAno = YEAR(pFecha).

IF xMes = 01 OR xMes = 02 THEN DO:  /* Enero o Febrero */
    ASSIGN
        xA = xAno - 1
        xB = TRUNCATE(xA / 4, 0) - TRUNCATE(xA / 100, 0) + TRUNCATE(xA / 400, 0)
        xC = TRUNCATE( (xA - 1) / 4, 0) - TRUNCATE( (xA - 1) / 100, 0) + TRUNCATE( (xA - 1) / 400, 0)
        xS = xB - xC
        xE = 0
        xF = xDia - 1 + (31 * (xMes - 1) ).
END.
ELSE DO:    /* de Marzo a Diciembre */
    ASSIGN
        xA = xAno
        xB = TRUNCATE(xA / 4, 0 ) - TRUNCATE(xA / 100, 0) + TRUNCATE(xA / 400, 0)
        xC = TRUNCATE( (xA - 1) / 4, 0 ) - TRUNCATE( (xA - 1) / 100, 0) + TRUNCATE( (xA - 1) / 400, 0)
        xS = xB - xC
        xE = xS + 1
        xF = xDia + TRUNCATE( ( ( 153 * (xMes - 3) ) + 2 ) / 5, 0) + 58 + xS.
END.
/* Adicionalmente sumándole 1 a la variable xF 
    se obtiene numero ordinal del dia de la fecha ingresada con referencia al año actual
    */
/* Estos cálculos se aplican a cualquier mes */
ASSIGN
    xG = (xA + xB) MODULO 7
    xD = (xF + xG - xE) MODULO 7 /* Adicionalmente esta variable nos indica el dia de la semana 0=Lunes, ... , 6=Domingo */
    xN = xF + 3 - xD.
IF xN < 0 THEN DO:
    /* Si la variable n es menor a 0 se trata de una semana perteneciente al año anterior */
    ASSIGN
        xSemana = 53 - TRUNCATE( (xG - xS) / 5, 0)
        xAno = xAno - 1.
END.
ELSE IF xN > (364 + xS) THEN DO:
    /* Si n es mayor a 364 + $s entonces la fecha corresponde a la primera semana del año siguiente.*/
    ASSIGN
        xSemana = 1
        xAno = xAno + 1.
END.
ELSE DO:
    /* En cualquier otro caso es una semana del año actual */
    xSemana = TRUNCATE(xN / 7, 0) + 1.
END.

ASSIGN
    pAno = xAno
    pSemana = xSemana.


        END PROCEDURE.
