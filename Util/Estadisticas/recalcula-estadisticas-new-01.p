DEF VAR x-CodFchI AS DATE.
DEF VAR x-CodFchF AS DATE.
DEF VAR FechaD AS INT.
DEF VAR FechaH AS INT.
DEF VAR s-CodCia AS INT INIT 001.
DEF VAR x-signo1 AS INT INIT 1.
DEF VAR x-ImpLin AS DEC NO-UNDO.
DEF VAR x-ImpTot AS DEC NO-UNDO.     /* IMporte NETO de venta */
DEF VAR x-TpoCmbCmp AS DECI INIT 1.
DEF VAR x-TpoCmbVta AS DECI INIT 1.
DEF VAR x-PorIgv AS DEC DECIMALS 4.

DEF VAR x-codven    AS CHAR.
DEF VAR x-fmapgo    as char.
DEF VAR x-canal     as char.
DEF VAR x-CodUnico  LIKE Gn-clie.CodUnico.
DEF VAR x-NroCard   LIKE GN-card.NroCard.
DEF VAR x-Sede      LIKE Gn-ClieD.Sede.
DEF VAR cl-CodCia   AS INT NO-UNDO.
DEF VAR pv-CodCia   AS INT NO-UNDO.
DEF VAR x-CodCli    LIKE Gn-clie.codcli.
DEF VAR x-Zona      AS CHAR NO-UNDO.
DEF VAR x-coe       AS DECI INIT 0.
DEF VAR x-can       AS DECI INIT 0.
DEF VAR f-factor    AS DECI INIT 0.
DEF VAR x-AlmDes    AS CHAR.
DEF VAR x-Tipo      AS CHAR.        /* MOSTRADOR o CREDITO */

DEF VAR s-clivar    AS CHAR FORMAT 'x(11)'.
DEF VAR s-CliUni    AS CHAR FORMAT 'x(11)' INIT '99999999999' NO-UNDO.

DEFINE VAR pCodDiv AS CHAR NO-UNDO.
DEFINE VAR pCanalVenta AS CHAR NO-UNDO.

DEF BUFFER B-CDOCU FOR CcbCdocu.
DEF BUFFER B-DDOCU FOR CcbDdocu.
DEF BUFFER B-DIVI  FOR Gn-Divi.

/* Fecha de Cierre */
ASSIGN
    x-CodFchI = 01/01/2010
    x-CodFchF = 12/31/2010.

ASSIGN
    FechaD = YEAR(x-CodFchI) * 10000 + MONTH(x-CodFchI) * 100 + DAY(x-CodFchI)
    FechaH = YEAR(x-CodFchF) * 10000 + MONTH(x-CodFchF) * 100 + DAY(x-CodFchF).

FIND FIRST Empresas WHERE Empresas.codcia = s-codcia NO-LOCK NO-ERROR.
IF AVAILABLE Empresas THEN DO:
    IF NOT Empresas.Campo-CodCli THEN cl-codcia = s-codcia.
    IF NOT Empresas.Campo-CodPro THEN pv-codcia = s-codcia.
END.
FIND FIRST FacCfgGn WHERE FacCFgGn.codcia = s-codcia NO-LOCK NO-ERROR.
s-CliVar = FacCfgGn.CliVar.


/* ESTADISTICAS */
DISPLAY 'borra estadisticas: ' DATETIME(TODAY, MTIME) SKIP. 
PAUSE 0.
RUN Borra-Estadisticas.

DISPLAY 'carga estadistica: ' DATETIME(TODAY, MTIME) SKIP. 
PAUSE 0.
RUN Carga-Estadisticas.

DISPLAY '** fin del proceso **' DATETIME(TODAY, MTIME) SKIP. 
PAUSE 0.


PROCEDURE Borra-Estadisticas:
/* ************************* */

FOR EACH dwh_ventas WHERE dwh_ventas.CodCia = s-codcia
    AND dwh_ventas.Fecha >= FechaD
    AND dwh_ventas.Fecha <= FechaH:
    DELETE dwh_ventas.
END.
FOR EACH dwh_ventas_cli WHERE dwh_ventas_cli.CodCia = s-codcia
    AND dwh_ventas_cli.Fecha >= FechaD
    AND dwh_ventas_cli.Fecha <= FechaH:
    DELETE dwh_ventas_cli.
END.
FOR EACH dwh_ventas_climat WHERE dwh_ventas_climat.CodCia = s-codcia
    AND dwh_ventas_climat.Fecha >= FechaD
    AND dwh_ventas_climat.Fecha <= FechaH:
    DELETE dwh_ventas_climat.
END.
FOR EACH dwh_ventas_vendcli WHERE dwh_ventas_vendcli.CodCia = s-codcia
    AND dwh_ventas_vendcli.Fecha >= FechaD
    AND dwh_ventas_vendcli.Fecha <= FechaH:
    DELETE dwh_ventas_vendcli.
END.

END PROCEDURE.

PROCEDURE Carga-Estadisticas:
/* ************************* */

FOR EACH dwh_ventas_cab NO-LOCK WHERE dwh_ventas_cab.CodCia = s-codcia
    AND dwh_ventas_cab.Fecha >= FechaD
    AND dwh_ventas_cab.Fecha <= FechaH,
    EACH dwh_ventas_det OF dwh_ventas_cab NO-LOCK,
    FIRST Almmmatg OF dwh_ventas_det NO-LOCK:
    /* VENTAS PRINCIPAL */
    RUN Carga-ventas.
    /* VENTAS POR CLIENTE */
    RUN Carga-cli.
    /* VENTAS POR CLIENTE Y PRODUCTO */
    RUN Carga-climat.
    /* VENTAS POR VENDEDOR Y CLIENTE */
    RUN Carga-vendcli.
END.

END PROCEDURE.

PROCEDURE Carga-ventas:
/* ******************* */
    FIND FIRST dwh_ventas WHERE dwh_ventas.codcia = s-codcia
        AND dwh_ventas.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas.codcli = dwh_ventas_cab.codcli
        AND dwh_ventas.codven = dwh_ventas_cab.codven
        AND dwh_Ventas.CodMat = dwh_ventas_det.codmat
        AND dwh_ventas.Tipo   = dwh_ventas_cab.tipo
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas THEN CREATE dwh_ventas.
    ASSIGN
        dwh_Ventas.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas.CodCli = dwh_ventas_cab.codcli
        dwh_Ventas.CodVen = dwh_ventas_cab.codven
        dwh_Ventas.CodMat = dwh_ventas_det.codmat
        dwh_Ventas.Tipo   = dwh_ventas_cab.tipo
        dwh_Ventas.Cantidad = dwh_ventas.cantidad + dwh_ventas_det.cantidad
        dwh_Ventas.ImpExtCIGV = dwh_ventas.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas.ImpExtSIGV = dwh_ventas.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas.ImpNacCIGV = dwh_ventas.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas.ImpNacSIGV = dwh_ventas.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas.CostoExtCIGV = dwh_ventas.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas.CostoExtSIGV = dwh_ventas.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas.CostoNacCIGV = dwh_ventas.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas.CostoNacSIGV = dwh_ventas.costonacsigv + dwh_ventas_det.costonacsigv.

END PROCEDURE.

PROCEDURE Carga-cli:
/* **************** */
    FIND FIRST dwh_ventas_cli WHERE dwh_ventas_cli.codcia = s-codcia
        AND dwh_ventas_cli.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_cli.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas_cli.codcli = dwh_ventas_cab.codcli
        AND dwh_ventas_cli.tipo   = dwh_ventas_cab.tipo
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_cli THEN CREATE dwh_ventas_cli.
    ASSIGN
        dwh_Ventas_cli.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas_cli.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas_cli.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas_cli.CodCli = dwh_ventas_cab.codcli
        dwh_Ventas_cli.Tipo   = dwh_ventas_cab.tipo
        dwh_Ventas_cli.ImpExtCIGV = dwh_ventas_cli.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas_cli.ImpExtSIGV = dwh_ventas_cli.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas_cli.ImpNacCIGV = dwh_ventas_cli.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas_cli.ImpNacSIGV = dwh_ventas_cli.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas_cli.CostoExtCIGV = dwh_ventas_cli.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas_cli.CostoExtSIGV = dwh_ventas_cli.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas_cli.CostoNacCIGV = dwh_ventas_cli.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas_cli.CostoNacSIGV = dwh_ventas_cli.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_Ventas_cli.PromExtCIGV = dwh_ventas_cli.promextcigv + dwh_ventas_det.promextcigv
        dwh_Ventas_cli.PromExtSIGV = dwh_ventas_cli.promextsigv + dwh_ventas_det.promextsigv
        dwh_Ventas_cli.PromNacCIGV = dwh_ventas_cli.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_Ventas_cli.PromNacSIGV = dwh_ventas_cli.promnacsigv + dwh_ventas_det.promnacsigv.

END PROCEDURE.

PROCEDURE Carga-climat:
/* ******************* */
        FIND FIRST dwh_ventas_climat WHERE dwh_ventas_climat.codcia = s-codcia
        AND dwh_ventas_climat.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_climat.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas_climat.codcli = dwh_ventas_cab.codcli
        AND dwh_Ventas_climat.CodFam = Almmmatg.CodFam
        AND dwh_Ventas_climat.SubFam = Almmmatg.SubFam
        AND dwh_Ventas_climat.DesMar = Almmmatg.DesMar
        AND dwh_Ventas_climat.Licencia = Almmmatg.Licencia[1]
        AND dwh_Ventas_climat.Tipo = dwh_ventas_cab.Tipo
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_climat THEN CREATE dwh_ventas_climat.
    ASSIGN
        dwh_Ventas_climat.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas_climat.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas_climat.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas_climat.CodCli = dwh_ventas_cab.codcli
        dwh_Ventas_climat.CodFam = Almmmatg.CodFam
        dwh_Ventas_climat.SubFam = Almmmatg.SubFam
        dwh_Ventas_climat.DesMar = Almmmatg.DesMar
        dwh_Ventas_climat.Licencia = Almmmatg.Licencia[1]
        dwh_Ventas_climat.Tipo     = dwh_ventas_cab.tipo
        dwh_Ventas_climat.ImpExtCIGV = dwh_ventas_climat.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas_climat.ImpExtSIGV = dwh_ventas_climat.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas_climat.ImpNacCIGV = dwh_ventas_climat.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas_climat.ImpNacSIGV = dwh_ventas_climat.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas_climat.CostoExtCIGV = dwh_ventas_climat.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas_climat.CostoExtSIGV = dwh_ventas_climat.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas_climat.CostoNacCIGV = dwh_ventas_climat.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas_climat.CostoNacSIGV = dwh_ventas_climat.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_Ventas_climat.PromExtCIGV = dwh_ventas_climat.promextcigv + dwh_ventas_det.promextcigv
        dwh_Ventas_climat.PromExtSIGV = dwh_ventas_climat.promextsigv + dwh_ventas_det.promextsigv
        dwh_Ventas_climat.PromNacCIGV = dwh_ventas_climat.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_Ventas_climat.PromNacSIGV = dwh_ventas_climat.promnacsigv + dwh_ventas_det.promnacsigv.
   
END PROCEDURE.

PROCEDURE Carga-vendcli:
/* ******************** */
    FIND FIRST dwh_ventas_vendcli WHERE dwh_ventas_vendcli.codcia = s-codcia
        AND dwh_ventas_vendcli.fecha = dwh_ventas_cab.fecha
        AND dwh_ventas_vendcli.coddiv = dwh_ventas_cab.coddiv
        AND dwh_ventas_vendcli.codcli = dwh_ventas_cab.codcli
        AND dwh_ventas_vendcli.codven = dwh_ventas_cab.codven
        AND dwh_ventas_vendcli.tipo   = dwh_ventas_cab.tipo
        NO-ERROR.
    IF NOT AVAILABLE dwh_ventas_vendcli THEN CREATE dwh_ventas_vendcli.
    ASSIGN
        dwh_Ventas_vendcli.Fecha = dwh_ventas_cab.fecha
        dwh_Ventas_vendcli.CodCia = dwh_ventas_cab.codcia
        dwh_Ventas_vendcli.CodDiv = dwh_ventas_cab.coddiv
        dwh_Ventas_vendcli.CodCli = dwh_ventas_cab.codcli
        dwh_Ventas_vendcli.CodVen = dwh_ventas_cab.codven
        dwh_Ventas_vendcli.Tipo   = dwh_ventas_cab.tipo
        dwh_Ventas_vendcli.ImpExtCIGV = dwh_ventas_vendcli.impextcigv + dwh_ventas_det.impextcigv
        dwh_Ventas_vendcli.ImpExtSIGV = dwh_ventas_vendcli.impextsigv + dwh_ventas_det.impextsigv
        dwh_Ventas_vendcli.ImpNacCIGV = dwh_ventas_vendcli.impnaccigv + dwh_ventas_det.impnaccigv
        dwh_Ventas_vendcli.ImpNacSIGV = dwh_ventas_vendcli.impnacsigv + dwh_ventas_det.impnacsigv
        dwh_Ventas_vendcli.CostoExtCIGV = dwh_ventas_vendcli.costoextcigv + dwh_ventas_det.costoextcigv
        dwh_Ventas_vendcli.CostoExtSIGV = dwh_ventas_vendcli.costoextsigv + dwh_ventas_det.costoextsigv
        dwh_Ventas_vendcli.CostoNacCIGV = dwh_ventas_vendcli.costonaccigv + dwh_ventas_det.costonaccigv
        dwh_Ventas_vendcli.CostoNacSIGV = dwh_ventas_vendcli.costonacsigv + dwh_ventas_det.costonacsigv
        dwh_Ventas_vendcli.PromExtCIGV = dwh_ventas_vendcli.promextcigv + dwh_ventas_det.promextcigv
        dwh_Ventas_vendcli.PromExtSIGV = dwh_ventas_vendcli.promextsigv + dwh_ventas_det.promextsigv
        dwh_Ventas_vendcli.PromNacCIGV = dwh_ventas_vendcli.promnaccigv + dwh_ventas_det.promnaccigv
        dwh_Ventas_vendcli.PromNacSIGV = dwh_ventas_vendcli.promnacsigv + dwh_ventas_det.promnacsigv.

END PROCEDURE.
