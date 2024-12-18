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
DEF INPUT PARAMETER s-TpoPed AS CHAR.   /* Tipo de Pedido */
DEF INPUT PARAMETER pCodDiv AS CHAR.    /* Lista de Precios */
DEF INPUT PARAMETER S-CODCLI AS CHAR.
DEF INPUT PARAMETER S-CODMON AS INT.
DEF INPUT-OUTPUT PARAMETER S-UNDVTA AS CHAR.
DEF OUTPUT PARAMETER f-Factor AS DEC.       /* Default 1 */
DEF INPUT PARAMETER S-CODMAT AS CHAR.
DEF INPUT PARAMETER S-CNDVTA AS CHAR.       /* Condicion de venta */     
DEF INPUT PARAMETER X-CANPED AS DEC.
DEF INPUT PARAMETER x-NroDec AS INT.
DEF OUTPUT PARAMETER F-PREBAS AS DEC DECIMALS 4.
DEF OUTPUT PARAMETER F-PREVTA AS DEC DECIMALS 4.    /* Precio - Dscto CondVta - ClasfCliente */
DEF OUTPUT PARAMETER F-DSCTOS AS DEC.       /* Descuento incluido en el precio unitario base */
DEF OUTPUT PARAMETER Y-DSCTOS AS DEC.       /* Descuento por Volumen y/o Promocional */
DEF OUTPUT PARAMETER Z-DSCTOS AS DEC.       /* Descuento por evento */
DEF OUTPUT PARAMETER X-TIPDTO AS CHAR.      /* Tipo de descuento aplicado (PROM, VOL) */ 
/* Se anula este par�metro por compatibilidad  */
DEF INPUT PARAMETER pClfCli AS CHAR.        /* SOlo si tiene un valor me sirve */
/* */
DEF OUTPUT PARAMETER f-FleteUnitario AS DEC.
DEF INPUT  PARAMETER s-TipVta AS CHAR.      /* Lista "A" o "B" (SOLO POR COMPATIBILIDAD) */
DEF INPUT PARAMETER pError AS LOG.          /* Mostrar el error en pantalla */

/* VARIABLES GLOBALES */
DEF SHARED VAR S-CODCIA AS INT.
DEF SHARED VAR cl-codcia AS INT.
DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR s-coddiv AS CHAR.

/* TIPO DE PEDIDO:
R: Remates
S: Supermercados
M: Contrato Marco Institucionales
P: Provincias 
N: Normal
E: Expolibrerias
VU: ValesUtilex
LF: Lista Express WEB
LU: Lista Express UTILEX
MM: Contrato Marco Especial
*/

/* VARIABLES LOCALES */
DEF VAR S-TPOCMB AS DEC NO-UNDO.
DEF VAR MaxCat AS DEC NO-UNDO.
DEF VAR MaxVta AS DEC NO-UNDO.
DEF VAR x-ClfCli  AS CHAR INIT "C" NO-UNDO.      /* Clasificacion para productos propios */
DEF VAR x-ClfCli2 AS CHAR INIT "C" NO-UNDO.      /* Clasificacion para productos de terceros */
DEF VAR X-PREVTA1 AS DECI NO-UNDO.
DEF VAR X-PREVTA2 AS DECI NO-UNDO.

/* VARIABLES DESCUENTOS CALCULADOS */
DEF VAR x-DctoPromocional AS DECIMAL DECIMALS 4 NO-UNDO.
DEF VAR x-DctoxVolumen    AS DECIMAL DECIMALS 4 NO-UNDO.

/* CONTROL POR DIVISION */
DEF VAR x-FlgDtoVol LIKE GN-DIVI.FlgDtoVol NO-UNDO.
DEF VAR x-FlgDtoProm LIKE GN-DIVI.FlgDtoProm NO-UNDO.
DEF VAR x-FlgDtoClfCli LIKE GN-DIVI.FlgDtoClfCli NO-UNDO.
DEF VAR x-FlgDtoCndVta LIKE GN-DIVI.FlgDtoCndVta NO-UNDO.
DEF VAR x-Libre_C01 LIKE GN-DIVI.Libre_C01 NO-UNDO.     /* Dcto Exluyenbres, acumulados o el mejor */
DEF VAR x-Ajuste-por-flete AS LOG NO-UNDO.

/* CONFIGURACIONES DE LA DIVISION */
/* OJO: Configuraci�n de la LISTA DE PRECIOS */
FIND gn-divi WHERE gn-divi.codcia = s-codcia AND gn-divi.coddiv = pCodDiv NO-LOCK.
ASSIGN
    x-FlgDtoVol = GN-DIVI.FlgDtoVol             /* Descuento por Volumen */
    x-FlgDtoProm = GN-DIVI.FlgDtoProm           /* Descuento Promocional */
    x-FlgDtoClfCli = GN-DIVI.FlgDtoClfCli       /* Descuento por Clasificacion */
    x-FlgDtoCndVta = GN-DIVI.FlgDtoCndVta       /* Descuento por venta */
    x-Libre_C01 = GN-DIVI.Libre_C01             /* Tipo de descuento */
    x-Ajuste-por-flete = GN-DIVI.Campo-Log[4].  /* Factor de Ajuste por Flete */

/* CATALOGO DEL PRODUCTO */
FIND FIRST Almmmatg WHERE Almmmatg.codcia = s-codcia AND Almmmatg.codmat = s-codmat NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN DO:
    IF pError = YES THEN MESSAGE 'Producto' s-CodMat 'NO registrado en el cat�logo general' VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.

/* CLIENTE */
IF s-CodCli > '' THEN DO:
    FIND FIRST gn-clie WHERE gn-clie.CodCia = CL-CODCIA 
        AND gn-clie.CodCli = S-CODCLI 
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE gn-clie AND pError = YES THEN DO:
        MESSAGE 'Cliente (' s-codcli ') NO registrado' SKIP
            'Comunicarse con gestor del maestro de clientes' SKIP
            'Se continuar� el c�lculo asumiendo la clasificaci�n C '
            VIEW-AS ALERT-BOX WARNING.
    END.
END.

DEF VAR pDiasDctoVol AS INT INIT 60 NO-UNDO.    /* Tope Para % Descuento por Volumen (hasta 60 dias ) */
DEF VAR pDiasDctoPro AS INT INIT 60 NO-UNDO.    /* Tope Para % Descuento Promocional (hasta 60 dias ) */

/* Ic - 23Ene2020, clasificacion de Cliente por Linea de producto */
DEFINE BUFFER x-vtatabla FOR vtatabla.
DEFINE VAR x-tabla-clsfclie-x-linea AS CHAR.

x-tabla-clsfclie-x-linea = "CLSF_CLIE_X_LINEA".

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
         HEIGHT             = 9.85
         WIDTH              = 56.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* *************************************************************************** */
/* ******************* CALCULO GENERAL DEL PRECIO DE VENTA ******************* */
/* *************************************************************************** */
ASSIGN
    MaxCat = 0
    MaxVta = 0
    F-Factor = 1                    /* Valor por Defecto */
    F-PreVta = Almmmatg.PreOfi.     /* Valor por Defecto */
    F-PreBas = Almmmatg.PreOfi.     /* Valor por Defecto */
/* *************************************************************************** */
/* PRECIOS ESPECIALES POR CONTRATO MARCO Y REMATES */
/* *************************************************************************** */
CASE s-TpoPed:
    WHEN "M" THEN DO:       /* CASO ESPECIAL -> Ventas CONTRATO MARCO */
        RUN Precio-Contrato-Marco.
        IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
        /* *************************************************************************** */
        /* OJO: Control de Precio Base */
        F-PREBAS = F-PREVTA.
        /* *************************************************************************** */
        IF RETURN-VALUE = "OK" THEN RETURN 'OK'.
        /* SI DEVUELVE ADM-OK buscamos precio de acuerdo a la divisi�n activa */
    END.
    WHEN "R" THEN DO:       /* CASO ESPECIAL -> REMATES */
        RUN Precio-Remate.
        IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
        /* *************************************************************************** */
        /* OJO: Control de Precio Base */
        F-PREBAS = F-PREVTA.
        /* *************************************************************************** */
        RETURN 'OK'.
    END.
    WHEN "LU" THEN DO:       /* LISTA EXPRESS UTILEX */
        RUN Precio-Utilex.
        IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
        /* *************************************************************************** */
        /* OJO: Control de Precio Base */
        F-PREBAS = F-PREVTA.
        /* *************************************************************************** */
        RETURN 'OK'.
    END.
END CASE.
/* *************************************************************************** */
/* CLASIFICACION DEL CLIENTE */
/* *************************************************************************** */
ASSIGN
    x-ClfCli  = "C"         /* Valores por defecto */
    x-ClfCli2 = "C".
IF AVAIL gn-clie AND gn-clie.clfcli  > '' THEN x-ClfCli  = gn-clie.clfcli.
IF AVAIL gn-clie AND gn-clie.clfcli2 > '' THEN x-ClfCli2 = gn-clie.clfcli2.

/* Ic - 23Ene2020, Clasificacion de Cliente por Linea de Producto */
/* RHC 07/05/2020 NO se usa */
/* FIND FIRST x-vtatabla WHERE x-vtatabla.codcia = s-codcia AND                        */
/*                             x-vtatabla.tabla = x-tabla-clsfclie-x-linea AND         */
/*                             x-vtatabla.llave_c1 = S-CODCLI AND                      */
/*                             x-vtatabla.llave_c2 = almmmatg.codfam NO-LOCK NO-ERROR. */
/* IF AVAILABLE x-vtatabla THEN DO:                                                    */
/*     x-ClfCli  = TRIM(x-vtatabla.libre_c01).                                         */
/*     x-ClfCli2 = TRIM(x-vtatabla.libre_c02).                                         */
/* END.                                                                                */
/* IF pClfCli > '' THEN ASSIGN x-ClfCli = pClfCli x-ClfCli2 = pClfCli.                 */
/* *************************************************************************** */
/* DE ACUERDO AL TIPO DE COTIZACION */
/* *************************************************************************** */
CASE TRUE:
    WHEN s-TpoPed = "E" THEN DO:       /* EXPOLIBRERIA */
        RUN Precio-Expolibreria.
        IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
    END.
    OTHERWISE DO:
        RUN Precio-Tiendas.
        IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
    END.
END CASE.
/* ***************************************************************************************** */
/* DESCUENTO ESPECIAL POR EVENTO Y POR DIVISION (SOLO SI NO TIENE DESCUENTO POR VOL O PROMO) */
/* ***************************************************************************************** */
ASSIGN z-Dsctos = 0.
IF y-Dsctos = 0 THEN DO:
    FIND FIRST VtaDtoEsp WHERE Vtadtoesp.codcia = s-codcia
        AND Vtadtoesp.coddiv = pCodDiv
        AND TODAY >= Vtadtoesp.FechaD 
        AND TODAY <= Vtadtoesp.FechaH
        NO-LOCK NO-ERROR.
    FIND FIRST gn-convt WHERE gn-ConVt.Codig = s-cndvta NO-LOCK NO-ERROR.
    IF AVAILABLE Vtadtoesp AND AVAILABLE gn-convt THEN DO:
        CASE Almmmatg.CHR__02:
            WHEN "P" THEN DO:       /* PROPIOS */
                IF gn-ConVt.TipVta = "1" THEN z-Dsctos = VtaDtoEsp.DtoPropios[1].   /* CONTADO */
                IF gn-ConVt.TipVta = "2" THEN z-Dsctos = VtaDtoEsp.DtoPropios[2].   /* CREDITO */
            END.
            WHEN "T" THEN DO:       /* TERCEROS */
                IF gn-ConVt.TipVta = "1" THEN z-Dsctos = VtaDtoEsp.DtoTerceros[1].   /* CONTADO */
                IF gn-ConVt.TipVta = "2" THEN z-Dsctos = VtaDtoEsp.DtoTerceros[2].   /* CREDITO */
            END.
        END CASE.
    END.
END.
/* *************************************************************************** */
/* *************************************************************************** */
/* Determinamos el Flete Unitario */
/* *************************************************************************** */
/* 1ro. Factor Harold Segura */
/* *************************************************************************** */

RUN vtagn/flete-unitario-general-v01.p (s-CodMat,
                                        pCodDiv,
                                        S-CODMON,
                                        f-Factor,
                                        OUTPUT f-FleteUnitario).
/* *************************************************************************** */
/* 2do. Factor Karin Rodhenberg */
/* *************************************************************************** */
RUN gn/factor-porcentual-flete-v2.p (INPUT pcoddiv, 
                                     INPUT s-CodMat,
                                     INPUT-OUTPUT f-FleteUnitario, 
                                     INPUT s-TpoPed, 
                                     INPUT f-factor, 
                                     INPUT s-CodMon).
/* *************************************************************************** */
/* RHC 08/02/2020 Solo clientes VIP HUANCAYO */
/* ***************************************************************************************** */
RUN VIP-Huancayo.
IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
/* ***************************************************************************************** */
/* OJO: Control de Precio Base */
F-PREBAS = F-PREVTA.
/* *************************************************************************** */
/* ***************************************************************************************** */
/* RHC 29/01/2020 
    Transferencias Gratuitas sea modificado considerando siempre el 
    Precio Neto de la �ltima Compra de cada cliente
*/
/* ***************************************************************************************** */
/* IF s-CndVta = "900" AND LOOKUP(s-CodDiv, '00097,00017,00019') > 0 THEN DO: */
/*     f-FleteUnitario = 0.                                                   */
/*     f-PreBas = 0.                                                          */
/*     f-PreVta = 0.                                                          */
/*     y-dsctos = 0.                                                          */
/*     z-dsctos = 0.                                                          */
/*     f-dsctos = 0.                                                          */
/*     x-tipdto = ''.                                                         */
/*     f-PreVta = f-PreBas.                                                   */
/*     RUN lib/RedondearMas (F-PREBAS, X-NRODEC, OUTPUT F-PREVTA).            */
/*     f-PreBas = f-PreVta.                                                   */
/* END.                                                                       */

RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Descuentos-Promocionales-Especiales) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Descuentos-Promocionales-Especiales Procedure 
PROCEDURE Descuentos-Promocionales-Especiales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* RHC 14/10/2013 *************************************************** */
/* SOLO PARA PROVINCIAS EN TODAS SUS MODALIDADES DE VENTA             */
/* PRECIO ESPECIAL PRODUCTO PROPIOS                                   */
/* PRECIO ESPECIAL PRODUCTO DE TERCEROS PARA LISTAS ACTIVAS "A" Y "B" */
/* ****************************************************************** */
IF s-TpoPed <> "P" THEN RETURN.

CASE TRUE:
    WHEN Almmmatg.CHR__02 = "T"                 /* PRODUCTOS DE TERCEROS */
        AND LOOKUP(s-TipVta, "A,B") > 0         /* LISTAS A y B */
        AND Almmmatg.PromMinDivi[1] = "00018"   /* <<< OJO <<< */
        AND TODAY >= Almmmatg.PromMinFchD[1] 
        AND TODAY <= Almmmatg.PromMinFchH[1]
        THEN DO:
            /* FILTRO DE ERRORES */
            IF s-TipVta = "A" AND Almmmatg.PromMinDto[1] < 0 THEN DO:
                IF pError = YES THEN MESSAGE 'Producto' Almmmatg.CodMat 'tiene un precio negativo en la lista especial'
                    VIEW-AS ALERT-BOX WARNING.
                RETURN "ADM-ERROR".
            END.
            IF s-TipVta = "B" AND Almmmatg.PromMinDto[2] < 0 THEN DO:
                IF pError = YES THEN MESSAGE 'Producto' Almmmatg.CodMat 'tiene un precio negativo en la lista especial'
                    VIEW-AS ALERT-BOX WARNING.
                RETURN "ADM-ERROR".
            END.
            /* Determinamos el Precio Base */
            IF s-TipVta = "A" AND Almmmatg.PromMinDto[1] > 0 THEN DO:
                ASSIGN
                    F-DSCTOS = 0                    /* Dcto por ClfCli y/o Cnd Vta */
                    Y-DSCTOS = 0                    /* Dcto por Volumen o Promocional */
                    Z-DSCTOS = 0                    /* Dcto por Evento */
                    X-TIPDTO = ""
                    /*f-PreBas = ROUND( Almmmatg.PreVta[1] * ( 1 - Almmmatg.PromMinDto[1] / 100 ), 4).*/
                    f-PreBas = Almmmatg.PromMinDto[1].
                /* ******************************** */
                /* PRECIO BASE A LA MONEDA DE VENTA */
                /* ******************************** */
                IF S-CODMON = 1 THEN DO:
                    IF Almmmatg.MonVta = 1 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                    ELSE ASSIGN F-PREBAS = F-PREBAS * S-TPOCMB /** F-FACTOR*/.
                END.
                IF S-CODMON = 2 THEN DO:
                    IF Almmmatg.MonVta = 2 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                    ELSE ASSIGN F-PREBAS = (F-PREBAS / S-TPOCMB) /** F-FACTOR*/.
                END.
                F-PREVTA = F-PREBAS * (1 - F-DSCTOS / 100).     /* Precio de venta descontado */
                /************************************************/
                /*RUN BIN/_ROUND1(F-PREVTA,X-NRODEC,OUTPUT F-PREVTA).*/
                RUN lib/RedondearMas (F-PREVTA, X-NRODEC, OUTPUT F-PREVTA).
                /************************************************/
            END.
            IF s-TipVta = "B" AND Almmmatg.PromMinDto[2] > 0 THEN DO:
                ASSIGN
                    F-DSCTOS = 0                    /* Dcto por ClfCli y/o Cnd Vta */
                    Y-DSCTOS = 0                    /* Dcto por Volumen o Promocional */
                    Z-DSCTOS = 0                    /* Dcto por Evento */
                    X-TIPDTO = ""
                    /*f-PreBas = ROUND( Almmmatg.PreVta[1] * ( 1 - Almmmatg.PromMinDto[2] / 100 ), 4).*/
                    f-PreBas = Almmmatg.PromMinDto[2].
                /* ******************************** */
                /* PRECIO BASE A LA MONEDA DE VENTA */
                /* ******************************** */
                IF S-CODMON = 1 THEN DO:
                    IF Almmmatg.MonVta = 1 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                    ELSE ASSIGN F-PREBAS = F-PREBAS * S-TPOCMB /** F-FACTOR*/.
                END.
                IF S-CODMON = 2 THEN DO:
                    IF Almmmatg.MonVta = 2 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                    ELSE ASSIGN F-PREBAS = (F-PREBAS / S-TPOCMB) /** F-FACTOR*/.
                END.
                F-PREVTA = F-PREBAS * (1 - F-DSCTOS / 100).     /* Precio de venta descontado */
                /************************************************/
                /*RUN BIN/_ROUND1(F-PREVTA,X-NRODEC,OUTPUT F-PREVTA).*/
                RUN lib/RedondearMas (F-PREVTA, X-NRODEC, OUTPUT F-PREVTA).
                /************************************************/
            END.
        END.
    WHEN Almmmatg.CHR__02 = "P"                 /* PRODUCTOS PROPIOS */
        AND Almmmatg.PromMinDivi[1] = "00018"    /* <<< OJO <<< */
        AND TODAY >= Almmmatg.PromMinFchD[1] 
        AND TODAY <= Almmmatg.PromMinFchH[1]
        THEN DO:
            /* FILTRO DE ERRORES */
            IF Almmmatg.PromMinDto[1] < 0 THEN DO:
                IF pError = YES THEN MESSAGE 'Producto' Almmmatg.CodMat 'tiene un descuento negativo en la lista especial'
                    VIEW-AS ALERT-BOX WARNING.
                RETURN "ADM-ERROR".
            END.
            /* Determinamos el Precio Base */
            IF Almmmatg.PromMinDto[1] > 0 THEN DO:
                ASSIGN
                    F-DSCTOS = 0                    /* Dcto por ClfCli y/o Cnd Vta */
                    Y-DSCTOS = 0                    /* Dcto por Volumen o Promocional */
                    Z-DSCTOS = 0                    /* Dcto por Evento */
                    X-TIPDTO = ""
                    f-PreBas = ROUND( Almmmatg.PreVta[1] * ( 1 - Almmmatg.PromMinDto[1] / 100 ), 4).
                /* ******************************** */
                /* PRECIO BASE A LA MONEDA DE VENTA */
                /* ******************************** */
                IF S-CODMON = 1 THEN DO:
                    IF Almmmatg.MonVta = 1 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                    ELSE ASSIGN F-PREBAS = F-PREBAS * S-TPOCMB /** F-FACTOR*/.
                END.
                IF S-CODMON = 2 THEN DO:
                    IF Almmmatg.MonVta = 2 THEN ASSIGN F-PREBAS = F-PREBAS /** F-FACTOR*/.
                    ELSE ASSIGN F-PREBAS = (F-PREBAS / S-TPOCMB) /** F-FACTOR*/.
                END.
                F-PREVTA = F-PREBAS * (1 - F-DSCTOS / 100).     /* Precio de venta descontado */
                /************************************************/
                /*RUN BIN/_ROUND1(F-PREVTA,X-NRODEC,OUTPUT F-PREVTA).*/
                RUN lib/RedondearMas (F-PREVTA, X-NRODEC, OUTPUT F-PREVTA).
                /************************************************/
            END.
        END.
END CASE.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precio-Contrato-Marco) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Contrato-Marco Procedure 
PROCEDURE Precio-Contrato-Marco :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* RHC 26/03/2015 DESCUENTO PROMOCIONAL Y VOLUMEN */
FIND FIRST Almmmatp OF Almmmatg NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatp THEN DO:
    IF pError = NO THEN RETURN "ADM-ERROR".
    MESSAGE 'Producto' Almmmatg.CodMat 'NO definido en la lista de CONTRATO MARCO' SKIP(1)
        '�Tomamos los precios de la lista por defecto?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE rpta AS LOG.
    IF rpta = NO THEN RETURN 'ADM-ERROR'.
    ELSE RETURN 'ADM-OK'.
END.
/* PRECIO BASE Y UNIDAD DE VENTA */
ASSIGN
    F-PreBas = Almmmatp.PreOfi
    s-UndVta = Almmmatp.Chr__01
    pDiasDctoPro = 999999
    x-FlgDtoClfCli = NO
    x-FlgDtoCndVta = NO
    x-FlgDtoProm = YES
    x-FlgDtoVol = YES
    x-Libre_C01 = ""    /* EXCLUYENTES */
    .

{pri/PrecioVentaMayorCredito.i &Tabla=Almmmatp ~
    &PreVta=Almmmatp.PreOfi ~
    &Promocional="~
        DO J = 1 TO 10:~
            IF Almmmatp.PromDivi[J] = pCodDiv~
                AND TODAY >= Almmmatp.PromFchD[J]~
                AND TODAY <= Almmmatp.PromFchH[J]~
                AND Almmmatp.PromDto[J] > 0 THEN DO:~
                x-DctoPromocional = Almmmatp.PromDto[J].~
            END.~
        END."}

/* DESCUENTOS ADICIONALES POR DIVISION */
z-Dsctos = 0.
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precio-Division) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Division Procedure 
PROCEDURE Precio-Division :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* PRECIO BASE Y UNIDAD DE VENTA */
FIND FIRST VtaListaMay OF Almmmatg WHERE VtaListaMay.CodDiv = pCodDiv NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaListaMay THEN DO:
    IF pError = YES THEN MESSAGE 'Producto' Almmmatg.CodMat 'NO definido en la lista de precios de la divisi�n' pCodDiv
        VIEW-AS ALERT-BOX WARNING.
    RETURN "ADM-ERROR".
END.

ASSIGN
    F-PreBas = VtaListaMay.PreOfi
    s-UndVta = VtaListaMay.Chr__01.

ASSIGN 
    f-Factor = 1.
/* Revisemos el factor de conversi�n */
IF Almmmatg.UndBas <> s-UndVta THEN DO:
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas AND 
        Almtconv.Codalter = s-UndVta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
        IF pError = YES THEN MESSAGE 'NO est� configurado el factor de equivalencia para el producto' Almmmatg.codmat
            'Unidad de venta:' s-UndVta VIEW-AS ALERT-BOX WARNING.
        RETURN "ADM-ERROR".
  END.
  f-Factor = Almtconv.Equival.
END.

/* RHC 20/10/2015 Configuraci�n por cada producto */
CASE VtaListaMay.FlagDesctos:
    WHEN 1 THEN                     /* SIN DESCUENTOS */
        ASSIGN
            x-FlgDtoClfCli = NO
            x-FlgDtoCndVta = NO 
            x-FlgDtoVol    = YES    /* NO 27/09/20717 Solicitado por Nohemi */
            x-FlgDtoProm   = NO.
END CASE.
/* ********************************************** */
/* ****************************************************************************************** */
/* Rutima �nica */
/* ****************************************************************************************** */
{pri/PrecioVentaMayorCredito.i &Tabla=VtaListaMay ~
    &PreVta=VtaListaMay.PreOfi ~
    &Promocional="~
    /* Tomamos el mayor descuento */ ~
    DEF VAR x-Old-Descuento AS DEC NO-UNDO. ~
    x-Old-Descuento = 0. ~
    FOR EACH VtaDctoProm NO-LOCK WHERE VtaDctoProm.CodCia = s-CodCia AND ~
        VtaDctoProm.CodDiv = pCodDiv AND ~
        VtaDctoProm.CodMat = Almmmatg.CodMat AND ~
        VtaDctoProm.FlgEst = 'A' AND ~
        (TODAY >= VtaDctoProm.FchIni AND TODAY <= VtaDctoProm.FchFin): ~
        /* Solo en caso de EVENTOS existe el VIP, MR */ ~
        x-DctoPromocional = VtaDctoProm.Descuento. ~
        x-DctoPromocional = MAXIMUM(x-DctoPromocional, x-Old-Descuento). ~
        x-Old-Descuento = x-DctoPromocional. ~
    END. ~
    " }
/* ****************************************************************************************** */
/* ****************************************************************************************** */

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precio-Empresa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Empresa Procedure 
PROCEDURE Precio-Empresa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* PRECIO BASE Y UNIDAD DE VENTA */
ASSIGN
    F-PreBas = Almmmatg.PreOfi
    s-UndVta = Almmmatg.Chr__01.

/* Valores por Defecto: De la Lista de Precios por Divisi�n */
IF TRUE <> (s-UndVta > '') THEN ASSIGN s-UndVta = Almmmatg.UndBas.
IF TRUE <> (s-UndVta > '') THEN DO:
    IF pError = YES THEN MESSAGE 'Producto' Almmmatg.CodMat 'NO definido la unidad de venta'
        VIEW-AS ALERT-BOX WARNING.
    RETURN "ADM-ERROR".
END.

ASSIGN 
    f-Factor = 1.
/* Revisemos el factor de conversi�n */
IF Almmmatg.UndBas <> s-UndVta THEN DO:
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas AND 
        Almtconv.Codalter = s-UndVta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
        IF pError = YES THEN MESSAGE 'NO est� configurado el factor de equivalencia para el producto' Almmmatg.codmat
            'Unidad de venta:' s-UndVta VIEW-AS ALERT-BOX WARNING.
        RETURN "ADM-ERROR".
  END.
  f-Factor = Almtconv.Equival.
END.

/* PROCEDIMIENTO NORMAL */
/* ****************************************************************************************** */
/* Rutina �nica */
/* ****************************************************************************************** */
{pri/PrecioVentaMayorCredito.i &Tabla=Almmmatg ~
    &PreVta=Almmmatg.PreVta[1] ~
    &Promocional="~
    /* Tomamos el mayor descuento */ ~
    DEF VAR x-Old-Descuento AS DEC NO-UNDO. ~
    x-Old-Descuento = 0. ~
    FOR EACH VtaDctoProm NO-LOCK WHERE VtaDctoProm.CodCia = s-CodCia AND ~
        VtaDctoProm.CodDiv = pCodDiv AND ~
        VtaDctoProm.CodMat = Almmmatg.CodMat AND ~
        VtaDctoProm.FlgEst = 'A' AND ~
        (TODAY >= VtaDctoProm.FchIni AND TODAY <= VtaDctoProm.FchFin): ~
        /* Solo en caso de EVENTOS existe el VIP, MR */ ~
        x-DctoPromocional = VtaDctoProm.Descuento. ~
        x-DctoPromocional = MAXIMUM(x-DctoPromocional, x-Old-Descuento). ~
        x-Old-Descuento = x-DctoPromocional. ~
    END. ~
    " }
/* ****************************************************************************************** */
/* ****************************************************************************************** */
IF GN-DIVI.CanalVenta = "TDA" AND s-codmon = 1 AND Almmmatg.codfam = '011' THEN DO:
    CASE TRUE:
        WHEN gn-ConVt.TotDias > 15 AND gn-ConVt.TotDias <= 30 THEN
            ASSIGN
            f-PreVta = f-Prevta * (1 + 0 / 100)
            f-PreBas = f-PreBas * (1 + 0 / 100).
        WHEN gn-ConVt.TotDias > 30 AND gn-ConVt.TotDias <= 45 THEN
            ASSIGN
            f-PreVta = f-Prevta * (1 + 1 / 100)
            f-PreBas = f-PreBas * (1 + 1 / 100). 
    END CASE.
END.
/*MESSAGE 'tres f-prebas: ' f-prebas.*/
/* ****************************************************** */
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precio-Expolibreria) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Expolibreria Procedure 
PROCEDURE Precio-Expolibreria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Solo pasan por aqu� si la lista de precios est� definid POR DIVISION */
IF NOT (GN-DIVI.VentaMayorista = 2) THEN DO:
    IF pError = YES THEN MESSAGE 'La lista de precios NO es una LISTA POR DIVISION:' pCodDiv
        VIEW-AS ALERT-BOX WARNING.
    RETURN "ADM-ERROR".
END.

/* *************************************************************************** */
/* PRECIO BASE Y UNIDAD DE VENTA */
/* *************************************************************************** */
FIND FIRST VtaListaMay OF Almmmatg WHERE VtaListaMay.CodDiv = pCodDiv NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaListaMay THEN DO:
    IF pError = YES THEN MESSAGE 'Producto' Almmmatg.CodMat 'NO definido en la lista de precios:' pCodDiv
        VIEW-AS ALERT-BOX WARNING.
    RETURN "ADM-ERROR".
END.
/* Valores por Defecto: De la Lista de Precios por Divisi�n */
IF VtaListaMay.CHR__01 > '' THEN ASSIGN s-UndVta = VtaListaMay.Chr__01.
IF TRUE <> (s-UndVta > '') THEN ASSIGN s-UndVta = Almmmatg.UndBas.
IF TRUE <> (s-UndVta > '') THEN DO:
    IF pError = YES THEN MESSAGE 'Producto' Almmmatg.CodMat 'NO definido la unidad de venta en la lista:' pCodDiv
        VIEW-AS ALERT-BOX WARNING.
    RETURN "ADM-ERROR".
END.
ASSIGN 
    f-Factor = 1
    F-PreBas = VtaListaMay.PreOfi.       /* Precio Lista Por Defecto */
/* Revisemos el factor de conversi�n */
IF Almmmatg.UndBas <> s-UndVta THEN DO:
    FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas AND 
        Almtconv.Codalter = s-UndVta NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
        IF pError = YES THEN MESSAGE 'NO est� configurado el factor de equivalencia para el producto' Almmmatg.codmat
            'Unidad de venta:' s-UndVta VIEW-AS ALERT-BOX WARNING.
        RETURN "ADM-ERROR".
  END.
  f-Factor = Almtconv.Equival.
END.
/* ****************************************************************************************** */
/* CONFIGURACION DE PRECIOS EVENTO */
/* ****************************************************************************************** */
/* Si no ubica una configuraci�n toma por defecto la de la divisi�n */
FOR EACH VtaDTabla NO-LOCK WHERE VtaDTabla.CodCia = s-CodCia AND
    VtaDTabla.Tabla = 'CFGLP' AND
    VtaDTabla.Llave = pCodDiv AND
    VtaDTabla.Tipo = Almmmatg.CodFam:
    IF Almmmatg.SubFam = VtaDTabla.Libre_c01 THEN NEXT.     /* Subfamilia (-) */
    IF VtaDTabla.LlaveDetalle > '' AND Almmmatg.SubFam <> VtaDTabla.LlaveDetalle THEN NEXT. /* Subfamilia (+) */
    /* CALCULO DE PRECIO Y DESCUENTO POR PRODUCTO */
    x-FlgDtoCndVta = VtaDTabla.Libre_l01.
    x-FlgDtoClfCli = VtaDTabla.Libre_l02.
    x-FlgDtoVol    = VtaDTabla.Libre_l03.
    x-FlgDtoProm   = VtaDTabla.Libre_l04.
    x-Libre_C01    = VtaDTabla.Libre_c02.   /* "" Excluyentes, "A" acumulados */
    LEAVE.
END.
/* ****************************************************************************************** */
/* ****************************************************************************************** */
/* ****************************************** */
/* Verificar que el producto es SIN DESCUENTO */
/* ****************************************** */
CASE VtaListaMay.FlagDesctos:
    WHEN 1 THEN                     /* SIN DESCUENTOS */
        ASSIGN
            x-FlgDtoClfCli = NO
            x-FlgDtoCndVta = NO.
END CASE.
/* ****************************************************************************************** */
/* Rutima �nica */
/* ****************************************************************************************** */
{pri/PrecioVentaMayorCredito.i &Tabla=VtaListaMay ~
    &PreVta=VtaListaMay.PreOfi ~
    &Promocional="~
    /* Tomamos el mayor descuento */ ~
    DEF VAR x-Old-Descuento AS DEC NO-UNDO. ~
    x-Old-Descuento = 0. ~
    FOR EACH VtaDctoProm NO-LOCK WHERE VtaDctoProm.CodCia = s-CodCia AND ~
        VtaDctoProm.CodDiv = pCodDiv AND ~
        VtaDctoProm.CodMat = Almmmatg.CodMat AND ~
        VtaDctoProm.FlgEst = 'A' AND ~
        (TODAY >= VtaDctoProm.FchIni AND TODAY <= VtaDctoProm.FchFin): ~
        /* Solo en caso de EVENTOS existe el VIP, MR */ ~
        IF gn-divi.CanalVenta = 'FER' THEN DO: ~
            CASE gn-clie.LocCli:  ~
                WHEN 'VIP' THEN x-DctoPromocional = VtaDctoProm.DescuentoVIP.  ~
                WHEN 'MR' THEN x-DctoPromocional = VtaDctoProm.DescuentoMR. ~
                OTHERWISE x-DctoPromocional = VtaDctoProm.Descuento. ~
            END CASE. ~
        END. ~
        ELSE DO: ~
            x-DctoPromocional = VtaDctoProm.Descuento. ~
        END.
        x-DctoPromocional = MAXIMUM(x-DctoPromocional, x-Old-Descuento). ~
        x-Old-Descuento = x-DctoPromocional. ~
    END. ~
    " }
/* ****************************************************************************************** */
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precio-Remate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Remate Procedure 
PROCEDURE Precio-Remate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* FACTOR DE EQUIVALENCIA */
s-UndVta = Almmmatg.Chr__01.
FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas
    AND Almtconv.Codalter = s-undvta
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtconv THEN DO:
    IF pError = YES THEN MESSAGE 'NO est� configurado el factor de equivalencia para el producto' Almmmatg.codmat SKIP
        '   Unidad Stock:' Almmmatg.UndBas SKIP
        'Unidad de Venta:' s-UndVta
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
F-FACTOR = Almtconv.Equival.

FIND VtaTabla WHERE Vtatabla.codcia = s-codcia
    AND Vtatabla.tabla = 'REMATES'
    AND Vtatabla.llave_c1 = s-codmat
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN DO:
    IF pError = YES THEN MESSAGE "Producto en REMATE NO tiene precio de venta"
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
IF s-CodMon = Almmmatg.MonVta THEN F-PREBAS = VtaTabla.Valor[1].
ELSE IF s-CodMon = 1 THEN F-PREBAS = ROUND ( VtaTabla.Valor[1] * Almmmatg.TpoCmb, 6 ).
ELSE F-PREBAS = ROUND ( VtaTabla.Valor[1] / Almmmatg.TpoCmb, 6 ).
     
ASSIGN
    F-PREVTA = F-PREBAS /** f-Factor*/.
RUN lib/RedondearMas (F-PREVTA, X-NRODEC, OUTPUT F-PREVTA).

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precio-Tiendas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Tiendas Procedure 
PROCEDURE Precio-Tiendas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* *************************************************************************** */
    /************ Descuento Promocional ************/
    /* RHC 15/07/17 LAS SIGUIENTES REGLAS VIENEN DE EVENTOS ANTERIORES */
    /* Solo afecta a las familias 011 y 013 con excepci�n de (familia 013 y subfamilia 014) */
    /* *************************************************************************** */
    PROMOCIONAL:    /* RHC 01/10/2020 Y.M. solo para l�nea 013 */
    DO:
        IF LOOKUP(Almmmatg.CodFam, "013") = 0 THEN LEAVE PROMOCIONAL.
        IF Almmmatg.CodFam = "013" AND Almmmatg.SubFam = "014" THEN LEAVE PROMOCIONAL.
        ASSIGN                                           
            x-FlgDtoVol = YES                                
            x-FlgDtoProm = NO                                
            x-FlgDtoClfCli = YES                             
            x-FlgDtoCndVta = YES                             
            /*x-Libre_C01 = "A"           /* ACUMULATIVAS */   */
            x-ClfCli = "C"              /* FORZAMOS A "C" */ 
            x-ClfCli2 = "C".    
    END.

    PROMOCIONAL:    /* RHC 29/09/2020 Y.M. no va la l�nea 013 */
    DO:
        IF LOOKUP(Almmmatg.CodFam, "011") = 0 THEN LEAVE PROMOCIONAL.
        ASSIGN                                           
            x-FlgDtoVol = YES                                
            x-FlgDtoProm = NO                                
            x-FlgDtoClfCli = YES                             
            x-FlgDtoCndVta = YES                             
            /*x-Libre_C01 = "A"           /* ACUMULATIVAS */  */
            x-Libre_C01 = ""           /* ACUMULATIVAS - Daniel Llican, con autorizacion de Mayra Padilla - 08Abr2022 */
            x-ClfCli = "C"              /* FORZAMOS A "C" */ 
            x-ClfCli2 = "C".    
    END.
    /* *************************************************************************** */
    /* *************************************************************************** */
    /* 12/04/2022: Para la divisi�n 00018 (PROVINCIAS) configurable -> SLM Caso 69512 */
    /* 29/04/2022 A�n no hay una aprobaci�n, se bloquea hasta nuevo aviso */
    /* *************************************************************************** */
    /* *************************************************************************** */
/*     IF pCodDiv = '00018' THEN DO:                                                                                        */
/*         /* Si no ubica una configuraci�n toma por defecto la de la divisi�n */                                           */
/*         FOR EACH VtaDTabla NO-LOCK WHERE VtaDTabla.CodCia = s-CodCia AND                                                 */
/*             VtaDTabla.Tabla = 'CFGLP' AND                                                                                */
/*             VtaDTabla.Llave = pCodDiv AND                                                                                */
/*             VtaDTabla.Tipo = Almmmatg.CodFam:                                                                            */
/*             IF Almmmatg.SubFam = VtaDTabla.Libre_c01 THEN NEXT.     /* Subfamilia (-) */                                 */
/*             IF VtaDTabla.LlaveDetalle > '' AND Almmmatg.SubFam <> VtaDTabla.LlaveDetalle THEN NEXT. /* Subfamilia (+) */ */
/*             /* CALCULO DE PRECIO Y DESCUENTO POR PRODUCTO */                                                             */
/*             x-FlgDtoCndVta = VtaDTabla.Libre_l01.                                                                        */
/*             x-FlgDtoClfCli = VtaDTabla.Libre_l02.                                                                        */
/*             x-FlgDtoVol    = VtaDTabla.Libre_l03.                                                                        */
/*             x-FlgDtoProm   = VtaDTabla.Libre_l04.                                                                        */
/*             x-Libre_C01    = VtaDTabla.Libre_c02.   /* "" Excluyentes, "A" acumulados */                                 */
/*             LEAVE.                                                                                                       */
/*         END.                                                                                                             */
/*     END.                                                                                                                 */
    /* *************************************************************************** */
    /* *************************************************************************** */
    /* RHC 17/02/2015 NO aplica descuento por volumen SOLO DIVISION ORIGINAL (s-coddiv) 00018 */
    /* Ic - 20Feb2014 - Se incluye la familia 012 x indicacion de E.Macchiu */
    IF s-TpoPed = "P" AND (Almmmatg.CodFam = "010" OR Almmmatg.CodFam = "012") THEN DO:
        ASSIGN x-FlgDtoVol = NO.
    END.
    /* *************************************************************************** */
    /* RHC 19/02/2015 En caso de Fotocopias (011) se amplia el l�mite a 60 dias */
    /* *************************************************************************** */
    IF Almmmatg.CodFam = "011" THEN pDiasDctoVol = 60.
    IF s-CodMon = 2 AND Almmmatg.CodFam = "011" THEN s-CndVta = "000".   /* Contado */
    /* **************************************************************************** */
    /* Ic - 29Ene2016, ValesUtiles sin Dscto */
    /* **************************************************************************** */
    IF s-TpoPed = "VU" THEN DO:
        ASSIGN 
            x-FlgDtoClfCli = NO 
            x-FlgDtoCndVta = NO.
    END.
    /* *************************************************************************** */
    /* RHC 22/07/2016  TRANSFERENCIA GRATUITA */
    /* *************************************************************************** */
    IF S-CNDVTA = "899" THEN DO:
        ASSIGN
            s-CndVta = "000".
    END.
    IF LOOKUP(S-CNDVTA, "899,900") > 0 THEN
        ASSIGN                           
        x-ClfCli = "C"              /* FORZAMOS A "C" */ 
        x-ClfCli2 = "C"
        /*s-CndVta = "000"*/
        x-FlgDtoVol = NO
        x-FlgDtoProm = NO                                
        x-FlgDtoClfCli = YES
        x-FlgDtoCndVta = YES.
    /* *************************************************************************** */
    /* PRECIO DE VENTA Y DESCUENTOS PROMOCIONALES O POR VOLUMEN DE VENTA  */
    /* *************************************************************************** */
    CASE gn-divi.VentaMayorista:
        WHEN 1 THEN DO:     /* Lista de Precios General */
            RUN Precio-Empresa.
            IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
        END.
        WHEN 2 THEN DO:     /* Lista de Precio por Divisi�n */
            RUN Precio-Division.
            IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
        END.
    END CASE.
    /* *************************************************************************** */
    /* RHC 14/10/2013 *************************************************** */
    /* SOLO PARA PROVINCIAS EN TODAS SUS MODALIDADES DE VENTA             */
    /* PRECIO ESPECIAL PRODUCTO DE TERCEROS PARA LISTAS ACTIVAS "A" Y "B" */
    /* *************************************************************************** */
/*     RUN Descuentos-Promocionales-Especiales.               */
/*     IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR". */

    RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precio-Utilex) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Utilex Procedure 
PROCEDURE Precio-Utilex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND FIRST VtaListaMinGn OF Almmmatg NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaListaMinGn THEN DO:
    MESSAGE 'Producto' Almmmatg.CodMat 'NO definido en la lista de precios minorista'
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
ASSIGN
    s-UndVta = VtaListaMinGn.Chr__01.

/* FACTOR DE EQUIVALENCIA */
FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
    AND Almtconv.Codalter = s-undvta
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtconv THEN DO:
    MESSAGE 'NO est� configurado el factor de equivalencia para el producto' Almmmatg.codmat SKIP
        'Unidad de venta:' s-UndVta
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.


/* RHC 12.06.08 tipo de cambio de la familia */
ASSIGN
    s-TpoCmb = Almmmatg.TpoCmb.
    
/* PRECIO BASE  */
IF S-CODMON = 1 THEN DO:
    IF Almmmatg.MonVta = 1
    THEN ASSIGN F-PREBAS = VtaListaMinGn.PreOfi.
    ELSE ASSIGN F-PREBAS = VtaListaMinGn.PreOfi * S-TPOCMB.
END.
IF S-CODMON = 2 THEN DO:
    IF Almmmatg.MonVta = 2
    THEN ASSIGN F-PREBAS = VtaListaMinGn.PreOfi.
    ELSE ASSIGN F-PREBAS = (VtaListaMinGn.PreOfi / S-TPOCMB).
END.
/* Definimos el precio de venta y el descuento aplicado */    
F-PREVTA = F-PREBAS * (1 - F-DSCTOS / 100).     /* Precio de venta descontado */

/* RHC 13/02/2014 PRODUCTOS SIN NINGUN TIPO DE DESCUENTO (SELLO ROJO )*/ 
DEF VAR x-Dia-de-Hoy AS DATE NO-UNDO.

x-Dia-de-Hoy = TODAY.
/* DESCUENTO ADICIONALES POR PROMOCION Y POR VOLUMEN DE VENTA */
ASSIGN
    Y-DSCTOS = 0
    Z-DSCTOS = 0.
DEF VAR J       AS INT          NO-UNDO.
DEF VAR X-CANTI AS DECI INIT 0  NO-UNDO.
DEF VAR X-RANGO AS DECI INIT 0  NO-UNDO.
DO J = 1 TO 10:
    IF VtaListaMinGn.PromDivi[J] = pCodDiv 
        AND x-Dia-de-Hoy >= VtaListaminGn.PromFchD[J] 
        AND TODAY <= VtaListaminGn.PromFchH[J] THEN DO:
        Y-DSCTOS = VtaListaMinGn.PromDto[J].
    END.
END.
X-CANTI = X-CANPED * F-Factor.
/* Determinamos el mejor descuento */
ASSIGN
    X-RANGO = 0.
DO J = 1 TO 10:
    IF X-CANTI >= VtaListaMinGn.DtoVolR[J] AND VtaListaMinGn.DtoVolR[J] > 0  THEN DO:
        IF X-RANGO  = 0 THEN X-RANGO = VtaListaMinGn.DtoVolR[J].
        IF X-RANGO <= VtaListaMinGn.DtoVolR[J] THEN DO:
            Z-DSCTOS = VtaListaMinGn.DtoVolD[J].
        END.   
    END.   
END.
IF Y-DSCTOS <> 0 OR Z-DSCTOS <> 0 THEN DO:
    IF Y-DSCTOS > Z-DSCTOS THEN DO:
        /* DESCUENTO PROMOCIONAL */
        IF GN-DIVI.FlgDtoProm = YES THEN DO:
            DO J = 1 TO 10:
                IF VtaListaMinGn.PromDivi[J] = pCodDiv 
                    AND x-Dia-de-Hoy >= VtaListaminGn.PromFchD[J] 
                    AND TODAY <= VtaListaminGn.PromFchH[J] 
                    AND VtaListaMinGn.PromDto[J] <> 0
                    THEN DO:
                    ASSIGN
                        F-DSCTOS = 0
                        F-PREVTA = VtaListaMinGn.PreOfi
                        Y-DSCTOS = VtaListaMinGn.PromDto[J]
                        X-TIPDTO = "PROM".
                    IF Almmmatg.Monvta = 1 THEN
                      ASSIGN X-PREVTA1 = F-PREVTA
                             X-PREVTA2 = ROUND(F-PREVTA / s-TpoCmb,6).
                    ELSE
                      ASSIGN X-PREVTA2 = F-PREVTA
                             X-PREVTA1 = ROUND(F-PREVTA * s-TpoCmb,6).
                    X-PREVTA1 = X-PREVTA1.
                    X-PREVTA2 = X-PREVTA2.
                END.
            END.
        END.
    END.
    ELSE DO:
        /* DESCUENTO POR VOLUMEN */
        ASSIGN
            X-CANTI = X-CANPED * F-Factor
            X-RANGO = 0.
        DO J = 1 TO 10:
            IF X-CANTI >= VtaListaMinGn.DtoVolR[J] AND VtaListaMinGn.DtoVolR[J] > 0  THEN DO:
                /* Determinamos cu�l es mayor, el Promocional o Por Vol�men */
                IF X-RANGO  = 0 THEN X-RANGO = VtaListaMinGn.DtoVolR[J].
                IF X-RANGO <= VtaListaMinGn.DtoVolR[J] THEN DO:
                    ASSIGN
                        X-RANGO  = VtaListaMinGn.DtoVolR[J]
                        F-DSCTOS = 0
                        F-PREVTA = VtaListaMinGn.PreOfi
                        Y-DSCTOS = VtaListaMinGn.DtoVolD[J] 
                        X-TIPDTO = "VOL".
                    IF Almmmatg.MonVta = 1 THEN
                       ASSIGN X-PREVTA1 = F-PREVTA
                              X-PREVTA2 = ROUND(F-PREVTA / s-TpoCmb,6).
                    ELSE
                       ASSIGN X-PREVTA2 = F-PREVTA
                              X-PREVTA1 = ROUND(F-PREVTA * s-TpoCmb,6).
                    X-PREVTA1 = X-PREVTA1.
                    X-PREVTA2 = X-PREVTA2.
                END.   
            END.   
        END.
    END.
    /* PRECIO FINAL */
    IF X-PREVTA1 + X-PREVTA2 > 0 THEN DO:
        IF S-CODMON = 1 THEN F-PREVTA = X-PREVTA1.
        ELSE F-PREVTA = X-PREVTA2.     
        IF S-CODMON = 1 THEN F-PREBAS = X-PREVTA1.
        ELSE F-PREBAS = X-PREVTA2.     
        /* RECALCULAMOS PRECIOS EN CASO DE PROMOCIONAL */
        IF x-TipDto = "PROM" THEN DO:
            ASSIGN
                F-PREVTA = F-PREBAS * (1 - Y-DSCTOS / 100)
                Y-DSCTOS = 0.
        END.
    END.    
END.

RUN lib/RedondearMas (F-PREBAS, X-NRODEC, OUTPUT F-PREBAS).
RUN lib/RedondearMas (F-PREVTA, X-NRODEC, OUTPUT F-PREVTA).

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VIP-Huancayo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VIP-Huancayo Procedure 
PROCEDURE VIP-Huancayo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF s-CodDiv <> "00072" THEN RETURN 'OK'.
                          
/* RHC 08/02/2020 Solo clientes VIP HUANCAYO */
FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia AND
    VtaTabla.Tabla = "CLIENTE_VIP_CONTI" AND
    VtaTabla.Llave_c1 = s-CodCli
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaTabla THEN RETURN 'OK'.

DEF VAR X-FACTOR AS DEC.

HUANCAYO:
DO:
    /* Buscamos si el art�culo est� en la lista de especiales */
    FIND VtaListaMay WHERE VtaListaMay.CodCia = s-CodCia AND
        VtaListaMay.CodDiv = s-CodDiv AND
        VtaListaMay.codmat = s-CodMat
        NO-LOCK NO-ERROR.
    IF AVAILABLE VtaListaMay THEN DO:
        /* Verifacamos Vigencia */
        IF VtaListaMay.PromFchD <> ?  AND TODAY < VtaListaMay.PromFchD THEN LEAVE HUANCAYO.
        IF VtaListaMay.PromFchH <> ?  AND TODAY > VtaListaMay.PromFchH THEN LEAVE HUANCAYO.
        /* NO hay descuento ni nada */
        ASSIGN
            Y-DSCTOS = 0
            /*F-FleteUnitario = 0*/
            F-DSCTOS = 0
            Z-DSCTOS = 0
            F-PREVTA = 0
            F-PREBAS = 0
            X-FACTOR = 1
            X-TIPDTO = "".
        /* El precio de la lista siempre est� siempre en soles */
        ASSIGN
            F-PREVTA = VtaListaMay.PreOfi
            F-PREBAS = VtaListaMay.PreOfi.
        /* Factor de Conversi�n */
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = VtaListaMay.CHR__01
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
            MESSAGE 'NO est� configurado el factor de equivalencia para el producto' Almmmatg.codmat SKIP
                'Unidad de venta:' VtaListaMay.CHR__01 SKIP
                'Revisar la lista de precios especial para clientes VIP'
                VIEW-AS ALERT-BOX ERROR.
            RETURN "ADM-ERROR".
        END.
        X-FACTOR = Almtconv.Equival.    /* Normalmente ser� 1 */
        ASSIGN
            F-PREVTA = F-PREVTA * X-FACTOR.     /* En unidades de STOCK */
        ASSIGN
            F-PREVTA = F-PREVTA / F-FACTOR.     /* Tn unidade de VENTA */
        ASSIGN
            F-PREBAS = F-PREVTA.
        RUN lib/RedondearMas (F-PREVTA, X-NRODEC, OUTPUT F-PREVTA).
    END.
END.
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

