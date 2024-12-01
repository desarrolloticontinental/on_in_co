&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
         HEIGHT             = 5
         WIDTH              = 47.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
DEF INPUT PARAMETER S-CODDIV AS CHAR.
DEF INPUT PARAMETER S-CODCLI AS CHAR.
DEF INPUT PARAMETER S-CODMON AS INT.
DEF INPUT PARAMETER S-TPOCMB AS DEC.
DEF OUTPUT PARAMETER S-UNDVTA AS CHAR.
DEF OUTPUT PARAMETER f-Factor AS DEC.
DEF INPUT PARAMETER S-CODMAT AS CHAR.
DEF INPUT PARAMETER S-CNDVTA AS CHAR.
DEF INPUT PARAMETER X-CANPED AS DEC.
DEF INPUT PARAMETER x-NroDec AS INT.

DEF OUTPUT PARAMETER F-PREBAS AS DEC.
DEF OUTPUT PARAMETER F-PREVTA AS DEC.
DEF OUTPUT PARAMETER F-DSCTOS AS DEC.
DEF OUTPUT PARAMETER Y-DSCTOS AS DEC.
DEF OUTPUT PARAMETER Z-DSCTOS AS DEC.

/* VARIABLES GLOBALES */
DEF SHARED VAR S-CODCIA AS INT.
DEF SHARED VAR cl-codcia AS INT.

/* VARIABLES LOCALES */
DEF VAR MaxCat AS DEC NO-UNDO.
DEF VAR MaxVta AS DEC NO-UNDO.
DEF VAR x-ClfCli  AS CHAR NO-UNDO.      /* Clasificacion para productos propios */
DEF VAR x-ClfCli2 AS CHAR NO-UNDO.      /* Clasificacion para productos de terceros */
DEFINE VARIABLE X-PREVTA1 AS DECI NO-UNDO.
DEFINE VARIABLE X-PREVTA2 AS DECI NO-UNDO.
/************ Descuento Promocional ************/
DEFINE VAR J AS INTEGER.
DEFINE VAR X-RANGO AS INTEGER INIT 0.   
DEFINE VAR X-CANTI AS DECI    INIT 0.   


/* *************************************************************************** */
/* ******************* CALCULO GENERAL DEL PRECIO DE VENTA ******************* */
/* *************************************************************************** */
FIND Almmmatg WHERE Almmmatg.codcia = s-codcia AND Almmmatg.codmat = s-codmat NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almmmatg THEN DO:
/*     MESSAGE "Codigo del producto no existe" VIEW-AS ALERT-BOX ERROR. */
    RETURN "ADM-ERROR".
END.

ASSIGN
    MaxCat = 0
    MaxVta = 0
    F-PreBas = Almmmatg.PreOfi.

/* CONFIGURACIONES DE LA DIVISION */
FIND gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = s-coddiv NO-LOCK.

/* LISTAS DE PRECIOS */
CASE gn-divi.VentaMayorista:
    WHEN 1 THEN DO:
        RUN Precio-Empresa.
        IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
    END.
    WHEN 2 THEN DO:
        RUN Precio-Division.
        IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".
    END.
END CASE.



RETURN "OK".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Precio-Division) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Division Procedure 
PROCEDURE Precio-Division :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* DESCUENTO POR CLASIFICACION DEL CLIENTE */
FIND gn-clie WHERE gn-clie.CodCia = CL-CODCIA AND gn-clie.CodCli = S-CODCLI NO-LOCK.
ASSIGN
    x-ClfCli  = "C"         /* Valor por defecto */
    x-ClfCli2 = "C".
IF AVAIL gn-clie AND gn-clie.clfcli <> '' THEN X-CLFCLI  = gn-clie.clfcli.
IF AVAIL gn-clie AND gn-clie.clfcli2 <> '' THEN x-ClfCli2 = gn-clie.clfcli2.

/* DESCUENTOS POR CLASIFICACION DE CLIENTES */
FIND ClfClie WHERE ClfClie.Categoria = X-CLFCLI NO-LOCK NO-ERROR.
IF AVAIL ClfClie THEN DO:
  IF Almmmatg.Chr__02 = "P" THEN 
      MaxCat = ClfClie.PorDsc.
  ELSE 
      MaxCat = ClfClie.PorDsc1.
END.
/* DESCUENTO POR CONDICION DE VENTA */    
FIND Dsctos WHERE Dsctos.CndVta = S-CNDVTA
    AND  Dsctos.clfCli = Almmmatg.Chr__02
    NO-LOCK NO-ERROR.
IF AVAIL Dsctos THEN MaxVta = Dsctos.PorDto.

/* FILTROS FINALES POR CONFIGURACION DE LA DIVISION */
IF GN-DIVI.FlgDtoClfCli = NO 
THEN MaxCat = 0.
ELSE IF x-ClfCli = 'C' AND GN-DIVI.PorDtoClfCli > 0 THEN MaxCat = GN-DIVI.PorDtoClfCli.
IF GN-DIVI.FlgDtoCndVta = NO THEN MaxVta = 0.
/* *********************************************** */

/* DESCUENTO TOTAL APLICADO */    
F-DSCTOS = (1 - (1 - MaxCat / 100) * (1 - MaxVta / 100)) * 100.

FIND FIRST VtaListaMay OF Almmmatg WHERE VtaListaMay.CodDiv = s-CodDiv NO-LOCK NO-ERROR.
IF NOT AVAILABLE VtaListaMay THEN DO:
/*     MESSAGE 'Producto NO definido en la lista de precios de esta divisi�n'  */
/*         VIEW-AS ALERT-BOX ERROR.                                            */
    RETURN "ADM-ERROR".
END.
ASSIGN
    s-UndVta = VtaListaMay.Chr__01.

/* FACTOR DE EQUIVALENCIA */
FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
    AND Almtconv.Codalter = s-undvta
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtconv THEN DO:
/*     MESSAGE "Equivalencia NO definida" SKIP               */
/*         "Unidad base :" Almmmatg.undbas SKIP              */
/*         "Unidad venta:" s-undvta VIEW-AS ALERT-BOX ERROR. */
    RETURN "ADM-ERROR".
END.
F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.

/* RHC 12.06.08 tipo de cambio de la familia */
s-tpocmb = VtaListaMay.TpoCmb.     /* �? */

/* PRECIO BASE  */
IF S-CODMON = 1 THEN DO:
    IF VtaListaMay.MonVta = 1 
    THEN ASSIGN F-PREBAS = VtaListaMay.PreOfi.
    ELSE ASSIGN F-PREBAS = VtaListaMay.PreOfi * S-TPOCMB.
END.
IF S-CODMON = 2 THEN DO:
    IF VtaListaMay.MonVta = 2 
    THEN ASSIGN F-PREBAS = VtaListaMay.PreOfi.
    ELSE ASSIGN F-PREBAS = (VtaListaMay.PreOfi / S-TPOCMB).
END.

/* Definimos el precio de venta y el descuento aplicado */    
F-PREVTA = F-PREBAS * (1 - F-DSCTOS / 100).     /* Precio de venta descontado */

/* DESCUENTO ADICIONALES POR PROMOCION Y POR VOLUMEN DE VENTA */
Y-DSCTOS = 0.        
FIND gn-convt WHERE gn-convt.Codig = S-CNDVTA NO-LOCK NO-ERROR.
IF AVAILABLE gn-convt AND  gn-convt.totdias <= 15 THEN DO:
    /************ Descuento Promocional ************/ 
    IF GN-DIVI.FlgDtoProm = YES THEN DO:
        IF TODAY >= VtaListaMay.PromFchD AND TODAY <= VtaListaMay.PromFchH THEN DO:
            F-DSCTOS = VtaListaMay.PromDto.
            F-PREVTA = VtaListaMay.PreOfi * (1 - F-DSCTOS / 100).
            Y-DSCTOS = VtaListaMay.PromDto.
            IF VtaListaMay.Monvta = 1 THEN 
              ASSIGN X-PREVTA1 = F-PREVTA
                     X-PREVTA2 = ROUND(F-PREVTA / VtaListaMay.TpoCmb,6).
            ELSE
              ASSIGN X-PREVTA2 = F-PREVTA
                     X-PREVTA1 = ROUND(F-PREVTA * VtaListaMay.TpoCmb,6).
            X-PREVTA1 = X-PREVTA1.
            X-PREVTA2 = X-PREVTA2.
         END.   
    END.
    /*************** Descuento por Volumen ****************/
    IF GN-DIVI.FlgDtoVol = YES THEN DO:
        X-CANTI = X-CANPED.
        DO J = 1 TO 10:
            IF X-CANTI >= VtaListaMay.DtoVolR[J] AND VtaListaMay.DtoVolR[J] > 0  THEN DO:
                IF X-RANGO  = 0 THEN X-RANGO = VtaListaMay.DtoVolR[J].
                IF X-RANGO <= VtaListaMay.DtoVolR[J] THEN DO:
                    X-RANGO  = VtaListaMay.DtoVolR[J].
                    F-DSCTOS = VtaListaMay.DtoVolD[J].         
                    F-PREVTA = VtaListaMay.PreOfi * (1 - F-DSCTOS / 100).
                    Y-DSCTOS = VtaListaMay.DtoVolD[J] .
                    IF VtaListaMay.MonVta = 1 THEN 
                       ASSIGN X-PREVTA1 = F-PREVTA
                              X-PREVTA2 = ROUND(F-PREVTA / VtaListaMay.TpoCmb,6).
                    ELSE
                       ASSIGN X-PREVTA2 = F-PREVTA
                              X-PREVTA1 = ROUND(F-PREVTA * VtaListaMay.TpoCmb,6).
                    X-PREVTA1 = X-PREVTA1.
                    X-PREVTA2 = X-PREVTA2.
                END.   
            END.   
        END.
    END.
    /* PRECIO FINAL */
    IF X-PREVTA1 + X-PREVTA2 > 0 THEN DO:
        IF S-CODMON = 1 
        THEN F-PREVTA = X-PREVTA1.
        ELSE F-PREVTA = X-PREVTA2.     
    END.    
END.

/* RECALCULAMOS EL PRECIO BASE */
F-PREBAS = F-PREVTA / ( 1 - F-DSCTOS / 100 ).

/************************************************/
RUN BIN/_ROUND1(F-PREVTA,X-NRODEC,OUTPUT F-PREVTA).

/* DESCUENTOS ADICIONALES POR DIVISION */
z-Dsctos = 0.
IF y-Dsctos = 0 THEN DO:
    FIND FIRST VtaDtoEsp WHERE Vtadtoesp.codcia = s-codcia
        AND Vtadtoesp.coddiv = s-coddiv
        AND TODAY >= Vtadtoesp.FechaD 
        AND TODAY <= Vtadtoesp.FechaH
        NO-LOCK NO-ERROR.
    FIND gn-convt WHERE gn-ConVt.Codig = s-cndvta NO-LOCK NO-ERROR.
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

/* RHC 06.11.10 PARCHE 
IF s-coddiv = '00000' AND gn-divi.VentaMayorista = 2 THEN DO:
    IF LOOKUP(s-CodMat, '040153,040154,033251,035722,035725,033252,037904,033711,026925,037914,015725,026930') > 0 
        THEN z-Dsctos = 5.
    IF LOOKUP(s-CodMat, '018178,031237,023030,037923,040152,040155,035387') > 0 
        THEN z-Dsctos = 5.
    IF LOOKUP(s-CodMat, '023063,039755,037917') > 0 
        THEN z-Dsctos = 10.
END.
*/

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Precio-Empresa) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-Empresa Procedure 
PROCEDURE Precio-Empresa :
./*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* DESCUENTO POR CLASIFICACION DEL CLIENTE */

FIND FIRST gn-clie WHERE gn-clie.CodCia = CL-CODCIA 
    AND gn-clie.CodCli = S-CODCLI 
    NO-LOCK NO-ERROR.
ASSIGN
    x-ClfCli  = "C"         /* Valor por defecto */
    x-ClfCli2 = "C".
MESSAGE cl-codcia s-codcli.
IF NOT AVAILABLE gn-clie THEN MESSAGE 'que raro'.
IF AVAIL gn-clie AND gn-clie.clfcli <> '' THEN X-CLFCLI  = gn-clie.clfcli.
IF AVAIL gn-clie AND gn-clie.clfcli2 <> '' THEN x-ClfCli2 = gn-clie.clfcli2.

CASE Almmmatg.CodFam:
    WHEN "000" OR WHEN "001" OR WHEN "002" THEN DO:
        /* Calculamos el margen de utilidad */
        DEF VAR x-MejPre AS DEC INIT 0 NO-UNDO.
        DEF VAR x-MejMar AS DEC INIT 0 NO-UNDO.
        DEF VAR x-Cols AS CHAR INIT 'A++,A+,A-,C' NO-UNDO.

        x-MejPre = Almmmatg.PreOfi * 0.9312.    /* (- 4% - 3%) */
        IF Almmmatg.CtoTot > 0 THEN x-MejMar = ( x-MejPre - Almmmatg.CtoTot ) / Almmmatg.CtoTot * 100.
        FIND FIRST VtaTabla WHERE VtaTabla.codcia = s-codcia
            AND VtaTabla.Tabla = 'DTOTER'
            AND x-MejMar >= VtaTabla.Rango_valor[1] 
            AND x-MejMar <  VtaTabla.Rango_valor[2]
            NO-LOCK NO-ERROR.
        IF AVAILABLE VtaTabla THEN DO:
            IF LOOKUP(x-ClfCli2, x-Cols) > 0 THEN MaxCat = VtaTabla.Valor[LOOKUP(x-ClfCli2, x-Cols)].
        END.
        x-MejPre = x-MejPre * ( 1 - MaxCat / 100 ).
        MaxCat = ( 1 - ( x-MejPre / Almmmatg.PreVta[1] ) ) * 100.
    END.
    OTHERWISE DO:
        FIND ClfClie WHERE ClfClie.Categoria = X-CLFCLI NO-LOCK NO-ERROR.
        IF AVAIL ClfClie THEN DO:
          IF Almmmatg.Chr__02 = "P" THEN 
              MaxCat = ClfClie.PorDsc.
          ELSE 
              MaxCat = ClfClie.PorDsc1.
        END.
        /* DESCUENTO POR CONDICION DE VENTA */    
        FIND Dsctos WHERE Dsctos.CndVta = S-CNDVTA
            AND  Dsctos.clfCli = Almmmatg.Chr__02
            NO-LOCK NO-ERROR.
        IF AVAIL Dsctos THEN MaxVta = Dsctos.PorDto.
        MESSAGE x-clfcli SKIP maxcat SKIP s-cndvta almmmatg.CHR__02 SKIP maxvta.
    END.
END CASE.
IF GN-DIVI.FlgDtoClfCli = NO THEN MaxCat = 0.
IF GN-DIVI.FlgDtoCndVta = NO THEN MaxVta = 0.

/* DESCUENTO TOTAL APLICADO */    
F-DSCTOS = (1 - (1 - MaxCat / 100) * (1 - MaxVta / 100)) * 100.
MESSAGE 'descuento' f-dsctos.
/* FACTOR DE EQUIVALENCIA */
ASSIGN
    s-UndVta = Almmmatg.Chr__01.

FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
    AND Almtconv.Codalter = s-undvta
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtconv THEN DO:
/*     MESSAGE "Equivalencia NO definida" SKIP               */
/*         "Unidad base :" Almmmatg.undbas SKIP              */
/*         "Unidad venta:" s-undvta VIEW-AS ALERT-BOX ERROR. */
    RETURN "ADM-ERROR".
END.
F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.

/* RHC 12.06.08 tipo de cambio de la familia */
s-tpocmb = Almmmatg.TpoCmb.     /* �? */

/* PRECIO BASE  */
IF S-CODMON = 1 THEN DO:
    IF Almmmatg.MonVta = 1 
    THEN ASSIGN F-PREBAS = F-PREBAS * F-FACTOR.
    ELSE ASSIGN F-PREBAS = F-PREBAS * S-TPOCMB * F-FACTOR.
END.
IF S-CODMON = 2 THEN DO:
    IF Almmmatg.MonVta = 2 
    THEN ASSIGN F-PREBAS = F-PREBAS * F-FACTOR.
    ELSE ASSIGN F-PREBAS = (F-PREBAS / S-TPOCMB) * F-FACTOR.
END.

/* Definimos el precio de venta y el descuento aplicado */    
F-PREVTA = F-PREBAS * (1 - F-DSCTOS / 100).     /* Precio de venta descontado */

/* DESCUENTO ADICIONALES POR PROMOCION Y POR VOLUMEN DE VENTA */
Y-DSCTOS = 0.        
FIND gn-convt WHERE gn-convt.Codig = S-CNDVTA NO-LOCK NO-ERROR.
IF AVAILABLE gn-convt AND  gn-convt.totdias <= 15 THEN DO:
    /************ Descuento Promocional ************/ 
    IF GN-DIVI.FlgDtoProm = YES THEN DO:
        DO J = 1 TO 10:
            IF Almmmatg.PromDivi[J] = S-CODDIV 
                    AND TODAY >= Almmmatg.PromFchD[J] 
                    AND TODAY <= Almmmatg.PromFchH[J] THEN DO:
                F-DSCTOS = Almmmatg.PromDto[J] .         
                F-PREVTA = Almmmatg.Prevta[1] * (1 - F-DSCTOS / 100).
                Y-DSCTOS = Almmmatg.PromDto[J] .
                IF Almmmatg.Monvta = 1 THEN 
                  ASSIGN X-PREVTA1 = F-PREVTA
                         X-PREVTA2 = ROUND(F-PREVTA / Almmmatg.TpoCmb,6).
                ELSE
                  ASSIGN X-PREVTA2 = F-PREVTA
                         X-PREVTA1 = ROUND(F-PREVTA * Almmmatg.TpoCmb,6).
                X-PREVTA1 = X-PREVTA1 * F-FACTOR.
                X-PREVTA2 = X-PREVTA2 * F-FACTOR.             
             END.   
        END.
    END.
    /*************** Descuento por Volumen ****************/
    IF GN-DIVI.FlgDtoVol = YES THEN DO:
        X-CANTI = X-CANPED * F-FACTOR .
        DO J = 1 TO 10:
            IF X-CANTI >= Almmmatg.DtoVolR[J] AND Almmmatg.DtoVolR[J] > 0  THEN DO:
                IF X-RANGO  = 0 THEN X-RANGO = Almmmatg.DtoVolR[J].
                IF X-RANGO <= Almmmatg.DtoVolR[J] THEN DO:
                    X-RANGO  = Almmmatg.DtoVolR[J].
                    F-DSCTOS = Almmmatg.DtoVolD[J].         
                    F-PREVTA = Almmmatg.Prevta[1] * (1 - F-DSCTOS / 100).
                    Y-DSCTOS = Almmmatg.DtoVolD[J] .
                    IF Almmmatg.MonVta = 1 THEN 
                       ASSIGN X-PREVTA1 = F-PREVTA
                              X-PREVTA2 = ROUND(F-PREVTA / Almmmatg.TpoCmb,6).
                    ELSE
                       ASSIGN X-PREVTA2 = F-PREVTA
                              X-PREVTA1 = ROUND(F-PREVTA * Almmmatg.TpoCmb,6).
                    X-PREVTA1 = X-PREVTA1 * F-FACTOR.
                    X-PREVTA2 = X-PREVTA2 * F-FACTOR.                                        
                END.   
            END.   
        END.
    END.
    /* PRECIO FINAL */
    IF X-PREVTA1 + X-PREVTA2 > 0 THEN DO:
        IF S-CODMON = 1 
        THEN F-PREVTA = X-PREVTA1.
        ELSE F-PREVTA = X-PREVTA2.     
    END.    
END.

/* RECALCULAMOS EL PRECIO BASE */
F-PREBAS = F-PREVTA / ( 1 - F-DSCTOS / 100 ).

/************************************************/
RUN BIN/_ROUND1(F-PREVTA,X-NRODEC,OUTPUT F-PREVTA).

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

