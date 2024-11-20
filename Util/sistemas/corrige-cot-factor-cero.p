DEF NEW SHARED VAR s-codcia AS INT.
DEF NEW SHARED VAR s-coddiv AS CHAR.

DISABLE TRIGGERS FOR LOAD OF facdpedi.
DISABLE TRIGGERS FOR LOAD OF faccpedi.

DEF BUFFER b-cpedi FOR faccpedi.
DEF BUFFER b-dpedi FOR facdpedi.

DEF TEMP-TABLE t-cpedi LIKE faccpedi.

ASSIGN
    s-codcia = 001
    s-coddiv = '00015,00018,10060,10067,10065,10011,10031,10032,10038,10039'.

FOR EACH b-cpedi NO-LOCK WHERE b-cpedi.codcia = s-codcia AND
    LOOKUP(b-cpedi.coddiv, s-coddiv) > 0 AND
    b-cpedi.coddoc = 'cot' AND
    b-cpedi.fchped >= DATE(09,20,2019) AND
    b-cpedi.flgest = 'P':
    FIND FIRST b-dpedi OF b-cpedi WHERE b-dpedi.factor = 0 OR 
        b-dpedi.preuni = 0 NO-LOCK NO-ERROR.
    IF AVAILABLE b-dpedi THEN DO:
        CREATE t-cpedi.
        BUFFER-COPY b-cpedi TO t-cpedi.
    END.
END.

DEFINE VARIABLE S-UNDVTA AS CHAR NO-UNDO.
DEFINE VARIABLE F-FACTOR AS DECI NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE X-CANPED AS DECI NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE x-TipDto AS CHAR NO-UNDO.
DEFINE VARIABLE f-FleteUnitario AS DEC DECIMALS 6 NO-UNDO.

DEF VAR x-articulo-ICBPer AS CHAR INIT '099268'.

FOR EACH t-cpedi, FIRST faccpedi OF t-cpedi EXCLUSIVE-LOCK:
    s-coddiv = Faccpedi.coddiv.
    FOR EACH facdpedi OF faccpedi EXCLUSIVE-LOCK WHERE facdpedi.factor = 0 OR facdpedi.preuni = 0,
        FIRST Almmmatg OF Facdpedi NO-LOCK:
        ASSIGN
            facdpedi.Factor = 1.
        ASSIGN
            F-FACTOR = facdpedi.Factor
            x-CanPed = facdpedi.CanPed
            s-UndVta = facdpedi.UndVta
            f-PreVta = facdpedi.PreUni
            f-PreBas = facdpedi.PreBas
            f-Dsctos = facdpedi.PorDto
            z-Dsctos = facdpedi.Por_Dsctos[2]
            y-Dsctos = facdpedi.Por_Dsctos[3]
            f-FleteUnitario = 0.

        RUN vta2/precio-de-venta-eventos (
            Faccpedi.TpoPed,
            Faccpedi.Libre_c01,
            Faccpedi.CodCli,
            Faccpedi.CodMon,
            INPUT-OUTPUT s-UndVta,
            OUTPUT f-Factor,
            Almmmatg.CodMat,
            Faccpedi.FmaPgo,
            x-CanPed,
            Faccpedi.Libre_d01,
            OUTPUT f-PreBas,
            OUTPUT f-PreVta,
            OUTPUT f-Dsctos,
            OUTPUT y-Dsctos,
            OUTPUT x-TipDto,
            "",
            OUTPUT f-FleteUnitario,
            TRUE
            ).

        ASSIGN 
            facdpedi.Factor = f-Factor
            facdpedi.UndVta = s-UndVta
            facdpedi.PreUni = F-PREVTA
            facdpedi.Libre_d02 = f-FleteUnitario    /* Flete Unitario */
            facdpedi.PreBas = F-PreBas 
            facdpedi.PreVta[1] = F-PreVta   /* CONTROL DE PRECIO DE LISTA */
            facdpedi.PorDto = F-DSCTOS      /* Ambos descuentos afectan */
            facdpedi.PorDto2 = 0            /* el precio unitario */
            facdpedi.Por_Dsctos[2] = z-Dsctos
            facdpedi.Por_Dsctos[3] = Y-DSCTOS 
            facdpedi.AftIgv = (IF Faccpedi.FmaPgo = '900' THEN NO ELSE Almmmatg.AftIgv)
            facdpedi.AftIsc = Almmmatg.AftIsc
            facdpedi.ImpIsc = 0
            facdpedi.ImpIgv = 0
            facdpedi.Libre_c04 = x-TipDto.
        ASSIGN
            facdpedi.ImpLin = ROUND ( facdpedi.CanPed * facdpedi.PreUni * 
                        ( 1 - facdpedi.Por_Dsctos[1] / 100 ) *
                        ( 1 - facdpedi.Por_Dsctos[2] / 100 ) *
                        ( 1 - facdpedi.Por_Dsctos[3] / 100 ), 2 ).
        IF facdpedi.Por_Dsctos[1] = 0 AND facdpedi.Por_Dsctos[2] = 0 AND facdpedi.Por_Dsctos[3] = 0 
            THEN facdpedi.ImpDto = 0.
        ELSE facdpedi.ImpDto = facdpedi.CanPed * facdpedi.PreUni - facdpedi.ImpLin.
        /* RHC 04/08/2015 Si existe f-FleteUnitario se recalcula el Descuento */
        IF f-FleteUnitario > 0 THEN DO:
          /* El flete afecta el monto final */
          IF facdpedi.ImpDto = 0 THEN DO:       /* NO tiene ningun descuento */
              ASSIGN
                  facdpedi.PreUni = ROUND(f-PreVta + f-FleteUnitario, INTEGER(Faccpedi.libre_d01))  /* Incrementamos el PreUni */
                  facdpedi.ImpLin = facdpedi.CanPed * facdpedi.PreUni.
          END.
          ELSE DO:      /* CON descuento promocional o volumen */
              ASSIGN
                  facdpedi.ImpLin = facdpedi.ImpLin + (facdpedi.CanPed * f-FleteUnitario)
                  facdpedi.PreUni = ROUND( (facdpedi.ImpLin + facdpedi.ImpDto) / facdpedi.CanPed, INTEGER(Facdpedi.libre_d01)).
          END.
        END.
        /* ***************************************************************** */
        ASSIGN
            facdpedi.ImpLin = ROUND(facdpedi.ImpLin, 2)
            facdpedi.ImpDto = ROUND(facdpedi.ImpDto, 2).
        IF facdpedi.AftIsc THEN 
            facdpedi.ImpIsc = ROUND(facdpedi.PreBas * facdpedi.CanPed * (Almmmatg.PorIsc / 100),4).
        ELSE facdpedi.ImpIsc = 0.
        IF facdpedi.AftIgv THEN  
            facdpedi.ImpIgv = facdpedi.ImpLin - ROUND(facdpedi.ImpLin  / (1 + (Faccpedi.PorIgv / 100)),4).
        ELSE facdpedi.ImpIgv = 0.
    END.
    RUN graba-totales.

END.

PROCEDURE graba-totales:

    {vta2/graba-totales-cotizacion-cred.i}

END PROCEDURE.


