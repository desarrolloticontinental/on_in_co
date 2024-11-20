DEF NEW SHARED VAR s-codcia AS INT INIT 001.
DEF NEW SHARED VAR s-coddiv AS CHAR INIT '00519'.
DEF NEW SHARED VAR cl-codcia AS INT INIT 000.

/* ICBPER */
DEFINE VAR x-articulo-ICBPER AS CHAR.

x-articulo-ICBPER = '099268'.

DEF TEMP-TABLE ITEM LIKE Facdpedi.
DEF VAR f-PreBas AS DEC NO-UNDO.
DEF VAR f-PreVta AS DEC NO-UNDO.
DEF VAR f-FleteUnitario AS DEC NO-UNDO.

FOR EACH faccpedi EXCLUSIVE-LOCK WHERE codcia = s-codcia
    AND coddoc = 'cot' 
    AND coddiv = s-coddiv:
    DISPLAY faccpedi.coddoc faccpedi.nroped faccpedi.fchped. PAUSE 0.
    EMPTY TEMP-TABLE ITEM.
    FOR EACH facdpedi OF faccpedi NO-LOCK:
        CREATE ITEM.
        BUFFER-COPY Facdpedi TO ITEM.
        /* Los precios están SIN IGV */
        IF ITEM.AftIgv = YES 
            THEN ITEM.preuni = ROUND(ITEM.preuni * ( 1 + ( Faccpedi.porigv / 100 )), 4).
        ASSIGN
            f-PreVta = ITEM.PreUni
            f-PreBas = ITEM.PreUni
            f-FleteUnitario = ITEM.Libre_d02.
        /* ************************* */
        FIND Almmmatg WHERE Almmmatg.CodCia = S-CODCIA AND Almmmatg.codmat = ITEM.codmat NO-LOCK NO-ERROR.
        /* Code placed here will execute AFTER standard behavior.    */
        ASSIGN 
            ITEM.PreBas = F-PreBas 
            /*ITEM.PreVta[1] = F-PreVta     /* CONTROL DE PRECIO DE LISTA */*/
            .
        ASSIGN
            ITEM.ImpLin = ROUND ( ITEM.CanPed * ITEM.PreUni * 
                          ( 1 - ITEM.Por_Dsctos[1] / 100 ) *
                          ( 1 - ITEM.Por_Dsctos[2] / 100 ) *
                          ( 1 - ITEM.Por_Dsctos[3] / 100 ), 2 ).
        IF ITEM.Por_Dsctos[1] = 0 AND ITEM.Por_Dsctos[2] = 0 AND ITEM.Por_Dsctos[3] = 0 
            THEN ITEM.ImpDto = 0.
            ELSE ITEM.ImpDto = ITEM.CanPed * ITEM.PreUni - ITEM.ImpLin.
        /* ***************************************************************** */
        ASSIGN
            ITEM.ImpLin = ROUND(ITEM.ImpLin, 2)
            ITEM.ImpDto = ROUND(ITEM.ImpDto, 2).
        IF ITEM.AftIsc 
        THEN ITEM.ImpIsc = ROUND(ITEM.PreBas * ITEM.CanPed * (Almmmatg.PorIsc / 100),4).
        ELSE ITEM.ImpIsc = 0.
        IF ITEM.AftIgv 
        THEN ITEM.ImpIgv = ITEM.ImpLin - ROUND( ITEM.ImpLin  / ( 1 + (Faccpedi.PorIgv / 100) ), 4 ).
        ELSE ITEM.ImpIgv = 0.
    END.
    FOR EACH Facdpedi OF Faccpedi EXCLUSIVE-LOCK:
        DELETE Facdpedi.
    END.
    FOR EACH ITEM:
        CREATE Facdpedi.
        BUFFER-COPY ITEM TO Facdpedi.
    END.

    RUN Graba-Totales.
END.


PROCEDURE Graba-Totales:
/* ******************** */

    DEFINE VARIABLE F-IGV AS DECIMAL NO-UNDO.
    DEFINE VARIABLE F-ISC AS DECIMAL NO-UNDO.
    DEFINE VARIABLE x-Dto2xExonerados AS DEC NO-UNDO.
    DEFINE VARIABLE x-Dto2xAfectosIgv AS DEC NO-UNDO.

    ASSIGN
        FacCPedi.ImpDto = 0
        FacCPedi.ImpDto2 = 0
        FacCPedi.ImpIgv = 0
        FacCPedi.ImpIsc = 0
        FacCPedi.ImpTot = 0
        FacCPedi.ImpExo = 0
        FacCPedi.Importe[3] = 0
        F-IGV = 0
        F-ISC = 0
        x-Dto2xExonerados = 0
        x-Dto2xAfectosIgv = 0.
    /* RHC 15/08/19 pedido por G.P.: Impuesto a la bolsa plástica */
    ASSIGN
        Faccpedi.AcuBon[10] = 0.

    /* VENTAS INAFECTAS A IGV */
    IF FacCPedi.FlgIgv = NO THEN DO:
        FacCPedi.PorIgv = 0.00.
        FOR EACH FacDPedi OF FacCPedi:
            ASSIGN
                FacDPedi.AftIgv = NO
                FacDPedi.ImpIgv = 0.00.
        END.
    END.

    FOR EACH FacDPedi OF FacCPedi NO-LOCK, FIRST Almmmatg OF FacDPedi NO-LOCK:
        F-Igv = F-Igv + FacDPedi.ImpIgv.
        F-Isc = F-Isc + FacDPedi.ImpIsc.

        FacCPedi.ImpTot = FacCPedi.ImpTot + FacDPedi.ImpLin.
        FacCPedi.ImpDto2 = FacCPedi.ImpDto2 + FacDPedi.ImpDto2.

        IF FacDPedi.AftIgv = YES
        THEN FacCPedi.ImpDto = FacCPedi.ImpDto + ROUND(FacDPedi.ImpDto / (1 + FacCPedi.PorIgv / 100), 2).
        ELSE FacCPedi.ImpDto = FacCPedi.ImpDto + FacDPedi.ImpDto.

        IF facdpedi.codmat = x-articulo-ICBPER THEN DO:
            /* Inafecto */
            ASSIGN Faccpedi.AcuBon[10] = Faccpedi.AcuBon[10] + facdpedi.implin.
        END.
        ELSE DO:
            IF NOT FacDPedi.AftIgv THEN FacCPedi.ImpExo = FacCPedi.ImpExo + FacDPedi.ImpLin.

            IF NOT FacDPedi.AftIgv THEN x-Dto2xExonerados = x-Dto2xExonerados + FacDPedi.ImpDto2.
            ELSE x-Dto2xAfectosIgv = x-Dto2xAfectosIgv + FacDPedi.ImpDto2.
        END.
    END.
    ASSIGN
        FacCPedi.ImpIgv = ROUND(F-IGV,2)
        FacCPedi.ImpIsc = ROUND(F-ISC,2).
    ASSIGN
        FacCPedi.ImpVta = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpIgv - Faccpedi.AcuBon[10].
    ASSIGN
        FacCPedi.ImpBrt = FacCPedi.ImpVta + FacCPedi.ImpDto
        FacCPedi.Importe[1] = FacCPedi.ImpTot.    /* Guardamos el importe original */
    /* RHC 06/05/2014 En caso tenga descuento por Encarte */
    IF Faccpedi.ImpDto2 > 0 THEN DO:
        ASSIGN
            Faccpedi.ImpTot = Faccpedi.ImpTot - Faccpedi.ImpDto2
            Faccpedi.ImpIgv = Faccpedi.ImpIgv -  ~
                ROUND(x-Dto2xAfectosIgv / ( 1 + Faccpedi.PorIgv / 100) * Faccpedi.PorIgv / 100, 2)
            Faccpedi.ImpExo = Faccpedi.ImpExo - x-Dto2xExonerados
            FacCPedi.ImpVta = FacCPedi.ImpTot - FacCPedi.ImpExo - FacCPedi.ImpIgv - Faccpedi.AcuBon[10]
            FacCPedi.ImpBrt = FacCPedi.ImpVta + FacCPedi.ImpDto.
    END.
    /* ******************************************** */
    /* RHC 15/08/19 IMPUESTO A LA BOLSA DE PLASTICO */
    /* ******************************************** */
    ASSIGN
        Faccpedi.ImpTot = Faccpedi.ImpTot /*+ Faccpedi.AcuBon[10]*/.

END PROCEDURE.
