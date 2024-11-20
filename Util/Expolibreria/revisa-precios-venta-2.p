DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00015' NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE X-CANPED AS DECI NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.       /* DESCUENTO EXPOLIBRERIA */

DEF TEMP-TABLE Detalle
    FIELD CodCia LIKE Facdpedi.codcia
    FIELD CodMat LIKE Facdpedi.codmat
    FIELD UndVta LIKE Facdpedi.undvta
    FIELD FchPed LIKE Faccpedi.fchped
    FIELD CanPed LIKE Facdpedi.canped
    FIELD ImpLin LIKE Facdpedi.implin
    FIELD ImpDto LIKE Facdpedi.impdto
    FIELD PorDto LIKE Facdpedi.pordto
    FIELD PreUni LIKE Facdpedi.preuni
    FIELD ImpLin2 LIKE Facdpedi.implin
    FIELD ImpDto2 LIKE Facdpedi.impdto
    FIELD PorDto2 LIKE Facdpedi.pordto
    FIELD PreUni2 LIKE Facdpedi.preuni
    INDEX Llave01 AS PRIMARY codcia codmat fchped.

FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddoc = 'cot'
    AND coddiv = s-coddiv
    AND fchped >= 11/30/09
    AND flgest <> 'a',
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    FIND Almtconv WHERE 
         Almtconv.CodUnid  = Almmmatg.UndBas AND  
         Almtconv.Codalter = Almmmatg.Chr__01 
         NO-LOCK NO-ERROR.
    FIND Detalle WHERE Detalle.codcia = Facdpedi.codcia
        AND Detalle.codmat = Facdpedi.codmat
        AND Detalle.fchped = Facdpedi.fchped
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Detalle THEN DO:
        CREATE Detalle.
        ASSIGN
            Detalle.codcia = Facdpedi.codcia
            Detalle.codmat = Facdpedi.codmat
            Detalle.fchped = Facdpedi.fchped
            Detalle.undvta = Facdpedi.undvta.
    END.
    ASSIGN
        Detalle.canped = Detalle.canped + Facdpedi.canped
        Detalle.implin = Detalle.implin + Facdpedi.implin
        Detalle.impdto = Detalle.impdto + Facdpedi.impdto
        Detalle.preuni = Facdpedi.preuni
        Detalle.pordto = Facdpedi.Por_Dsctos[1].
    x-CanPed = Facdpedi.CanPed.
    RUN vtaexp/PrecioVenta (s-CodCia,
                        s-CodDiv,
                        Faccpedi.CodCli,
                        Faccpedi.CodMon,
                        Faccpedi.TpoCmb,
                        1,
                        Almmmatg.CodMat,
                        Faccpedi.FmaPgo,
                        x-CanPed,
                        4,
                        OUTPUT f-PreBas,
                        OUTPUT f-PreVta,
                        OUTPUT f-Dsctos,
                        OUTPUT y-Dsctos).

    /* RHC 8.11.05  DESCUENTO ADICIONAL POR EXPOLIBRERIA */
    /* RHC 10.01.08 SOLO SI NO TIENE DESCUENTO PROMOCIONAL */
    z-Dsctos = 0.
    FIND FacTabla WHERE factabla.codcia = s-codcia
        AND factabla.tabla = 'EL'
        AND factabla.codigo = STRING(YEAR(TODAY), '9999')
        NO-LOCK NO-ERROR.
    IF AVAILABLE FacTabla 
        AND y-Dsctos = 0 
        AND LOOKUP(TRIM(Faccpedi.FmaPgo), '000,001') > 0 
        THEN DO:    /* NO Promociones */
        CASE Almmmatg.Chr__02:
            WHEN 'P' THEN z-Dsctos = FacTabla.Valor[1].
            WHEN 'T' THEN z-Dsctos = FacTabla.Valor[2].
        END CASE.
    END.
    IF AVAILABLE FacTabla 
            AND y-Dsctos = 0 
            AND LOOKUP(TRIM(Faccpedi.FmaPgo), '400,401') > 0 
            THEN DO:    /* NO Promociones */
        CASE Almmmatg.Chr__02:
            WHEN 'P' THEN z-Dsctos = FacTabla.Valor[1] /* - 1*/.
            WHEN 'T' THEN z-Dsctos = FacTabla.Valor[2] /* - 2*/.
        END CASE.
    END.
    /* ************************************************* */
    ASSIGN
        Detalle.pordto2 = z-Dsctos
        Detalle.preuni2 = f-PreVta
        Detalle.implin2 = Detalle.implin2 + ROUND( f-PreVta * Facdpedi.CanPed , 2 ) - ROUND( f-PreVta * Facdpedi.CanPed * (z-Dsctos / 100),4 )
        Detalle.impdto2 = Detalle.impdto2 + ROUND( f-PreVta * Facdpedi.CanPed * (z-Dsctos / 100),4 ).
END.
    
OUTPUT TO c:\tmp\diferencias2.txt.    
FOR EACH Detalle NO-LOCK,
    FIRST almmmatg OF Detalle NO-LOCK:
    DISPLAY
        Detalle.fchped
        '|'
        Detalle.codmat
        '|'
        Almmmatg.desmat
        '|'
        Detalle.undvta
        '|'
        Almmmatg.desmar
        '|'
        Almmmatg.codfam
        '|'
        Detalle.canped
        '|'
        Detalle.preuni
        '|'
        Detalle.pordto
        '|'
        Detalle.impdto
        '|'
        Detalle.implin
        '|'
        Detalle.preuni2
        '|'
        Detalle.pordto2
        '|'
        Detalle.impdto2
        '|'
        Detalle.implin2
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.
