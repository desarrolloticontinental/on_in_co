DEF TEMP-TABLE PEDI LIKE Facdpedi
    FIELD ImpLin2 LIKE Facdpedi.implin
    FIELD ImpDto2 LIKE Facdpedi.impdto
    FIELD PreUni2 LIKE Facdpedi.preuni.

DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00015' NO-UNDO.
DEFINE VARIABLE F-PREBAS LIKE Almmmatg.PreBas NO-UNDO.
DEFINE VARIABLE F-DSCTOS LIKE Almmmatg.PorMax NO-UNDO.
DEFINE VARIABLE X-CANPED AS DECI NO-UNDO.
DEFINE VARIABLE F-PREVTA AS DECI NO-UNDO.
DEFINE VARIABLE Y-DSCTOS AS DECI NO-UNDO.
DEFINE VARIABLE Z-DSCTOS AS DECI NO-UNDO.       /* DESCUENTO EXPOLIBRERIA */
    

FOR EACH faccpedi NO-LOCK WHERE codcia = s-codcia
    AND coddoc = 'cot'
    AND coddiv = s-coddiv
    AND fchped = 11/30/09
    AND flgest <> 'a',
    EACH facdpedi OF faccpedi NO-LOCK,
    FIRST almmmatg OF facdpedi NO-LOCK:
    FIND Almtconv WHERE 
         Almtconv.CodUnid  = Almmmatg.UndBas AND  
         Almtconv.Codalter = Almmmatg.Chr__01 
         NO-LOCK NO-ERROR.
    CREATE PEDI.
    BUFFER-COPY Facdpedi TO PEDI.
    x-CanPed = PEDI.CanPed.
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
        Pedi.PreUni2 = f-PreVta
        Pedi.PorDto2 = z-Dsctos
        Pedi.implin2 = Pedi.implin2 + ROUND( f-PreVta * Facdpedi.CanPed , 2 ) - ROUND( f-PreVta * Facdpedi.CanPed * (z-Dsctos / 100),4 )
        Pedi.impdto2 = Pedi.impdto2 + ROUND( f-PreVta * Facdpedi.CanPed * (z-Dsctos / 100),4 ).
END.
    
OUTPUT TO c:\tmp\diferencias3.txt.    
FOR EACH pedi NO-LOCK,
    FIRST faccpedi OF pedi NO-LOCK,
    FIRST almmmatg OF pedi NO-LOCK:
    DISPLAY
        pedi.fchped
        '|'
        pedi.nroped
        '|'
        faccpedi.nomcli
        '|'
        faccpedi.fmapgo
        '|'
        faccpedi.codven
        '|'
        pedi.codmat
        '|'
        almmmatg.desmat
        '|'
        almmmatg.desmar
        '|'
        almmmatg.codfam
        '|'
        pedi.canped
        '|'
        pedi.undvta
        '|'
        pedi.preuni
        '|'
        pedi.por_dsctos[1]
        '|'
        pedi.implin
        '|'
        pedi.preuni2
        '|'
        pedi.pordto2
        '|'
        pedi.implin2
        WITH STREAM-IO NO-BOX NO-LABELS WIDTH 320.
END.
OUTPUT CLOSE.
