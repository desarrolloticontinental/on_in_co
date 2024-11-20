DEF VAR x-newtc AS DEC DECIMALS 6 NO-UNDO.
DEF VAR x-factor AS DEC DECIMALS 10 NO-UNDO.
x-newtc = 3.10.
FOR EACH almmmatg WHERE almmmatg.codcia = 001
    AND codfam = '010' BY codmat:
    DISPLAY almmmatg.codmat WITH STREAM-IO.
    PAUSE 0.
    ASSIGN x-factor = almmmatg.tpocmb / x-newtc.
    IF almmmatg.monvta = 2 THEN DO: /* Solo cuando es US$ */
        /* Precio FER */
        FOR EACH vtalistamay WHERE vtalistamay.codcia = 001
            AND vtalistamay.codmat = almmmatg.codmat:
            ASSIGN  
                VtaListaMay.TpoCmb = x-newtc 
                VtaListaMay.PreOfi = VtaListaMay.PreOfi *  x-factor.
        END.
        /* Precio MIN */
        FOR EACH vtalistamingn WHERE vtalistamingn.codcia = 001
            AND vtalistamingn.codmat = almmmatg.codmat:
            ASSIGN
                VtaListaMinGn.PreOfi = VtaListaMinGn.PreOfi * x-factor
                VtaListaMinGn.TpoCmb = x-factor.
        END.
        ASSIGN
            Almmmatg.Prevta[1] = ROUND(Almmmatg.Prevta[1] * x-factor, 4)
            Almmmatg.Prevta[2] = ROUND(Almmmatg.Prevta[2] * x-factor, 4)
            Almmmatg.Prevta[3] = ROUND(Almmmatg.Prevta[3] * x-factor, 4)
            Almmmatg.Prevta[4] = ROUND(Almmmatg.Prevta[4] * x-factor, 4)
            Almmmatg.PreOfi = ROUND(Almmmatg.PreOfi * x-factor, 4).
    END.
    ASSIGN
        almmmatg.tpocmb = x-newtc.
END.
