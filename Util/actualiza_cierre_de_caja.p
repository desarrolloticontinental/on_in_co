
DEFINE VARIABLE X-HORCIE AS CHARACTER.

X-HORCIE = "18:07".

FOR EACH ccbcierr WHERE
        CcbCierr.CodCia = 1 AND
        CcbCierr.HorCie = X-HORCIE AND
        CcbCierr.usuario = "SLM-200" AND
        CcbCierr.FchCie = 02/26/08 :
        DISPLAY
            CcbCierr.HorCie
            CcbCierr.ImpNac[1]
            CcbCierr.ImpNac[2]
            CcbCierr.ImpNac[3]
            CcbCierr.ImpNac[4]
            CcbCierr.ImpNac[5]
            CcbCierr.ImpNac[6]
            CcbCierr.ImpNac[7]
            CcbCierr.ImpNac[8]
            CcbCierr.ImpNac[9]
            CcbCierr.ImpNac[10]
            CcbCierr.ImpUsa[1]
            CcbCierr.ImpUsa[2]
            CcbCierr.ImpUsa[3]
            CcbCierr.ImpUsa[4]
            CcbCierr.ImpUsa[5]
            CcbCierr.ImpUsa[6]
            CcbCierr.ImpUsa[7]
            CcbCierr.ImpUsa[8]
            CcbCierr.ImpUsa[9]
            CcbCierr.ImpUsa[10]
            WITH 2 COL.
END.

/*
FOR EACH ccbdecl WHERE
        Ccbdecl.FchCie = 02/26/08 AND
        Ccbdecl.CodCia = 1 AND
        Ccbdecl.HorCie = X-HorCie AND
        Ccbdecl.usuario = "SLM-200":
    DISPLAY
        Ccbdecl.ImpNac[1]
        Ccbdecl.ImpNac[2]
        Ccbdecl.ImpNac[3]
        Ccbdecl.ImpNac[4]
        Ccbdecl.ImpNac[5]
        Ccbdecl.ImpNac[6]
        Ccbdecl.ImpNac[7]
        Ccbdecl.ImpNac[8]
        Ccbdecl.ImpNac[9]
        Ccbdecl.ImpNac[10]
        Ccbdecl.ImpUsa[1]
        Ccbdecl.ImpUsa[2]
        Ccbdecl.ImpUsa[3]
        Ccbdecl.ImpUsa[4]
        Ccbdecl.ImpUsa[5]
        Ccbdecl.ImpUsa[6]
        Ccbdecl.ImpUsa[7]
        Ccbdecl.ImpUsa[8]
        Ccbdecl.ImpUsa[9]
        Ccbdecl.ImpUsa[10]
        WITH 2 COL.
*/
