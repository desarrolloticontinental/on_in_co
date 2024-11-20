DISABLE TRIGGERS FOR LOAD OF almmmatg.

DEF VAR s-codcia AS INTE INIT 001 NO-UNDO.
DEF VAR pv-codcia AS INTE INIT 000 NO-UNDO.
DEF VAR s-coddiv AS CHAR INIT '00015' NO-UNDO.

/* FOR EACH almmmatg WHERE codcia = s-codcia */
/*     AND tpoart <> 'D':                    */
/*     tpoart = "D".                         */
/* END.                                      */

FOR EACH VtaCatVenta WHERE VtaCatVenta.CodCia = s-codcia
    AND VtaCatVenta.CodDiv = s-coddiv NO-LOCK,
    EACH gn-prov WHERE gn-prov.CodPro = VtaCatVenta.CodPro
    AND gn-prov.CodCia = pv-codcia NO-LOCK,
    EACH AlmCatVtaC OF VtaCatVenta  NO-LOCK,
    EACH AlmCatVtaD OF AlmCatVtaC NO-LOCK,
    FIRST almmmatg OF almcatvtad:
    almmmatg.tpoart = "A".
    DISPLAY almmmatg.codmat.
    PAUSE 0.
END.

