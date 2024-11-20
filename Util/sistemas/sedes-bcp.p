DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-nomdep AS CHAR FORMAT 'x(30)'.
DEF VAR x-nompro AS CHAR FORMAT 'x(30)'.
DEF VAR x-nomdis AS CHAR FORMAT 'x(30)'.
DEF VAR x-dircli AS CHAR.
DEF VAR x-Sede AS INT NO-UNDO.

DEF VAR s-codcli AS CHAR INIT '20382036655'.    /* Mi Banco */

INPUT FROM d:\tmp\sedes.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-dircli = SUBSTRING(x-linea,1,120)
        x-nomdep = SUBSTRING(x-linea,121,30)
        x-nompro = SUBSTRING(x-linea,151,30)
        x-nomdis = SUBSTRING(x-linea,181,30).
    FIND TabDepto WHERE TabDepto.NomDepto = x-nomdep NO-LOCK NO-ERROR.
    IF NOT AVAILABLE TabDepto THEN DO:
        DISPLAY x-nomdep. LEAVE.
    END.
    FIND TabProvi WHERE TabProvi.CodDepto = TabDepto.CodDepto AND
        TabProvi.NomProvi = x-nompro NO-LOCK NO-ERROR.
    IF NOT AVAILABLE TabProvi THEN DO:
        DISPLAY x-nomdep x-nompro WITH STREAM-IO NO-BOX . LEAVE.
    END.
    FIND TabDistr WHERE TabDistr.CodDepto = TabProvi.CodDepto AND
        TabDistr.CodProvi = TabProvi.CodProvi AND
        TabDistr.NomDistr = x-nomdis NO-LOCK NO-ERROR.
    IF NOT AVAILABLE TabDistr THEN DO:
        DISPLAY x-nomdep x-nompro x-nomdis WITH STREAM-IO NO-BOX . LEAVE.
    END.
    x-Sede = 0.
    FOR EACH gn-clied NO-LOCK WHERE codcia = 0 and codcli = s-codcli
        AND sede <> "@@@"
        BY sede:
        x-Sede = MAXIMUM(INTEGER(gn-clied.sede), x-Sede).
    END.

    x-Sede = x-Sede + 1.
    CREATE gn-clied.
    ASSIGN
        gn-clied.codcia = 0 
        gn-clied.codcli = s-codcli
        gn-clied.sede = STRING(x-Sede,'9999')
        gn-clied.domfiscal = NO
        gn-clied.swsedesunat = "M"
        gn-clied.dircli = x-dircli
        Gn-ClieD.CodDept = TabDistr.CodDepto 
        Gn-ClieD.CodProv = TabDistr.CodProvi 
        Gn-ClieD.CodDist = TabDistr.CodDistr
        Gn-ClieD.Codpos = TabDistr.CodPos.
    ASSIGN
        Gn-ClieD.FchModificacion = TODAY
        Gn-ClieD.UsrModificacion = "SYSTEM".
END.
INPUT CLOSE.
