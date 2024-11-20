DEF VAR s-codcia AS INT INIT 001 NO-UNDO.
/* Cargamos informacion */
/* Dice */
DEF VAR x-depto AS CHAR.
DEF VAR x-provi AS CHAR.
DEF VAR x-distr AS CHAR.
DEF VAR x-linea AS CHAR.
/* Debe decir */
DEF VAR y-depto AS CHAR.
DEF VAR y-provi AS CHAR.
DEF VAR y-distr AS CHAR.

INPUT FROM d:\tmp\ubigeoversus.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    /* DICE */
    ASSIGN
        x-depto = SUBSTRING(x-linea,1,5)
        x-provi = SUBSTRING(x-linea,6,5)
        x-distr = SUBSTRING(x-linea,11,5).
    /* DEBE DECIR */
    ASSIGN
        y-depto = SUBSTRING(x-linea,16,5)
        y-provi = SUBSTRING(x-linea,21,5)
        y-distr = SUBSTRING(x-linea,26,5).
    FOR EACH VtaDTabla EXCLUSIVE-LOCK WHERE VtaDTabla.CodCia = s-codcia AND
        VtaDTabla.Tabla = "SZGHR" AND 
        VtaDTabla.LlaveDetalle = "D" AND
        VtaDTabla.Libre_c01 = x-depto AND
        VtaDTabla.Libre_c02 = x-provi AND
        VtaDTabla.Libre_c03 = x-distr:
        ASSIGN
            VtaDTabla.Libre_c01 = y-depto
            VtaDTabla.Libre_c02 = y-provi
            VtaDTabla.Libre_c03 = y-distr.
    END.
    
END.
INPUT CLOSE.
