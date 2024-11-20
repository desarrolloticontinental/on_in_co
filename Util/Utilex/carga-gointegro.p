DEF VAR x-linea AS CHAR.
DEF VAR k AS INT INIT 1.

INPUT FROM d:\tmp\gointegro.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE vtadtabla.
    ASSIGN
        VtaDTabla.CodCia = 001
        VtaDTabla.Tabla = 'UTILEX-ENCARTE'
        VtaDTabla.Llave = '1242'
        VtaDTabla.Tipo = "CA"
        VtaDTabla.LlaveDetalle = STRING(k, '99999999999')
        VtaDTabla.Libre_c01 = x-linea.
    k = k + 1.

END.
INPUT CLOSE.

