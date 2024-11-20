/* Carga CIIU a la tabla almtabla.Tabla = 'GN' */
FOR EACH almtabla WHERE almtabla.tabla = 'GN':
    DELETE almtabla.
END.
DEF VAR x-linea AS CHAR NO-UNDO.
INPUT FROM d:\tmp\ciiu.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    CREATE almtabla.
    ASSIGN
        tabla = 'GN'
        codigo = SUBSTRING(x-linea,1,15)
        nombre = SUBSTRING(x-linea,16).
END.
INPUT CLOSE.

