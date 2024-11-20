DEF VAR x-nombre LIKE DimProducto.DesMat.

FOR EACH dimproducto BY codmat DESC:
    RUN lib/limpiar-texto (dimproducto.desmat, "", OUTPUT x-nombre).
    IF dimproducto.desmat <> x-nombre THEN
        DISPLAY codmat dimproducto.desmat x-nombre WITH 1 COL STREAM-IO NO-BOX.
END.
        
/*
DEF INPUT PARAMETER cadenaTexto AS CHAR.
DEF INPUT PARAMETER sustituirPor AS CHAR.
DEF OUTPUT PARAMETER cadenaResultado AS CHAR.
*/
