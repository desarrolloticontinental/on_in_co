DEF VAR x-linea AS CHAR NO-UNDO.
DEF VAR x-codmat AS CHAR.
DEF VAR x-candes AS DEC.
INPUT FROM d:\tmp\codigos.prn.
OUTPUT TO d:\tmp\compras.txt.
PUT UNFORMATTED 
    'ALMACEN|TIP|MOV|FECHA|SERIE|NUMERO|O/C|ARTICULO|CANTIDAD|UNIDAD|TOTAL'
    SKIP.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    IF x-linea = '' THEN LEAVE.
    ASSIGN
        x-codmat = SUBSTRING(x-linea,1,6)
        x-candes = DEC(SUBSTRING(x-linea,11))
        NO-ERROR.
    FOR EACH almdmov NO-LOCK USE-INDEX almd02 WHERE codcia = 1
        AND codmat = x-codmat
        AND tipmov = 'i'
        AND codmov = 90
        AND fchdoc >= 01/01/17
        AND fchdoc <= 03/31/17,
        FIRST almcmov OF almdmov NO-LOCK:
        PUT UNFORMATTED
            Almcmov.CodAlm '|'
            Almcmov.TipMov '|'
            Almcmov.CodMov '|'
            Almcmov.FchDoc '|'
            Almcmov.Nroser '|'
            Almcmov.NroDoc '|'
            Almcmov.NroRf1 '|'
            Almdmov.codmat '|'
            Almdmov.CanDes '|'
            Almdmov.CodUnd '|'
            DEC(SUBSTRING(x-linea,11))
            SKIP.
        x-candes = x-candes - Almdmov.CanDes.
        IF x-candes <= 0 THEN LEAVE.
    END.
END.
INPUT CLOSE.
OUTPUT CLOSE.

