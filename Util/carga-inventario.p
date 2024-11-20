DEF VAR x-linea AS CHAR FORMAT 'x(50)'.
DEF VAR x-fecha AS DATE.
DEF VAR x-codalm AS CHAR.

x-fecha = 12/06/08.
x-codalm = '85'.

INPUT FROM m:\tmp\alm85cissac.prn.
inventario:
REPEAT:
    IMPORT UNFORMATTED x-linea.
    /*
    DISPLAY
        SUBSTRING(x-linea,1,6)
        decimal(SUBSTRING(x-linea,7,14)) FORMAT '>>>,>>>,>>9.99'.
    */
    IF SUBSTRING(x-linea,1,6) <> ''  THEN DO:
        FIND invconteo WHERE invconteo.codcia = 001
            AND invconteo.codalm = x-codalm
            AND invconteo.codmat = SUBSTRING(x-linea,1,6)
            AND invconteo.fchinv = x-fecha
            NO-LOCK NO-ERROR.
        IF AVAILABLE invconteo THEN NEXT inventario.
        CREATE invconteo.
        ASSIGN 
               InvConteo.CodCia      = 001
               InvConteo.CodAlm      = x-codalm
               InvConteo.CodMat      = SUBSTRING(x-linea,1,6)
               InvConteo.FchInv      = x-fecha
               InvConteo.CanBien     = decimal(SUBSTRING(x-linea,7,14))
               InvConteo.CanInv      = decimal(SUBSTRING(x-linea,7,14))
               InvConteo.Responsable = ''.
        CREATE invrecont.
        ASSIGN
               InvRecont.CodCia      = 001
               InvRecont.CodAlm      = x-codalm
               InvRecont.FchInv      = x-fecha
               InvRecont.codmat      = InvConteo.codmat
               InvRecont.CanBien     = InvConteo.CanBien
               InvRecont.CanInv      = InvConteo.CanInv
               InvRecont.Responsable = ''.
    END.
    
END.
