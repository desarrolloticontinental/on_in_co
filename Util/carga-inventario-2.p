DEF VAR x-linea AS CHAR FORMAT 'x(50)'.

INPUT FROM m:\tmp\ate.prn.
REPEAT:
    IMPORT UNFORMATTED x-linea.
    /*
    DISPLAY
        SUBSTRING(x-linea,1,6)
        decimal(SUBSTRING(x-linea,7,16)) FORMAT '>>>,>>>,>>9.99'
        SUBSTRING(x-linea,24,3).
    */
    IF SUBSTRING(x-linea,1,6) <> ''  THEN DO:
        CREATE invconteo.
        ASSIGN 
               InvConteo.CodCia      = 001
               InvConteo.CodAlm      = SUBSTRING(x-linea,24,3)
               InvConteo.CodMat      = SUBSTRING(x-linea,1,6)
               InvConteo.FchInv      = 12/06/08
               InvConteo.CanBien     = decimal(SUBSTRING(x-linea,7,16))
               InvConteo.CanInv      = decimal(SUBSTRING(x-linea,7,16))
               InvConteo.Responsable = ''.
        CREATE invrecont.
        ASSIGN
               InvRecont.CodCia      = 001
               InvRecont.CodAlm      = SUBSTRING(x-linea,24,3)
               InvRecont.FchInv      = 12/06/08
               InvRecont.codmat      = InvConteo.codmat
               InvRecont.CanBien     = InvConteo.CanBien
               InvRecont.CanInv      = InvConteo.CanInv
               InvRecont.Responsable = ''.
    END.
    
END.




