def var x-codmon as int init 1.
def var x-codcta as char.
def var s-codcia as int init 001.
def var s-periodo as int init 2006.
def var x-debe as dec  format '->>>,>>>,>>9.99'.
def var x-haber as dec format '->>>,>>>,>>9.99'.
def var s-nromes as int init 12.
def var x-saldoan as dec format '->>>,>>>,>>9.99'.
def var x-saldoac as dec format '->>>,>>>,>>9.99'.
def var x-totdebe  as dec format '->>>,>>>,>>9.99'.
def var x-tothaber as dec format '->>>,>>>,>>9.99'.

output to c:\tmp\saldos.txt.
for each cb-ctas where codcia = 0 and length(trim(codcta)) = 6
    and codcta >= '42' and codcta <= '469999':
    assign
        x-codcta = cb-ctas.codcta
        x-saldoan = 0
        x-saldoac = 0
        x-totdebe = 0
        x-tothaber = 0.
    RUN Detalle.
    x-saldoac = x-saldoan + x-totdebe - x-tothaber.
    DISPLAY 
        x-codcta
        x-saldoan
        x-totdebe    
        x-tothaber   
        x-saldoac
        WITH DOWN STREAM-IO NO-BOX WIDTH 200.
end.
output close.


PROCEDURE Detalle:

    FOR EACH cb-dmov NO-LOCK WHERE cb-dmov.codcia = s-codcia
                            AND cb-dmov.periodo = s-periodo 
                            AND cb-dmov.nromes <= s-NroMes
                            AND cb-dmov.codcta  = x-codcta:
        IF NOT tpomov THEN DO:
            CASE x-codmon:
            WHEN 1 THEN DO:
                x-debe  = ImpMn1.
                x-haber = 0.
            END.
            WHEN 2 THEN DO:
                x-debe  = ImpMn2.
                x-haber = 0.
            END.
            END CASE.
        END.
        ELSE DO:      
            CASE x-codmon:
            WHEN 1 THEN DO:
                x-debe  = 0.
                x-haber = ImpMn1.
            END.
            WHEN 2 THEN DO:
                x-debe  = 0.
                x-haber = ImpMn2.
            END.
            END CASE.            
        END.
        
        IF cb-dmov.nromes = 0 THEN DO:
            x-saldoan = x-saldoan + (x-debe - x-haber).
        END.
        ELSE DO:
            x-totdebe = x-totdebe + x-debe.
            x-tothaber = x-tothaber + x-haber.
        END.
        
   END.
   
END PROCEDURE.
