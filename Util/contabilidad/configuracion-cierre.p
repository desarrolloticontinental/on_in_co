DEF VAR s-codcia AS INT INIT 001.
DEF VAR cb-codcia AS INT INIT 000.

OUTPUT TO c:\tmp\cofg-cierre.txt.
FOR EACH cb-tbal NO-LOCK WHERE cb-tbal.CodCia = s-CodCia 
    AND cb-tbal.TpoBal = "C":
    FOR EACH cb-nbal NO-LOCK WHERE cb-nbal.CodCia = cb-tbal.CodCia 
        AND cb-nbal.TpoBal = cb-tbal.TpoBal 
        AND cb-nbal.CodBal = cb-tbal.CodBal :
        FOR EACH cb-dbal NO-LOCK WHERE cb-dbal.CodCia = cb-CodCia 
            AND cb-dbal.TpoBal = cb-nbal.TpoBal
            AND cb-dbal.CodBal = cb-nbal.CodBal
            AND cb-dbal.ForBal = cb-nbal.ForBal
            AND cb-dbal.Item   = cb-nbal.ITEM:
            DISPLAY 
                cb-tbal.codbal
                cb-tbal.desbal
                cb-nbal.ITEM 
                cb-nbal.nota 
                cb-nbal.glosa
                cb-dbal.codcta
                cb-dbal.codaux
                cb-dbal.signo
                cb-dbal.metodo
                WITH STREAM-IO NO-BOX WIDTH 320.
        END.
    END.
END.
OUTPUT CLOSE.


