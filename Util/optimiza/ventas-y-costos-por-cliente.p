DEF TEMP-TABLE detalle LIKE gn-clie
    INDEX llave01 codcia codcli.
DEF TEMP-TABLE t-clie LIKE gn-clie
    INDEX llave01 codcia codcli.
DEF VAR x-linea AS CHAR FORMAT 'x(120)'.

INPUT FROM c:\tmp\clientes.prn.
REPEAT :
    IMPORT UNFORMATTED x-linea.
    CREATE detalle.
    detalle.nomcli = x-linea.
END.
INPUT CLOSE.

FOR EACH detalle WHERE detalle.nomcli <> '':
    DISPLAY detalle.nomcli.
    PAUSE 0.
    FOR EACH gn-clie WHERE gn-clie.nomcli = detalle.nomcli 
        AND gn-clie.codcia = 0
        NO-LOCK:
        CREATE t-clie.
        BUFFER-COPY gn-clie TO t-clie NO-ERROR.
    END.
END.

DEF VAR x-ventas AS DEC FORMAT '(>>>,>>>,>>9.99)'.
DEF VAR x-costos AS DEC FORMAT '(>>>,>>>,>>9.99)'.

OUTPUT TO c:\tmp\FINAL.txt.
FOR EACH t-clie WHERE t-clie.codcia = 0 BY t-clie.nomcli:
    ASSIGN
        x-ventas = 0
        x-costos = 0.
    FOR EACH  evtclarti NO-LOCK WHERE evtclarti.Codano = 2009
        AND evtclarti.CodCia = 1
        AND evtclarti.CodCli = t-clie.codcli
        AND evtclarti.CodDiv = '00000'
        AND evtclarti.Codmes >= 01
        AND evtclarti.codmes <= 05:
        x-ventas = x-ventas + evtclarti.VtaxMesMn.
        x-costos = x-costos + evtclarti.CtoxMesMn.
    END.
    DISPLAY 
        t-clie.codcli 
        t-clie.nomcli 
        t-clie.clfcli 
        t-clie.flgsit 
        t-clie.cndvta FORMAT 'x(3)'
        x-ventas
        x-costos
        WITH STREAM-IO NO-BOX WIDTH 320.
END.
OUTPUT CLOSE.

